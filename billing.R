################################################
#                                              #
# Automatisiertes Rechnungen schreiben         #
#                                              #
################################################

rm(list=ls())

# libraries used
library(dplyr)
library(stringr)
library(readr)
library(xtable)
library(rstudioapi)


# set working directory
file_path <- getSourceEditorContext()$path
file_pos <- file_path %>% str_locate_all("/") %>% unlist()
work_path <- file_path %>% str_sub(end = file_pos[length(file_pos)])

# set the path to the folder of Billing
setwd(paste0(work_path, "bills"))

# Select project for billing
project <- "kraut" # if out-commented, this information should be given by stats_sh.R, which executes this file(not jet working).

# variable for version date
VDate <- paste0(format(Sys.time(), "%Y%m%d"), ".csv")

# read the last created time record file
rec <- read_csv(paste0("time_records/", sort(list.files(path = "time_records", pattern = glob2rx("timerecord*")), decreasing = T)[1]),
                col_types = "iDcccTTdddcll")

# Running billing number:
bill_num <- read_file("bill_number") %>% trimws()
bill_num <- unlist(str_split(bill_num, "-"))
year <- format(Sys.time(), "%Y")
bill_num[1] <- year
bill_num[2] <- as.numeric(bill_num[2]) + 1
bill_num[2] <- str_pad(bill_num[2], width = 2, side = "left", pad = "0")
bill_num <- paste(bill_num[1], bill_num[2], sep = "-")


# filter recordings for outstanding working times for the project
bill <- rec %>% dplyr::filter(project == !!project & bill_send != T )
bill_contents <- dplyr::select(bill, AuftrNr)
customer <- bill$customer[1]


# check if billing Items are available.  If not the script will stop
if(is.na(customer)) {print("No outstanding tasks for billing"); stop()}


for (i in bill_contents$AuftrNr) {
  a <- dplyr::filter(rec, AuftrNr == i)
  rec <- dplyr::filter(rec, AuftrNr != i)
  a$RechNr <- bill_num
  a$bill_send <- T
  rec <- bind_rows(rec, a)
  }
rm(bill_contents, i)
# write the updated version into the file.
write_csv(rec, paste0("time_records/timerecord_", VDate))


##### prepare data for the bill table

bill_table <- bill %>% dplyr::select(date, workload, project, content, hLohn, lohn)

bill_table$date <- format(bill_table$date, "%d.%m.%Y")
bill_table$date <- as.character(bill_table$date)
bill_table$hLohn <- round(as.numeric(bill_table$hLohn), digits = 2)

# Summarise hours and costs
summs <- colSums(bill_table %>% dplyr::select(workload, lohn))
bill_table <- bind_rows(bill_table, summs)

# Fill The last line with stuff
bill_table$date[nrow(bill_table)] <- "Stunden Gesamt:"
bill_table$hLohn[nrow(bill_table)] <- "Gesamt Brutto:"

# taxes:
taxes <- paste0("Berrechnet wurden 19\\% MwSt. - Brutto ", bill_table$lohn[nrow(bill_table)], "{\\euro}, ",
               " - Netto ", round(bill_table$lohn[nrow(bill_table)] - bill_table$lohn[nrow(bill_table)] * 0.19, digits = 2), "{\\euro}, ",
               " - MwSt. ", round(bill_table$lohn[nrow(bill_table)] * 0.19, digits = 2), "{\\euro}")


# apply German colnames
names(bill_table) <- c("Datum", "Stunden", "Für Projekt", "Tätigkeiten", "h-Lohn ({\\euro})", "Gesamt ({\\euro})")
bill_table <- xtable(bill_table, auto = T)
eurosym <- function(x){paste0('{\\normalsize ', x, '}')} # this weird snipped makes the Euro-symbols to appear.


#### Write all required latex files to the current bill folder ####

bill_name <- paste0(bill_num, "-", project)
bill_folder <- paste0(year, "/", bill_name, "/")

# create bill folder:
system(paste0("mkdir ", bill_folder))

# copy bill template to the folder
system(paste0("cp initial_files/template.tex ", 
              bill_folder, "Rechnung-", bill_num, ".tex"))

# copy customer address data to the folder
system(paste0("cp customer_addresses/", customer, ".tex ", 
              bill_folder, "customer.tex"))

# copy customer bill number data to the folder
system(paste0("cp bill_number ", 
              bill_folder, "bill_number.tex"))

# write the table file
write_file(x = print(xtable(bill_table, digits = c(0,0,2,0,0,2,2)), 
                     include.rownames = F, 
                     hline.after = c(0, nrow(bill_table)-1), 
                     sanitize.colnames.function = eurosym),
          file = paste0(bill_folder, "bill_table.tex"))

# write the file for taxes
write_file(taxes, paste0(bill_folder, "taxes.tex"))

#### produce the PDF with pdflatex. ####
# (for this we have to change the working directory otherwise the input files are not found)
setwd(bill_folder)
system(paste0("pdflatex Rechnung-", bill_num, ".tex"))


#### Add the Bill into the bill summary table ####

setwd("../../bill_summary")
x <- read_csv(sort(list.files(pattern = glob2rx("bill_summary_*")), decreasing = T)[1],
                col_types = "cDccdddddlc")

line <- tibble(RechNr = bill_num,
               date = as.Date(format(Sys.time(), "%Y-%m-%d")),
               customer = customer,
               project = project,
               workload = summs[1],
               hLohn = bill$hLohn[1],
               brutto = summs[2],
               netto = round(summs[2] - summs[2] * 0.19, digits = 2),
               MwSt = round(summs[2] * 0.19, digits = 2),
               bill_paid = as.logical(FALSE),
               AuftrNr = str_c(bill$AuftrNr, collapse = " "))

x <- bind_rows(x, line)

write_csv(x, paste0("bill_summary_", VDate))

# if everything went fine new versions are saved.
setwd("../")
write_file(bill_num, "bill_number")
# write the updated version into the file.
write_csv(rec, paste0("time_records/timerecord_", VDate))
# done