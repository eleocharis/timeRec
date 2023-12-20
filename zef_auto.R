#!/usr/bin/env Rscript --vanilla
#########################################
#                                       #
#   Progrämmchen zur Zeiterfassung      #
#                                       #
#########################################


# somehow the time is wrongly saved if CET, it is correct if I change it to UTC
Sys.setenv(TZ = "UTC")
#rm(list=ls())

suppressMessages(library(dplyr))
library(rstudioapi)

# set working directory
file_path <- getSourceEditorContext()$path
file_pos <- file_path %>% str_locate_all("/") %>% unlist()
work_path <- file_path %>% str_sub(end = file_pos[length(file_pos)])
setwd(work_path)

VDate <- paste0(format(Sys.time(), "%Y%m%d_%H%M"), ".csv") # version date

# read the last created time record file
rec <- read_csv(paste0("time_records/", sort(list.files(path = "time_records", pattern = glob2rx("timerecord*")), decreasing = T)[1]),
                col_types = "iDcccTTdddcll")

# create entries for the record
job_no <- max(rec$job_no, na.rm = F) + 1
date <- format(Sys.time(), "%Y-%m-%d")
startingtime <- format(Sys.time(), "%Y-%m-%d %H:%M")

cat("For the Project:")
project <- readLines(con = "stdin", 1)

cat("Activities:")
content <- readLines(con = "stdin", 1)

cat("Press ENTER to stop timerecord\n")
invisible(readLines(con = "stdin", 1))
endtime <- format(Sys.time(), "%Y-%m-%d %H:%M")

duration <- round((as.numeric(as.POSIXct(endtime)) - as.numeric(as.POSIXct(startingtime))) / 60 / 60, digits = 2)

# attach a new line to the time records 
new_entry <- tibble(job_no = job_no, date = as.Date(date), 
                  project = project, content = content, 
                  start = as.POSIXct(startingtime), 
                  end = as.POSIXct(endtime), 
                  workload = workload,
                  hlohn = hlohn,
                  lohn = workload * hlohn,
                  RechNr = NA,
                  bill_send = F,
                  bill_paid = F)

# load additional details from the missions file 
suppressMessages(missions <- read_csv("projects.csv"))
mission <- missions %>% dplyr::filter(project == !!project) %>% dplyr::select(customer, hLohn)

# merge generated and form mission loaded entries
new_entry <- bind_cols(new_entry, mission)

# add the new entry to timerecords
rec <- bind_rows(rec, new_entry)

# calculate income for this time interval
rec$lohn[nrow(rec)] <- workload * rec$hLohn[nrow(rec)]

write_csv(rec, paste0("time_records/timerecord_", VDate))
cat("Time Record accomplished.\n\n")