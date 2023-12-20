#!/usr/bin/Rscript
########################################################
#                                                      #
# Script to manually type in worked or sport times     #
#                                                      #
# Start of development: 6.1.2021                       #
#                                                      #
########################################################


# somehow the time is wrongly saved if CET, it is correct if I change it to UTC
Sys.setenv(TZ = "UTC")

##################### Libraries ########################
library(tidyverse)
library(shiny)
library(shinyTime)
library(lubridate)
library(zoo) # this package provides an easy way for moving average.
library(rstudioapi)
###################### Datasets ########################
# set working directory
file_path <- getSourceEditorContext()$path
file_pos <- file_path %>% str_locate_all("/") %>% unlist()
work_path <- file_path %>% str_sub(end = file_pos[length(file_pos)])
setwd(work_path)

# read in the time records
ttab <- read_csv(file = paste0("time_records/", sort(list.files(path = "time_records/", pattern = glob2rx("timerecord*")), decreasing = T)[1]),
                 col_types = "iDcccTTdddcll")
ttab <- ttab %>% mutate(date = as.Date(date, "%d-%m-%Y")) %>% arrange(date)



VDate <- paste0(format(Sys.time(), "%Y%m%d"), ".csv")

# create variables which are needed later in the GUI
min_date <- min(ttab$date) # needed for time range filter
max_date <- max(ttab$date) # needed for time range filter

projects <- unique(as.vector(ttab$project)) # needed for Checkboxes
project <- ttab$project[nrow(ttab)] # initial value to accelerate input
content <- ttab$content[nrow(ttab)]  # initial value to accelerate input

###################### Plot style ########################
source(paste0(work_path, "plot_style.R"))

################# Implement in Shiny ###################

# Frontend
ui <- fluidPage(
  tabsetPanel(type = "tabs",
              tabPanel("Eingabe",    # Panel for the Input of new Data
                       titlePanel("Zeiteingabe"),
                       tags$p("Type in the times manually done"),
                       tags$hr(),
                       sidebarLayout(
                         sidebarPanel(width = 3,
                           dateInput(inputId = "date",
                                     label = "Datum",
                                     language = "de",
                                     format = "dd.mm.yyyy",
                                     weekstart = 1),
                           
                           timeInput(inputId = "starttime",
                                     label = "Startzeit",
                                     seconds = F),
                           
                           timeInput(inputId = "endtime",
                                     label = "Endzeit",
                                     value = Sys.time(),
                                     seconds = F),
                           
                           textInput(inputId = "project",
                                     label = "Projekt / Sportart",
                                     value = project),
                           
                           textInput(inputId = "content",
                                     label = "Tätigkeiten",
                                     value = content),
                           
                           actionButton(inputId = "save",
                                        label = "Zeitaufnahme speichern")
                           )
                       ),
                      
                         mainPanel(width = 9,
                           tableOutput("ttab_input")
                            )
              )
  )
)

# Backend
server <- function(input, output, session) {
  ## Input the time table
  reacVals <- reactiveValues(
    ttab = read_csv(sort(list.files(pattern = glob2rx("timerecord*")), decreasing = T)[1],
                     col_types = "iDcccTTdddcll")
  )
  
  ## Take values from the user and create a new line to add to the time table
  addLine <- observeEvent(input$save, {
    workload <-  round(as.numeric(as.POSIXct(input$endtime) - as.POSIXct(input$starttime)), digits = 2)
    project <- as.character(input$project)
    new_entry <- tibble(AuftrNr = max(reacVals$ttab$AuftrNr)+1, 
                        date = as.Date(input$date), 
                        project = input$project, 
                        content = input$content, 
                        start = as.POSIXlt(input$starttime), 
                        end = as.POSIXlt(input$endtime), 
                        workload = workload, 
                        bill_send = F, 
                        bill_paid = F)
    print(unclass(new_entry$start[1]))
    # load additional details from the missions file
    mission <- missions %>% dplyr::filter(project == !!input$project) %>% dplyr::select(customer, hLohn)
    new_entry <- bind_cols(new_entry, mission)
    
    # calculate the earning of this work package
    new_entry$lohn <- new_entry$workload * new_entry$hLohn
    print(new_entry)
    # Attach the new line to the timerecords table and save it.
    reacVals$ttab <- bind_rows(reacVals$ttab, new_entry)
    write_csv(reacVals$ttab, paste0("timerecord_", VDate))
    # update reactive value
    reacVals$ttab <- read_csv(sort(list.files(pattern = glob2rx("timerecord*")), decreasing = T)[1],
                             col_types = "iDcccTTdddcll")
    
    # prepare the table for the UI
    ttab_input <- reacVals$ttab
    ttab_input$date <- as.character(ttab_input$date)
    ttab_input$start <- str_sub(as.character(ttab_input$start), start = -9, end = -4)
    ttab_input$end <- str_sub(as.character(ttab_input$end), start = -9, end = -4)
    ttab_input <- ttab_input %>% dplyr::select(-RechNr, -bill_send, -bill_paid)
    # and plot it
    output$ttab_input  <- renderTable(ttab_input, include.rownames = F)
    })
  

  
  
  
  ## Filter table for the overview ttab
  ttab_stats <- reactive({
    reacVals$ttab %>% dplyr::filter(between(date, input$daterange[1], input$daterange[2]) & 
                                                customer %in% input$select_customer &
                                                project %in% input$select_project &
                                                bill_send %in% input$billed)

  })
  
  observeEvent(input$nicer, {
    ttab_stats_table <- ttab_stats()
    ttab_stats_table$date <- as.character(ttab_stats_table$date)
    #print(ttab_stats_table %>% head())
    ttab_stats_table$start <- str_sub(as.character(ttab_stats_table$start), start = -9, end = -4)
    ttab_stats_table$end <- str_sub(as.character(ttab_stats_table$end), start = -9, end = -4)
    output$ttab_disp <- renderTable(ttab_stats_table)
    isolate(ttab_stats_table)
  })

  
  output$plot <- renderPlot({ttab_stats() %>% ggplot(aes(x = date, y = cumsum(lohn))) + 
      #geom_area(fill = colour.palette[1], alpha = .7) + 
      geom_line(col = colour.palette[3], size = 1) +
      geom_point(col = colour.palette[2], size = 2) +
      # geom_bar(data = ttab_stats_table %>% mutate(month = as.character(as.yearmon(as.Date(date)))) %>% group_by(month) %>% summarise(mon_sum = sum(lohn)),
      #          mapping = aes(x = month, y = mon_sum),
      #          col = colour.palette[4],
      #          stat = "identity") +
      geom_point(aes(x = date, y = rollmean(x = lohn, 1, na.pad=TRUE))) 
      #geom_text(mapping = aes(x = date, y = lohn, label = project), col = colour.palette[5], size = 6, hjust = 1, )
  })
  
  
  ## Billing ttab
  open_bills <- reactive({reacVals$ttab %>% dplyr::filter(bill_send %in% input$billed2)})
  output$open_bills_table <- renderTable(open_bills())
  
  observeEvent(input$bill_button, {
    bill_project <- input$bill_input
    output$bill_project <- renderText({ input$bill_input })
    #source("../billing.R")
  })
}

# Start
shinyApp(ui, server)


attr(ttab)
