#!/usr/bin/Rscript
########################################################
#                                                      #
# Script to get an overview of finances within self-   #
# employed buissnes and to create tables which can be  #
# load with latex to automatically create a bill.      #
#                                                      #
# Start of development: 28.12.2020                     #
#                                                      #
########################################################


rm(list=ls())
##################### Libraries ########################
library(tidyverse)
library(shiny)
library(rstudioapi)
# library(zoo) # this package provides an easy way for moving average.
###################### Datasets ########################

# set working directory
file_path <- getSourceEditorContext()$path
file_pos <- file_path %>% str_locate_all("/") %>% unlist()
work_path <- file_path %>% str_sub(end = file_pos[length(file_pos)])
setwd(work_path)


# read time_records
tab <- read_csv(paste0("time_records/", sort(list.files(path = "time_records", pattern = glob2rx("timerecord*")), decreasing = T)[1]),
                col_types = "iDcccTTdddcll")

tab$start <- as.character(tab$start) %>% str_sub(start = 11, end = -4)
tab$end <- as.character(tab$end) %>% str_sub(start = 11, end = -4)

project <- NULL


# create varables which are needed later in the GUI
min_date <- min(tab$date)
max_date <- max(tab$date)
customers <- unique(as.vector(tab$customer))
projects <- unique(as.vector(tab$project))

###################### Plotstyle ########################
source(paste0(work_path, "plot_style.R"))

################# Implement in Shiny ###################

# Frontend
ui <- fluidPage(
  # Filter data to display in map
  titlePanel("Finanzübersicht"),
  # tags$h1("Nimburger Ried"),
  tags$p("Mit iesem Programm können die Finanzen im Verlauf angeschaut werden und rechnungen geschrieben werden."),
  tags$hr(),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      # Filter by Date:
      sliderInput(inputId = "daterange",
                  label = "Aufträge von-bis:",
                  min = as.Date(min_date),
                  max = as.Date(max_date),
                  value = c(as.Date(min_date), as.Date(max_date)),
                  timeFormat = "%Y-%m-%d"),
      
      # Filter by Species group:
      # checkboxGroupInput(inputId = "select_customer",
      #                    label = "Filter nach Auftraggeber",
      #                    choices = customers,
      #                    selected = customers),
      
      # Filter by Species group:
      checkboxGroupInput(inputId = "select_project",
                         label = "Filter nach Projekten",
                         choices = projects,
                         selected = projects),
      
      # Bill send or not?
      # checkboxGroupInput(inputId = "billed",
      #                    label = "Rechnung erstellt?",
      #                    choiceNames = c("ja", "nein"),
      #                    choiceValues = c(TRUE, FALSE),
      #                    selected = c(TRUE, FALSE)),
      
      # Print a bill according to the selected projects / customers 
      # textInput(inputId = "bill_input",
      #           label = "Für welches Projekt soll eine Rechnung erstellt werden?",
      #           placeholder = "Bsp. Ökopunkte A5"),
      # verbatimTextOutput("project"),
      # 
      # actionButton(inputId = "bill_button", 
      #              label = "Rechnung erstellen")
      

    ),
    
    mainPanel(
      plotOutput("plot"),
      tableOutput("table")
    )
  )
)

# Backend
server <- function(input, output, session) {
  disp_tab <- reactive({tab %>% dplyr::filter(between(date, input$daterange[1], input$daterange[2]) & 
                                              project %in% input$select_project)
    
  })
  
  output$table <- renderTable(disp_tab())
  
  output$plot <- renderPlot({disp_tab() %>% ggplot(aes(x = date, y = duration)) + 
      #geom_area(fill = colour.palette[1], alpha = .7) + 
      geom_point(col = colour.palette[2], size = 3) + 
      #geom_line(aes(y = rollmean(x = duration, 3, na.pad=TRUE))) +
      geom_line(col=colour.palette[3]) + geom_smooth(col = colour.palette[4], se = F) +
      geom_text(mapping = aes(x = date, y = duration, label = project), col = colour.palette[5], size = 6, hjust = 1.5)
  })
  
  observeEvent(input$bill_button, {
    project <- input$bill_input
    output$project <- renderText({ input$bill_input })
    #source("../billing.R")
  })
}

# Start
shinyApp(ui, server)
