#
# This is a Shiny web application.
# You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(readr)
library(tidyverse)



ui <- fluidPage(navbarPage(
  "XRF data Parsing",
  tabPanel("Data Overview",
  sidebarPanel(
    fileInput(
      "file1",
      "Choose CSV File",
      accept = c("text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv")
    )
    
  ),
  mainPanel(
    tags$h3("Main data"),
    tableOutput("data_clean"),
    tags$h3("Summary"),
    tableOutput("data_overview")
    
    
  )
),

tabPanel("Data with High Standard Diviation",
         sidebarPanel(
           sliderInput(
             "cutoff",
             "Deviation Percentage cutoff",
             min = 0,
             max = 10,
             value = 3
           )
         ),
         mainPanel(
           tags$h3("High SD items - Summary"),
           tableOutput("high_sd_overview"),
           tags$h3("High SD items - Data"),
           tableOutput("high_sd_data")
         )
  
)

))

  
  server <- function(input, output) {
    
    elements <- reactive({
      c("Al", "Si", "S", "Mn", "Fe", "Co", "Ni", "Cu", "Zn", 
        "As", "Ag", "Sn", "Sb", "Au", "Pb", "Bi")
    })
    
    # Loading the Data
    
    data_set <- reactive({
      inFile <- input$file1 
      
      if (is.null(inFile))
        return(NULL)
      
      read_csv(inFile$datapath)
    })
    
    # Clean data
    
    data_set_clean <- reactive({
      data_set() %>%
        select(!contains("Error")) %>%
        select(Lab_ID, starts_with(elements())) %>% # Removes all Other elements
        select(!`Collimation Status`)
    })
    
    
    data_overview <- reactive({
      data_set_clean() %>%
        group_by(Lab_ID) %>% 
        summarise(across(everything(), list(mean = mean, sd = sd)))
    })
    
    # Isolating High SD data
    
    high_sd <- reactive({
      high_sd <- 
        function(x){
          x >= input$cutoff
        }
      
      data_overview() %>% 
        filter(if_any(ends_with("sd"), high_sd)) %>% 
        select(Lab_ID, ends_with("sd")) %>% 
        select_if(~ any(high_sd(.)))
    })
    
    high_sd_overview <- reactive({
      columns <-
        substr(colnames(high_sd()[2:(max(col(high_sd())))]),
               start = 1,
               stop = 2)
      
      data_overview() %>% 
        filter(Lab_ID %in% (high_sd()$Lab_ID %>% as.array())) %>% 
        arrange(Lab_ID) %>% 
        group_by(Lab_ID) %>% 
        select(Lab_ID, starts_with(columns))
    })
    
    high_sd_data <- reactive({
      columns <-
        substr(colnames(high_sd()[2:(max(col(high_sd())))]),
               start = 1,
               stop = 2)
      
      data_set_clean() %>% 
        filter(Lab_ID %in% (high_sd()$Lab_ID %>% as.array())) %>% 
        arrange(Lab_ID) %>% 
        group_by(Lab_ID) %>% 
        select(Lab_ID, starts_with(columns))
    })
    
    # Outputs
    
    output$data_clean <- renderTable({
      
      data_set_clean()
    })
    
    output$data_overview <- renderTable({
      
      data_overview()
      
    })
    
    output$high_sd <- renderTable({
      high_sd()
    })
    
    output$high_sd_data <- renderTable({
      high_sd_data()
    })
    output$high_sd_overview <- renderTable({
      high_sd_overview()
    })
    
  }
  
  shinyApp(ui, server)

  