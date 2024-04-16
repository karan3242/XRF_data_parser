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
  tabPanel(
    "Data Overview",
    sidebarPanel(
      fileInput(
        "file1",
        "Choose CSV File",
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
      ),
      checkboxGroupInput(
        "elements",
        "Select Elements",
        choices = c(
          "Ag",
          "Al",
          "As",
          "Au",
          "Bi",
          "Cd",
          "Co",
          "Cr",
          "Cu",
          "Fe",
          "Hf",
          "Mg",
          "Mn",
          "Mo",
          "Nb",
          "Ni",
          "P",
          "Pb",
          "Pd",
          "Re",
          "S",
          "Sb",
          "Sc",
          "Si",
          "Sn",
          "Sr",
          "Ta",
          "Ti",
          "V",
          "W",
          "Zn",
          "Zr"
        ) ,
        inline = TRUE,
        selected = c(
          "Al",
          "Si",
          "S",
          "Mn",
          "Fe",
          "Co",
          "Ni",
          "Cu",
          "Zn",
          "As",
          "Ag",
          "Sn",
          "Sb",
          "Au",
          "Pb",
          "Bi"
        )
      )
      
    ),
    mainPanel(
      tags$h3("Main data"),
      tableOutput("data_clean"),
      tags$h3("Summary"),
      tableOutput("data_overview")
      
      
    )
  ),
  
  tabPanel(
    "Data with High Standard Diviation",
    sidebarPanel(
      sliderInput(
        "cutoff",
        "Deviation Percentage cutoff",
        min = 0,
        max = 10,
        value = 3
      ),
      
      sliderInput(
        "z_score",
        "Devation",
        min = 0,
        max = 3,
        value = 1
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
  # Loading the Data
  
  data_set <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read_csv(inFile$datapath) %>%
      select(id = Lab_ID, everything()) %>%
      select(!contains("Error")) %>%
      select(!`Collimation Status`) %>% 
      arrange(id)
    
    
  })
  
  # Clean data
  
  data_set_clean <- reactive({
    
    data_set() %>%
      select(id , starts_with(input$elements)) %>% 
      select(id, sort(names(.)))
    
    
    
  })

  
  
  data_overview <- reactive({
    data_set_clean() %>%
      group_by(id) %>%
      summarise(across(everything(), list(mean = mean, sd = sd)))
  })
  
  # Isolating High SD data
  
  high_sd <- reactive({
    high_sd <-
      function(x) {
        x >= input$cutoff
      }
    
    data_overview() %>%
      filter(if_any(ends_with("sd"), high_sd)) %>%
      select(id, ends_with("sd")) %>%
      select_if( ~ any(high_sd(.)))
  })
  
  high_sd_overview <- reactive({
    columns <-
      substr(colnames(high_sd()[2:(max(col(high_sd())))]),
             start = 1,
             stop = 2)
    
    data_overview() %>%
      filter(id %in% (high_sd()$id %>% as.array())) %>%
      arrange(id) %>%
      group_by(id) %>%
      select(id, starts_with(columns))
  })
  
  high_sd_data <- reactive({
    columns <-
      substr(colnames(high_sd()[2:(max(col(high_sd())))]),
             start = 1,
             stop = 2)
    
    outliers <- function(x) {
      x > input$z_score | x < (input$z_score*(-1))
    }
    
    data_set_clean() %>%
      filter(id %in% (high_sd()$id %>% as.array())) %>%
      arrange(id) %>%
      group_by(id) %>%
      select(id, starts_with(columns)) %>% 
      mutate(across(everything(), list(z_score = scale))) %>%
      select(id, sort(names(.))) %>% 
      filter(if_any(ends_with("z_score"), outliers))
    
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

