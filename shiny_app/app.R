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
library(viridis)
library(plotly)

ui <- fluidPage(navbarPage(
  "XRF data Parsing",
  tabPanel("Data table",
           
           mainPanel(
             fileInput(
               "file1",
               "Choose CSV File",
               accept = c("text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
             ),
             tags$h1("Main data"),
             tags$body("<LOD values have been converted to 0"),
             tableOutput("data_set")
           )),
  tabPanel(
    "Data Overview",
    mainPanel(
      tags$h1("Selected Elements"),
      uiOutput("lab_items"),
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
        )
      ),
      tableOutput("data_clean"),
      tags$h2("Summary"),
      tableOutput("data_overview"),
      tags$h2("Range"),
      tableOutput("data_summary_minmax")
    )
  ),
  tabPanel(
    "Normalized Data",
    mainPanel(
      tags$h1("Normalized Data of Selected Elements"),
      tableOutput("data_normal"),
      tags$h2("Summary"),
      tableOutput("data_overview_norm"),
      tags$h2("Range"),
      tableOutput("data_overview_minmax")
    )
  ),
  tabPanel(
    "High SD Readings",
    mainPanel(
      tags$h1("High SD items - Summary"),
      sliderInput(
        "cutoff",
        "Deviation Percentage cutoff",
        min = 0,
        max = 10,
        value = 3,
        step = 0.01,
        ticks = FALSE
      ),
      tags$h2("Mean and Standard Deviation"),
      tableOutput("high_sd_overview"),
      tags$h2("Range"),
      tableOutput("data_minmax_highsd"),
      tags$h1("High SD items - Data"),
      sliderInput(
        "z_score",
        "Devation steps",
        min = 0,
        max = 3,
        value = 1,
        step = 0.01,
        ticks = FALSE
      ),
      tableOutput("high_sd_data")
    )
    
  ),
  tabPanel(
    "Plot",
    mainPanel(
      tags$h1("Plot"),
      plotlyOutput("plot", height = "500px"),
      tags$caption("Plot of Normalized data")
    )
  )
  
))

server <- function(input, output) {
  # Loading the Data
  
  data_set <- reactive({
    inFile <- input$file1
    
    df <- read_csv(inFile$datapath) %>%
      select(id = Lab_ID, everything()) %>%
      arrange(id)
    
    df[df == '<LOD'] <- NA
    
    df <- df %>% mutate_at(vars(-id,
                                -Date,
                                -Time,
                                -Units,
                                -Description,
                                -matches("Test Label"),
                                -matches("Collimation Status"),
                                -matches("Method Name"),
                                -matches("Instrument Serial Num"),
                                -matches("Reading #")
                                ),
                                as.numeric)
    
    df[is.na(df)] <- 0
    
    df$id <- as.character(df$id)
    df$`Instrument Serial Num` <- as.integer(df$`Instrument Serial Num`)
    df$`Reading #` <- as.integer(df$`Reading #`)
    df$`Test Label` <- as.integer(df$`Test Label`)
    
    df$Notes[df$Notes == 0] <- NA
    
    df$Notes <- as.character(df$Notes)
    
    return(df)
  })
  # Clean data
  
  # Lab items out put
  output$lab_items <- renderUI({
    req(data_set())
    print(unique(data_set()$id))
    checkboxGroupInput(
      "lab_id",
      "Select Lab Items",
      choices = unique(data_set()$id), 
      inline = TRUE, 
      selected = unique(data_set()$id)
    )
  })
  
  data_set_clean <- reactive({
    req(input$lab_id, data_set())
    data_set() %>%
      select(!contains("Error")) %>%
      select(!`Collimation Status`) %>%
      select(id , starts_with(input$elements)) %>%
      select(id, contains("Concentration")) %>% 
      select(id, sort(names(.))) %>% 
      filter(id %in% input$lab_id)
  })
  
  # Normal data
  
  data_set_normal <- reactive({
    normalize_rows <- function(data) {
      # Extracting IDs (assuming they are in the first column)
      ids <- data[, 1]
      
      # Normalizing each row to 100%
      normalized_data <-
        t(apply(data[,-1, drop = FALSE], 1, function(row) {
          row_sum <- sum(row, na.rm = TRUE)
          
          normalized_row <- row / row_sum * 100
          
          return(normalized_row)
        }))
      
      # Combining IDs with normalized data
      normalized_data <- cbind(ids, normalized_data)
      
      return(normalized_data)
    }
    
    normalize_rows(data_set_clean())
  })
  
  data_overview <- reactive({
    data_set_clean() %>%
      group_by(id) %>%
      summarise(across(everything(), list(mean = mean, sd = sd)))
  })
  
  data_summary_minmax <-reactive({
    data_set_clean() %>%
      group_by(id) %>%
      summarise(across(everything(), list(min = min, max = max)))
  })
  
  data_overview_norm <- reactive({
    data_set_normal() %>%
      group_by(id) %>%
      summarise(across(everything(), list(mean = mean, sd = sd)))
  })
  
  data_overview_minmax <-reactive({
    data_set_normal() %>%
      group_by(id) %>%
      summarise(across(everything(), list(min = min, max = max)))
  })
  
  # Isolating High SD data
  
  high_sd <- reactive({
    high_sd <-
      function(x) {
        x >= input$cutoff
      }
    
    data_overview_norm() %>%
      filter(if_any(ends_with("sd"), high_sd)) %>%
      select(id, ends_with("sd")) %>%
      select_if(~ any(high_sd(.)))
  })
  
  high_sd_overview <- reactive({
    columns <-
      substr(colnames(high_sd()[2:(max(col(high_sd())))]),
             start = 1,
             stop = 2)
    
    data_overview_norm() %>%
      filter(id %in% (high_sd()$id %>% as.array())) %>%
      arrange(id) %>%
      group_by(id) %>%
      select(id, starts_with(columns))
  })
  
  data_minmax_highsd <- reactive({
    columns <-
      substr(colnames(high_sd()[2:(max(col(high_sd())))]),
             start = 1,
             stop = 2)
    
    data_overview_minmax() %>%
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
      x > input$z_score | x < (input$z_score * (-1))
    }
    
    data_set_normal() %>%
      filter(id %in% (high_sd()$id %>% as.array())) %>%
      arrange(id) %>%
      group_by(id) %>%
      select(id, starts_with(columns)) %>%
      mutate(across(everything(), list(z_score = scale))) %>%
      select(id, sort(names(.))) %>%
      filter(if_any(ends_with("z_score"), outliers))
    
  })
  
  data_plot <- reactive({
    data_mean <-
      data_overview_norm() %>% pivot_longer(cols = (contains("mean")),
                                     names_to = "Type",
                                     values_to = "Value_mean") %>%  select(id, Type, Value_mean)
    
    data_sd <-
      data_overview_norm() %>% pivot_longer(cols = (contains("sd")),
                                     names_to = "Type",
                                     values_to = "Value_sd") %>% select(id, Type, Value_sd)
    
    data_mean$Type <- data_mean$Type %>% substr(1, 2)
    data_sd$Type <- data_sd$Type %>% substr(1, 2)
    
    data_join <-
      full_join(data_mean, data_sd, by = join_by(id, Type))
    
   p <- ggplot(data_join) +
      geom_bar(aes(x = id, y = Value_mean, fill = Type),
               stat = "identity",
               position = "dodge") +
      geom_errorbar(
        aes(
          x = id,
          ymin = Value_mean - Value_sd,
          ymax = Value_mean + Value_sd,
          fill = Type
        ),
        position = "dodge",
        colour="red", 
        alpha=0.9, 
        size=0.8
      ) +
      scale_fill_viridis(discrete = TRUE, name = "")+
      theme_classic()+
      xlab("Items.")+
      ylab("Percentage.")
   
   ggplotly(p)
   
  })

  # Outputs
  
  output$data_set <- renderTable({
    data_set()
  })
  
  output$data_clean <- renderTable({
    data_set_clean()
  })
  
  
  output$data_normal <- renderTable({
    data_set_normal()
  })
  
  output$data_overview <- renderTable({
    data_overview()
    
  })
  
  output$data_overview_norm <- renderTable({
    data_overview_norm()
    
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
  
  output$data_overview_minmax <- renderTable({
    data_overview_minmax()
  })
  output$data_summary_minmax <- renderTable({
    data_summary_minmax()
  })
  output$data_minmax_highsd <- renderTable({
    data_minmax_highsd()
  })
  
  output$plot <- renderPlotly({
    data_plot()
  })
  
}

shinyApp(ui, server)
