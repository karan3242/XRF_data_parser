#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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

function(input, output) {
  # Loading the Data
  
  data_set <- reactive({
    inFile <- input$file1
    
    df <- read_csv(inFile$datapath) %>%
      select(id = Lab_ID, everything()) %>%
      arrange(id)
    
    df[df == '<LOD'] <- NA
    
    df <- df %>%
      mutate_at(
        vars(
          -id,
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
        as.numeric
      )
    
    df[is.na(df)] <- 0
    
    df$id <- as.character(df$id)
    df$`Instrument Serial Num` <-
      as.integer(df$`Instrument Serial Num`)
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
      data_overview_norm() %>%
      pivot_longer(cols = (contains("mean")),
                   names_to = "Type",
                   values_to = "Value_mean") %>%
      select(id, Type, Value_mean)
    
    data_sd <-
      data_overview_norm() %>%
      pivot_longer(cols = (contains("sd")),
                   names_to = "Type",
                   values_to = "Value_sd") %>%
      select(id, Type, Value_sd)
    
    data_mean$Type <- data_mean$Type %>% substr(1, 2)
    data_sd$Type <- data_sd$Type %>% substr(1, 2)
    
    data_join <-
      full_join(data_mean, data_sd, by = join_by(id, Type))
    
    ggplotly(
      ggplot(data_join) +
        geom_bar(
          aes(x = id, y = Value_mean, fill = Type),
          stat = "identity",
          position = "dodge"
        ) +
        geom_errorbar(
          aes(
            x = id,
            ymin = Value_mean - Value_sd,
            ymax = Value_mean + Value_sd,
            fill = Type
          ),
          position = "dodge",
          colour = "#FF0000",
          alpha = 0.9,
          size = 0.8
        ) +
        scale_fill_viridis(discrete = TRUE, name = "") +
        theme_classic() +
        xlab("Items.") +
        ylab("Percentage.")
    )
    
  })
  
  # Outputs
  
  output$data_set <- renderTable({data_set()})
  output$data_clean <- renderTable({data_set_clean()})
  output$data_normal <- renderTable({data_set_normal()})
  output$data_overview <- renderTable({data_overview()})
  output$data_overview_norm <- renderTable({data_overview_norm()})
  output$high_sd <- renderTable({high_sd()})
  output$high_sd_data <- renderTable({high_sd_data()})
  output$high_sd_overview <- renderTable({high_sd_overview()})
  output$data_overview_minmax <- renderTable({data_overview_minmax()})
  output$data_summary_minmax <- renderTable({data_summary_minmax()})
  output$data_minmax_highsd <- renderTable({data_minmax_highsd()})
  output$plot <- renderPlotly({data_plot()})
  
}
