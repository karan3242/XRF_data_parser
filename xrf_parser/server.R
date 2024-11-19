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
library(writexl)
library(CC)

function(input, output, session) {
  # Base data set with pre-processing
  
  data_set1 <- reactive({
    inFile <- input$file1
    
    df <- read_csv(inFile$datapath) %>%
      select(id = Lab_ID, everything()) %>%
      arrange(id)
    
    df[df == '<LOD'] <- NA
    
    df <- df %>%
      mutate_at(
        vars(
          -id,-Date,-Time,-Units,-Description,-matches("Test Label"),-matches("Collimation Status"),-matches("Method Name"),-matches("Instrument Serial Num"),-matches("Reading #")
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
  output$data_set1 <- renderTable({
    data_set1()
  })
  
  # Lab items output selection
  #lab itmes selections
  output$lab_items <- renderUI({
    req(data_set1())
    checkboxGroupInput(
      "lab_id",
      "Select Lab Items",
      choices = unique(data_set1()$id),
      inline = TRUE,
      selected = unique(data_set1()$id)
    )
  })
  ## element selections
  output$elm <- renderUI({
    req(data_set1())
    elmt <- sort(gsub('.{14}$', '', colnames(select(
      data_set1(), contains("Concentration")
    ))))
    checkboxGroupInput(
      "elm",
      "Select Elements",
      choices = elmt,
      inline = TRUE,
      selected = elmt
    )
  })
  
  # Cleaned And Normalized Data
  ## Clean data
  data_set_clean <- reactive({
    req(input$lab_id, data_set1())
    df1 <- data_set1() %>%
      select(id , `Reading #`, starts_with(input$elm)) %>%
      select(id, `Reading #`, contains("Concentration")) %>%
      select(id, `Reading #`, sort(names(.))) %>%
      filter(id %in% input$lab_id)
    
    coln <- gsub('.Concentration$', '', colnames(select(df1, contains("Concentration"))))
    
    colnames(df1) <- c("id", "reading", coln)
    
    print(df1)
  })
  ## Nomral Data
  data_set_normal <- reactive({
    normalize_rows <- function(data) {
      # Extracting IDs (assuming they are in the first column)
      ids <- data[, 1]
      readings <- data[, 2]
      
      # Normalizing each row to 100%
      normalized_data <-
        t(apply(data[, -c(1, 2), drop = FALSE], 1, function(row) {
          row_sum <- sum(row, na.rm = TRUE)
          
          normalized_row <- row / row_sum * 100
          
          return(normalized_row)
        }))
      
      # Combining IDs with normalized data
      normalized_data <- cbind(ids, readings, normalized_data)
      
      return(normalized_data)
    }
    
    normalize_rows(data_set_clean())
  })
 
  # Render data set after items and elements selected
  data_set2 <- reactive({
    if(input$normal_select == FALSE){data_set_clean()}else{data_set_normal()}
  })
  
  output$data_set2 <- renderTable({
    data_set2()
  })
  
  # Mean and SD of clean Data
  
  data_overview <- reactive({
    data_set2() %>%
      select(!contains("reading")) %>% 
      group_by(id) %>%
      summarise(across(everything(), list(mean = mean, sd = sd)))
  })
  output$data_overview <- renderTable({
    data_overview()
  })
  
  # Min-Max of clean Data
  
  data_summary_minmax <- reactive({
    data_set2() %>%
      select(!contains("reading")) %>% 
      group_by(id) %>%
      summarise(across(everything(), list(min = min, max = max)))
  })
  output$data_summary_minmax <- renderTable({
    data_summary_minmax()
  })
  
  # Isolating High SD data
  
  high_sd <- reactive({
    high_sd <- function(x) {
        x >= input$cutoff
      }
    
    data_overview() %>%
      filter(if_any(ends_with("sd"), high_sd)) %>%
      select(id, ends_with("sd")) %>%
      select_if( ~ any(high_sd(.)))
  })
  output$high_sd <- renderTable({
    high_sd()
  })
  
  # Mean and SD of date with high SD determined by Threshold
  
  high_sd_overview <- reactive({
    columns <-
      substr(colnames(high_sd()[2:(max(col(high_sd())))]), start = 1, stop = 2)
    
    data_overview() %>%
      filter(id %in% (high_sd()$id %>% as.array())) %>%
      arrange(id) %>%
      group_by(id) %>%
      select(id, starts_with(columns))
  })
  output$high_sd_overview <- renderTable({
    high_sd_overview()
  })
  
  # Table for min-max range of items with high standard divination determined by Threshold
  
  data_minmax_highsd <- reactive({
    columns <-
      substr(colnames(high_sd()[2:(max(col(high_sd())))]), start = 1, stop = 2)
    
    data_summary_minmax() %>%
      filter(id %in% (high_sd()$id %>% as.array())) %>%
      arrange(id) %>%
      group_by(id) %>%
      select(id, starts_with(columns))
  })
  output$data_minmax_highsd <- renderTable({
    data_minmax_highsd()
  })
  
  # Filters items with Z score above set threshold
  
  high_sd_data <- reactive({
    columns <-
      substr(colnames(high_sd()[2:(max(col(high_sd())))]), start = 1, stop = 2)
    
    
    outliers <- function(x){
      x > input$z_score | x < (input$z_score * (-1))
    }
    
    data_set2() %>%
      filter(id %in% (high_sd()$id %>% as.array())) %>%
      arrange(id) %>%
      group_by(id) %>%
      select(id, starts_with(columns)) %>%
      mutate(across(everything(), list(z_score = scale))) %>%
      select(id, sort(names(.))) %>%
      filter(if_any(ends_with("z_score"), outliers))
    
  })
  output$high_sd_data <- renderTable({
    high_sd_data()
  })
  
  # Individual Item summary
  
  ## Item Data
  
  output$one_item <- renderUI({
    selectInput(
      "one_item", 
      "Item", 
      choices = unique(unlist(data_set_clean()$id)),
      selected = NULL)
  })
  
  output$elm2 <- renderUI({
    req(data_set2())
    elmt <- data_set2() %>% 
      select(!contains("id")) %>%
      select(!contains("reading")) %>% 
      colnames() %>% 
      sort()

    checkboxGroupInput(
      "elm2",
      "Select Elements",
      choices = elmt,
      inline = TRUE,
      selected = elmt
    )
  })
  
  one_item_data <- reactive({
    data_set2() %>% 
      filter(id %in% input$one_item) %>% 
      select(id, reading, input$elm2)
  })
  
  one_item_box <- reactive({
    df1 <- one_item_data() %>% select(!contains("reading"))
    df2 <- pivot_longer(df1, 
                        cols = colnames(df1)[2:max(col(df1))], 
                        names_to = 'Elements', 
                        values_to = 'counts')
    plot <- ggplot(df2, aes(x = Elements, y = counts, fill = Elements)) + 
      geom_boxplot() + 
      scale_fill_viridis(discrete = TRUE, alpha=0.6) +
      geom_jitter() +
      theme(legend.position="none")
    
    print(plot)
  })
  
  output$one_item_data <- renderTable({one_item_data()})
  

  output$boxplot <- renderPlot({one_item_box()})
  
  ## Item Outlighers
  
  one_item_outlier <- reactive({
    columns <-
      substr(colnames(high_sd()[2:(max(col(high_sd())))]), start = 1, stop = 2)
    
    one_item_data() %>% select(id, reading, starts_with(columns))
  })
  
  output$one_item_outlier <- renderTable({one_item_outlier()})
  
  output$one_item_summary <- renderPrint({
    summary({one_item_outlier() %>% select(!contains("id")) %>%  
        select(!contains("reading"))})
    
  })
  
  output$one_item_high_sd <- renderTable({
    
    one_item_outlier()  %>% 
      select(!contains("reading")) %>% 
      group_by(id) %>%
      summarise(across(everything(), list(sd = sd, range = diffrange)))
    
    })
  
  outlier_boxplot <- reactive({
    
    p1 <- pivot_longer(one_item_outlier()[,], 
                 cols = colnames(one_item_outlier())[2:max(col(one_item_outlier()))], 
                 names_to = 'Elements', 
                 values_to = 'counts')
    p1 <- p1 %>% filter(Elements != "reading")
    plot <- ggplot(p1, aes(x = Elements, y = counts, fill = Elements)) + 
      geom_boxplot() + 
      scale_fill_viridis(discrete = TRUE, alpha=0.6) +
      theme(legend.position="none")
    
    print(plot)
  })
  
  output$outlier_boxplot <- renderPlot({outlier_boxplot()})
  
  # Piloting the data
  
  data_plot <- reactive({
    data_mean <-
      data_overview() %>%
      pivot_longer(
        cols = (contains("mean")),
        names_to = "Type",
        values_to = "Value_mean"
      ) %>%
      select(id, Type, Value_mean)
    
    data_sd <-
      data_overview() %>%
      pivot_longer(
        cols = (contains("sd")),
        names_to = "Type",
        values_to = "Value_sd"
      ) %>%
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
        theme(axis.line = element_blank(), axis.ticks = element_blank(), ) +
        labs(x = NULL, y = "Element %", title = "Elemet values - Normalized Data")
    )
    
  })
  output$plot <- renderPlotly({
    data_plot()
  })
  
  
  output$dl <- downloadHandler(
    filename = function() {
      paste0("df_dmodel", "_Table", ".xlsx")
    },
    content = function(file) {
      tbl_primary <- data_set1()
      tbl_selected_itmes <- data_set_clean()
      tbl_elements_overview <- data_overview()
      tbl_elements_minmax <- data_summary_minmax()
      tbl_normalized <- data_set2()()
      tbl_normal_overview <- data_overview_norm()
      tbl_normal_minmax <- data_overview_minmax()
      tbl_highsd <- high_sd()
      tbl_highsd_overview <- high_sd_overview()
      tbl_highsd_minmax <- data_minmax_highsd()
      tbl_highsd_values <- high_sd_data()
      
      sheets <- mget(ls(pattern = "tbl")) # getting all objects in your environment with tbl in the name
      #names(sheets) <- paste0("sheet", seq_len(length(sheets))) # changing the names in your list
      writexl::write_xlsx(sheets, path = file) # saving the file
    }
  )
  
  
}
