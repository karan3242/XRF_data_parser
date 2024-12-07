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
    elmt <- sort(gsub(' Concentration', '', colnames(select(
      data_set1(), contains(" Concentration")
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
    
    coln <- gsub(' Concentration', '', colnames(select(df1, contains("Concentration"))))
    
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
      choices = unique(unlist(data_set2()$id)),
      selected = NULL)
  })
  
  one_item_data0 <- reactive({
    data_set2() %>% 
      filter(id %in% input$one_item)
  })
  
  output$elm2 <- renderUI({
    req(one_item_data0())
    elmt <- sort(colnames(select(data_set2(), !contains(c("id", "reading")))))  
      
    df1 <- one_item_data0()[3:ncol(one_item_data0())]
    
    elmt2 <- colnames(df1[,as.vector(which(round(colSums(df1),2) > 0))])

    checkboxGroupInput(
      "elm2",
      "Select Elements",
      choices = elmt,
      inline = TRUE,
      selected = elmt2
    )
  })
  
  
  one_item_data <- reactive({
    
    is_outlier <- function(x) {
      q1 <- quantile(x, 0.25)
      q3 <- quantile(x, 0.75)
      iqr <- q3 - q1
      x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr)
    }
    df1 <- one_item_data0()
    # Apply the function to each numeric column and create a logical matrix
    outliers_matrix <- 
      sapply(df1 [3:max(col(df1))], is_outlier)
    df1$outliers_count <- rowSums(outliers_matrix)
    df1 %>% 
      select(id, reading, input$elm2, outliers_count)
    
    
  })
  
  one_item_data_nooutlier <- reactive({
    data <- one_item_data()
    elm <- colnames(select(data, !contains(c("id", "reading", "outliers_count"))))
    is_outlier <- function(x) {
      q1 <- quantile(x, 0.25)
      q3 <- quantile(x, 0.75)
      iqr <- q3 - q1
      x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr)
    }
    for (i in elm) {
      data[[i]][is_outlier(data[[i]])] <- NA
    }
    
    print(data)  
    
  })

  one_item_table <- reactive({
    if(input$rm_outlier){one_item_data_nooutlier()}
    else{one_item_data()}
  })
  
  output$one_item_table <- renderTable({one_item_table()})
  
  #output$one_item_data_nooutlier <- renderTable({one_item_data_nooutlier()})
  
  one_item_box <- reactive({
    df1 <- one_item_table() %>% select(!contains(c("reading", "outliers_count")))
    df2 <- pivot_longer(df1, 
                        cols = colnames(df1)[2:max(col(df1))], 
                        names_to = 'Elements', 
                        values_to = 'counts')
    
    plot <- ggplot(df2, aes(x = Elements, y = counts, fill = Elements)) + 
      geom_boxplot(outliers = TRUE) + 
      scale_fill_viridis(discrete = TRUE , alpha=0.6) +
      theme(legend.position = "none") +
      theme_minimal()
    
    plot2 <- plot+geom_jitter(color="red", alpha=0.9)
    
    if(input$jitter){print(plot2)}else{print(plot)}
  })
  
  output$elm_outlier_count <- renderText({
    is_outlier <- function(x) {
      q1 <- quantile(x, 0.25)
      q3 <- quantile(x, 0.75)
      iqr <- q3 - q1
      x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr)
    }
    df1 <- one_item_data()
    # Apply the function to each numeric column and create a logical matrix
    outliers_matrix <- 
      sapply(df1 [3:max(col(df1))], is_outlier)
    
    colSums(outliers_matrix)
  })

  output$boxplot <- renderPlot({one_item_box()})
  
  ## Sinle Item SD
  
  
  one_item_outlier <- reactive({
    columns <-
      substr(colnames(high_sd()[2:(max(col(high_sd())))]), start = 1, stop = 2)
    # !outliers_count
    
    one_item_data() %>% 
      #select(id, reading, columns) %>%
      group_by(id) %>% 
      mutate(across(everything(), list(z_score = scale))) %>% 
      select(id, reading, sort(names(.))) %>% 
      select(!contains(c("reading", "outliers_count")))
  })
  
  output$one_item_outlier <- renderTable({one_item_outlier()})
  
  output$one_item_summary <- renderPrint({
    summary(select(one_item_outlier(), !contains(c("id", "reading", "z_score")))) 
  })
  
  output$one_item_high_sd <- renderTable({
    
    one_item_outlier()  %>% 
      select(!contains(c("reading", "z_score"))) %>% 
      group_by(id) %>%
      summarise(across(everything(), list(sd = sd, range = diffrange)))
    
    })
  
  # Cleaned Reading Tables.
  
  output$reading_item <- renderUI({
    checkboxGroupInput(
      "reading_item", 
      "Reading Number", 
      choices = sort(unique(unlist(data_set2()$reading))),
      selected = sort(unique(unlist(data_set2()$reading))),
      inline = TRUE)
  })
  
  reading_item_selected <- reactive({
    data_set2() %>% filter(reading %in% input$reading_item)
  })
  
  output$reading_item_selected <- renderTable({reading_item_selected()})
  
  reading_item_selected_sd <- reactive({
    reading_item_selected() %>%
      select(!contains("reading")) %>% 
      group_by(id) %>%
      summarise(across(everything(), list(mean = mean, sd = sd)))
  })
  
  output$reading_item_selected_sd <- renderTable({reading_item_selected_sd()})
  
  reading_item_selected_minmax <- reactive({
    reading_item_selected() %>%
      select(!contains("reading")) %>% 
      group_by(id) %>%
      summarise(across(everything(), list(min = min, max = max)))
  })
  
  output$reading_item_selected_minmax <- 
    renderTable({reading_item_selected_minmax()})
  
  # Ploting the data
  
  data_plot <- reactive({
    
    dfx <- reading_item_selected_minmax()
    df_max <- select(dfx, c("id", contains("_max")))
    colnames(df_max) <- c("id", gsub('_max', '', colnames(select(df_max, contains("max")))))
    
    no_0 <- colnames(select(df_max[,as.vector(which(colSums(df_max[2:ncol(df_max)]) >0))],!id))
    
     
    
    plot_data <- reading_item_selected_sd() %>% select(id, starts_with(no_0)) 
    
    data_mean <-
      plot_data %>%
      pivot_longer(
        cols = (contains("mean")),
        names_to = "Type",
        values_to = "Value_mean"
      ) %>%
      select(id, Type, Value_mean)
    
    data_sd <-
      plot_data %>%
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
      tbl1_primary <- data_set1()
      tbl2_total <- data_set2()
      tbl3_clean <- reading_item_selected()
      tbl4_sd <- reading_item_selected_sd()
      tbl5_minmax <- reading_item_selected_minmax()
      
      
      sheets <- mget(ls(pattern = "tbl")) # getting all objects in your environment with tbl in the name
      #names(sheets) <- paste0("sheet", seq_len(length(sheets))) # changing the names in your list
      writexl::write_xlsx(sheets, path = file) # saving the file
    }
  )
  
  
}
