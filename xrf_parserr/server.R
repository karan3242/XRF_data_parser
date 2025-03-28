#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

##### Load Preamble #####
# source("./SubFiles/preamble.R")
# source("./SubFiles/functions.R")
##### Main Function #####
function(input, output, session) {
  
  ##### Primary data #####
  # source("./SubFiles/primarydata.R")
  data_set1 <- primarydata(input, output, session)
  output$data_set1 <- renderTable({
    data_set1()
  })
  
  ##### Data Overview #####
  # Lab items output selection
  ## lab items selections
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
      select(id , `reading`, starts_with(input$elm)) %>%
      select(id, `reading`, contains("Concentration")) %>%
      select(id, `reading`, sort(names(.))) %>%
      filter(id %in% input$lab_id)
    
    coln <- gsub(' Concentration', '', colnames(select(df1, contains(
      "Concentration"
    ))))
    
    colnames(df1) <- c("id", "reading", coln)
    
    print(df1)
  })
  
  ## Nomral Data
  data_set_normal <- reactive({
    normalize_rows(data_set_clean())
  })
  
  # Render data set after items and elements selected
  data_set2 <- reactive({
    if (input$normal_select == FALSE) {
      data_set_clean()
    } else{
      data_set_normal()
    }
  })
  output$data_set2 <- renderTable({
    data_set2()
  })
  
  # Mean and SD of clean Data
  
  data_overview <- reactive({
    data_set2() %>%
      select(!contains("reading")) %>%
      group_by(id) %>%
      summarise(across(everything(), list(
        sd = sd, skew = ~ skewness(.x)
      )))
  })
  
  output$data_overview <- renderTable({
    data_overview()
  })
  
  # Min-Max of clean Data
  
  data_summary_minmax <- reactive({
    data_set2() %>%
      select(!contains("reading")) %>%
      group_by(id) %>%
      summarise(across(
        everything(),
        list(
          mean = mean,
          median = median,
          range = diffrange
        )
      ))
  })
  output$data_summary_minmax <- renderTable({
    data_summary_minmax()
  })
  
  # Isolating High SD data
  
  high_sd <- reactive({
    high_sd <- function(x) {
      x >= input$cutoff
    }
    skew_abs <- function(x) {
      x >= input$skew_cut
    }
    
    data_overview() %>%
      filter(if_any(ends_with("sd"), high_sd)) %>%
      filter(if_any(ends_with("skew"), skew_abs)) %>%
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
    
    
    outliers <- function(x) {
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
  
  
  
  ##### Item Wise Data #####
  
  output$one_item <- renderUI({
    selectInput("one_item",
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
    elmt <- sort(colnames(select(data_set2(), !contains(
      c("id", "reading")
    ))))
    
    df1 <- one_item_data0()[3:ncol(one_item_data0())]
    
    elmt2 <- colnames(df1[, as.vector(which(round(colSums(df1), 2) > 0))])
    
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
    elm <- colnames(select(data, !contains(c(
      "id", "reading", "outliers_count"
    ))))
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
    if (input$rm_outlier) {
      one_item_data_nooutlier()
    }
    else{
      one_item_data()
    }
  })
  
  output$one_item_table <- renderTable({
    one_item_table()
  })
  
  #output$one_item_data_nooutlier <- renderTable({one_item_data_nooutlier()})
  
  one_item_box <- reactive({
    df1 <- one_item_table() %>% select(!contains(c("reading", "outliers_count")))
    df2 <- pivot_longer(
      df1,
      cols = colnames(df1)[2:max(col(df1))],
      names_to = 'Elements',
      values_to = 'counts'
    )
    
    plot <- ggplot(df2, aes(x = Elements, y = counts, fill = Elements)) +
      geom_boxplot(outliers = TRUE) +
      scale_fill_viridis(discrete = TRUE , alpha = 0.6) +
      theme(legend.position = "none")
    
    
    plot2 <- ggplot(df2, aes(x = Elements, y = counts, fill = Elements)) +
      geom_boxplot(outliers = FALSE) +
      geom_jitter(color = "red", alpha = 0.9) +
      scale_fill_viridis(discrete = TRUE , alpha = 0.6) +
      theme(legend.position = "none")
    
    
    if (input$jitter) {
      print(plot2)
    } else{
      print(plot)
    }
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
  
  output$boxplot <- renderPlot({
    one_item_box()
  })
  
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
  
  # Test
  
  one_item_summary <- reactive({
    table_data <- one_item_data0()
    
    # Pivot the data so rows for `reading` become columns
    wider_table <- pivot_longer(
      table_data,
      cols = -c(id, reading),
      # Keep `id` and `reading` intact
      names_to = "element",
      # Column for variable names
      values_to = "value"       # Column for values
    ) %>%
      pivot_wider(names_from = reading, # Use `reading` as new column names
                  values_from = value)     # Fill the new columns with values)
                  
                  std <- apply(wider_table[, -c(1, 2)], 1, sd)
                  avg <- apply(wider_table[, -c(1, 2)], 1, mean)
                  median <- apply(wider_table[, -c(1, 2)], 1, median)
                  sum <- apply(wider_table[, -c(1, 2)], 1, sum)
                  range <- apply(wider_table[, -c(1, 2)], 1, diffrange)
                  
                  df <- cbind(wider_table, std, avg, median, range, sum)
                  df <- filter(df, sum != 0)
                  df <- select(df, -id, -sum)
                  
                  df
  })
    
    output$one_item_summary <- renderTable({
      one_item_summary()
      
    })
    
    # Cleaned Reading Tables.
    
    output$reading_item <- renderUI({
      checkboxGroupInput(
        "reading_item",
        "Reading Number",
        choices = sort(unique(unlist(
          data_set2()$reading
        ))),
        selected = sort(unique(unlist(
          data_set2()$reading
        ))),
        inline = TRUE
      )
    })
    
    reading_item_selected <- reactive({
      data_set2() %>% filter(reading %in% input$reading_item)
    })
    
    output$reading_item_selected <- renderTable({
      reading_item_selected()
    })
    
    reading_item_selected_sd <- reactive({
      reading_item_selected() %>%
        select(!contains("reading")) %>%
        group_by(id) %>%
        summarise(across(
          everything(),
          list(
            sd = sd,
            mean = mean,
            median = median,
            range = diffrange
          )
        ))
    })
    
    output$reading_item_selected_sd <- renderTable({
      reading_item_selected_sd()
    })
    
    reading_item_selected_minmax <- reactive({
      reading_item_selected() %>%
        select(!contains("reading")) %>%
        group_by(id) %>%
        summarise(across(everything(), list(min = min, max = max)))
    })
    
    output$reading_item_selected_minmax <-
      renderTable({
        reading_item_selected_minmax()
      })
    
    
    
  ##### Plotting and Saving #####
    
    data_plot <- reactive({
      dfx <- reading_item_selected_minmax()
      df_max <- select(dfx, c("id", contains("_max")))
      colnames(df_max) <- c("id", gsub('_max', '', colnames(select(
        df_max, contains("max")
      ))))
      
      no_0 <- colnames(select(df_max[, as.vector(which(colSums(df_max[2:ncol(df_max)]) >
                                                         0))], !id))
      
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
          theme(axis.line = element_blank(), axis.ticks = element_blank(), ) +
          labs(x = NULL, y = "Element %", title = "Elemet values - Normalized Data")
      )
      
    })
    output$plot <- renderPlotly({
      data_plot()
    })
    
    # Item Type Breakdown
    
    item_type_summary <- reactive({
      data <- reading_item_selected_sd() %>% select(id, contains("mean"))
      
      ids <- data$id
      
      
      for (i in ids) {
        # Set Element Variables
        cu <- filter(data, id == i) %>% select(contains("Cu")) %>% as.double()
        sn <- filter(data, id == i) %>% select(contains("Sn")) %>% as.double()
        pb <- filter(data, id == i) %>% select(contains("Pb")) %>% as.double()
        fe <- filter(data, id == i) %>% select(contains("Fe")) %>% as.double()
        ag <- filter(data, id == i) %>% select(contains("Ag")) %>% as.double()
        
        
        # Replace NA values with 0
        cu <- ifelse(is.na(cu), 0, cu)
        sn <- ifelse(is.na(sn), 0, sn)
        pb <- ifelse(is.na(pb), 0, pb)
        fe <- ifelse(is.na(fe), 0, fe)
        ag <- ifelse(is.na(ag), 0, ag)
        
        # Initialize variables for results
        el <- ""
        cutoff <- 9
        br <- NA
        tin_val <- NA
        
        # Main logic
        if (cu > max(fe, pb, ag)) {
          if (sn >= 1) {
            el <- "Bronze"
            if (cu / sn < cutoff) {
              if (cu / sn < 1) {
                br <- round(cu / sn, 2)
                tin_val <- "(Mostly Tin)"
              }
              br <- round(cu / sn, 2)
              tin_val <- "(High Tin content)"
            } else {
              br <- round(cu / sn, 2)
              tin_val <- "(Low Tin content)"
            }
          } else {
            el <- "Copper"
            tinq <- NA
            br <- NA
            tin_val <- NA
          }
        } else if (fe > max(pb, ag)) {
          el <- "Iron"
        } else if (pb > ag) {
          el <- "Lead"
        } else {
          el <- "Silver"
        }
        
        # Create a Data frame
        if (!exists("df_summary")) {
          df_summary <- data.frame(
            id = character(0),
            element = character(0),
            Cu_content = double(0),
            Sn_content = double(0),
            Pb_content = double(0),
            Fe_content = double(0),
            Cu_Sn = double(0),
            Bronze_quant = character(0),
            stringsAsFactors = FALSE
          )
        }
        # Append to the data frame
        
        
        df_summary <- rbind(
          df_summary,
          data.frame(
            id = i,
            element = el,
            Cu_content = ifelse(is.na(cu), 0, cu),
            Sn_content = ifelse(is.na(sn), 0, sn),
            Pb_content = ifelse(is.na(pb), 0, pb),
            Fe_content = ifelse(is.na(fe), 0, fe),
            Cu_Sn = ifelse(is.na(br) & el != "Bronze", NA, br),
            Bronze_quant = ifelse(is.na(tin_val) &
                                    el != "Bronze", "", tin_val),
            stringsAsFactors = FALSE
          )
        )
        
        
      }
      
      df_summary
      
    })
    
    output$item_type_summary <- renderTable({
      item_type_summary()
    })
    
    # Download Handel
    output$dl <- downloadHandler(
      filename = function() {
        paste0("df_dmodel", "_Table", ".xlsx")
      },
      content = function(file) {
        tbl1_primary <- data_set1()
        tbl2_total <- data_set2()
        tbl3_no_outlier <- reading_item_selected()
        tbl4_summary <- reading_item_selected_sd()
        tbl5_type <- item_type_summary()
        
        
        sheets <- mget(ls(pattern = "tbl")) # getting all objects in your environment with tbl in the name
        #names(sheets) <- paste0("sheet", seq_len(length(sheets))) # changing the names in your list
        writexl::write_xlsx(sheets, path = file) # saving the file
      }
    )
    
    # Report
    
    output$report <- downloadHandler(
      filename = function() {
        "report.html"  # File name for the downloaded file
      },
      
      content = function(file) {
        # Create a temporary file path for the RMarkdown report
        tempReport <- file.path(tempdir(), "report.Rmd")
        
        # Copy the RMarkdown file to the temporary directory
        file.copy(
          "./report.Rmd",
          tempReport,
          overwrite = TRUE
        )
        
        # Define the parameters for the RMarkdown report
        params <- list(
          choice = reading_item_selected(),
          reading_value = data_set1(),
          norm = input$normal_select
        )
        
        # alt data data_set2()
        
        # Render the RMarkdown report to the specified output file
        out <- rmarkdown::render(
          input = tempReport,
          params = params,
          envir = new.env(parent = globalenv())
        ) #End render
        file.rename(out,file)
      }
    )
    

    ###### Beam spectra #####
    
    # Source xray Energy Files

    
    beam1 <- reactive({
      inFile <- input$fileb
      df1 <- read_csv(inFile$datapath) %>% slice(2, 4, 40:2086) %>% select(-1)
      df1 <- df1[, colSums(is.na(df1)) == 0] #remove Na colums
    })
    #output$beam1 <- renderTable({beam1()})
    
    output$readings <- renderUI({
      selectInput("reading",
                  "Reading",
                  choices = unique(unlist(beam1()[1, ])),
                  selected = NULL)
      
    })
    
    xrayEnergy <- reactive({readxl::read_xlsx("./../xray_energy_table.xlsx") %>%
        mutate(Element = word(Element,1))})
    
    output$xrayElement <- renderUI({
      selectInput(
        "xrayElement",
        "Element Emission Energies",
        choices = xrayEnergy()$Element
      )
    })
    
    EnergyLines <- reactive({
        filter(xrayEnergy(), Element == input$xrayElement) %>% select(-1)
    })
    
    output$EnergyLines <- renderTable({
      EnergyLines()
    })
    
    df3 <- reactive({
      df2 <- as.vector(beam1()[1, ] == input$reading)
      df3 <- as.data.frame(beam1()[, df2]) %>% slice(2:max(row(beam1())))
      names(df3) <- df3[1, ]
      df3 <- df3 %>% slice(2:max(row(beam1())))
      
      kev <- seq(0.02 , by = 0.02, length.out = max(row(df3)))
      
      df3$kev <- kev
      df3 <- mutate_all(df3, as.double)
    })
    #output$df3 <-renderTable({df3()})
    
    output$xaxis <- renderUI({
      xmax <- max(df3()$kev)
      sliderInput("xaxis", "X-axis", 0, xmax, value = c(0, 10))
    })
    
    output$yaxis <- renderUI({
      lim <- max(max(df3()[, 1]), max(df3()[, 2]))
      sliderInput("yaxis", "Y-axis", 0, lim, value = lim)
    })
    
    
    plot1 <- reactive({
      
     ggplot(df3(), aes(x = kev)) +
        geom_area(aes(y = `2`, fill = 'Exposer 2')) +
        geom_area(aes(y = `1`, fill = 'Exposer 1')) +
        scale_fill_manual(
          values = c("#CD6C53", "#839FBA"),
          labels = c("Exposer 1", "Exposer 2")
        ) +
        coord_cartesian(xlim = c(input$xaxis[1], input$xaxis[2]),
                        ylim = c(0, input$yaxis)) +
        scale_y_continuous(expand = expansion(mult = 0)) +
        geom_vline(xintercept = as.numeric(EnergyLines()$Ka), color = 'blue') + 
        geom_vline(xintercept = as.numeric(EnergyLines()$Kb), color = 'darkblue') + 
        geom_vline(xintercept = as.numeric(EnergyLines()$La), color = 'green') + 
        geom_vline(xintercept = as.numeric(EnergyLines()$Lb), color = 'darkgreen') + 
        labs(y = "Counts/s", x = "KeV") +
        theme(
          legend.position = "bottom",
          #legend.justification = c("left", "top"),
          legend.title = element_blank(),
          legend.background = element_rect(size = 1, color = "grey"),
          axis.title = element_text(face = "bold")
        ) +
        labs(title = input$title, col = 'variable')
      
     
    })
    
    plot2 <- reactive({
      ggplot(df3(), aes(x = kev)) +
        geom_area(aes(y = `2`, fill = 'Exposer 2')) +
        geom_area(aes(y = `1`, fill = 'Exposer 1')) +
        scale_fill_manual(
          values = c("#CD6C53", "#839FBA"),
          labels = c("Exposer 1", "Exposer 2")
        ) +
        coord_cartesian(xlim = c(input$xaxis[1], input$xaxis[2]),
                        ylim = c(0, input$yaxis)) +
        scale_y_continuous(expand = expansion(mult = 0)) +
        labs(y = "Counts/s", x = "KeV") +
        theme(
          legend.position = "bottom",
          #legend.justification = c("left", "top"),
          legend.title = element_blank(),
          legend.background = element_rect(size = 1, color = "grey"),
          axis.title = element_text(face = "bold")
        ) +
        labs(title = input$title, col = 'variable')
    })
    
    
    output$beam_plot <- renderPlot({
      if(input$ElementLines){
        plot1()
      } else {
        plot2()
      }
      
      
      # plot2 <- plot1 +
      #   geom_area(aes(y=`3`, fill = 'Exposer 3'))
      
    })
    
    #Save plot to file
    observeEvent(input$savePlot, {
      ggsave(
        filename = file.path(
          input$dstpath,
          paste0(input$dstplot, "_", input$title, ".png")
        ),
        plot = plot1(),
        width = as.integer(input$pwidth),
        height = as.integer(input$pheight),
        units = "px",
        dpi = as.integer(input$dpi)
      )
    })
    
    
}
