##### Main Function #####
function(input, output, session) {


# Data Persistence in Analytics -------------------------------------------

  # Data Persistence values
  rv <- reactiveValues(
    selected_rows_store = list(),
    elements_store = list()
  )


# Raw data ----------------------------------------------------------------


  raw_data <- eventReactive(input$raw_csv, {
    req(input$raw_csv)
    list_of_dfs <- purrr::map(input$raw_csv$datapath, read_file)
    combined_df <- dplyr::bind_rows(list_of_dfs)
    return(combined_df)
  })

  observeEvent(raw_data(), {
    req(raw_data())
    raw_data <- raw_data()
    # 1. Get the list of unique Lab_IDs
    lab_ids <- unique(raw_data$Lab_ID)
    methods <- unique(raw_data$`Method Name`)

    # 2. Update the selectInput widget
    updateSelectInput(
      session = session,
      inputId = "samples",
      # Set the new list of unique Lab_IDs as the choices
      choices = lab_ids,
      selected = lab_ids
    )
    updateSelectInput(
      session = session,
      inputId = "methods",
      # Set the new list of unique Lab_IDs as the choices
      choices = methods,
      selected = methods
    )

  })

  raw_data_filtred <- reactive({
    raw_data <- raw_data()
    raw_data <- raw_data[c(raw_data$Lab_ID %in% input$samples), ]
    raw_data <- raw_data[c(raw_data$`Method Name` %in% input$methods), ]
    return(raw_data)
  })
  
  notes_summary <- reactive({
    req(raw_data_filtred())
    raw_data_filtred() %>% 
      select(Lab_ID, Description, Notes) %>% 
      group_by(Lab_ID) %>% 
      dplyr::summarise("Description" = paste(unique(Description), sep = ";"),
                       "Notes" = paste(unique(Notes), sep = ";")) %>% 
      dplyr::mutate(Description = ifelse(Description == "0", NA_character_, Description), Notes = ifelse(Notes == "NA", NA_character_, Notes)) %>% 
      ungroup()
  })

  output$raw_data <- renderReactable({
    reactable(raw_data_filtred(), showPageSizeOptions = TRUE)
    })
  
  

# Subset data -------------------------------------------------------------

  subset_data <- reactive({
    raw_data <- req(raw_data_filtred())
    if (nrow(raw_data) == 0) return(NULL)
    output <- subset_fun(raw_data)
    req(output)
    return(output)
  })

  observeEvent(subset_data(), {
    # Only proceed if the data frame is valid and has columns
    df <- req(subset_data())
    if (is.null(df) || ncol(df) <= 1) return() 
    # Graceful exit if no data/elements

    # Use tryCatch for extra safety when dealing with external functions
    tryCatch({
      elements <- get_elements(df)

      updateSelectInput(
        session = session,
        inputId = "elements",
        choices = elements,
        selected = elements
      )
    }, error = function(e) {
      # If get_elements or updateSelectInput fails, 
      # print error instead of crashing
      message(paste("Error in element update:", e$message))
      return(NULL) # Fail gracefully
    })
  })

  subset_data_clean <- reactive({

    subset_clean <- req(subset_data())
    elements <- input$elements

    cols_to_keep <- grep(paste0("^", elements,".Concentration$", collapse = "|"), names(subset_clean), value = TRUE)
    valid_cols <- c("Lab_ID", cols_to_keep)
    subset_clean <- subset_clean[, valid_cols]

    output <- subset_clean

    if(input$drop_0val){
      output <- drop_0cols(output)
    }
    
    if(input$normalize){
      output <- normlization_fun(output)
    }

    req(output)
    return(output)

  })

  output$subset_data <- renderReactable({
    reactable(clean_colnames(subset_data_clean()),showPageSizeOptions = TRUE)
  })


# Analytics ---------------------------------------------------------------

  # Creates List of Each Sample with its own data frame.
  sample_wise_list <- reactive({
    samples <- req(input$samples)
    df <- req(subset_data_clean())

    list_xrf <- setNames(lapply(seq_along(samples), \(x){

      df <- df[df$Lab_ID == samples[x],]
      if(input$drop_0val){
        df <- drop_0cols(df)
      }
      if(input$normalize){
        df <- normlization_fun(df)
      }

      df$Reading <- seq(max(row(df)))
      return(df)
    }), samples)
  })

  # Get the names of the Current sample lab ids
  samples <- reactive({
    req(sample_wise_list())
    df <- sample_wise_list()
    return(names(df))
  })

  # Create a Drop down for selecting Sample lab ids
  output$samples2 <- renderUI({
    req(samples())
    selectInput("samples2",
                "Selecte Sample",
                choices = samples(),
                selected = samples()[1])
  })

  # Retrive the Data frame form the list which corresponds to the selected lab id.
  selected_list_item <- reactive({sample_wise_list()[[input$samples2]]})

  # Use renderUI to create the checkboxes dynamically
  # Modified output$dynamic_checkboxes
  output$dynamic_checkboxes <- renderUI({
    current_df <- selected_list_item()
    current_sample_id <- input$samples2 # Get the current sample ID

    choices <- current_df$Reading

    # Check if a saved selection exists for this sample
    saved_selection <- rv$selected_rows_store[[current_sample_id]]

    # Use the saved selection if it exists, otherwise use all choices
    initial_selected <- if (is.null(saved_selection)) choices else saved_selection

    checkboxGroupInput(
      inputId = "selected_rows",
      label = paste("Available IDs in", current_sample_id), # Use current_sample_id instead of input$df_choice
      choices = choices,
      selected = initial_selected
    )
  })

  # Observe changes in selected rows and save them
  observeEvent(input$selected_rows, {
    current_sample_id <- input$samples2
    if (!is.null(current_sample_id)) {
      rv$selected_rows_store[[current_sample_id]] <- input$selected_rows
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE) # ignoreInit prevents saving 'NULL' on startup

  # Creata a Sliletcize for Element selection
  elements_sub <- reactive({
    req(selected_list_item())
    get_elements(selected_list_item())
  })

  # Modified output$elements2
  output$elements2 <- renderUI({
  req(elements_sub())
  current_sample_id <- input$samples2 # Get the current sample ID

  all_elements <- elements_sub()

  # Check if a saved selection exists for this sample
  saved_selection <- rv$elements_store[[current_sample_id]]

  # Use the saved selection if it exists, otherwise use all elements
  initial_selected <- if (is.null(saved_selection)) all_elements else saved_selection

  selectizeInput("elements2",
    "Select elements",
    choices = all_elements,
    selected = initial_selected,
    multiple = TRUE
  )
})

  # Observe changes in element selections and save them
  observeEvent(input$elements2, {
    current_sample_id <- input$samples2
    if (!is.null(current_sample_id)) {
      rv$elements_store[[current_sample_id]] <- input$elements2
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # Created Filedred Sample table
  selected_list_item_read <- reactive({
    req(input$elements2)
    req(input$selected_rows)
    req(selected_list_item())

    elements <- input$elements2
    df <- selected_list_item()
    df <- df[df$Reading %in% input$selected_rows,]

    cols_to_keep <- grep(paste0("^", elements,".Concentration$", collapse = "|"), names(df), value = TRUE)
    valid_cols <- c("Lab_ID", "Reading", cols_to_keep)
    df <- df[, valid_cols]

    if(input$normalize){
      df <- normlization_fun(df)
    }

    df <- df[, cols_to_keep]
    return(df)

  })

  # Creata Anlayised table Req selected_list_item_read()
  selected_list_item_analysized <- reactive({
    req(selected_list_item_read())
    x <- selected_list_item_read()
    t <- round(
    data.frame(t(data.frame(
      "Min"= apply(x[select_elements(x)], 2, min, na.rm=TRUE),
      "Q1" = apply(x[select_elements(x)], 2, quantile,probs = 0.25, na.rm=TRUE),
      "Median" = apply(x[select_elements(x)], 2, median, na.rm=TRUE),
      "Mean"= apply(x[select_elements(x)], 2, mean, na.rm=TRUE),
      "Q3" = apply(x[select_elements(x)], 2, quantile,probs = 0.75, na.rm=TRUE),
      "Max"= apply(x[select_elements(x)], 2, max, na.rm=TRUE),
      "StDev" = apply(x[select_elements(x)], 2, sd, na.rm=TRUE)
    ))), 3)
    names(t) <- names(x)
    t <- t %>% 
      mutate(across(-1, ~ ifelse(is.finite(.x), .x, NA)))
    return(t)
  })

  selected_list_item_combined <- reactive({
    req(selected_list_item_read(), selected_list_item_analysized())
    dplyr::bind_rows(selected_list_item_read(),selected_list_item_analysized())
  })
  # Table Outputes
  output$sample_wise_list_analysied <-  renderReactable({
    # reactable(clean_colnames(selected_list_item_analysized()))
    reactable(clean_colnames(selected_list_item_combined()))
  })


# Summary List ------------------------------------------------------------

  # Get Persistence List of Items
  final_sample_wise_list <- reactive({
    # Depend on the storage to rerun whenever a selection for *any* sample changes
    req(length(rv$selected_rows_store) > 0)
    req(length(rv$elements_store) > 0)

    all_samples_list <- req(sample_wise_list())

    # This will be the list of all final, filtered data frames
    final_list <- list()

    # Iterate over every sample ID
    for (sample_id in names(all_samples_list)) {
      df <- all_samples_list[[sample_id]]

      # 1. Get the current selections for this sample from persistent storage

      # Get the selected rows (Reading IDs). Default to all if not saved yet.
      selected_rows <- rv$selected_rows_store[[sample_id]]
      if (is.null(selected_rows)) {
        selected_rows <- df$Reading
      }

      # Get the selected elements. Default to all elements if not saved yet.
      selected_elements <- rv$elements_store[[sample_id]]
      if (is.null(selected_elements)) {
        # Use the function you defined to get all possible elements for this sample
        selected_elements <- get_elements(df)
      }

      # 2. Filter the data frame

      # Filter by Reading
      df <- df[df$Reading %in% selected_rows, , drop = FALSE]

      # Filter by Element Columns
      elements_pattern <- paste0("^", selected_elements, ".Concentration$", collapse = "|")
      element_cols <- grep(elements_pattern, names(df), value = TRUE)

      # FIX: Explicitly include Lab_ID and Reading
      cols_to_keep <- c("Lab_ID", "Reading", element_cols)

      # Ensure all columns to keep actually exist in df (safeguard)
      cols_to_keep <- intersect(cols_to_keep, names(df))

      # Keep the necessary columns
      df <- df[, cols_to_keep, drop = FALSE]

      # 3. Apply normalization again if necessary
      # Note: Ensure this step is consistent with your original logic in 'selected_list_item_read'
      if(input$normalize){
        df <- normlization_fun(df)
      }

      # 4. Add the final filtered DF to the list
      final_list[[sample_id]] <- df
    }

    return(final_list)
  })

  # Collaps List into dataframe.
  final_sample_wise_df <- reactive({
    list <- req(final_sample_wise_list())
    final_df <- as.data.frame(dplyr::bind_rows(list))
    return(final_df)
  })
  # From final_sample_List Create a list of analtics.
  final_sample_wise_analytics <- reactive({
    list_xrf <- req(final_sample_wise_list())

    test <- lapply(list_xrf, \(x){
      Lab_ID <- unique(x$Lab_ID)
      list_df <- data.frame(t(data.frame(
        "Min" = apply(x[select_elements(x)], 2, min, na.rm = TRUE),
        "Q1" = apply(x[select_elements(x)], 2, quantile,probs = 0.25, na.rm = TRUE),
        "Median" = apply(x[select_elements(x)], 2, median, na.rm = TRUE),
        "Mean" = apply(x[select_elements(x)], 2, mean, na.rm = TRUE),
        "Q3" = apply(x[select_elements(x)], 2, quantile,probs = 0.75, na.rm = TRUE),
        "Max" = apply(x[select_elements(x)], 2, max, na.rm = TRUE),
        "StDev" = apply(x[select_elements(x)], 2, sd, na.rm = TRUE)
      )))
      list_df$Analytics <- rownames(list_df)
      rownames(list_df) <- NULL
      list_df$Lab_ID <- rep(Lab_ID, nrow(list_df))
      
      return(list_df)

    })

    test_df <- dplyr::bind_rows(test)

    test_df <- test_df[,c("Lab_ID", "Analytics", select_elements(test_df))]
  })

  # Filtering Analysis Types to display and save.
  final_sample_wise_analytics_filtered <- reactive({
    req(notes_summary())
    analytics_df <- req(final_sample_wise_analytics())
    analytics <- req(input$Analytics)
    analytics_df <- analytics_df[analytics_df$Analytics %in% analytics,]
    analytics_df <- left_join(analytics_df, notes_summary(), by = join_by(Lab_ID))
    return(analytics_df)
  })

  #Table Output
  output$final_sample_wise_df <- renderReactable({
    reactable(clean_colnames(final_sample_wise_df()))
  })
  output$final_sample_wise_analytics <- renderReactable({
    reactable(clean_colnames(final_sample_wise_analytics_filtered()))
  })

  # ---- Doanload Handler ----

  output$save_analysis <- downloadHandler(
    filename = paste0("xrf_analysis", Sys.Date(),".xlsx"),
    content = \(file) {
      write_xlsx(
        list(
          "Raw Data" = req(raw_data_filtred()),
          "Subset Data" = req(clean_colnames(subset_data_clean())),
          "Cleaned Data" = req(clean_colnames(final_sample_wise_df())),
          "Analytical Data" = req(clean_colnames(final_sample_wise_analytics_filtered()))
        ),
        path = file
      )
    }
  )

}
