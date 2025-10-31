##### Main Function #####
function(input, output, session) {
  # ----- Raw Data ----
  raw_data <- eventReactive(input$raw_csv,{
    req(input$raw_csv)
    read_file(input$raw_csv$datapath)
  })
  
  observeEvent(raw_data(), {
    
    # 1. Get the list of unique Lab_IDs
    lab_ids <- unique(raw_data()$Lab_ID)
    methods <- unique(raw_data()$Method.Name)
    
    # 2. Update the selectInput widget
    updateSelectInput(
      session = session,
      inputId = "samples",
      # Set the new list of unique Lab_IDs as the choices
      choices = lab_ids ,
      selected = lab_ids
    )
    updateSelectInput(
      session = session,
      inputId = "methods",
      # Set the new list of unique Lab_IDs as the choices
      choices = methods ,
      selected = methods
    )
    
  })
  
  raw_data_filtred <- reactive({
    raw_data <- raw_data()
    raw_data <- raw_data[c(raw_data$Lab_ID %in% input$samples), ]
    raw_data <- raw_data[c(raw_data$Method.Name %in% input$methods), ]
    return(raw_data)
  })
  
  output$raw_data <- renderReactable({

    reactable(raw_data_filtred(),showPageSizeOptions = TRUE)
    })
  # ---- Subset Data ----
  
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
    if (is.null(df) || ncol(df) <= 1) return() # Graceful exit if no data/elements
    
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
      # If get_elements or updateSelectInput fails, print error instead of crashing
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
  
  # ---- Analytics ----
  
  # Creates List of Each Sample with its own data frame.
  sample_wise_list <- reactive({
    req(input$drop_0val)
    req(input$normalize)
    samples <- req(input$samples)
    df <- req(subset_data_clean())
    
    list_xrf <- setNames(lapply(seq(length(samples)), \(x){
      
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
  output$dynamic_checkboxes <- renderUI({
    current_df <- selected_list_item()
    
    choices <- current_df$Reading

    checkboxGroupInput(
      inputId = "selected_rows",
      label = paste("Available IDs in", input$df_choice),
      choices = choices,
      selected = choices
    )
    
  })
  
  # Creata a Sliletcize for Element selection
  elements_sub <- reactive({
    req(selected_list_item())
    get_elements(selected_list_item())
  })
  
  output$elements2 <- renderUI({
    
    req(elements_sub())
    
    elements <- get_elements(sample_wise_list())
    
    selectizeInput("elements2",
                "Select elements",
                choices =  elements_sub(),
                selected =  elements_sub(),
                multiple = TRUE)
  })
  
  # Created Filedred Sample table
  selected_list_item_read <- reactive({
    req(input$normalize)
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
  slected_list_item_analysized <- reactive({
    req(selected_list_item_read())
    x <- selected_list_item_read()
    round(
    data.frame(t(data.frame(
      "Mean"= apply(x[select_elements(x)], 2, mean, na.rm=TRUE),
      "Median" = apply(x[select_elements(x)], 2, median, na.rm=TRUE),
      "Standard Deviantion" = apply(x[select_elements(x)], 2, sd, na.rm=TRUE)
    ))), 3)
  })
  
  
  # Table Outputes
  output$sample_wise_list <-  renderReactable({
    reactable(clean_colnames(selected_list_item_read()))
  })
  output$sample_wise_list_analysied <-  renderReactable({
    reactable(clean_colnames(slected_list_item_analysized()))
  })
  
  
  
}
