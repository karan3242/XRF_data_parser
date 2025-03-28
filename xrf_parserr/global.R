pkgs <- c("shiny", "readr", "tidyverse",
          "viridis", "plotly", "writexl",
          "CC","knitr","shinythemes", "bslib", "readxl")
pkginst <- lapply(pkgs, library, character.only = TRUE)
theme_set(theme_classic())



# Primary data parsing

primarydata <- function(input, output, session) {
  reactive({
    inFile <- input$file1
    
    # Read Input file and arragne it according to the Lab ID
    df <- read_csv(inFile$datapath) %>% # Reads file
      arrange(Lab_ID) %>% # Arranges according to Lab ID
      select(-Date, -Time) # Removes Date and Time
    
    # Create a new reading numbers for each row
    reading <- c(1:max(row(df)))
    
    # merge reading vector with the data frame and select relevant columns
    df <- cbind(df, reading) %>% 
      select(id = Lab_ID, reading, everything()) # Renames Lab ID to id
    
    # Convert all '<LOD' values to NA
    df[df == '<LOD'] <- NA
    
    # Format all columns which were characters as numeric
    df <- df %>%
      mutate_at(vars(contains("Concentration")), as.numeric)
    
    # Replace all NA values which were '<LOD' with 0
    df[is.na(df)] <- 0
    
    # Make all Lab ID characters
    df$id <- as.character(df$id)
    
    # Make all Instrument serial numbers as integer
    df$`Instrument Serial Num` <- as.integer(df$`Instrument Serial Num`)
    
    # Make all Test labels as integer
    df$`Test Label` <- as.integer(df$`Test Label`)
    
    # Convert all 0s in notes column to NA
    df$Notes[df$Notes == 0] <- NA
    
    # Convert notes column to character
    df$Notes <- as.character(df$Notes)
    
    # Returns Table
    return(df)
  })
}##### Normalisation of Rows function #####

normalize_rows <- function(data) {
  # Input validation
  if (!is.data.frame(data))
    stop("Input must be a data frame.")
  if (ncol(data) < 3)
    stop("Data must have at least 3 columns.")
  
  # Extracting IDs and readings
  id <- data[[1]]       # Assuming first column is ID
  reading <- data[[2]]  # Assuming second column is Reading
  
  # Normalizing rows
  normalized_data <- t(apply(data[, -c(1, 2), drop = FALSE], 1, function(row) {
    row_sum <- sum(row, na.rm = TRUE)
    if (row_sum == 0) {
      return(rep(0, length(row)))  # Handle rows that sum to 0
    }
    normalized_row <- round((row / row_sum * 100), digits = 2)
    return(normalized_row)
  }))
  
  # Combining results into a data frame
  normalized_data <- data.frame(id, reading, normalized_data)
  colnames(normalized_data) <- c(colnames(data)[1:2], colnames(data)[-c(1, 2)])
  
  return(normalized_data)
}

##### Skewness ######

skewness <- function(x) {
  abs(mean(x, na.rm = TRUE) - median(x, na.rm = TRUE))
}