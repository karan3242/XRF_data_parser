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
}