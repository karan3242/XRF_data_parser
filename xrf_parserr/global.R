# Load packages -----------------------------------------------------------

pkgs <- c("shiny", "readr", "tidyverse",
          "plotly", "writexl",
          "CC","shinythemes", "bslib", "readxl",
          "reactable")
pkginst <- lapply(pkgs, library, character.only = TRUE)
theme_set(theme_classic())

# Function to Load Files --------------------------------------------------

read_file <- \(file_path) {
  ext <- tools::file_ext(file_path)
  
  df <- switch(tolower(ext),
               "csv"  = readr::read_csv(file_path),
               "xlsx" = readxl::read_excel(path = file_path),
               "xls"  = readxl::read_excel(path = file_path),
               stop("Unsupported file extension")
  ) %>% 
    mutate(across(contains("Concentration"), ~ as.numeric(as.character(.x))))
  
  if (!("Lab_ID" %in% names(df))) {
    warning(paste("Column 'Lab_ID' not available in file:", file_path))
  }
  
  return(df)
  }

# Function Select Elements ------------------------------------------------

## Function Slectes Colums which are 
select_elements <- \(df){grep(".Concentration", names(df), value = TRUE)}
## Function Gets Element names
get_elements <- \(df){gsub(".Concentration","",select_elements(df))}


# Normalization function --------------------------------------------------

normlization_fun <- \(df){
  Lab_ID <- df["Lab_ID"]
  rows <- df[, select_elements(df)]
  normalized_row <- t(apply(rows, 1, \(row) {
    row_sum <- sum(row, na.rm = TRUE)
    if (row_sum == 0) {
      return(rep(0, length(row)))  # Handle rows that sum to 0
    }
    normalized_row <- row / row_sum * 100
    return(normalized_row)
  }))
  normalized_row <- cbind(Lab_ID, normalized_row)
  return(normalized_row)
}


# Drop Null columns function ----------------------------------------------

drop_0cols <- \(df){
  sub <- df[,c("Lab_ID", select_elements(df))]
  noval_cols = colSums(sub[select_elements(sub)], na.rm = TRUE) != 0
  Lab_ID <- sub[, !(names(sub) %in% select_elements(sub))]
  
  sub_elements <- sub[, select_elements(sub)]
  sub_elements_drop <- sub_elements[,noval_cols]
  
  sub_droped <- cbind(Lab_ID, sub_elements_drop)
  output <- sub_droped
  return(output)
  
}

# Function to clean column names ------------------------------------------

clean_colnames <- \(df){
  colnames(df) <- gsub(".Concentration", "", names(df))
  df <- df %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
  return(df)
}

# Function to subset elements ---------------------------------------------

subset_fun <- \(df){
  
  df[, select_elements(df)] <- lapply(df[, select_elements(df)], as.numeric)
  output <- df[,c("Lab_ID", select_elements(df))]
  return(output)
}
