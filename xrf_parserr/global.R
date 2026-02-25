# Load packages -----------------------------------------------------------

pkgs <- c("shiny", "readr", "tidyverse",
          "plotly", "writexl",
          "CC","shinythemes", "bslib", "readxl",
          "reactable", "Rcpp")
pkginst <- lapply(pkgs, library, character.only = TRUE)
sourceCpp("norm_function.cpp")

# Function to Load Files --------------------------------------------------
#function to capitalize Names
.simpleCap <- function(x) {
  s <- strsplit(x, "_")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = "_")
}

read_file <- \(file_path) {

  ext <- tools::file_ext(file_path)
  
  df <- switch(tolower(ext),
               "csv"  = readr::read_csv(file_path),
               "xlsx" = readxl::read_excel(path = file_path),
               "xls"  = readxl::read_excel(path = file_path),
               stop("Unsupported file extension")
  ) %>% 
    mutate(across(contains("Concentration"), ~ as.numeric(as.character(.x))),) %>%
    janitor::clean_names()
  names <- names(df)
  names(df) <- sapply(names, .simpleCap)
  if (!("Lab_Id" %in% names(df))) {
    warning(paste("Column 'Lab_Id' not available in file:", file_path))
  }
  
  return(df)
  }


# Function Select Elements ------------------------------------------------

## Function Slectes Colums which are 
select_elements <- \(df){grep("Concentration", names(df), value = TRUE)}
## Function Gets Element names
get_elements <- \(df){gsub(".Concentration","",select_elements(df))}


# Normalization function --------------------------------------------------

normlization_fun <- \(df){
  col_names <- names(df)[-1]
  return(normalization_cpp_std(df, col_names))
}


# Drop Null columns function ----------------------------------------------

drop_0cols <- function(df) {
  # 1. Identify which columns are elements (e.g., those ending in .Concentration)
  elem_cols <- select_elements(df)
  
  # Safety check: if no elements exist, return the original data
  if (length(elem_cols) == 0) return(df)
  
  # 2. Identify non-zero columns safely
  # We convert to numeric inside the check to avoid 'invalid type' errors
  is_not_zero <- sapply(df[elem_cols], function(col) {
    num_col <- as.numeric(as.character(col))
    sum(num_col, na.rm = TRUE) != 0
  })
  
  # 3. Filter columns to keep
  cols_to_keep <- elem_cols[is_not_zero]
  
  # 4. Use select() to maintain data frame structure (avoids dim(X) error)
  # We keep all non-element columns (metadata) plus the active elements
  output <- df %>% 
    dplyr::select(!all_of(elem_cols), all_of(cols_to_keep))
  
  return(output)
}

# Function to clean column names ------------------------------------------

clean_colnames <- \(df, round = TRUE){
  colnames(df) <- gsub(".Concentration", "", names(df))
  if(round){
    df <- df %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
  }
  return(df)
}

# Function to subset elements ---------------------------------------------

subset_fun <- \(df){
  
  df[, select_elements(df)] <- lapply(df[, select_elements(df)], as.numeric)
  output <- df[,c("Lab_Id", select_elements(df))]
  output[output == 0] <- NA_real_
  return(output)
}
