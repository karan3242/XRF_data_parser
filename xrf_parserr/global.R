pkgs <- c("shiny", "readr", "tidyverse",
          "plotly", "writexl",
          "CC","shinythemes", "bslib", "readxl",
          "reactable")
pkginst <- lapply(pkgs, library, character.only = TRUE)
theme_set(theme_classic())

# ---- Raw Data Loading -----
# Function to Load Files
read_file <- \(file_path) {
  if(grepl("\\.csv$", file_path, ignore.case = TRUE)) {
    df <- read.csv(file = file_path)
  } else if(grepl("\\.xlsx$|\\.xls$", file_path, ignore.case = TRUE)) {
    df <- readxl::read_excel(path = file_path)
  }
  
  if(!("Lab_ID" %in% names(df))) {
    warning("Colum Lab_ID no avilable")
    }
  
  return(df)
  }

# ---- Helper Functions ----
# Function Select Elements
## Function Slectes Colums which are 
select_elements <- \(df){grep("Concentration", names(df), value = TRUE)}
## Function Gets Element names
get_elements <- \(df){gsub(".Concentration","",select_elements(df))}

## Normalization Function.
normlization_fun <- \(df){
  Lab_ID <- df["Lab_ID"]
  rows <- df[, select_elements(df)]
  normalized_row <- t(apply(rows, 1, \(row) {
    row_sum <- sum(row, na.rm = TRUE)
    if (row_sum == 0) {
      return(rep(0, length(row)))  # Handle rows that sum to 0
    }
    normalized_row <- round((row / row_sum * 100), digits = 2)
    return(normalized_row)
  }))
  normalized_row <- cbind(Lab_ID, normalized_row)
  return(normalized_row)
}

## Drop Cols which Value to 0
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

# Function Clean Col names
clean_colnames <- \(df){
  colnames(df) <- gsub(".Concentration", "", names(df))
  return(df)
}

# ---- Subseting Function ----

subset_fun <- \(df){
  
  df[, select_elements(df)] <- lapply(df[, select_elements(df)], as.numeric)
  output <- df[,c("Lab_ID", select_elements(df))]
  return(output)
}


