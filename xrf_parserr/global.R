pkgs <- c("shiny", "readr", "tidyverse",
          "viridis", "plotly", "writexl",
          "CC","knitr","shinythemes", "bslib", "readxl",
          "reactable")
pkginst <- lapply(pkgs, library, character.only = TRUE)
theme_set(theme_classic())

# Function to Load Files

read_file <- \(file_path){
  df <- read.csv(file = file_path)
  return(df)
  }
