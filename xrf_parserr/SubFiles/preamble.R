pkgs <- c("shiny", "readr", "tidyverse",
         "viridis", "plotly", "writexl",
         "CC","knitr","shinythemes", "bslib", "readxl")
pkginst <- lapply(pkgs, library, character.only = TRUE)
theme_set(theme_classic())
