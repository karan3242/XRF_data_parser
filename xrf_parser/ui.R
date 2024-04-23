#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(readr)
library(tidyverse)
library(viridis)
library(plotly)

# Define the UI
fluidPage(
  navbarPage(
    "XRF Data Parsing",
    
    # Primary Data tab
    tabPanel(
      "Primary Data",
      mainPanel(
        fileInput(
          "file1",
          "Choose CSV File",
          accept = c("text/csv", 
                     "text/comma-separated-values,text/plain", 
                     ".csv")
        ),
        tagList(
          tags$h1("Primary Data"),
          tags$p("<LOD values have been converted to 0"),
          tableOutput("data_set")
        )
      )
    ),
    
    # Data Overview tab
    tabPanel(
      "Data Overview",
      mainPanel(
        tags$h1("Selected Elements"),
        uiOutput("lab_items"),
        checkboxGroupInput(
          "elements",
          "Select Elements",
          choices = c(
            "Ag", "Al", "As", "Au", "Bi", "Cd", "Co", "Cr", "Cu", "Fe", "Hf",
            "Mg", "Mn", "Mo", "Nb", "Ni", "P", "Pb", "Pd", "Re", "S", "Sb",
            "Sc", "Si", "Sn", "Sr", "Ta", "Ti", "V", "W", "Zn", "Zr"
          ),
          inline = TRUE,
          selected = c(
            "Ag", "Al", "As", "Au", "Bi", "Cd", "Co", "Cr", "Cu", "Fe",
            "Hf", "Mg", "Mn", "Mo", "Nb", "Ni", "P", "Pb", "Pd", "Re", "S",
            "Sb", "Sc", "Si", "Sn", "Sr", "Ta", "Ti", "V", "W", "Zn", "Zr"
          )
        ),
        tableOutput("data_clean"),
        tags$h2("Summary"),
        tableOutput("data_overview"),
        tags$h2("Range"),
        tableOutput("data_summary_minmax")
      )
    ),
    
    # Normalized Data tab
    tabPanel(
      "Normalized Data",
      mainPanel(
        tags$h1("Normalized Data of Selected Elements"),
        tableOutput("data_normal"),
        tags$h2("Summary"),
        tableOutput("data_overview_norm"),
        tags$h2("Range"),
        tableOutput("data_overview_minmax")
      )
    ),
    
    # High SD Readings tab
    tabPanel(
      "High SD Readings",
      mainPanel(
        tags$h1("High SD items - Summary"),
        sliderInput(
          "cutoff",
          "Deviation Percentage Cutoff",
          min = 0,
          max = 10,
          value = 3,
          step = 0.01,
          ticks = FALSE
        ),
        tags$h2("Mean and Standard Deviation"),
        tableOutput("high_sd_overview"),
        tags$h2("Range"),
        tableOutput("data_minmax_highsd"),
        tags$h1("High SD items - Data"),
        sliderInput(
          "z_score",
          "Deviation Steps",
          min = 0,
          max = 3,
          value = 1,
          step = 0.01,
          ticks = FALSE
        ),
        tableOutput("high_sd_data")
      )
    ),
    
    # Plot tab
    tabPanel(
      "Plot",
      mainPanel(
        tags$h1("Plot"),
        plotlyOutput("plot"),
        tags$caption("Plot of Normalized Data")
      )
    )
  )
)
