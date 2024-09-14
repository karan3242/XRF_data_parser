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
library(shinythemes)
library(bslib)

# Define the UI
fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(
    "XRF Data Parsing",
    
    # Primary Data tab
    tabPanel("Primary Data",
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
             ),
    
    # Data Overview tab
    tabPanel(
      "Data Overview",
        uiOutput("lab_items"),
        # checkboxInput("it_all", "Select all", value = TRUE),
        checkboxGroupInput(
          "elements",
          "Select Elements",
          choices = c( "Ag", "Al", "As", "Au", "Bi", "Cd", "Co", "Cr", "Cu", "Fe", "Hf", "Mg", "Mn", "Mo", "Nb", "Ni", "P", "Pb", "Pd", "Re", "S", "Sb", "Sc", "Si", "Sn", "Sr", "Ta", "Ti", "V", "W", "Zn", "Zr")
        ),
        checkboxInput("all", "Select all", value = TRUE),
      navset_tab(
        nav_panel("Selected Items",
                  
                  tableOutput("data_clean")),
        nav_panel(
          "Elements Overview",
          tableOutput("data_overview"),
          tableOutput("data_summary_minmax")
        )
      )
    ),
    
    # Normalized Data tab
    tabPanel("Normalized Data",
             navset_tab(
               nav_panel("Normalized Data", tableOutput("data_normal")),
               nav_panel(
                 "Summary",
                 tableOutput("data_overview_norm"),
                 tableOutput("data_overview_minmax")
               )
             )),
    
    # High SD Readings tab
    tabPanel("Outliers",
             navset_tab(
               nav_panel(
                 "Summary",
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
                 tableOutput("data_minmax_highsd")
               ),
               nav_panel(
                 "Readings",
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
             )),
    
    # Plot tab
    tabPanel("Plot",
            # tags$h1("Plot"),
            plotlyOutput("plot"),
            downloadButton('dl',"Download the data"),
            ),
    
    # About Page
    tabPanel("About",
             includeMarkdown("../README.md"))
  )
)
