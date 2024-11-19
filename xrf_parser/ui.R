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
    tabPanel(
      "Primary Data",
      fileInput(
        "file1",
        "Choose CSV File",
        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
      ),
      tagList(
        tags$p("<LOD values have been converted to 0"),
        tableOutput("data_set1")
      )
    ),
    
    # Data Overview tab
    tabPanel(
      "Data Overview",
      uiOutput("lab_items"),
      uiOutput("elm"),
      checkboxInput("normal_select", "Normalized Data"),
      navset_tab(
        nav_panel("Selected Items", tableOutput("data_set2")),
        nav_panel(
          "Elements Overview",
          tableOutput("data_overview"),
          tableOutput("data_summary_minmax")
        ),
        nav_panel(
          "Deviations",
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
          "Z-Score",
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
      )
    ),
    
    tabPanel(
      "Item Summary",
      uiOutput("one_item"),
      navs_tab(
        nav_panel(
        "Item Data",
      uiOutput("elm2"),
      tableOutput("one_item_data"),
      plotOutput("boxplot")),
      nav_panel(
        "Items with high HD",
        tableOutput("one_item_outlier"),
        tableOutput("one_item_high_sd"),
        verbatimTextOutput("one_item_summary"),
        plotOutput("outlier_boxplot")
      )
      )
    ),
    tabPanel(
      "Selected Readings",
      uiOutput("reading_item"),
      navs_tab(
        nav_panel(
          "Selected Reading data",
          tableOutput("reading_item_selected")
        ),
        nav_panel(
          "Summary of Reading Data",
          tableOutput("reading_item_selected_sd"),
          tableOutput("reading_item_selected_minmax")
        )
      )
    ),
    
    # Plot tab
    tabPanel(
      "Plot",
      # tags$h1("Plot"),
      plotlyOutput("plot"),
      downloadButton('dl', "Download the data"),
    ),
    
    # About Page
    tabPanel("About", includeMarkdown("../README.md"))
  )
)
