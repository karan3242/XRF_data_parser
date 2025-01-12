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
            step = 0.1,
            ticks = FALSE
          ),
          sliderInput(
            "skew_cut",
            "Deviation Percentage Cutoff",
            min = 0,
            max = 5,
            value = 1,
            step = 0.1,
            ticks = FALSE
          ),
          tags$h2("Sd and Skew"),
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
      "Item Details",
      uiOutput("one_item"),
      navs_tab(
        nav_panel(
        "Item Data",
      uiOutput("elm2"),
      checkboxInput("rm_outlier", "Remove Outlier"),
      tableOutput("one_item_table"),
      #verbatimTextOutput("elm_outlier_count"),
      checkboxInput("jitter", "Jitter"),
      plotOutput("boxplot")),
      nav_panel(
        "Items Summary",
        tableOutput("one_item_summary")
        #tableOutput("one_item_outlier"),
        #tableOutput("one_item_high_sd"),
        #verbatimTextOutput("one_item_summary")
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
        )
      )
    ),
    
    # Plot tab
    tabPanel(
      "Item Summary",
      # tags$h1("Plot"),
      plotlyOutput("plot"),
      tableOutput("item_type_summary"),
      downloadButton('dl', "Download the data"),
      downloadButton("report","Generate report"),
    ),
    
    # Beam Spectra
    tabPanel(
      "Beam Spectra",
      sidebarPanel(
        fileInput(
          "fileb",
          "Choose CSV File",
          accept = c("text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv")
        ),
        uiOutput("readings"),
        uiOutput("xaxis"),
        uiOutput("yaxis"),
        textInput("title", "Plot Title", "Item", placeholder = "Item"),
        tags$div(
          style = "display: flex; flex-direction: column;",
          tags$div(
            style = "display: flex; flex-direction: row;",
            textInput("dpi", "Select dpi:", 250, placeholder = "250"),
            textInput("pwidth", "Select Width:", 1920, placeholder = "1920"),
            textInput("pheight", "Select Height:", 1440, placeholder = "1440")
          ),
          tags$div(
            style = "display: flex; flex-direction: row;",
            textInput("dstpath", "Select Destination Folder:", "~/Pictures", placeholder = "~/Pictures"),
            textInput("dstplot", "Select File name:", "plot", placeholder = "plot"),
            actionButton("savePlot", "Save Plot")
          )
        )
        
      ),
      mainPanel(plotOutput("beam_plot", height = 700))
    ),
    
    # About Page
    tabPanel("About", includeMarkdown("../README.md"))
  )
)
