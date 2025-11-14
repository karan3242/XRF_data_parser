##### Load Preamble #####
# Define the UI
fluidPage(
  theme = shinytheme("flatly"),
  navset_pill(
    # ----- Raw Data ----
    nav_panel(
      "Raw Data",
      sidebarPanel(
        fileInput("raw_csv", "Choose File"),
        selectInput("samples", "Select Samples:",
                    choices = NULL, multiple = TRUE),
        selectInput("methods", "Select Methods:",
                    choices = NULL, multiple = TRUE),
        strong("This is your initial loading and pre-filter stage."),
        tags$ul(
          tags$li("Load Data: Use the input feature to upload your Geo Chem CSV or Excel file (Raw data file should be on Sheet 1)."),
          tags$li("Initial Filtering: Apply basic filters by Sample Name and the Analysis Method used to narrow down the dataset.")
        )
      ),
      mainPanel(
        reactableOutput("raw_data")
      )
    ),
    # ---- Subset Data ----
    nav_panel("Subset Data",
              sidebarPanel(fluidRow(
                column(4, input_switch("drop_0val",
                                       "Drop Colums with Null Value",
                                       value = TRUE)),
                column(3, input_switch("normalize",
                                       "Normalize Data", value = TRUE))
              ),
              selectInput("elements", "Select elements:",
                          choices = NULL, multiple = TRUE),
              strong("This tab presents a cleaned and refined dataset ready for analysis."),
              tags$ul(
                tags$li("Automatic Cleanup: Only the essential LAB_IDs and element concentration columns are retained."),
                tags$li("Drop Columns with Null Value: Automatically remove element columns where the sum of readings is 0 (i.e., elements that were not detected)."),
                tags$li("Normalize Data: Apply normalization to the remaining element readings for comparative analysis."),
                tags$li("Manual Element Removal: You can remove additional unwanted element columns by deselecting them from the Input Selection in the sidebar.")
              )
              ),
              mainPanel(reactableOutput("subset_data"))),
    # ---- Analytics ----
    nav_panel("Analytics",
              sidebarPanel(uiOutput("samples2"),
                           uiOutput("dynamic_checkboxes"),
                           uiOutput("elements2"),
                           strong("This is the individual sample inspection stage."),
                           tags$ul(
                             tags$li("Individual Item Filtering: Select individual LAB_ID items from the dropdown to view their specific readings and analytics."),
                             tags$li("Interactive Visualizations: Review table showing the composition for the selected sample(s).")
                           )),
              mainPanel(
                        reactableOutput("sample_wise_list_analysied")
                        )
              ),
    # ---- Summary List ----
    nav_panel("Summary",
              sidebarPanel(
                selectInput("Analytics", "Filter Analytics",
                            choices = c("Min", "Q1", "Median",
                                        "Mean", "Q3", "Max", "StDev"),
                            selected = c("Min", "Q1", "Median",
                                         "Mean", "Q3", "Max", "StDev"),
                            multiple = TRUE),
                downloadButton("save_analysis", "Save Analysis"),
                br(),br(),
                strong("This final tab aggregates your key findings and handles data output."),
                tags$ul(
                  tags$li("Aggregated Results: Displays a summary of the LAB_ID items and their analytics that you selected and finalized in the Analytics tab."),
                  tags$li("Analytics Selection (Sidebar): Choose the specific type of analytics (e.g., summary statistics, specific ratios) you wish to include in the final report via the Input Select tab in the sidebar."),
                  tags$li("Saving Data: When the Save button is clicked, the application exports a compilation of Raw Data, Subset Data, processed reading values and selected analytics results.")
                )
              ),
              mainPanel(reactableOutput("final_sample_wise_df"),
                        reactableOutput("final_sample_wise_analytics"))
              ),
    # ---- About Page ----
    nav_panel("Help",
              includeMarkdown("../README.md"))
  )
)
