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
                    choices = NULL, multiple = TRUE)
      ),
      mainPanel(
        reactableOutput("raw_data")
      )
      
    ),
    
    # ---- Subset Data ----
    nav_panel("Subset Data",
              
              sidebarPanel(fluidRow(
                column(4,input_switch("drop_0val", 
                                      "Drop Colums with Null Value", 
                                      value = TRUE)), 
                column(3, input_switch("normalize", "Normalize Data", value = TRUE))
              ),
              selectInput("elements", "Select elements:",
                          choices = NULL, multiple = TRUE)
              ),
              
              mainPanel(reactableOutput("subset_data"))),
    
    # ---- Analytics ----
    nav_panel("Analytics",
              sidebarPanel(uiOutput("samples2"),
                           uiOutput("dynamic_checkboxes"),
                           uiOutput("elements2")),
              mainPanel(reactableOutput("sample_wise_list"),
                        reactableOutput("sample_wise_list_analysied")
                        )
              ),
    # ---- Summary List ----
    nav_panel("Summary",
              sidebarPanel(
                selectInput("Analytics", "Filter Analytics",
                            choices = c("Min", "Q1", "Median", "Mean", "Q3", "Max", "StDev"),
                            selected = c("Min", "Q1", "Median", "Mean", "Q3", "Max", "StDev"),
                            multiple = TRUE),
                downloadButton("save_analysis", "Save Analysis")
              ),
              mainPanel(reactableOutput("final_sample_wise_df"),
                        reactableOutput("final_sample_wise_analytics"))
              ),
    # ---- About Page ----
    nav_panel("Help",
              includeMarkdown("../README.md"))
    
    
    
    
  )
)