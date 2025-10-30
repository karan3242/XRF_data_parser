##### Load Preamble #####
# Define the UI
fluidPage(
  theme = shinytheme("flatly"),
  
  # ----- Raw Data ----
  fileInput("raw_csv", "Choose File"),
  reactableOutput("raw_data")
  
)
