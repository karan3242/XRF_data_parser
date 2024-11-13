#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(readr)
library(tidyverse)
library(viridis)
library(shinythemes)
library(hrbrthemes)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Ploting Data"),
  
        # Show a plot of the generated distribution
        sidebarPanel(
          fileInput(
            "file1",
            "Choose CSV File",
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
                    ),
          uiOutput("readings"),
          sliderInput("xaxis", "X-axis", 0, 41, value = c(0,41))),
        mainPanel(plotOutput("plot"))
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
beam1 <- reactive({
  inFile <- input$file1
  df1 <- read_csv(inFile$datapath) %>% slice(2, 4, 40:2086) %>% select(-1)
  df1 <- df1[ , colSums(is.na(df1)) == 0] #remove Na colums
})
#output$beam1 <- renderTable({beam1()})

output$readings <- renderUI({
  selectInput(
    "reading", 
    "Reading", 
    choices = unique(unlist(beam1()[1, ])),
    selected = NULL)

})

df3 <- reactive({
  df2 <- as.vector(beam1()[1, ] == input$reading)
  df3 <- as.data.frame(beam1()[,df2]) %>% slice(2:max(row(beam1())))
  names(df3) <- df3[1,]
  df3 <- df3 %>% slice(2:max(row(beam1())))
  
  kev <- seq(0.02 , by = 0.02, length.out = max(row(df3)))
  
  df3$kev <- kev
  df3 <- mutate_all(df3, as.double)
})
output$df3 <-renderTable({df3()})

output$plot <- renderPlot({
  if(max(col(df3())) < 4){
  Plot <- ggplot(df3(), aes(x=kev, y = `1`, fill = 'Exposer 1')) +
    geom_area()+
    geom_area(aes(y=`2`, fill = 'Exposer 2')) +
    xlim(input$xaxis[1], input$xaxis[2]) +
    labs(y = "Counts", x = "Kev")} +
    theme_ipsum() +
    theme(legend.position = "bottom", legend.title = element_blank())
  else{
  Plot <- ggplot(df3(), aes(x=kev, y = `1`, colour = 'Exposer 1')) +
    geom_area()+
    geom_area(aes(y=`2`, fill = 'Exposer 2')) +
    geom_area(aes(y=`3`, fill = 'Exposer 3')) +
    xlim(input$xaxis[1], input$xaxis[2]) +
    labs(y = "Counts", x = "Kev") +
    theme_ipsum() +
    theme(legend.position = "bottom", legend.title = element_blank())
    }
},
height = 700)


} # Run the application 
shinyApp(ui = ui, server = server)
