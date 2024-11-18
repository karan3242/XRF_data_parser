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
library(paletteer)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("XRF Beamspectra Plotter"),
  
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
        mainPanel(plotOutput("plot", height = 700))
    
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
#output$df3 <-renderTable({df3()})

output$xaxis <- renderUI({
  xmax <- max(df3()$kev)
  sliderInput("xaxis", "X-axis", 0, xmax, value = c(0,10))
})

output$yaxis <- renderUI({
  lim <- max(max(df3()[,1]),max(df3()[,2]))
  sliderInput("yaxis", "Y-axis", 0, lim, value = lim)
})


plot1 <- reactive({ggplot(df3(), aes(x=kev)) +
  geom_area(aes(y=`2`, fill ='Exposer 2')) +
  geom_area(aes(y = `1`, fill = 'Exposer 1'))+
  scale_fill_manual(values = c("#CD6C53", "#839FBA"), 
                    labels = c("Exposer 1", "Exposer 2")) +
  coord_cartesian(xlim= c(input$xaxis[1], input$xaxis[2]), ylim = c(0, input$yaxis)) +
  scale_y_continuous(expand = expansion(mult = 0)) +
  theme_light() +
  labs(y = "Counts/s", x = "KeV") +
  theme(legend.position = "bottom",
        #legend.justification = c("left", "top"),
        legend.title = element_blank(),
        legend.background = element_rect(size = 1,
                                         color = "grey",
                                         ),
        axis.title = element_text(face="bold")) +
  labs(title = input$title, col = 'variable')
  
})
output$plot <- renderPlot({plot1()
   
  # plot2 <- plot1 + 
  #   geom_area(aes(y=`3`, fill = 'Exposer 3'))

})

#Save plot to file
observeEvent(input$savePlot, {
  ggsave(
    filename = file.path(input$dstpath, paste0(input$dstplot,"_", input$title, ".png")),
    plot = plot1(),
    width = as.integer(input$pwidth),
    height = as.integer(input$pheight),
    units = "px",
    dpi = as.integer(input$dpi)
  )
})


} # Run the application 
shinyApp(ui = ui, server = server)
