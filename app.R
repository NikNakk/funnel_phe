#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(cowplot)

sample_data <- read_tsv("sample_data.txt")
source("funnel_phe.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Demo of PHE funnel plots in R"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "pct",
            label = "PCT to highlight",
            choices = sort(sample_data$pct)
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("funnel_plot"),
           textOutput("test")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$test <- renderText(
    input$pct
  )
    output$funnel_plot <- renderPlot({
      make_funnel_phe(sample_data, numerator, denominator, if (is.null(input$pct)) FALSE else pct == input$pct)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
