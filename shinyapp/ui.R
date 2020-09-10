library(shiny)
library(plyr)
library(tidyverse)

ui <- pageWithSidebar(
  
  # App title ----
  headerPanel(title = "Fourthdown Tool", windowTitle = "Fourthdown Tool"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    
    selectInput("quarter", "Quarter:", 
                choices = c("1st" = 1,
                            "2nd" = 2,
                            "3rd" = 3,
                            "4th" = 4)),
    textInput("time", "Time Remaining in the Quarter:", "2:00"),
    numericInput("score", "Score Differential:", -3),
    numericInput("ydstogo", "Yards to Go:", 1),
    numericInput("yardline", "Yardline:", 40),
    sliderInput("bounds", "Expand Data: (Recommended: 50)",
                min = 0, max = 900, value = 50, step = 10),
    actionButton("go", "Analyze Situation", class = "btn-primary btn-lg", width = "100%"),
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    plotOutput("probPlot", width = "auto", height = "700px"),
  )
  
)