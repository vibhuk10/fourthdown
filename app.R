library(plyr)
library(tidyverse)
library(shiny)

source("./R/fourthdown_functions/10-source_functions.R")

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
    numericInput("score", "Score Differential:", -7),
    numericInput("ydstogo", "Yards to Go:", 1),
    numericInput("yardline", "Yardline:", 35),
    sliderInput("lower_bound", "Amount of Seconds for Lower Bound for Time Left:",
                min = -900, max = 0, value = -50, step = 10),
    sliderInput("upper_bound", "Amount of Seconds for Upper Bound for Time Left:",
                min = 0, max = 900, value = 50, step = 10),
    
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    plotOutput("probPlot", width = "auto", height = "700px"),
  )
  
)

server <- function(input, output) {
  
  output$probPlot <- renderPlot({
    
    # gets user inputs
    qtrInput <- input$quarter
    timeInput <- input$time
    scoreInput <- input$score
    ydstogoInput <- input$ydstogo
    yardlineInput <-  input$yardline
    lower_boundInput <- input$lower_bound
    upper_boundInput <- input$upper_bound
    
    # creates table with all probabilities
    probabilities_table <- 
      display(
        quarter = qtrInput,
        time = timeInput,
        score = scoreInput,
        yards_to_go = ydstogoInput,
        yardline = yardlineInput,
        lower_seconds_bound = lower_boundInput,
        upper_seconds_bound = upper_boundInput,
        field_data = fieldgoal, 
        go_data = fourthdown,
        punt_data = punt,
        base_plays_data = data_small,
        last_plays_data = last_plays,
        drives_data = drives
      )
    
    theme_set(theme_classic(base_size = 20))
    
    # create graph of table 
    probabilities_table %>% 
      ggplot(aes(x = play_type, y = win_prob, fill = play_type)) +
      geom_col() +
      geom_text(size = 8, aes(y = win_prob - 0.02, label = paste0(round(win_prob, 3) * 100, "% "))) +
      geom_text(size = 5, aes(y = win_prob + 0.05, label = paste0(round(play_prob, 3) * 100, "% chance of "))) +
      geom_text(size = 5, aes(y = win_prob + 0.035, label = play_name)) +
      geom_text(size = 5, aes(y = win_prob + 0.015, label = paste0(" out of ", games, " games"))) +
      scale_fill_manual(values = c("#ff6666", "#70DBDB", "#FFE4B5")) + 
      guides(fill = FALSE) +
      labs(x = "", y = "Win probability") +
      ggtitle("Should you go for it?")+
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_y_continuous(labels = scales::percent)
    
  })
}
shinyApp(ui, server)