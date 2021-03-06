library(shiny)
library(plyr)
library(tidyverse)

source("./fourthdown_functions/12-source_functions-app.R")

server <- function(input, output) {
  
  # gets user inputs
  getInputs <- eventReactive(input$go, {
    tibble(
      qtr = input$quarter,
      time = input$time,
      score = input$score,
      ydstogo = input$ydstogo,
      side = input$side,
      yardline_half = input$yardline,
      lower_bound = -input$bounds,
      upper_bound = input$bounds
    )
  })
  
  output$probPlot <- renderPlot({
    
    userInputs <- getInputs()
    
    # creates table with all probabilities
  probabilities_table <- 
    display(
      quarter = userInputs$qtr,
      time = userInputs$time,
      score = userInputs$score,
      yards_to_go = userInputs$ydstogo,
      side = userInputs$side,
      yardline_half = userInputs$yardline_half,
      lower_seconds_bound = userInputs$lower_bound,
      upper_seconds_bound = userInputs$upper_bound,
      field_data = fieldgoal, 
      go_data = fourthdown,
      punt_data = punt,
      base_plays_data = data_small,
      last_plays_data = last_plays,
      drives_data = drives
    )
  
    
  theme_set(theme_classic(base_size = 20))
  
  # create graph of table 
  title_graph <- greatest_win_prob(probabilities_table)
  
  probabilities_table %>% 
    ggplot(aes(x = play_type, y = win_prob, fill = play_type)) +
    geom_col() +
    geom_text(size = 5, aes(y = win_prob + 0.1, label = paste0(round(play_prob, 3) * 100, "% chance of \n",play_name, "\n out of ", games, " games"))) +
    geom_text(size = 8, aes(y = win_prob - 0.05, label = paste0(round(win_prob, 3) * 100, "% "))) +
    scale_fill_manual(values = c("#ff6666", "#70DBDB", "#FFE4B5")) + 
    guides(fill = FALSE) +
    labs(x = "", y = "Win Probability", title = title_graph) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(labels = scales::percent, lim = c(0,1))
    
  })
}