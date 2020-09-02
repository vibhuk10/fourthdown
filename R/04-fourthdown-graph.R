source("./R/fourthdown_functions/10-source_functions.R")

# amount of times the fourth down conversion tool said there was a higher win probability with a run vs. a pass
# in the 4th quarter with less than 5 minutes remaining and how many times did teams actually go for it in 2019

seconds_to_time <-function(secondsInput) {
  minutes <- as.integer(secondsInput/60)
  seconds <- secondsInput - (60*minutes)
  time <- paste0(minutes, ":", seconds)
  time
}

graph_data <- 
  data %>% 
  mutate(year = str_sub(game_date,1,4)) %>% 
  filter(down == 4, play_type == "punt" | play_type == "field_goal",
         year == "2019" | year == "2018", qtr == 4, quarter_seconds_remaining <= 300 & quarter_seconds_remaining >= 15,
         abs(total_home_score - total_away_score) <= 8) %>% 
  mutate(timeleft = seconds_to_time(quarter_seconds_remaining))

graph_data <-  
  graph_data %>% 
  mutate(prob_go_greater = -1)

get_var <- function(row, column, data) {
  x <- data[row, column]
  x <- x[[1]]
}

for (i in 1:nrow(graph_data)) {
  print(i)
  
  # qtr_test <- get_var(i, 18, graph_data)
  # time_test <- get_var(i, 258, graph_data)
  # score_test <- get_var(i, 55, graph_data)
  # yards_test <- get_var(i, 23, graph_data)
  # yardline_test <- get_var(i, 9, graph_data)
  
  probs_table <- 
    display(quarter = get_var(i, 18, graph_data),
            time = get_var(i, 258, graph_data),
            score = get_var(i, 55, graph_data),
            yards_to_go = get_var(i, 23, graph_data),
            yardline = get_var(i, 9, graph_data),
            lower_seconds_bound = -50,
            upper_seconds_bound = 50, 
            field_data = fieldgoal, 
            go_data = fourthdown,
            punt_data = punt,
            base_plays_data = data,
            last_plays_data = last_plays,
            drives_data = drives
    )
  
  play_type <- get_var(i, 26, graph_data)
  
  if(nrow(probs_table)<3) {
    if("Punt" %in% probs_table$play_type) {
      punt_prob <- probs_table$win_prob[probs_table$play_type == "Punt"]
    }
    if("Field_Goal" %in% probs_table$play_type) {
      field_prob <- probs_table$win_prob[probs_table$play_type == "Field Goal"]
    }
  }
  
  if(nrow(probs_table)>=3) {
    punt_prob <- probs_table$win_prob[probs_table$play_type == "Punt"]
    field_prob <- probs_table$win_prob[probs_table$play_type == "Field Goal"]
  }
  
  go_prob <- probs_table$win_prob[probs_table$play_type == "Go For It"]
  
  if(play_type == "punt") {
    graph_data$prob_go_greater[i] <- go_prob > punt_prob  
  }
  
  if(play_type == "field_goal") {
    graph_data$prob_go_greater[i] <- go_prob > field_prob  
  }
  
}

graph_data <- 
  graph_data %>% 
  mutate(prob_go_greater, ifelse(is.na(prob_go_greater), 0, prob_go_greater))

graph_data %>% 
  count(prob_go_greater)

team_graph_data <- 
  graph_data %>% 
  count(posteam, prob_go_greater) %>% 
  filter(prob_go_greater == 1)

graph_data %>% write_csv("data-clean/NFL_fourthdown_tool_data_2019.csv")
graph_data %>% write_csv("data-clean/NFL_fourthdown_tool_data_2018-19.csv")

graph_data %>% write_csv("data-clean/NFL_fourthdown_tool_data_5_2019.csv")
graph_data %>% write_csv("data-clean/NFL_fourthdown_tool_data_10_2019.csv")

#graph amount of times the fourth down conversion tool said there was a higher win probability with a run vs. a pass
# in the 4th quarter with less than 5 minutes remaining and how many times did teams actually go for it in 2019
theme_set(theme_light(base_size = 16))

graph_data_19 <- read_csv("data-clean/NFL_fourthdown_tool_data_2019.csv")
graph_data_18_19 <- read_csv("data-clean/NFL_fourthdown_tool_data_2018-19.csv")

graph_data_five <- read_csv("data-clean/NFL_fourthdown_tool_data_5_2019.csv")
graph_data_ten <- read_csv("data-clean/NFL_fourthdown_tool_data_10_2019.csv")

fourth_graph <- 
  graph_data_18_19 %>%
  count(posteam, prob_go_greater) %>%
  na.omit() %>%
  mutate(posteam = fct_reorder(posteam, n, sum)) %>% 
  ggplot() +
  geom_col(aes(x=posteam, y = n, fill = posteam)) +
  scale_fill_manual(values = c("#00FF00",
                               "#11FF00",
                               "#22FF00",
                               "#33FF00",
                               "#44FF00",
                               "#55FF00",
                               "#66FF00",
                               "#71FF00",
                               "#77FF00",
                               "#88FF00",
                               "#99FF00",
                               "#AAFF00",
                               "#BBFF00",
                               "#CCFF00",
                               "#DDFF00",
                               "#EEFF00",
                               "#FFFF00",
                               "#FFEE00",
                               "#FFDD00",
                               "#FFCC00",
                               "#FFBB00",
                               "#FFAA00",
                               "#FF9900",
                               "#FF8800",
                               "#FF7700",
                               "#FF7100",
                               "#FF6600",
                               "#FF5500",
                               "#FF4400",
                               "#FF3300",
                               "#FF2200",
                               "#FF1100",
                               "#FF0000")) + 
  guides(fill = FALSE) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x = "",
       y = "Amount of 4th Downs Teams Made the Wrong Choice",
       title = "Do Teams Make the Right Choice on 4th Down?",
       subtitle = "Comparing how often teams made the right choice on 4th down according to the tool. Uses 
all fourthdowns from one score games with less than 5 minutes remaning in 2018 and 2019")
fourth_graph