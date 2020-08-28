theme_set(theme_classic(base_size = 16))

# sample size at each yardage

fourthdown %>% 
  count(ydstogo)

fourthdown %>%
  count(ydstogo, fourth_down_converted) %>% 
  ggplot() +
  geom_col(aes(x=ydstogo, y = n, fill = fourth_down_converted)) +
  labs(x = "yards to go",title = "Amount of fourthdowns attempted per each yard to go between 2009-2019 and success rate")

# 4th down completion percentage 

fourthdown %>% 
  count(fourth_down_converted) %>% 
  mutate(prob = n/sum(n))

fourthdown %>% 
  count(ydstogo, fourth_down_converted)
  
fourthdown %>%
  count(fourth_down_converted) %>% 
  mutate(prob = n/sum(n)) %>% 
  ggplot() +
  geom_col(aes(x=fourth_down_converted, y = prob, fill = fourth_down_converted)) +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = FALSE) +
  labs(x = "fourth down result",title = "success rate of fourthdowns between 2009-2019")

# how often do teams go for it on 4th down

fourthdown %>%
  count(posteam, fourth_down_converted)

fourthdown %>%
  count(posteam, fourth_down_converted) %>% 
  mutate(posteam = fct_reorder(posteam, n, sum)) %>% 
  ggplot() +
  geom_col(aes(x=n, y = posteam, fill = fourth_down_converted)) +
  labs(title = "Amount of two point conversions attempted by each team between 2009-2019 and success rate")

fourthdown %>%
  count(posteam, fourth_down_converted) %>% 
  ggplot() +
  geom_col(aes(x=posteam, y = n, fill = fourth_down_converted), position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = FALSE) +
  labs(title = "Amount of two point conversions attempted by each team between 2009-2019 and success rate")

# epa for play_type on runs

data %>% 
  select(play_type, down, epa) %>% 
  filter(play_type == "punt", down == 4) %>%
  pull(epa) %>%
  na.omit() %>% 
  mean()

data %>% 
  select(play_type, down, epa, ydstogo) %>% 
  filter(play_type == "pass" | play_type == "pass", down == 4, ydstogo <= 5) %>%
  pull(epa) %>%
  na.omit() %>% 
  mean()

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
         year == "2019", qtr == 4, quarter_seconds_remaining <= 300 & quarter_seconds_remaining >= 15,
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
    graph_data$prob_go_greater[i] <- go_prob > punt_prob + 0.05  
  }
  
  if(play_type == "field_goal") {
    graph_data$prob_go_greater[i] <- go_prob > field_prob + 0.05  
  }
    
}

graph_data <- 
  graph_data %>% 
  mutate(prob_go_greater, ifelse(is.na(prob_go_greater), 0, prob_go_greater))
  
graph_data %>% 
  count(prob_go_greater)

graph_data %>% write_csv("data-clean/NFL_fourthdown_tool_data_2019.csv")

graph_data <- read_csv("data-clean/NFL_fourthdown_tool_data_2019.csv")

# what is the average distance of punts
punts <- 
  data %>% 
  filter(play_type == "punt")

punts2 <- 
  data %>% 
  filter(play_type == "punt" & punt_in_endzone == 1)
