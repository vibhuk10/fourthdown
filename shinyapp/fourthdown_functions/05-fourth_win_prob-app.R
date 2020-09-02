prob_game_result_go_yes <- function(quarter, timeleft, score, yards_to_go, play_type, result, yardline, lower_seconds_bound, upper_seconds_bound, base_plays_data, last_plays_data, drives_data) {
  # this functions takes in the quarter, timeleft, score, yards to go, play type and yardline and result and outputs the probabilities of winning if you convert the 4th down
  if(yardline - yards_to_go > 0) {    
    base_plays <- 
      base_plays_data %>% 
      filter(yardline_100 == (yardline-yards_to_go) & down == 1) %>% 
      group_by(drive_id) %>% 
      slice(1) %>% 
      ungroup()
    
    drive_probs <- 
      base_plays %>% 
      left_join(drives_data, by = "drive_id") %>%
      filter(!(drive_result == "safety")) %>% 
      count(drive_result) %>% 
      na.omit() %>% 
      mutate(prob = n/(sum(n)),
             prob = round(prob, digits = 4))
    
    field_goal_win <- 
      prob_game_result(quarter = quarter,
                       timeleft = timeleft,
                       score = score,
                       yards_to_go = yards_to_go,
                       play_type = "field_goal",
                       result = "yes",
                       yardline = yardline,
                       lower_seconds_bound = lower_seconds_bound,
                       upper_seconds_bound = upper_seconds_bound,
                       base_plays_data = base_plays_data,
                       last_plays_data = last_plays_data)
    field_goal_win_prob <- field_goal_win$prob
    field_goal_game_count <- field_goal_win$count
    
    touchdown_win <- 
      prob_game_result(quarter = quarter,
                       timeleft = timeleft,
                       score = score,
                       yards_to_go = yards_to_go,
                       play_type = "touchdown",
                       result = "yes",
                       yardline = yardline,
                       lower_seconds_bound = lower_seconds_bound,
                       upper_seconds_bound = upper_seconds_bound,
                       base_plays_data = base_plays_data,
                       last_plays_data = last_plays_data)
    touchdown_win_prob <- touchdown_win$prob
    touchdown_win_game_count <- touchdown_win$count
    
    turnover_win <- 
      prob_game_result(quarter = quarter,
                       timeleft = timeleft,
                       score = score,
                       yards_to_go = yards_to_go,
                       play_type = "go",
                       result = "no",
                       yardline = yardline,
                       lower_seconds_bound = lower_seconds_bound,
                       upper_seconds_bound = upper_seconds_bound,
                       base_plays_data = base_plays_data,
                       last_plays_data = last_plays_data)
    turnover_win_prob <- turnover_win$prob
    turnover_win_game_count <- turnover_win$count
    
    punt_win <- 
      prob_game_result_punt(quarter = quarter,
                            timeleft = timeleft,
                            score = score,
                            yards_to_go = yards_to_go,
                            play_type = "punt",
                            result = "yes",
                            yardline = yardline,
                            lower_seconds_bound = lower_seconds_bound,
                            upper_seconds_bound = upper_seconds_bound,
                            base_plays_data = base_plays_data,
                            last_plays_data = last_plays_data)
    punt_win_prob <- punt_win$prob
    punt_win_game_count <- punt_win$count
    
    if(length(punt_win_prob) == 0) {
      win_probs <- 
        tibble(drive_result = c("field_goal", "touchdown", "turnover"),
               win_probs = c(field_goal_win_prob, touchdown_win_prob, turnover_win_prob),
               count = c(field_goal_game_count, touchdown_win_game_count, turnover_win_game_count))
    }
    
    if(!(length(punt_win_prob) == 0)) {
      win_probs <- 
        tibble(drive_result = c("field_goal", "touchdown", "turnover", "punt"),
               win_probs = c(field_goal_win_prob, touchdown_win_prob, turnover_win_prob, punt_win_prob),
               count = c(field_goal_game_count, touchdown_win_game_count, turnover_win_game_count, punt_win_game_count))
    }
    
    if(nrow(win_probs)==3) {
      drive_probs <- 
        base_plays %>% 
        left_join(drives_data, by = "drive_id") %>%
        filter(!(drive_result == "safety"), !(drive_result == "punt") ) %>% 
        count(drive_result) %>% 
        na.omit() %>% 
        mutate(prob = n/(sum(n)),
               prob = round(prob, digits = 4))
    }
    
    combined_probs <- 
      drive_probs %>% 
      left_join(win_probs, by = "drive_result")
    
    total_win_prob <- sum(combined_probs$prob * combined_probs$win_probs)
    
    total_games_count <- 
      combined_probs %>% 
      pull(count) %>% 
      sum()
    
    total_games_mean <- 
      combined_probs %>% 
      pull(count) %>% 
      mean(na.rm=TRUE) %>% 
      round(digits = 0)
    
    prediction_go_yes <- 
      tibble(comeback = c("win"),
             n = c(total_games_count),
             count = c(total_games_mean),
             prob = c(total_win_prob),
             key = c(1))
  }
  
  if(yardline - yards_to_go <= 0) {
    prediction_go_yes <- 
      prob_game_result(quarter = quarter,
                       timeleft = timeleft,
                       score = score,
                       yards_to_go = yards_to_go,
                       play_type = "touchdown",
                       result = "yes",
                       yardline = yardline,
                       lower_seconds_bound = lower_seconds_bound,
                       upper_seconds_bound = upper_seconds_bound,
                       base_plays_data = base_plays_data,
                       last_plays_data = last_plays_data)
  }
  
  prediction_go_yes
}
