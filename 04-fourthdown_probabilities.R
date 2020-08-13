time_to_seconds <- function(time) {
  # this functions takes in string time input and outputs the time in seconds
  minutes <- 
    str_sub(time, 1, str_locate(time, ":")[1, 1] - 1)
  minutes <- 
    as.numeric(minutes) * 60
  seconds <- 
    str_sub(time, str_locate(time, ":")[1, 1] + 1)
  seconds <- 
    as.numeric(seconds)
  seconds_left <- minutes + seconds
  seconds_left
}

create_prob_field_goal <- function(yardline, data) {
  # this functions takes in the yardline and outputs the probabilities for a field goal
  
  field_goal_plays <- 
    data %>% 
    filter(yardline_100 < (yardline + 1) & yardline_100 > (yardline - 1)) %>% 
    mutate(
      field_goal_result2 = 
        case_when(
          field_goal_result == "missed" ~ "failed",
          field_goal_result == "blocked" ~ "failed",
          field_goal_result == "made" ~ "good"
        )
    )
  
  field_goal_prob <- 
    field_goal_plays %>% 
    count(field_goal_result2) %>% 
    mutate(
      prob = n/(sum(n)),
      prob = round(prob, digits = 3),
      key = case_when(
        field_goal_result2 == "failed" ~ 2,
        field_goal_result2 == "good" ~ 1,
      ))
  
  field_goal_prob
}

create_prob_go <- function(yards_to_go, data) {
  # this functions takes in the yards to go and outputs the probabilities for a two point conversion
  
  go_plays <- 
    data %>% 
    filter(ydstogo == yards_to_go)
  
  go_prob <- 
    go_plays %>% 
    count(fourth_down_result) %>% 
    mutate(
      prob = n/(sum(n)),
      prob = round(prob, digits = 3),
      key = case_when(
        fourth_down_result == "failed" ~ 2,
        fourth_down_result == "good" ~ 1,
      )
    )
  
  go_prob
}

prob_game_result <- function(quarter, timeleft, score, yards_to_go, play_type, result, yardline, lower_seconds_bound, upper_seconds_bound, base_plays_data, last_plays_data) {
  # this functions takes in the quarter, timeleft, score, and yardline and result and outputs the probabilities of winning
  
  if(play_type == "field_goal" & result == "yes") {
    base_plays1 <- 
      base_plays_data %>% 
      filter(qtr == quarter & quarter_seconds_remaining>(timeleft+lower_seconds_bound) & quarter_seconds_remaining<(timeleft+upper_seconds_bound) & score_differential == (-1*(score+3)) & play_type == "kickoff") %>% 
      group_by(game_id) %>% 
      slice(n()) %>% 
      ungroup()
    
    base_plays2 <- 
      base_plays_data %>% 
      filter(qtr == quarter & quarter_seconds_remaining>(timeleft+lower_seconds_bound) & quarter_seconds_remaining<(timeleft+upper_seconds_bound) & score_differential == (-1*(score+3)) & yardline_100 < 85 & yardline_100 > 70) %>% 
      group_by(game_id) %>% 
      slice(1) %>% 
      ungroup()
    
    base_plays <- rbind(base_plays1, base_plays2)
    base_plays <- distinct(base_plays, game_id, .keep_all = TRUE)
  }
  
  if(play_type == "field_goal" & result == "no") {
    base_plays <- 
      base_plays_data %>% 
      filter(qtr == quarter & quarter_seconds_remaining>(timeleft+lower_seconds_bound) & quarter_seconds_remaining<(timeleft+upper_seconds_bound) & score_differential == (-1*(score)) & yardline_100 < (100-((yardline+7)-5)) & yardline_100 > (100-((yardline+7)+5))) %>% 
      group_by(game_id) %>% 
      slice(1) %>% 
      ungroup()
  }
  
  if(play_type == "go" & result == "no") {
    base_plays <- 
      base_plays_data %>% 
      filter(qtr == quarter & quarter_seconds_remaining>(timeleft+lower_seconds_bound) & quarter_seconds_remaining<(timeleft+upper_seconds_bound) & score_differential == (-1*(score)) & yardline_100 < (100-(yardline-5)) & yardline_100 > (100-(yardline+5))) %>% 
      group_by(game_id) %>% 
      slice(1) %>% 
      ungroup()
  }
  
  if(play_type == "touchdown" & result == "yes") {
    base_plays1 <- 
      base_plays_data %>% 
      filter(qtr == quarter & quarter_seconds_remaining>(timeleft+lower_seconds_bound) & quarter_seconds_remaining<(timeleft+upper_seconds_bound) & score_differential == (-1*(score+7)) & play_type == "kickoff") %>% 
      group_by(game_id) %>% 
      slice(n()) %>% 
      ungroup()
    
    base_plays2 <- 
      base_plays_data %>% 
      filter(qtr == quarter & quarter_seconds_remaining>(timeleft+lower_seconds_bound) & quarter_seconds_remaining<(timeleft+upper_seconds_bound) & score_differential == (-1*(score+7)) & yardline_100 < 85 & yardline_100 > 70) %>% 
      group_by(game_id) %>% 
      slice(1) %>% 
      ungroup()
    
    base_plays <- rbind(base_plays1, base_plays2)
    base_plays <- distinct(base_plays, game_id, .keep_all = TRUE)
  }
  
  combined <- 
    base_plays %>% 
    left_join(last_plays_data, by = "game_id") %>% 
    mutate(
      score_differential2 = ifelse(defteam == away_team, -score_differential2, score_differential2),
      comeback = case_when(
        score_differential2 > 0 ~ "win"
      )
    )
  
  prediction <- 
    combined %>% 
    count(comeback) %>% 
    mutate(
      count = sum(n),
      prob = n/(sum(n)),
      prob = round(prob, digits = 4)
    ) %>% 
    na.omit(prediction) %>% 
    mutate(key = ifelse(result == 'yes', 1, 2))
  
  prediction
}

prob_game_result_go_yes <- function(quarter, timeleft, score, yards_to_go, play_type, result, yardline, lower_seconds_bound, upper_seconds_bound, base_plays_data, last_plays_data, drives_data) {
  
    base_plays <- 
      base_plays_data %>% 
      filter(yardline_100 == (yardline-yards_to_go) & down == 1) %>% 
      group_by(drive_id) %>% 
      slice(1) %>% 
      ungroup()
    
    drive_probs <- 
      base_plays %>% 
      left_join(drives_data, by = "drive_id") %>%
      filter(!(drive_result == "punt" | drive_result == "safety")) %>% 
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
    
    win_probs <- 
      tibble(drive_result = c("field_goal", "touchdown", "turnover"),
             win_probs = c(field_goal_win_prob, touchdown_win_prob, turnover_win_prob),
             count = c(field_goal_game_count, touchdown_win_game_count, turnover_win_game_count))
              
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

create_prob_final <- function(play, yes, no) {
  # this functions combines the probabities using expected probabilty output the final probabilty of winning the game
  
  yes_no <- rbind(yes, no)
  
  final <- play %>% 
    full_join(yes_no, by = "key") %>% 
    rename(n_play = n.x, prob_play = prob.x, n_game = n.y, prob_game = prob.y) %>%
    mutate(
      multiplied = prob_play*prob_game,
    )
  
  final_prob <- 
    final %>%
    pull(multiplied) %>%
    sum() 
  
  final_prob
}

amount_of_games <- function(yes_data, no_data) {
  # this functions takes the data used and outputs the amount of games used to come up with the probabilities
  
  games <- yes_data$count + no_data$count
}

display <- function(quarter, time, score, yards_to_go, yardline, lower_seconds_bound, upper_seconds_bound, field_data, go_data, base_plays_data, last_plays_data, drives_data) {
  
  # convert time (string) to seconds (numeric)
  seconds <- 
    time_to_seconds(time)
  
  field_goal_prob <- 
    create_prob_field_goal(yardline = yardline,
                           data = field_data
                           )
  
  go_prob <- 
    create_prob_go(yards_to_go = yards_to_go,
                   data = go_data
                   )
  
  prediction_field_yes <- 
    prob_game_result(quarter = quarter,
                     timeleft = seconds,
                     score = score,
                     yards_to_go = yards_to_go,
                     play_type = "field_goal",
                     result = "yes",
                     yardline = yardline,
                     lower_seconds_bound = lower_seconds_bound,
                     upper_seconds_bound = upper_seconds_bound,
                     base_plays_data = base_plays_data,
                     last_plays_data = last_plays_data
    ) 
  
  prediction_field_no <-
    prob_game_result(quarter = quarter,
                     timeleft = seconds,
                     score = score,
                     yards_to_go = yards_to_go,
                     play_type = "field_goal",
                     result = "no",
                     yardline = yardline,
                     lower_seconds_bound = lower_seconds_bound,
                     upper_seconds_bound = upper_seconds_bound,
                     base_plays_data = base_plays_data,
                     last_plays_data = last_plays_data
    ) 
  
  prediction_go_no <-
    prob_game_result(quarter = quarter,
                     timeleft = seconds,
                     score = score,
                     yards_to_go = yards_to_go,
                     play_type = "go",
                     result = "no",
                     yardline = yardline,
                     lower_seconds_bound = lower_seconds_bound,
                     upper_seconds_bound = upper_seconds_bound,
                     base_plays_data = base_plays_data,
                     last_plays_data = last_plays_data
    ) 
  
  prediction_go_yes <- 
    prob_game_result_go_yes(quarter = quarter,
                            timeleft = seconds,
                            score = score,
                            yards_to_go = yards_to_go,
                            play_type = "go",
                            result = "yes",
                            yardline = yardline,
                            lower_seconds_bound = lower_seconds_bound,
                            upper_seconds_bound = upper_seconds_bound,
                            base_plays_data = base_plays_data,
                            last_plays_data = last_plays_data,
                            drives_data = drives_data
    )
  
  #takes the probability for a field goal happening
  prob_field <- 
    field_goal_prob[2,3]
  prob_field <- 
    prob_field[[1]]
  
  #takes the probability for a field goal happening
  prob_go <- 
    go_prob[2,3]
  prob_go <- 
    prob_go[[1]]
  
  # uses expected probability to create a final win probability if you went for the two point conversion
  final_field <- 
    create_prob_final(
      field_goal_prob,
      prediction_field_yes,
      prediction_field_no
    )
  
  # uses expected probability to create a final win probability if you went for the extra point
  final_go <- 
    create_prob_final(
      go_prob,
      prediction_go_yes,
      prediction_go_no
    )
  
  #summarizes the number of games used to calculate the probability given for two point conversion
  games_field <- 
    amount_of_games(
      yes_data =  prediction_field_yes,
      no_data = prediction_field_no
    )
  
  #summarizes the number of games used to calculate the probability given for extra point
  games_go <- 
    amount_of_games(
      yes_data =  prediction_go_yes,
      no_data = prediction_go_no
    )
  
  #creates a table with win probabilities for each option
  final <- 
    tibble(
      play_type = c("Field Goal", "Go For It"),
      play_prob = c(prob_field, prob_go),
      win_prob = c(final_field, final_go),
      games = c(games_field, games_go)
    )
  
  final
}

display(quarter = 4,
        time = "1:21",
        score = -6,
        yards_to_go = 1,
        yardline = 30,
        lower_seconds_bound = -100,
        upper_seconds_bound = 100, 
        field_data = fieldgoal, 
        go_data = fourthdown,
        base_plays_data = data,
        last_plays_data = last_plays,
        drives_data = drives
)

