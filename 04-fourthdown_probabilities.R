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
  # this functions takes in the yards to go and outputs the probabilities for a 4th down conversion
  
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

create_prob_punt <-  function(yardline, data) {
  # this functions takes in the yardline and outputs the probabilities for a punt being blocked
  punt_plays <-  
    data %>% 
    filter(play_type == "punt" & yardline_100 == yardline) %>% 
    mutate(
      punt_blocked2 = case_when(
        punt_blocked == 1 ~ "blocked",
        punt_blocked == 0 ~ "punt",
      )
    )
  
  punt_prob <- 
    punt_plays %>% 
    count(punt_blocked2) %>% 
    mutate(
      prob = n/(sum(n)),
      prob = round(prob, digits = 3),
      key = case_when(
        punt_blocked2 == "blocked" ~ 2,
        punt_blocked2 == "punt" ~ 1,
      )
    )
  if(empty(punt_plays %>% filter(punt_blocked == 1)) == TRUE) {
    punt_prob2 <- 
      tibble(punt_blocked2 = c("blocked"),
             n = c(0),
             prob = c(0),
             key = c(2))
    punt_prob <- rbind(punt_prob, punt_prob2)
  }
  
  punt_prob
    
}

prob_game_result <- function(quarter, timeleft, score, yards_to_go, play_type, result, yardline, lower_seconds_bound, upper_seconds_bound, base_plays_data, last_plays_data) {
  # this functions takes in the quarter, timeleft, score, yards to go, play type and yardline and result and outputs the probabilities of winning
  
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
    mutate(key = ifelse(result == 'yes', 1, 2))
  
  check_empty <- 
    prediction[1,1]
  check_empty <- 
    check_empty[[1]]
  
  if(is.na(check_empty) == TRUE) {
    n_empty <- 
      prediction[1,2]
    n_empty <- 
      n_empty[[1]]
    
    count_empty <- 
      prediction[1,3]
    count_empty <- 
      count_empty[[1]]
    
    key_empty <- 
      ifelse(result == 'yes', 1, 2)
    
    prediction <- 
      tibble(comeback = c("win"),
             n = c(n_empty),
             count = c(count_empty),
             prob = c(0),
             key = c(key_empty))
  }
  
  prediction <- 
    prediction %>% 
    na.omit()
  
  prediction
}

prob_game_result_go_yes <- function(quarter, timeleft, score, yards_to_go, play_type, result, yardline, lower_seconds_bound, upper_seconds_bound, base_plays_data, last_plays_data, drives_data) {
  # this functions takes in the quarter, timeleft, score, yards to go, play type and yardline and result and outputs the probabilities of winning if you convert the 4th down
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

prob_game_result_punt <- function(quarter, timeleft, score, yards_to_go, play_type, result, yardline, lower_seconds_bound, upper_seconds_bound, base_plays_data, last_plays_data) {
  # this functions takes in the quarter, timeleft, score, yards to go, play type and yardline and result and outputs the probabilities of winning for punts
  punt_plays <- 
    base_plays_data %>% 
    filter(yardline_100 == yardline & play_type == "punt" & !(punt_blocked == 1) & !(is.na(kick_distance)==TRUE))
  
  kick_distance_mean <- 
    punt_plays %>% 
    pull(kick_distance) %>% 
    mean()
  
  ball_placement <- ifelse(kick_distance_mean > yardline, 20, yardline-kick_distance_mean)
  ball_placement <- round(ball_placement, digits = 0)
  
  base_plays <- 
    base_plays_data %>% 
    filter(qtr == quarter & quarter_seconds_remaining>(timeleft+lower_seconds_bound) & quarter_seconds_remaining<(timeleft+upper_seconds_bound) & score_differential == (-1*(score)) & yardline_100 > (ball_placement-5) & yardline_100 < (ball_placement+5)) %>% 
    group_by(game_id) %>% 
    slice(1) %>% 
    ungroup()
  
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
    mutate(key = ifelse(result == 'yes', 1, 2))
  
  check_empty <- 
    prediction[1,1]
  check_empty <- 
    check_empty[[1]]
  
  if(is.na(check_empty) == TRUE) {
    n_empty <- 
      prediction[1,2]
    n_empty <- 
      n_empty[[1]]
    
    count_empty <- 
      prediction[1,3]
    count_empty <- 
      count_empty[[1]]
    
    prediction <- 
      tibble(comeback = c("win"),
             n = c(n_empty),
             count = c(count_empty),
             prob = c(0),
             key = c(1))
  }
  
  prediction <- 
    prediction %>% 
    na.omit()
  
  prediction
  
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

display <- function(quarter, time, score, yards_to_go, yardline, lower_seconds_bound, upper_seconds_bound, field_data, go_data, punt_data, base_plays_data, last_plays_data, drives_data) {
  
  # convert time (string) to seconds (numeric)
  seconds <- 
    time_to_seconds(time)
  
  # creates probability of making field goal
  field_goal_prob <- 
    create_prob_field_goal(yardline = yardline,
                           data = field_data
                           )
  
  # creates probability of converting a 4th down
  go_prob <- 
    create_prob_go(yards_to_go = yards_to_go,
                   data = go_data
                   )
  
  # creates probability of punt not being blocked
  punt_prob <- 
    create_prob_punt(yardline = yardline, 
                     data = punt_data)
  
  # creates win probability if you make the field goal
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
  
  # creates win probability if you miss the field goal
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
  
  # creates win probability if you don't convert the 4th down
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
  
  # creates win probability if you convert the 4th down
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
  
  # creates win probability if the punt isn't blocked
  prediction_punt_yes <-
    prob_game_result_punt(quarter = quarter,
                          timeleft = seconds,
                          score = score,
                          yards_to_go = yards_to_go,
                          play_type = "punt",
                          result = "yes",
                          yardline = yardline,
                          lower_seconds_bound = lower_seconds_bound,
                          upper_seconds_bound = upper_seconds_bound,
                          base_plays_data = base_plays_data,
                          last_plays_data = last_plays_data
    ) 
  
  # creates win probability if the punt is blocked
  prediction_punt_no <-
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
  
  #gets the probability for a field goal happening
  prob_field <- 
    field_goal_prob[2,3]
  prob_field <- 
    prob_field[[1]]
  
  #gets the probability for converting a 4th down happening
  prob_go <- 
    go_prob[2,3]
  prob_go <- 
    prob_go[[1]]
  
  #gets the probability for a punt happening
  prob_punt <- 
    punt_prob[2,3]
  prob_punt <- 
    prob_punt[[1]]
  prob_punt <- 
    ifelse(prob_punt == 0, 1, prob_punt)
  
  # uses expected probability to create a final win probability if you went for the field goal
  final_field <- 
    create_prob_final(
      field_goal_prob,
      prediction_field_yes,
      prediction_field_no
    )
  
  # uses expected probability to create a final win probability if you went for the 4th down conversion
  final_go <- 
    create_prob_final(
      go_prob,
      prediction_go_yes,
      prediction_go_no
    )
  
  # uses expected probability to create a final win probability if you went for the punt
  final_punt <- 
    create_prob_final(
      punt_prob,
      prediction_punt_yes,
      prediction_punt_no
    )
  
  #summarizes the number of games used to calculate the probability given for field goal
  games_field <- 
    amount_of_games(
      yes_data =  prediction_field_yes,
      no_data = prediction_field_no
    )
  
  #summarizes the number of games used to calculate the probability given for 4th down conversion
  games_go <- 
    amount_of_games(
      yes_data =  prediction_go_yes,
      no_data = prediction_go_no
    )
  
  #summarizes the number of games used to calculate the probability given for punr
  games_punt <- 
    amount_of_games(
      yes_data =  prediction_punt_yes,
      no_data = prediction_punt_no
    )
  
  #creates a table with all probabilities if punt is not possible from the give situation
  if(prob_punt == 0 | is.na(prob_punt) == TRUE) {
    final <- 
      tibble(
        play_type = c("Field Goal", "Go For It"),
        play_prob = c(prob_field, prob_go),
        win_prob = c(final_field, final_go),
        games = c(games_field, games_go),
        play_name = c("Field Goal", "4th Down Conversion")
      )
  }
  
  #creates a table with all probabilities if field goal is not possible from the give situation
  if(prob_field == 0 | is.na(prob_field) == TRUE) {
    final <- 
      tibble(
        play_type = c("Go For It", "Punt"),
        play_prob = c(prob_go, prob_punt),
        win_prob = c(final_go, final_punt),
        games = c(games_go, games_punt),
        play_name = c("4th Down Conversion", "Punt")
      )
  }
  
  #creates a table with all probabilities if all play types are possible from the give situation
  if(!(prob_field == 0 | prob_punt == 0 | is.na(prob_field) == TRUE | is.na(prob_punt) == TRUE)) {
    final <- 
      tibble(
        play_type = c("Field Goal", "Go For It", "Punt"),
        play_prob = c(prob_field, prob_go, prob_punt),
        win_prob = c(final_field, final_go, final_punt),
        games = c(games_field, games_go, games_punt),
        play_name = c("Field Goal", "4th Down Conversion", "Punt")
      )
  }
  
  
  final
}

display(quarter = 1,
        time = "2:00",
        score = -7,
        yards_to_go = 1,
        yardline = 67,
        lower_seconds_bound = -100,
        upper_seconds_bound = 100, 
        field_data = fieldgoal, 
        go_data = fourthdown,
        punt_data = punt,
        base_plays_data = data,
        last_plays_data = last_plays,
        drives_data = drives
)

