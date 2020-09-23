display <- function(quarter, time, score, yards_to_go, side, yardline_half, lower_seconds_bound, upper_seconds_bound, field_data, go_data, punt_data, base_plays_data, last_plays_data, drives_data) {
  
  # convert time (string) to seconds (numeric)
  seconds <- 
    time_to_seconds(time)
  
  #convert yardline as half to yardline 100
  yardline <- 
    convert_yardline(yardline_half, side)
  
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
  if(prob_punt == 0 | is.na(prob_punt) == TRUE | is.na(final_punt) == TRUE) {
    final <- 
      tibble(
        play_type = c("Field Goal", "Go For It"),
        play_prob = c(prob_field, prob_go),
        win_prob = c(final_field, final_go),
        games = c(games_field, games_go),
        play_name = c("Field Goal", "4th Down Conversion"),
        play_name2 = c("Kick a Field Goal!", "Go For It!")
      )
  }
  
  #creates a table with all probabilities if field goal is not possible from the give situation
  if(prob_field == 0 | is.na(prob_field) == TRUE | is.na(final_field) == TRUE) {
    final <- 
      tibble(
        play_type = c("Go For It", "Punt"),
        play_prob = c(prob_go, prob_punt),
        win_prob = c(final_go, final_punt),
        games = c(games_go, games_punt),
        play_name = c("4th Down Conversion", "Punt"),
        play_name2 = c("Go For It!", "Punt!")
      )
  }
  
  #creates a table with all probabilities if all play types are possible from the give situation
  if(!(prob_field == 0 | prob_punt == 0 | is.na(prob_field) == TRUE | is.na(prob_punt) == TRUE | is.na(final_punt) == TRUE | is.na(final_field) == TRUE)) {
    final <- 
      tibble(
        play_type = c("Field Goal", "Go For It", "Punt"),
        play_prob = c(prob_field, prob_go, prob_punt),
        win_prob = c(final_field, final_go, final_punt),
        games = c(games_field, games_go, games_punt),
        play_name = c("Field Goal", "4th Down Conversion", "Punt"),
        play_name2 = c("Kick a Field Goal!", "Go For It!", "Punt!")
      )
  }
  
  
  final
}