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
    filter(qtr == quarter & quarter_seconds_remaining>(timeleft+lower_seconds_bound) & quarter_seconds_remaining<(timeleft+upper_seconds_bound) & score_differential == (-1*(score)) & yardline_100 < (100-(ball_placement-5)) & yardline_100 > (100-(ball_placement+5))) %>% 
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

  prob_game_result_punt(quarter = 4,
                        timeleft = 300,
                        score = -7,
                        yards_to_go = 1,
                        play_type = "punt",
                        result = "yes",
                        yardline = 35,
                        lower_seconds_bound = -50,
                        upper_seconds_bound = 50,
                        base_plays_data = data_small,
                        last_plays_data = last_plays
  ) 
  