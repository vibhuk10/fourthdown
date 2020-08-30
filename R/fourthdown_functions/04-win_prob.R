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
