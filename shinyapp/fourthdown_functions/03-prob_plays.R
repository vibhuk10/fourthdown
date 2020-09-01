# this functions takes in the yardline and outputs the probabilities for a field goal
create_prob_field_goal <- function(yardline, data) {
  
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

# this functions takes in the yards to go and outputs the probabilities for a 4th down conversion
create_prob_go <- function(yards_to_go, data) {

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

# this functions takes in the yardline and outputs the probabilities for a punt being blocked
create_prob_punt <-  function(yardline, data) {

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
