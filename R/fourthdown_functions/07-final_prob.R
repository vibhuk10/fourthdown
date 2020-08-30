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