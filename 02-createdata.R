#full NFL play by play data 2009-2019
data <- 
  data %>% 
  mutate(drive_id = paste0(drive,"-",game_id))

data %>% write_csv("data-raw/NFL_pbp_2009-2019.csv")

#all fourth down plays from 2009-2019
fourthdown <-  
  data %>% 
  filter(down == 4, !(play_type == "field_goal" | play_type == "punt" | play_type == "no_play")) %>% 
  select(play_id:game_id, drive_id, home_team:posteam, defteam, yardline_100, game_date, 
         qtr, quarter_seconds_remaining, ydstogo, yards_gained, ydsnet, down,
         play_type, total_home_score:score_differential, fourth_down_converted,
         fourth_down_failed
  )

fourthdown %>% write_csv("data-clean/NFL_fourthdown_2009-2019.csv")

#all drives from 2009-2019
drives_start <- 
  data %>% 
  group_by(drive_id) %>% 
  slice(1) %>% 
  mutate(score_differential_start = total_home_score-total_away_score,
         score_differential_start = ifelse(posteam == away_team, -score_differential_start, score_differential_start)) %>% 
  select(play_id:game_id, drive_id, home_team:posteam, defteam, yardline_100, 
          qtr, quarter_seconds_remaining, score_differential_start, play_type, desc) %>% 
  rename(yardline_start = yardline_100, qtr_start = qtr, qtr_secs_start = quarter_seconds_remaining, posteam_start = posteam)
    
drives_end <- 
  data %>% 
  group_by(drive_id) %>% 
  slice(n()) %>% 
  mutate(score_differential_end = total_home_score-total_away_score,
         score_differential_end = ifelse(posteam == away_team, -score_differential_end, score_differential_end)) %>% 
  select(drive_id, play_type, posteam, defteam, yardline_100, qtr, quarter_seconds_remaining, score_differential_end, desc) %>% 
  rename(yardline_end = yardline_100, qtr_end = qtr, qtr_secs_end = quarter_seconds_remaining, posteam_end = posteam)

drives <- 
  drives_start %>% 
  left_join(drives_end, by = "drive_id") %>% 
  mutate(score_differential = score_differential_end - score_differential_start,
         drive_yards = yardline_start - yardline_end,
         passed_time = ifelse(qtr_start == qtr_end, qtr_secs_start - qtr_secs_end, qtr_secs_start+(900-qtr_secs_end)),
         check = ifelse(posteam_start == posteam_end, 0, 1)
         ) %>% 
  filter(!(play_type.x == "field_goal" | play_type.x == "no_play" | play_type.y == "no_play" | check == 1 | score_differential > 8)) %>% 
  select(play_id:score_differential_start, play_type.y, yardline_end:passed_time) %>% 
  rename(posteam = posteam_start, defteam = defteam.x, play_type = play_type.y, desc = desc.y) %>% 
  na.omit()

drives %>% write_csv("data-clean/NFL_drives_2009-2019.csv")


