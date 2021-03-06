theme_set(theme_classic(base_size = 16))

data %>% 
  filter(down == 4 & !(play_type == "no_play" | play_type == "qb_kneel")) %>% 
  count(play_type) %>% 
  mutate(prob = n/sum(n)) %>% 
  filter(play_type == "pass" | play_type == "run") %>% 
  mutate(prob2 = sum(prob))

data %>% 
  filter(down == 4 & !(play_type == "no_play" | play_type == "qb_kneel" ) & (!qtr == 4)) %>% 
  count(play_type) %>% 
  mutate(prob = n/sum(n)) %>% 
  filter(play_type == "pass" | play_type == "run") %>% 
  mutate(prob2 = sum(prob))

data %>% 
  filter(down == 4 & !(play_type == "no_play" | play_type == "qb_kneel" ) & score_differential > -9) %>% 
  count(play_type) %>% 
  mutate(prob = n/sum(n)) %>% 
  filter(play_type == "pass" | play_type == "run") %>% 
  mutate(prob2 = sum(prob))
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
  
# what is the average distance of punts
punts <- 
  data %>% 
  filter(play_type == "punt")

punts2 <- 
  data %>% 
  filter(play_type == "punt" & punt_in_endzone == 1)
