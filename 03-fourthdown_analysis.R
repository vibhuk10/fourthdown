theme_set(theme_classic(base_size = 16))

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

# what is the average distance of punts
punts <- 
  data %>% 
  filter(play_type == "punt")

punts2 <- 
  data %>% 
  filter(play_type == "punt" & punt_in_endzone == 1)
