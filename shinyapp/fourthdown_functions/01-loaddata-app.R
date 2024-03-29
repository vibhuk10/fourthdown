library(plyr)
library(tidyverse)

fourthdown <- read_csv("./data-app/NFL_fourthdown_2009-2019.csv")

drives <- read_csv("./data-app/NFL_drives_2009-2019.csv")

fieldgoal <- read_csv("./data-app/NFL_fielgoal_2009-2019.csv")

last_plays <- read_csv("./data-app/last_plays_2009-2019.csv")

punt <- read_csv("./data-app/NFL_punt_2009-2019.csv")

data_small <- read_csv("./data-app/NFL_pbp_small_2009-2019.csv")