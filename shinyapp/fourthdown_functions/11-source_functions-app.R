library(plyr)
library(tidyverse)

source("./fourthdown_functions/01-loaddata-app.R")
source("./fourthdown_functions/02-time_to_seconds-app.R")
source("./fourthdown_functions/03-prob_plays-app.R")
source("./fourthdown_functions/04-win_prob-app.R")
source("./fourthdown_functions/05-fourth_win_prob-app.R")
source("./fourthdown_functions/06-punt_win_prob-app.R")
source("./fourthdown_functions/07-final_prob-app.R")
source("./fourthdown_functions/08-amount_of_games-app.R")
source("./fourthdown_functions/09-greatest_win_prob-app.R")
source("./fourthdown_functions/10-call_functions-app.R")

# test <- display(quarter = 4,
#         time = "2:00",
#         score = -7,
#         yards_to_go = 20,
#         yardline = 60,
#         lower_seconds_bound = -50,
#         upper_seconds_bound = 50,
#         field_data = fieldgoal,
#         go_data = fourthdown,
#         punt_data = punt,
#         base_plays_data = data_small,
#         last_plays_data = last_plays,
#         drives_data = drives
# )
# test
# greatest_win_prob(test)

