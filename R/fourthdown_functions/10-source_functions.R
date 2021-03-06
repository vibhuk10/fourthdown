library(plyr)
library(tidyverse)

source("./R/fourthdown_functions/01-loaddata_app.R")
source("./R/fourthdown_functions/02-time_to_seconds.R")
source("./R/fourthdown_functions/03-prob_plays.R")
source("./R/fourthdown_functions/04-win_prob.R")
source("./R/fourthdown_functions/05-fourth_win_prob.R")
source("./R/fourthdown_functions/06-punt_win_prob.R")
source("./R/fourthdown_functions/07-final_prob.R")
source("./R/fourthdown_functions/08-amount_of_games.R")
source("./R/fourthdown_functions/09-call_functions.R")

display(quarter = 3,
        time = "1:05",
        score = 10,
        yards_to_go = 2,
        yardline = 47,
        lower_seconds_bound = -20,
        upper_seconds_bound = 20,
        field_data = fieldgoal,
        go_data = fourthdown,
        punt_data = punt,
        base_plays_data = data_small,
        last_plays_data = last_plays,
        drives_data = drives
)

