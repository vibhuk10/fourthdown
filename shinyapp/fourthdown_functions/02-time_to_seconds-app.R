# this functions takes in string time input and outputs the time in seconds
time_to_seconds <- function(time) {
  minutes <- 
    str_sub(time, 1, str_locate(time, ":")[1, 1] - 1)
  minutes <- 
    as.numeric(minutes) * 60
  seconds <- 
    str_sub(time, str_locate(time, ":")[1, 1] + 1)
  seconds <- 
    as.numeric(seconds)
  seconds_left <- minutes + seconds
  seconds_left
}