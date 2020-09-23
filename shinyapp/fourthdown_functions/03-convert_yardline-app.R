convert_yardline <- function(yardline_half, side) {
  if(side == "own") {
    yardline <- 100-yardline_half
  }
  if(side == "opp") {
    yardline <- yardline_half
  }
  yardline
}
