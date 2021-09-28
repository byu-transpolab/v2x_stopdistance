# This is where you write functions that can be called from 
# _targets
# 
# 

#' Summarize a mean value
#' 
#' @param dataset A data frame or tibble with an `x` variable.
summ <- function(dataset) {
  summarize(dataset, mean_x = mean(x))
}



#' Function to move point onto curve
#' 
#' @param point A single point 
#' @param curve A curve or line to move the point onto
#' 
move2curve <- function(point, curve){
  # TODO Shannon should fill out this function
  # Possibly change labeling system to make it easier to change variables
}

# Create another function to measure distance to curve from any given point