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
}


#' Measure distance along curve
#' 
#' @param point A single point, previously moved to the curve
#' @param curve A 
#' 
#' 
dist_along <- function(point, curve){
  # TODO: shannon will fill this one out also.
}
