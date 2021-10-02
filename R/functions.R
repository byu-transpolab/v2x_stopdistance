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
move2curve <- function(cx1, cy1, cx2, cy2, lng, lat){
  m <- (cy1 - cy2)/(cx1 - cx2)
  k <- cy2 - m * cx2
  nx <- (m * lat + lng - m * k)/(1 + m^2)
  ny <- m * nx + k
  n_point <- c(nx, ny)
  return(n_point)
}


move2curve(-111.7228, 40.6336, -111.7226, 40.63341, 40.63344, -111.7226)
#' Measure distance along curve
#' 
#' @param point A single point, previously moved to the curve
#' @param curve A 
#' 
#' 
dist_along <- function(point, curve){
  # TODO: shannon will fill this one out also.
}
