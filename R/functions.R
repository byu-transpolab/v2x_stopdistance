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

move2curve(-111.7228, 40.6336, -111.7226, 40.63341, -111.7226, 40.63344)

filter_distance <- function()

MapPoints <- function( center_df, data_df, east_west ){
  a=1
  new_df <- data.frame(LONGITUDE = numeric(),
                       LATITUDE = numeric())
  if (east_west == east){
   for (i in 1:length(center_df)-1){
     for (j in 1:length(data_df)){
       move2curve(center_df$E_LONG[i], 
                  center_df$E_LAT[i], 
                  center_df$E_LONG[i+1],
                  center_df$E_LAT[i+1],
                  data_df$LONGITUDE[j],
                  data_df$LATITUDE[j])
       new_df$LONGITUDE[a]<-n_point[1]
       new_df$LATITUDE[a]<-n_point[1]
       a=a+1
       
   
   }}
}}

#' Measure distance along curve
#' 
#' @param point A single point, previously moved to the curve
#' @param curve A 
#' 
#' 
dist_along <- function(point, curve){
  # TODO: shannon will fill this one out also.
}
