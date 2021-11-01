

#' Summarize a mean value
#'
#' 
#' @param dataset A data frame or tibble with an `x` variable.
summ <- function(dataset) {
  summarize(dataset, mean_x = mean(x))
}



#' Function to move point onto curve
#' 
#' @param point Latitude and longitude coordinates for a data point
#' @param points latitude and longitude coordinates for two points on the road centerline
#' 
move2curve <- function(cx1, cy1, cx2, cy2, lng, lat){
    m <- (cy1 - cy2)/(cx1 - cx2)
    k <- cy2 - m * cx2
    nx <- (m * lat + lng - m * k)/(1 + m^2)
    ny <- m * nx + k
    center_distance <- sqrt((nx - lng)^2 + (ny - lat)^2)*364000
    return(c(nx, ny, center_distance))
}

#' Function to create a new data frame by mapping points onto a curve and adding them in two new columns
#' @param new_df the name of the new data frame being created
#' @param old_df the existing df that contains the current data points
#' @param cp centerline point number
add_points_to_df <-function(new_df, old_df, cp){
  cp <- as.numeric(cp)
  new_df <-as.data.frame(old_df)
  old_df <-as.data.frame(old_df)
  new_df<-mutate(new_df, center_x =new_df$LONGITUDE-new_df$LONGITUDE,
                 new_df, center_y =new_df$LATITUDE-new_df$LATITUDE,
                 new_df, center_d =new_df$LATITUDE-new_df$LATITUDE)
  for(i in 1:length(old_df$LATITUDE)){
    cx1=S_Curve_Centerlines$W_LONG[cp+1]
    cy1=S_Curve_Centerlines$W_LAT[cp+1]
    cx2=S_Curve_Centerlines$W_LONG[cp]
    cy2=S_Curve_Centerlines$W_LAT[cp]
    lng=new_df$LONGITUDE[i]
    lat=new_df$LATITUDE[i]
    output<-move2curve(cx1, cy1, cx2, cy2, lng, lat)
    a=output[1]
    b=output[2]
    c=output[3]
    new_df$center_x[i]=a
    new_df$center_y[i]=b
    new_df$center_d[i]=c
  }
  # new_df<-filter(new_df,
  #         center_d<30,
  #         LONGITUDE<=cx1,
  #         LATITUDE>=cy1)
  # new_df<-group_by(new.df, TEMP_ID)
  # new_df<-mutate(new.df,
  #                ELEVATION_CHANGE = lead(ELEVATION_FT)-ELEVATION_FT)
  # new_df<-ungroup(new_df, TEMP_ID)
  # new_df<-filter(new_df, ELEVATION_CHANGE<0)
  # return(new_df)
}

#' Transforms latitude/longitude data into sf
transform_sf <- function(df){
  pts <- st_as_sf(df, coords = c("LONGITUDE", "LATITUDE"), crs = 32612)
  View(pts)
}



#' Condensed version of addCircles leaflet function
#' @param map leave blank, this comes from the preceding leaflet function
#' @param DF data frame that contains data to be mapped
#' @param color optional - indicate circle color
#' @param RAD default circle radius is 0.5
#' @param GROUP group points for convenience when mapping
my_add_circles <-function(map, DF, COLOR, RAD=0.5, GROUP, ...){
  addCircles(map,
            data = DF,
            lng = as.numeric(DF$LONGITUDE),
            lat = as.numeric(DF$LATITUDE),
            color = COLOR,
            radius = RAD,
            group = GROUP,
            ...)
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
