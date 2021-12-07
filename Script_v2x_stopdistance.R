library(tidyverse)
library(odbc)
library(ggplot2)
library(tmap)
library (tmaptools)
library (leaflet)
library (dplyr)
library (ggmap)
library(sf)
library (ggthemes)
library (XML2R)
library (XML)
library (methods)
library (snow)


source("R/functions.R")
# make odb connection=====================================

con <- DBI::dbConnect(odbc::odbc(),
                      Driver       = "SnowflakeDSIIDriver",
                      Server       = "cirrus-panasonic.snowflakecomputing.com",
                      UID          = "SHANNON_ANDERSEN",
                      PWD          = "sn0wf.N@n")
# gather data from connection===============================

Big_Cottonwood_Canyon <-con %>%
  tbl("V2X_PARSED_MESSAGES_CONVERTED_VW") %>%
  filter (LATITUDE < 40.677  & LATITUDE > 40.564, 
          LONGITUDE < -111.586 & LONGITUDE > -111.797,
          SPEED_MPH > 10 & SPEED_MPH < 75)%>%
  # RECEIVED_TMSTP_EPOCH > 1627434000) %>%
  select(TEMP_ID,
         SPEED_MPH, 
         LATITUDE, 
         LONGITUDE, 
         ELEVATION_FT,
         RECEIVED_TMSTP_UTC,
         RECEIVED_TMSTP_EPOCH,
         LONGITUDINAL_ACCELERATION_MS2,
         BRAKE_APPLIED_STATUS)%>%
  collect()%>%
  # slice_sample (prop = 1/10)%>%
  print()

S_Curve <- Big_Cottonwood_Canyon %>%
  filter ( LONGITUDE >= -111.7267576 & LONGITUDE <= -111.7219015,
           LATITUDE >= 40.6329933 & LATITUDE <= 40.6349524) %>%
  print ()
#Add Centerline Points ============================================================================
S_Curve_Centerlines <- read.csv("C:/Users/shann/OneDrive/Desktop/Transportation RA/V2X/V2X_Braking_Data/S_CURVE_CENTERLINES.csv")
#Color palettes ===================================================================================================
pal <- colorNumeric(
  palette = "Spectral",
  domain = S_Curve$SPEED_MPH)

blu <- colorNumeric(
  palette = "BuPu",
  domain = S_Curve$SPEED_MPH)
# Test add_points function=========================================================
W_1<-add_points_to_df(Map_1, S_Curve, 1, -1)
W_2<-add_points_to_df(Map_2, S_Curve, 2, -1)
W_3<-add_points_to_df(Map_3, S_Curve, 3, -1)
W_4<-add_points_to_df(Map_4, S_Curve, 4, -1)
W_5<-add_points_to_df(Map_5, S_Curve, 5, -1)
W_6<-add_points_to_df(Map_6, S_Curve, 6, -1)
W_7<-add_points_to_df(Map_7, S_Curve, 7, -1)
W_8<-add_points_to_df(Map_8, S_Curve, 8, -1)
W_9<-add_points_to_df(Map_9, S_Curve, 9, -1)
W_10<-add_points_to_df(Map_10, S_Curve, 10, -1)
W_11<-add_points_to_df(Map_11, S_Curve, 11, -1)
W_12<-add_points_to_df(Map_12, S_Curve, 12, -1)
W_13<-add_points_to_df(Map_13, S_Curve, 13, -1)
W_14<-add_points_to_df(Map_14, S_Curve, 14, -1)
W_15<-add_points_to_df(Map_15, S_Curve, 15, -1)
W_16<-add_points_to_df(Map_16, S_Curve, 16, -1)
W_17<-add_points_to_df(Map_17, S_Curve, 17, -1)
W_18<-add_points_to_df(Map_18, S_Curve, 18, -1)
W_19<-add_points_to_df(Map_19, S_Curve, 19, -1)
W_20<-add_points_to_df(Map_20, S_Curve, 20, -1)
W_21<-add_points_to_df(Map_21, S_Curve, 21, -1)
W_22<-add_points_to_df(Map_22, S_Curve, 22, -1)
W_23<-add_points_to_df(Map_23, S_Curve, 23, -1)
W_24<-add_points_to_df(Map_24, S_Curve, 24, -1)
W_25<-add_points_to_df(Map_25, S_Curve, 25, -1)
W_26<-add_points_to_df(Map_26, S_Curve, 26, -1)
W_27<-add_points_to_df(Map_27, S_Curve, 27, -1)
W_28<-add_points_to_df(Map_28, S_Curve, 28, -1)

E_1<-add_points_to_df(Map_1, S_Curve, 1, 1)
E_2<-add_points_to_df(Map_2, S_Curve, 2, 1)
E_3<-add_points_to_df(Map_3, S_Curve, 3, 1)
E_4<-add_points_to_df(Map_4, S_Curve, 4, 1)
E_5<-add_points_to_df(Map_5, S_Curve, 5, 1)
E_6<-add_points_to_df(Map_6, S_Curve, 6, 1)
E_7<-add_points_to_df(Map_7, S_Curve, 7, 1)
E_8<-add_points_to_df(Map_8, S_Curve, 8, 1)
E_9<-add_points_to_df(Map_9, S_Curve, 9, 1)
E_10<-add_points_to_df(Map_10, S_Curve, 10, 1)
E_11<-add_points_to_df(Map_11, S_Curve, 11, 1)
E_12<-add_points_to_df(Map_12, S_Curve, 12, 1)
E_13<-add_points_to_df(Map_13, S_Curve, 13, 1)
E_14<-add_points_to_df(Map_14, S_Curve, 14, 1)
E_15<-add_points_to_df(Map_15, S_Curve, 15, 1)
E_16<-add_points_to_df(Map_16, S_Curve, 16, 1)
E_17<-add_points_to_df(Map_17, S_Curve, 17, 1)
E_18<-add_points_to_df(Map_18, S_Curve, 18, 1)
E_19<-add_points_to_df(Map_19, S_Curve, 19, 1)
E_20<-add_points_to_df(Map_20, S_Curve, 20, 1)
E_21<-add_points_to_df(Map_21, S_Curve, 21, 1)
E_22<-add_points_to_df(Map_22, S_Curve, 22, 1)
E_23<-add_points_to_df(Map_23, S_Curve, 23, 1)
E_24<-add_points_to_df(Map_24, S_Curve, 24, 1)
E_25<-add_points_to_df(Map_25, S_Curve, 25, 1)
E_26<-add_points_to_df(Map_26, S_Curve, 26, 1)
E_27<-add_points_to_df(Map_27, S_Curve, 27, 1)
E_28<-add_points_to_df(Map_28, S_Curve, 28, 1)

lead(S_Curve_Centerlines$W_LONG)-S_Curve_Centerlines$W_LONG


leaflet () %>%
  addTiles () %>%
  addCircles(data = S_Curve_Centerlines,
             lng = S_Curve_Centerlines$W_LONG,
             lat = S_Curve_Centerlines$W_LAT,
             color = "red",
             radius = 5,
             label = ~paste ("Latitude:",S_Curve_Centerlines$W_LAT,
                             "Longitude:", S_Curve_Centerlines$W_LONG),
             group = "Westbound")%>%
  addCircles (data = W_1,
              lng = W_1$center_lng,
              lat = W_1$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
  addCircles (data = W_2,
              lng = W_2$center_lng,
              lat = W_2$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
  addCircles (data = W_3,
              lng = W_3$center_lng,
              lat = W_3$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
  addCircles (data = W_4,
              lng = W_4$center_lng,
              lat = W_4$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
  addCircles (data = W_5,
              lng = W_5$center_lng,
              lat = W_5$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
  addCircles (data = W_6,
              lng = W_6$center_lng,
              lat = W_6$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
  addCircles (data = W_7,
              lng = W_7$center_lng,
              lat = W_7$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
  addCircles (data = W_8,
              lng = W_8$center_lng,
              lat = W_8$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
  addCircles (data = W_9,
              lng = W_9$center_lng,
              lat = W_9$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
  addCircles (data = W_10,
              lng = W_10$center_lng,
              lat = W_10$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
  addCircles (data = W_11,
              lng = W_11$center_lng,
              lat = W_11$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
  addCircles (data = W_12,
              lng = W_12$center_lng,
              lat = W_12$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
  addCircles (data = W_13,
              lng = W_13$center_lng,
              lat = W_13$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
  addCircles (data = W_14,
              lng = W_14$center_lng,
              lat = W_14$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
  addCircles (data = W_15,
              lng = W_15$center_lng,
              lat = W_15$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
  addCircles (data = W_16,
              lng = W_16$center_lng,
              lat = W_16$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
  addCircles (data = W_17,
              lng = W_17$center_lng,
              lat = W_17$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
  addCircles (data = W_18,
              lng = W_18$center_lng,
              lat = W_18$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
  addCircles (data = W_19,
              lng = W_19$center_lng,
              lat = W_19$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
  addCircles (data = W_20,
              lng = W_20$center_lng,
              lat = W_20$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
  addCircles (data = W_21,
              lng = W_21$center_lng,
              lat = W_21$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
  addCircles (data = W_22,
              lng = W_22$center_lng,
              lat = W_22$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
  addCircles (data = W_23,
              lng = W_23$center_lng,
              lat = W_23$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
  addCircles (data = W_24,
              lng = W_24$center_lng,
              lat = W_24$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
  addCircles (data = W_25,
              lng = W_25$center_lng,
              lat = W_25$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
  addCircles (data = W_26,
              lng = W_26$center_lng,
              lat = W_26$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
  addCircles (data = W_27,
              lng = W_27$center_lng,
              lat = W_27$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
  addCircles (data = W_28,
              lng = W_28$center_lng,
              lat = W_28$center_lat,
              color = ~pal(SPEED_MPH),
              radius = .5,
              group = "mapped points")%>%
 
  addLayersControl(overlayGroups = "mapped points")

Trial_3<-t(Trial_3)
# Actual Map ======================================================================
#Poster Picture ======================================================================
#Speed Map =========================================================
W_1_List<-W_1%>%
  group_by(TEMP_ID)%>%
  tally()%>%
  print()

W_ID_LIST<- c("/VLFGg==",
              "36Pc9g==",
              "almn6w==",
              "Cq831A==",
              "jkU6/w==",
              "JrlKbg==",
              "MIAncA==",
              "UiqXtQ==",
              "W66GkQ==",
              "XQCHIQ==",
              "XV2YnQ==")
n<-length(W_ID_LIST)

W_1_Test <-W_1

W_1_Test%>%
  filter(TEMP_ID == W_ID_LIST[1],
         TEMP_ID == W_ID_LIST[1])

#Cars leaving the curve going westbound
ggplot(NULL, aes(x=dist_to_curve, 
                y=LONGITUDINAL_ACCELERATION_MS2, col = TEMP_ID))+
  geom_line(data = W_1)+
  geom_point(data = W_2)+
  geom_point(data = W_3)+
  geom_point(data = W_4)+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  ggtitle("Westbound Departure")
 #Cars entering curve going westbound
ggplot(NULL, aes(x=dist_to_curve, 
                 y=LONGITUDINAL_ACCELERATION_MS2, col = TEMP_ID))+
  geom_point(data = W_22)+
  geom_point(data = W_23)+
  geom_point(data = W_24)+
  geom_point(data = W_25)+
  geom_point(data = W_26)+
  geom_point(data = W_27)+
  geom_point(data = W_28)+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  ggtitle("Westbound Approach")
#Cars approaching the curve going eastbound
ggplot(NULL, aes(x=dist_to_curve, 
                 y=LONGITUDINAL_ACCELERATION_MS2, col = TEMP_ID))+
  geom_point(data = E_1)+
  geom_point(data = E_2)+
  geom_point(data = E_3)+
  geom_point(data = E_4)+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  ggtitle("Eastbound Approach")
#Cars exiting the curve going eastbound
ggplot(East_approach%>%
         filter(TEMP_ID%in% sample(unique(East_approach$TEMP_ID), 10)),
                  aes(x=dist_to_curve, 
                 y=LONGITUDINAL_ACCELERATION_MS2, col = TEMP_ID))+
  geom_line(alpha=0.1)+
  stat_smooth()+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  scale_x_reverse()+
  ggtitle("Eastbound Approach")


East_departure<-bind_rows(E_22, E_23, E_24, E_25, E_26, E_27, E_28)
East_approach<-bind_rows(E_1, E_2, E_3, E_4)
