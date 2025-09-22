library(foreign)
library(tidyverse)

trip_2017<- read.csv("data/2017/trip_2017.csv")
hogar_2017<- read.dbf("data/2017/THOGAR.DBF", as.is = TRUE)
dem_2017<- read.dbf("data/2017/TSDEM.DBF", as.is = TRUE)

hogar_2017 <- hogar_2017 %>%
  select(ID_HOG, ENT, MUN)
dem_2017 <- dem_2017 %>%
  select(ID_SOC, ID_HOG)
dem_hogar_2017 <- dem_2017 %>%
  left_join(hogar_2017, by = "ID_HOG")

complete_trip_2017 <- trip_2017 %>%
  left_join(dem_hogar_2017, by ="ID_SOC")

mexico_city_trips_2017 <- complete_trip_2017 %>%
  filter(ENT == "09")%>%
  filter(P5_3==1)%>%
  select(starts_with("P5_14"), MUN)

mexico_city_trips_2017[mexico_city_trips_2017 == 2] <- NA

