library(foreign)
library(tidyverse)

# Load 2017 data
trip_2017 <- read.csv("data/2017/trip_2017.csv")
# hogar_2017 <- read.dbf("data/2017/THOGAR.DBF", as.is = TRUE)
# dem_2017 <- read.dbf("data/2017/TSDEM.DBF", as.is = TRUE)
# 
# # Join household and demographic data
# hogar_2017 <- hogar_2017 %>%
#   select(ID_HOG, ENT)
# dem_2017 <- dem_2017 %>%
#   select(ID_SOC, ID_HOG)
# dem_hogar_2017 <- dem_2017 %>%
#   left_join(hogar_2017, by = "ID_HOG")

# Join trip data with household ENT (origin ENT)
trip_2017 <- trip_2017 %>%
  rename(origin_ent = P5_7_7,
         dest_ent = P5_12_7) 

# Filter for valid trips only (P5_3 == 1)
trip_2017 <- trip_2017 %>%
  filter(P5_3 == 1)

# City to city trips (origin 9 and destination 9)
city_to_city <- trip_2017 %>%
  filter(origin_ent == "9" & dest_ent == "9") %>%
  select(starts_with("P5_14"), FACTOR)

# Suburb to suburb trips (origin 15 or 13 and destination 15 or 13)
suburb_to_suburb <- trip_2017 %>%
  filter(origin_ent %in% c("13", "15") & dest_ent %in% c("13", "15")) %>%
  select(starts_with("P5_14"), FACTOR)

