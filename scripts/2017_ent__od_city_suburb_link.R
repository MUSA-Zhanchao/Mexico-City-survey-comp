library(foreign)
library(tidyverse)

# Load 2017 data
trip_2017 <- read.csv("data/2017/trip_2017.csv")

# Join trip data with household ENT (origin ENT)
trip_2017 <- trip_2017 %>%
  rename(origin_ent = P5_7_7,
         dest_ent = P5_12_7)

# Filter for valid trips only (P5_3 == 1)
trip_2017 <- trip_2017 %>%
  filter(P5_3 == 1)

# City to suburb trips (origin 9 and destination 13 or 15)
city_to_suburb <- trip_2017 %>%
  filter(origin_ent == "9" & dest_ent %in% c("13", "15")) %>%
  select(starts_with("P5_14"), FACTOR)

# Suburb to city trips (origin 13 or 15 and destination 9)
suburb_to_city <- trip_2017 %>%
  filter(origin_ent %in% c("13", "15") & dest_ent == "9") %>%
  select(starts_with("P5_14"), FACTOR)
complete_suburb_city <- rbind(city_to_suburb, suburb_to_city)

#turns all 2 to NA
complete_suburb_city[complete_suburb_city == 2] <- NA
