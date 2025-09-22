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

# Categorize transportation modes for spatial analysis
# Metro-only trips (no matter internal transfer)
metro_only_trips <- mexico_city_trips_2017 %>%
  mutate(
    # Categorize all P5_14 columns to transportation modes
    P5_14_01 = ifelse(is.na(P5_14_01), NA, "Other"),
    P5_14_02 = ifelse(is.na(P5_14_02), NA, "Bus"),
    P5_14_03 = ifelse(is.na(P5_14_03), NA, "Other"),
    P5_14_04 = ifelse(is.na(P5_14_04), NA, "Other"),
    P5_14_05 = ifelse(is.na(P5_14_05), NA, "Metro"),
    P5_14_06 = ifelse(is.na(P5_14_06), NA, "Bus"),
    P5_14_07 = ifelse(is.na(P5_14_07), NA, "Other"),
    P5_14_08 = ifelse(is.na(P5_14_08), NA, "Bus"),
    P5_14_09 = ifelse(is.na(P5_14_09), NA, "Other"),
    P5_14_10 = ifelse(is.na(P5_14_10), NA, "Bus"),
    P5_14_11 = ifelse(is.na(P5_14_11), NA, "Metro"),
    P5_14_12 = ifelse(is.na(P5_14_12), NA, "Metro"),
    P5_14_13 = ifelse(is.na(P5_14_13), NA, "Metro"),
    P5_14_14 = ifelse(is.na(P5_14_14), NA, NA),
    P5_14_15 = ifelse(is.na(P5_14_15), NA, "Metro"),
    P5_14_16 = ifelse(is.na(P5_14_16), NA, "Other"),
    P5_14_17 = ifelse(is.na(P5_14_17), NA, "Other"),
    P5_14_18 = ifelse(is.na(P5_14_18), NA, "Bus"),
    P5_14_19 = ifelse(is.na(P5_14_19), NA, "Other"),
    P5_14_20 = ifelse(is.na(P5_14_20), NA, "Other")
  ) %>%
  rowwise() %>%
  mutate(
    # Get unique modes used in trip (excluding NA)
    unique_modes = list(unique(na.omit(c_across(starts_with("P5_14_")))))
  ) %>%
  ungroup() %>%
  filter(
    # Metro-only trips: only Metro mode used, no other modes
    lengths(unique_modes) == 1 & sapply(unique_modes, function(x) "Metro" %in% x)
  )

metro_only_by_mun <- metro_only_trips %>%
  group_by(MUN) %>%
  summarise(
    metro_only_count = n(),
    .groups = 'drop'
  )

# Bus-only trips (same principle)
bus_only_trips <- mexico_city_trips_2017 %>%
  mutate(
    # Categorize all P5_14 columns to transportation modes
    P5_14_01 = ifelse(is.na(P5_14_01), NA, "Other"),
    P5_14_02 = ifelse(is.na(P5_14_02), NA, "Bus"),
    P5_14_03 = ifelse(is.na(P5_14_03), NA, "Other"),
    P5_14_04 = ifelse(is.na(P5_14_04), NA, "Other"),
    P5_14_05 = ifelse(is.na(P5_14_05), NA, "Metro"),
    P5_14_06 = ifelse(is.na(P5_14_06), NA, "Bus"),
    P5_14_07 = ifelse(is.na(P5_14_07), NA, "Other"),
    P5_14_08 = ifelse(is.na(P5_14_08), NA, "Bus"),
    P5_14_09 = ifelse(is.na(P5_14_09), NA, "Other"),
    P5_14_10 = ifelse(is.na(P5_14_10), NA, "Bus"),
    P5_14_11 = ifelse(is.na(P5_14_11), NA, "Metro"),
    P5_14_12 = ifelse(is.na(P5_14_12), NA, "Metro"),
    P5_14_13 = ifelse(is.na(P5_14_13), NA, "Metro"),
    P5_14_14 = ifelse(is.na(P5_14_14), NA, NA),
    P5_14_15 = ifelse(is.na(P5_14_15), NA, "Metro"),
    P5_14_16 = ifelse(is.na(P5_14_16), NA, "Other"),
    P5_14_17 = ifelse(is.na(P5_14_17), NA, "Other"),
    P5_14_18 = ifelse(is.na(P5_14_18), NA, "Bus"),
    P5_14_19 = ifelse(is.na(P5_14_19), NA, "Other"),
    P5_14_20 = ifelse(is.na(P5_14_20), NA, "Other")
  ) %>%
  rowwise() %>%
  mutate(
    # Get unique modes used in trip (excluding NA)
    unique_modes = list(unique(na.omit(c_across(starts_with("P5_14_")))))
  ) %>%
  ungroup() %>%
  filter(
    # Bus-only trips: only Bus mode used, no other modes
    lengths(unique_modes) == 1 & sapply(unique_modes, function(x) "Bus" %in% x)
  )

bus_only_by_mun <- bus_only_trips %>%
  group_by(MUN) %>%
  summarise(
    bus_only_count = n(),
    .groups = 'drop'
  )

# Bus + Metro transfer trips
bus_metro_trips <- mexico_city_trips_2017 %>%
  mutate(
    # Categorize all P5_14 columns to transportation modes
    P5_14_01 = ifelse(is.na(P5_14_01), NA, "Other"),
    P5_14_02 = ifelse(is.na(P5_14_02), NA, "Bus"),
    P5_14_03 = ifelse(is.na(P5_14_03), NA, "Other"),
    P5_14_04 = ifelse(is.na(P5_14_04), NA, "Other"),
    P5_14_05 = ifelse(is.na(P5_14_05), NA, "Metro"),
    P5_14_06 = ifelse(is.na(P5_14_06), NA, "Bus"),
    P5_14_07 = ifelse(is.na(P5_14_07), NA, "Other"),
    P5_14_08 = ifelse(is.na(P5_14_08), NA, "Bus"),
    P5_14_09 = ifelse(is.na(P5_14_09), NA, "Other"),
    P5_14_10 = ifelse(is.na(P5_14_10), NA, "Bus"),
    P5_14_11 = ifelse(is.na(P5_14_11), NA, "Metro"),
    P5_14_12 = ifelse(is.na(P5_14_12), NA, "Metro"),
    P5_14_13 = ifelse(is.na(P5_14_13), NA, "Metro"),
    P5_14_14 = ifelse(is.na(P5_14_14), NA, NA),
    P5_14_15 = ifelse(is.na(P5_14_15), NA, "Metro"),
    P5_14_16 = ifelse(is.na(P5_14_16), NA, "Other"),
    P5_14_17 = ifelse(is.na(P5_14_17), NA, "Other"),
    P5_14_18 = ifelse(is.na(P5_14_18), NA, "Bus"),
    P5_14_19 = ifelse(is.na(P5_14_19), NA, "Other"),
    P5_14_20 = ifelse(is.na(P5_14_20), NA, "Other")
  ) %>%
  rowwise() %>%
  mutate(
    # Get unique modes used in trip (excluding NA)
    unique_modes = list(unique(na.omit(c_across(starts_with("P5_14_")))))
  ) %>%
  ungroup() %>%
  filter(
    # Bus+Metro transfer trips: exactly 2 modes used, and they are Bus and Metro
    lengths(unique_modes) == 2 &
    sapply(unique_modes, function(x) "Bus" %in% x & "Metro" %in% x)
  )

bus_metro_by_mun <- bus_metro_trips %>%
  group_by(MUN) %>%
  summarise(
    bus_metro_count = n(),
    .groups = 'drop'
  )

# Summary by municipality for ENT=09 spatial distribution
spatial_distribution_ent09_2017 <- mexico_city_trips_2017 %>%
  distinct(MUN) %>%
  left_join(metro_only_by_mun, by = "MUN") %>%
  left_join(bus_only_by_mun, by = "MUN") %>%
  left_join(bus_metro_by_mun, by = "MUN") %>%
  mutate(
    metro_only_count = ifelse(is.na(metro_only_count), 0, metro_only_count),
    bus_only_count = ifelse(is.na(bus_only_count), 0, bus_only_count),
    bus_metro_count = ifelse(is.na(bus_metro_count), 0, bus_metro_count),
  )

write.csv(spatial_distribution_ent09_2017, "municipal_level_analysis/export/ent09_trip_categories_17.csv", row.names = FALSE)
