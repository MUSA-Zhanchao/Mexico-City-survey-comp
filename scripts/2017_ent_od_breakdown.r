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

# Function to classify 2017 trips into mode categories
classify_trips_2017 <- function(df) {
  # Categorize each P5_14 column into mode types
  df_categorized <- df %>%
    mutate(
      P5_14_01_cat = ifelse(!is.na(P5_14_01), "Other", NA_character_),
      P5_14_02_cat = ifelse(!is.na(P5_14_02), "Bus", NA_character_),
      P5_14_03_cat = ifelse(!is.na(P5_14_03), "Other", NA_character_),
      P5_14_04_cat = ifelse(!is.na(P5_14_04), "Other", NA_character_),
      P5_14_05_cat = ifelse(!is.na(P5_14_05), "Metro", NA_character_),
      P5_14_06_cat = ifelse(!is.na(P5_14_06), "Bus", NA_character_),
      P5_14_07_cat = ifelse(!is.na(P5_14_07), "Other", NA_character_),
      P5_14_08_cat = ifelse(!is.na(P5_14_08), "Bus", NA_character_),
      P5_14_09_cat = ifelse(!is.na(P5_14_09), "Other", NA_character_),
      P5_14_10_cat = ifelse(!is.na(P5_14_10), "Bus", NA_character_),
      P5_14_11_cat = ifelse(!is.na(P5_14_11), "BRT", NA_character_),
      P5_14_12_cat = ifelse(!is.na(P5_14_12), "Metro", NA_character_),
      P5_14_13_cat = ifelse(!is.na(P5_14_13), "Metro", NA_character_),
      P5_14_14_cat = ifelse(!is.na(P5_14_14), NA_character_, NA_character_),
      P5_14_15_cat = ifelse(!is.na(P5_14_15), "Metro", NA_character_),
      P5_14_16_cat = ifelse(!is.na(P5_14_16), "Other", NA_character_),
      P5_14_17_cat = ifelse(!is.na(P5_14_17), "Other", NA_character_),
      P5_14_18_cat = ifelse(!is.na(P5_14_18), "Bus", NA_character_),
      P5_14_19_cat = ifelse(!is.na(P5_14_19), "Other", NA_character_),
      P5_14_20_cat = ifelse(!is.na(P5_14_20), "Other", NA_character_)
    )
  
  # Get unique modes for each trip
  df_with_modes <- df_categorized %>%
    rowwise() %>%
    mutate(
      unique_modes = list(unique(na.omit(c_across(ends_with("_cat")))))
    ) %>%
    ungroup()
  
  # Classify trips into categories
  high_capacity_only <- df_with_modes %>%
    filter(
      lengths(unique_modes) >= 1 &
      sapply(unique_modes, function(x) all(x %in% c("Metro", "BRT")) & length(x) > 0)
    )
  
  bus_only <- df_with_modes %>%
    filter(
      lengths(unique_modes) == 1 &
      sapply(unique_modes, function(x) "Bus" %in% x)
    )
  
  high_capacity_bus <- df_with_modes %>%
    filter(
      lengths(unique_modes) >= 2 &
      sapply(unique_modes, function(x) {
        has_high_capacity <- any(x %in% c("Metro", "BRT"))
        has_bus <- "Bus" %in% x
        has_high_capacity && has_bus
      })
    )
  
  list(
    high_capacity_only = nrow(high_capacity_only),
    bus_only = nrow(bus_only),
    high_capacity_bus = nrow(high_capacity_bus),
    total = nrow(df)
  )
}

# Classify city to city trips
cat("\n=== City to City Trips (9 to 9) ===\n")
city_results <- classify_trips_2017(city_to_city)
cat("High-capacity only (Metro/BRT):", city_results$high_capacity_only, "\n")
cat("Bus only:", city_results$bus_only, "\n")
cat("High-capacity + Bus:", city_results$high_capacity_bus, "\n")
cat("Total trips:", city_results$total, "\n")

# Classify suburb to suburb trips
cat("\n=== Suburb to Suburb Trips (13/15 to 13/15) ===\n")
suburb_results <- classify_trips_2017(suburb_to_suburb)
cat("High-capacity only (Metro/BRT):", suburb_results$high_capacity_only, "\n")
cat("Bus only:", suburb_results$bus_only, "\n")
cat("High-capacity + Bus:", suburb_results$high_capacity_bus, "\n")
cat("Total trips:", suburb_results$total, "\n")
