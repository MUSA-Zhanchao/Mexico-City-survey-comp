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
sum(complete_suburb_city$FACTOR)
#turns all 2 to NA
complete_suburb_city[complete_suburb_city == 2] <- NA

# mode 1
suburb_city_1 <- complete_suburb_city %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14_")))) == 1)
n_suburb_city_1 <- sum(suburb_city_1$FACTOR)

summary_tbl_suburb_city_1 <- suburb_city_1 %>%
  summarise(across(starts_with("P5_14_"), ~ sum(FACTOR[!is.na(.)]))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "weighted_n") %>%
  arrange(desc(weighted_n))
summary_tbl_suburb_city_1 <- summary_tbl_suburb_city_1 %>%
  mutate(mode = case_when(
    column == "P5_14_01" ~ "Drive",
    column == "P5_14_02" ~ "Bus",
    column == "P5_14_03" ~ "Taxi",
    column == "P5_14_04" ~ "Taxi",
    column == "P5_14_05" ~ "Metro",
    column == "P5_14_06" ~ "Bus",
    column == "P5_14_07" ~ "Bicycle",
    column == "P5_14_08" ~ "Bus",
    column == "P5_14_09" ~ "Moto",
    column == "P5_14_10" ~ "Bus",
    column == "P5_14_11" ~ "BRT",
    column == "P5_14_12" ~ "Metro",
    column == "P5_14_13" ~ "Metro",
    column == "P5_14_14" ~ "Walk",
    column == "P5_14_15" ~ "Metro",
    column == "P5_14_16" ~ "Taxi",
    column == "P5_14_17" ~ "Taxi",
    column == "P5_14_18" ~ "Bus",
    column == "P5_14_19" ~ "Other",
    column == "P5_14_20" ~ "Other",
    TRUE ~ "Unknown"
  )) %>%
  group_by(mode) %>%
  summarise(weighted_n = sum(weighted_n)) %>%
  arrange(desc(weighted_n)) %>%
  mutate(percentage = weighted_n / n_suburb_city_1 * 100)

complete_suburb_city_2 <- complete_suburb_city %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 2)
# get rid of walking
complete_suburb_city2 <- complete_suburb_city_2 %>%
  mutate(P5_14_14 = ifelse(is.na(P5_14_14), NA, NA))
complete_suburb_city_mode1_plus <- complete_suburb_city2 %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 1)
summary_tbl_one_mode_plus <- complete_suburb_city_mode1_plus %>%
  summarise(across(starts_with("P5_14_"), ~ sum(FACTOR[!is.na(.)]))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "weighted_n") %>%
  arrange(desc(weighted_n))
summary_tbl_one_mode_plus <- summary_tbl_one_mode_plus %>%
  mutate(mode = case_when(
    column == "P5_14_01" ~ "Drive",
    column == "P5_14_02" ~ "Bus",
    column == "P5_14_03" ~ "Taxi",
    column == "P5_14_04" ~ "Taxi",
    column == "P5_14_05" ~ "Metro",
    column == "P5_14_06" ~ "Bus",
    column == "P5_14_07" ~ "Bicycle",
    column == "P5_14_08" ~ "Bus",
    column == "P5_14_09" ~ "Moto",
    column == "P5_14_10" ~ "Bus",
    column == "P5_14_11" ~ "BRT",
    column == "P5_14_12" ~ "Metro",
    column == "P5_14_13" ~ "Metro",
    column == "P5_14_14" ~ "Walk",
    column == "P5_14_15" ~ "Metro",
    column == "P5_14_16" ~ "Taxi",
    column == "P5_14_17" ~ "Taxi",
    column == "P5_14_18" ~ "Bus",
    column == "P5_14_19" ~ "Other",
    column == "P5_14_20" ~ "Other",
    TRUE ~ "Unknown"
  )) %>%
  group_by(mode) %>%
  summarise(weighted_n = sum(weighted_n)) %>%
  arrange(desc(weighted_n)) %>%
  mutate(percentage = weighted_n / n_suburb_city_1 * 100)
summary_one <- rbind(summary_tbl_suburb_city_1, summary_tbl_one_mode_plus) %>%
  group_by(mode) %>%
  summarise(weighted_n = sum(weighted_n)) %>%
  arrange(desc(weighted_n))
# write.csv(summary_one, "data/city-city-suburb/2017_suburb_city_single_mode_summary_weighted.csv", row.names = FALSE)

# Process multimodal trips for complete_suburb_city
# actual two mode processing
suburb_city_two_mode <- complete_suburb_city2 %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 2)

suburb_city_two_mode <- suburb_city_two_mode %>%
  mutate(P5_14_01 = ifelse(is.na(P5_14_01), NA, "Other"),
         P5_14_02 = ifelse(is.na(P5_14_02), NA, "Bus"),
         P5_14_03 = ifelse(is.na(P5_14_03), NA, "Other"),
         P5_14_04 = ifelse(is.na(P5_14_04), NA, "Other"),
         P5_14_05 = ifelse(is.na(P5_14_05), NA, "Metro"),
         P5_14_06 = ifelse(is.na(P5_14_06), NA, "Bus"),
         P5_14_07 = ifelse(is.na(P5_14_07), NA, "Other"),
         P5_14_08 = ifelse(is.na(P5_14_08), NA, "Bus"),
         P5_14_09 = ifelse(is.na(P5_14_09), NA, "Other"),
         P5_14_10 = ifelse(is.na(P5_14_10), NA, "Bus"),
         P5_14_11 = ifelse(is.na(P5_14_11), NA, "BRT"),
         P5_14_12 = ifelse(is.na(P5_14_12), NA, "Metro"),
         P5_14_13 = ifelse(is.na(P5_14_13), NA, "Metro"),
         P5_14_14 = ifelse(is.na(P5_14_14), NA, NA),
         P5_14_15 = ifelse(is.na(P5_14_15), NA, "Metro"),
         P5_14_16 = ifelse(is.na(P5_14_16), NA, "Other"),
         P5_14_17 = ifelse(is.na(P5_14_17), NA, "Other"),
         P5_14_18 = ifelse(is.na(P5_14_18), NA, "Bus"),
         P5_14_19 = ifelse(is.na(P5_14_19), NA, "Other"),
         P5_14_20 = ifelse(is.na(P5_14_20), NA, "Other"))

suburb_city_two_mode_combined <- suburb_city_two_mode %>%
  rowwise() %>%
  mutate(
    P5_14_merged = {
      x <- c_across(starts_with("P5_14_"))
      x <- unique(na.omit(trimws(x)))
      if (length(x) == 0) NA_character_
      else paste(sort(x), collapse = "_")
    }
  ) %>%
  ungroup()

suburb_city_mode2_combined <- suburb_city_two_mode_combined %>%
  group_by(P5_14_merged) %>%
  summarise(weighted_n = sum(FACTOR), .groups = 'drop')

# Three mode trips
suburb_city_three_mode <- complete_suburb_city %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 3)

suburb_city_three_mode <- suburb_city_three_mode %>%
  mutate(P5_14_01 = ifelse(is.na(P5_14_01), NA, "Other"),
         P5_14_02 = ifelse(is.na(P5_14_02), NA, "Bus"),
         P5_14_03 = ifelse(is.na(P5_14_03), NA, "Other"),
         P5_14_04 = ifelse(is.na(P5_14_04), NA, "Other"),
         P5_14_05 = ifelse(is.na(P5_14_05), NA, "Metro"),
         P5_14_06 = ifelse(is.na(P5_14_06), NA, "Bus"),
         P5_14_07 = ifelse(is.na(P5_14_07), NA, "Other"),
         P5_14_08 = ifelse(is.na(P5_14_08), NA, "Bus"),
         P5_14_09 = ifelse(is.na(P5_14_09), NA, "Other"),
         P5_14_10 = ifelse(is.na(P5_14_10), NA, "Bus"),
         P5_14_11 = ifelse(is.na(P5_14_11), NA, "BRT"),
         P5_14_12 = ifelse(is.na(P5_14_12), NA, "Metro"),
         P5_14_13 = ifelse(is.na(P5_14_13), NA, "Metro"),
         P5_14_14 = ifelse(is.na(P5_14_14), NA, NA),
         P5_14_15 = ifelse(is.na(P5_14_15), NA, "Metro"),
         P5_14_16 = ifelse(is.na(P5_14_16), NA, "Other"),
         P5_14_17 = ifelse(is.na(P5_14_17), NA, "Other"),
         P5_14_18 = ifelse(is.na(P5_14_18), NA, "Bus"),
         P5_14_19 = ifelse(is.na(P5_14_19), NA, "Other"),
         P5_14_20 = ifelse(is.na(P5_14_20), NA, "Other"))

suburb_city_three_mode_combined <- suburb_city_three_mode %>%
  rowwise() %>%
  mutate(
    P5_14_merged = {
      x <- c_across(starts_with("P5_14_"))
      x <- unique(na.omit(trimws(x)))
      if (length(x) == 0) NA_character_
      else paste(sort(x), collapse = "_")
    }
  ) %>%
  ungroup()

suburb_city_mode3_combined <- suburb_city_three_mode_combined %>%
  group_by(P5_14_merged) %>%
  summarise(weighted_n = sum(FACTOR), .groups = 'drop')

# Four mode trips
suburb_city_four_mode <- complete_suburb_city %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 4)

suburb_city_four_mode <- suburb_city_four_mode %>%
  mutate(P5_14_01 = ifelse(is.na(P5_14_01), NA, "Other"),
         P5_14_02 = ifelse(is.na(P5_14_02), NA, "Bus"),
         P5_14_03 = ifelse(is.na(P5_14_03), NA, "Other"),
         P5_14_04 = ifelse(is.na(P5_14_04), NA, "Other"),
         P5_14_05 = ifelse(is.na(P5_14_05), NA, "Metro"),
         P5_14_06 = ifelse(is.na(P5_14_06), NA, "Bus"),
         P5_14_07 = ifelse(is.na(P5_14_07), NA, "Other"),
         P5_14_08 = ifelse(is.na(P5_14_08), NA, "Bus"),
         P5_14_09 = ifelse(is.na(P5_14_09), NA, "Other"),
         P5_14_10 = ifelse(is.na(P5_14_10), NA, "Bus"),
         P5_14_11 = ifelse(is.na(P5_14_11), NA, "BRT"),
         P5_14_12 = ifelse(is.na(P5_14_12), NA, "Metro"),
         P5_14_13 = ifelse(is.na(P5_14_13), NA, "Metro"),
         P5_14_14 = ifelse(is.na(P5_14_14), NA, NA),
         P5_14_15 = ifelse(is.na(P5_14_15), NA, "Metro"),
         P5_14_16 = ifelse(is.na(P5_14_16), NA, "Other"),
         P5_14_17 = ifelse(is.na(P5_14_17), NA, "Other"),
         P5_14_18 = ifelse(is.na(P5_14_18), NA, "Bus"),
         P5_14_19 = ifelse(is.na(P5_14_19), NA, "Other"),
         P5_14_20 = ifelse(is.na(P5_14_20), NA, "Other"))

suburb_city_four_mode_combined <- suburb_city_four_mode %>%
  rowwise() %>%
  mutate(
    P5_14_merged = {
      x <- c_across(starts_with("P5_14_"))
      x <- unique(na.omit(trimws(x)))
      if (length(x) == 0) NA_character_
      else paste(sort(x), collapse = "_")
    }
  ) %>%
  ungroup()

suburb_city_mode4_combined <- suburb_city_four_mode_combined %>%
  group_by(P5_14_merged) %>%
  summarise(weighted_n = sum(FACTOR), .groups = 'drop')

# Five mode trips
suburb_city_five_mode <- complete_suburb_city %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 5)

suburb_city_five_mode <- suburb_city_five_mode %>%
  mutate(P5_14_01 = ifelse(is.na(P5_14_01), NA, "Other"),
         P5_14_02 = ifelse(is.na(P5_14_02), NA, "Bus"),
         P5_14_03 = ifelse(is.na(P5_14_03), NA, "Other"),
         P5_14_04 = ifelse(is.na(P5_14_04), NA, "Other"),
         P5_14_05 = ifelse(is.na(P5_14_05), NA, "Metro"),
         P5_14_06 = ifelse(is.na(P5_14_06), NA, "Bus"),
         P5_14_07 = ifelse(is.na(P5_14_07), NA, "Other"),
         P5_14_08 = ifelse(is.na(P5_14_08), NA, "Bus"),
         P5_14_09 = ifelse(is.na(P5_14_09), NA, "Other"),
         P5_14_10 = ifelse(is.na(P5_14_10), NA, "Bus"),
         P5_14_11 = ifelse(is.na(P5_14_11), NA, "BRT"),
         P5_14_12 = ifelse(is.na(P5_14_12), NA, "Metro"),
         P5_14_13 = ifelse(is.na(P5_14_13), NA, "Metro"),
         P5_14_14 = ifelse(is.na(P5_14_14), NA, NA),
         P5_14_15 = ifelse(is.na(P5_14_15), NA, "Metro"),
         P5_14_16 = ifelse(is.na(P5_14_16), NA, "Other"),
         P5_14_17 = ifelse(is.na(P5_14_17), NA, "Other"),
         P5_14_18 = ifelse(is.na(P5_14_18), NA, "Bus"),
         P5_14_19 = ifelse(is.na(P5_14_19), NA, "Other"),
         P5_14_20 = ifelse(is.na(P5_14_20), NA, "Other"))

suburb_city_five_mode_combined <- suburb_city_five_mode %>%
  rowwise() %>%
  mutate(
    P5_14_merged = {
      x <- c_across(starts_with("P5_14_"))
      x <- unique(na.omit(trimws(x)))
      if (length(x) == 0) NA_character_
      else paste(sort(x), collapse = "_")
    }
  ) %>%
  ungroup()

suburb_city_mode5_combined <- suburb_city_five_mode_combined %>%
  group_by(P5_14_merged) %>%
  summarise(weighted_n = sum(FACTOR), .groups = 'drop')

# Six mode trips
suburb_city_six_mode <- complete_suburb_city %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 6)

suburb_city_six_mode <- suburb_city_six_mode %>%
  mutate(P5_14_01 = ifelse(is.na(P5_14_01), NA, "Other"),
         P5_14_02 = ifelse(is.na(P5_14_02), NA, "Bus"),
         P5_14_03 = ifelse(is.na(P5_14_03), NA, "Other"),
         P5_14_04 = ifelse(is.na(P5_14_04), NA, "Other"),
         P5_14_05 = ifelse(is.na(P5_14_05), NA, "Metro"),
         P5_14_06 = ifelse(is.na(P5_14_06), NA, "Bus"),
         P5_14_07 = ifelse(is.na(P5_14_07), NA, "Other"),
         P5_14_08 = ifelse(is.na(P5_14_08), NA, "Bus"),
         P5_14_09 = ifelse(is.na(P5_14_09), NA, "Other"),
         P5_14_10 = ifelse(is.na(P5_14_10), NA, "Bus"),
         P5_14_11 = ifelse(is.na(P5_14_11), NA, "BRT"),
         P5_14_12 = ifelse(is.na(P5_14_12), NA, "Metro"),
         P5_14_13 = ifelse(is.na(P5_14_13), NA, "Metro"),
         P5_14_14 = ifelse(is.na(P5_14_14), NA, NA),
         P5_14_15 = ifelse(is.na(P5_14_15), NA, "Metro"),
         P5_14_16 = ifelse(is.na(P5_14_16), NA, "Other"),
         P5_14_17 = ifelse(is.na(P5_14_17), NA, "Other"),
         P5_14_18 = ifelse(is.na(P5_14_18), NA, "Bus"),
         P5_14_19 = ifelse(is.na(P5_14_19), NA, "Other"),
         P5_14_20 = ifelse(is.na(P5_14_20), NA, "Other"))

suburb_city_six_mode_combined <- suburb_city_six_mode %>%
  rowwise() %>%
  mutate(
    P5_14_merged = {
      x <- c_across(starts_with("P5_14_"))
      x <- unique(na.omit(trimws(x)))
      if (length(x) == 0) NA_character_
      else paste(sort(x), collapse = "_")
    }
  ) %>%
  ungroup()

suburb_city_mode6_combined <- suburb_city_six_mode_combined %>%
  group_by(P5_14_merged) %>%
  summarise(weighted_n = sum(FACTOR), .groups = 'drop')

# Combine all suburb_city multimodal trips
suburb_city_complete <- rbind(suburb_city_mode2_combined, suburb_city_mode3_combined, suburb_city_mode4_combined,
                              suburb_city_mode5_combined, suburb_city_mode6_combined)
suburb_city_complete <- suburb_city_complete %>%
  group_by(P5_14_merged) %>%
  summarise(weighted_n = sum(weighted_n), .groups = 'drop')

suburb_city_complete <- suburb_city_complete %>%
  rename(mode = P5_14_merged)
# Final suburb_city summary
suburb_city_final <- rbind(summary_one, suburb_city_complete) %>%
  group_by(mode) %>%
  summarise(weighted_n = sum(weighted_n))

# write.csv(suburb_city_final, "data/city-city-suburb/2017_suburb_city_mode_breakdown_weighted.csv", row.names = FALSE)
