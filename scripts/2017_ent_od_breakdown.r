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

#turns all 2 to NA
city_to_city[city_to_city == 2] <- NA
suburb_to_suburb[suburb_to_suburb == 2] <- NA

# mode 1
city_1<- city_to_city%>%
  filter(rowSums(!is.na(select(., starts_with("P5_14_"))))==1)
n_city_1 <- nrow(city_1)

summary_tbl_city_1 <- city_1 %>%
  summarise(across(starts_with("P5_14_"), ~ sum(!is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "n_valid") %>%
  arrange(desc(n_valid))
summary_tbl_city_1 <- summary_tbl_city_1 %>%
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
  ))%>%
  group_by(mode)%>%
  summarise(n=sum(n_valid))%>%
  arrange(desc(n))%>%
  mutate(percentage = n / n_city_1 * 100)

city_to_city_2<-  city_to_city%>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 2)
# get rid of walking
city_to_city2<- city_to_city_2 %>%
  mutate(P5_14_14 = ifelse(is.na(P5_14_14), NA, NA))
city_to_city_mode1_plus<- city_to_city2%>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 1)
summary_tbl_one_mode_plus <- city_to_city_mode1_plus %>%
  summarise(across(starts_with("P5_14_"), ~ sum(!is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "n_valid") %>%
  arrange(desc(n_valid))
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
  ))%>%
  group_by(mode)%>%
  summarise(n=sum(n_valid))%>%
  arrange(desc(n))%>%
  mutate(percentage = n / n_city_1 * 100)
summary_one<-rbind(summary_tbl_city_1, summary_tbl_one_mode_plus)%>%
  group_by(mode) %>%
  summarise(n = sum(n))%>%
  arrange(desc(n))

# Process multimodal trips for city_to_city
# actual two mode processing
city_two_mode <- city_to_city2 %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 2)

city_two_mode <- city_two_mode %>%
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

city_two_mode_combined <- city_two_mode %>%
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

city_mode2_combined <- city_two_mode_combined %>%
  group_by(P5_14_merged) %>%
  summarise(n = n(), .groups = 'drop')

# Three mode trips
city_three_mode <- city_to_city %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 3)

city_three_mode <- city_three_mode %>%
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

city_three_mode_combined <- city_three_mode %>%
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

city_mode3_combined <- city_three_mode_combined %>%
  group_by(P5_14_merged) %>%
  summarise(n = n(), .groups = 'drop')

# Four mode trips
city_four_mode <- city_to_city %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 4)

city_four_mode <- city_four_mode %>%
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

city_four_mode_combined <- city_four_mode %>%
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

city_mode4_combined <- city_four_mode_combined %>%
  group_by(P5_14_merged) %>%
  summarise(n = n(), .groups = 'drop')

# Five mode trips
city_five_mode <- city_to_city %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 5)

city_five_mode <- city_five_mode %>%
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

city_five_mode_combined <- city_five_mode %>%
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

city_mode5_combined <- city_five_mode_combined %>%
  group_by(P5_14_merged) %>%
  summarise(n = n(), .groups = 'drop')

# Six mode trips
city_six_mode <- city_to_city %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 6)

city_six_mode <- city_six_mode %>%
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

city_six_mode_combined <- city_six_mode %>%
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

city_mode6_combined <- city_six_mode_combined %>%
  group_by(P5_14_merged) %>%
  summarise(n = n(), .groups = 'drop')

# Combine all city multimodal trips
city_complete <- rbind(city_mode2_combined, city_mode3_combined, city_mode4_combined, 
                       city_mode5_combined, city_mode6_combined)
city_complete <- city_complete %>%
  group_by(P5_14_merged) %>%
  summarise(n = sum(n), .groups = 'drop')

city_complete<- city_complete %>%
  rename(mode=P5_14_merged)
# Final city summary
city_final <- rbind(summary_one, city_complete) %>%
  group_by(mode) %>%
  summarise(n = sum(n))

# write.csv(city_final, "data/city-city-suburb/2017_city_to_city_mode_breakdown.csv", row.names = FALSE)

# === SUBURB TO SUBURB ANALYSIS ===

# Single mode for suburbs
suburb_1 <- suburb_to_suburb %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14_")))) == 1)
n_suburb_1 <- nrow(suburb_1)

summary_tbl_suburb_1 <- suburb_1 %>%
  summarise(across(starts_with("P5_14_"), ~ sum(!is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "n_valid") %>%
  arrange(desc(n_valid))

summary_tbl_suburb_1 <- summary_tbl_suburb_1 %>%
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
  summarise(n = sum(n_valid)) %>%
  arrange(desc(n)) %>%
  mutate(percentage = n / n_suburb_1 * 100)

# Two mode suburbs with walking filter
suburb_to_suburb_2 <- suburb_to_suburb %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 2)

suburb_to_suburb2 <- suburb_to_suburb_2 %>%
  mutate(P5_14_14 = ifelse(is.na(P5_14_14), NA, NA))

suburb_to_suburb_mode1_plus <- suburb_to_suburb2 %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 1)

summary_tbl_suburb_one_mode_plus <- suburb_to_suburb_mode1_plus %>%
  summarise(across(starts_with("P5_14_"), ~ sum(!is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "n_valid") %>%
  arrange(desc(n_valid))

summary_tbl_suburb_one_mode_plus <- summary_tbl_suburb_one_mode_plus %>%
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
  summarise(n = sum(n_valid)) %>%
  arrange(desc(n)) %>%
  mutate(percentage = n / n_suburb_1 * 100)

suburb_summary_one <- rbind(summary_tbl_suburb_1, summary_tbl_suburb_one_mode_plus) %>%
  group_by(mode) %>%
  summarise(n = sum(n)) %>%
  arrange(desc(n))

# Process multimodal trips for suburb_to_suburb
suburb_two_mode <- suburb_to_suburb2 %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 2)

suburb_two_mode <- suburb_two_mode %>%
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

suburb_two_mode_combined <- suburb_two_mode %>%
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

suburb_mode2_combined <- suburb_two_mode_combined %>%
  group_by(P5_14_merged) %>%
  summarise(n = n(), .groups = 'drop')

# Three mode trips
suburb_three_mode <- suburb_to_suburb %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 3)

suburb_three_mode <- suburb_three_mode %>%
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

suburb_three_mode_combined <- suburb_three_mode %>%
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

suburb_mode3_combined <- suburb_three_mode_combined %>%
  group_by(P5_14_merged) %>%
  summarise(n = n(), .groups = 'drop')

# Four mode trips
suburb_four_mode <- suburb_to_suburb %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 4)

suburb_four_mode <- suburb_four_mode %>%
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

suburb_four_mode_combined <- suburb_four_mode %>%
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

suburb_mode4_combined <- suburb_four_mode_combined %>%
  group_by(P5_14_merged) %>%
  summarise(n = n(), .groups = 'drop')

# Five mode trips
suburb_five_mode <- suburb_to_suburb %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 5)

suburb_five_mode <- suburb_five_mode %>%
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

suburb_five_mode_combined <- suburb_five_mode %>%
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

suburb_mode5_combined <- suburb_five_mode_combined %>%
  group_by(P5_14_merged) %>%
  summarise(n = n(), .groups = 'drop')

# Six mode trips
suburb_six_mode <- suburb_to_suburb %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 6)

suburb_six_mode <- suburb_six_mode %>%
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

suburb_six_mode_combined <- suburb_six_mode %>%
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

suburb_mode6_combined <- suburb_six_mode_combined %>%
  group_by(P5_14_merged) %>%
  summarise(n = n(), .groups = 'drop')

# Combine all suburb multimodal trips
suburb_complete <- rbind(suburb_mode2_combined, suburb_mode3_combined, suburb_mode4_combined, 
                         suburb_mode5_combined, suburb_mode6_combined)
suburb_complete <- suburb_complete %>%
  group_by(P5_14_merged) %>%
  summarise(n = sum(n), .groups = 'drop')


suburb_complete<- suburb_complete %>%
  rename(mode = P5_14_merged)
suburb_final<-rbind(suburb_summary_one, suburb_complete) %>%
  group_by(mode) %>%
  summarise(n = sum(n))
# write.csv(suburb_final, file = "data/city-city-suburb/suburb_mode_summary_2017.csv", row.names = FALSE)
