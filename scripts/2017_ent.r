library(foreign)
library(tidyverse)

trip_2017<- read.csv("data/2017/trip_2017.csv")
hogar_2017<- read.dbf("data/2017/THOGAR.DBF", as.is = TRUE)
dem_2017<- read.dbf("data/2017/TSDEM.DBF", as.is = TRUE)

hogar_2017 <- hogar_2017 %>%
  select(ID_HOG, ENT)
dem_2017 <- dem_2017 %>%
  select(ID_SOC, ID_HOG)
dem_hogar_2017 <- dem_2017 %>%
  left_join(hogar_2017, by = "ID_HOG")

complete_trip_2017 <- trip_2017 %>%
  left_join(dem_hogar_2017, by ="ID_SOC")

# Filter for Mexico City trips (ENT == "09") - core Mexico City area
mexico_city_trips_2017 <- complete_trip_2017 %>%
  filter(ENT == "09")%>%
  filter(P5_3==1)%>%
  select(starts_with("P5_14"), FACTOR)

mexico_city_trips_2017[mexico_city_trips_2017 == 2] <- NA

# === SINGLE MODE ANALYSIS ===
one_mode_mex <- mexico_city_trips_2017 %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 1)

n_total_mex <- nrow(one_mode_mex)

summary_tbl_mex <- one_mode_mex %>%
  summarise(across(starts_with("P5_14_"), ~ sum(!is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "n_valid") %>%
  arrange(desc(n_valid))

# === TWO MODE ANALYSIS ===
two_mode_mex <- mexico_city_trips_2017 %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 2)

# Get rid of walking
two_mode_mex <- two_mode_mex %>%
  mutate(P5_14_14 = ifelse(is.na(P5_14_14), NA, NA))

two_one_combined_walking_mex <- two_mode_mex %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 1)

summary_tbl_one_mode_plus_mex <- two_one_combined_walking_mex %>%
  summarise(across(starts_with("P5_14_"), ~ sum(!is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "n_valid") %>%
  arrange(desc(n_valid))

summary_one_mex <- rbind(summary_tbl_mex, summary_tbl_one_mode_plus_mex) %>%
  group_by(column) %>%
  summarise(n_valid = sum(n_valid))
#sum(summary_one_mex$n_valid)
# Actual two mode processing
two_mode_mex <- two_mode_mex %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 2)

two_mode_mex <- two_mode_mex %>%
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

# Build merged label per row
two_mode_combined_mex <- two_mode_mex %>%
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

mode2_combined_mex <- two_mode_combined_mex %>%
  group_by(P5_14_merged) %>%
  summarise(n = n(), .groups = 'drop')

# === THREE MODE ANALYSIS ===
three_mode_mex <- mexico_city_trips_2017 %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 3)

three_mode_mex <- three_mode_mex %>%
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

three_mode_combined_mex <- three_mode_mex %>%
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

mode3_combined_mex <- three_mode_combined_mex %>%
  group_by(P5_14_merged) %>%
  summarise(n = n(), .groups = 'drop')

# === FOUR MODE ANALYSIS ===
four_mode_mex <- mexico_city_trips_2017 %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 4)

four_mode_mex <- four_mode_mex %>%
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

four_mode_combined_mex <- four_mode_mex %>%
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

mode4_combined_mex <- four_mode_combined_mex %>%
  group_by(P5_14_merged) %>%
  summarise(n = n(), .groups = 'drop')

# === FIVE MODE ANALYSIS ===
five_mode_mex <- mexico_city_trips_2017 %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 5)

five_mode_mex <- five_mode_mex %>%
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

five_mode_combined_mex <- five_mode_mex %>%
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

mode5_combined_mex <- five_mode_combined_mex %>%
  group_by(P5_14_merged) %>%
  summarise(n = n(), .groups = 'drop')

# === SIX MODE ANALYSIS ===
six_mode_mex <- mexico_city_trips_2017 %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 6)

six_mode_mex <- six_mode_mex %>%
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

six_mode_combined_mex <- six_mode_mex %>%
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

mode6_combined_mex <- six_mode_combined_mex %>%
  group_by(P5_14_merged) %>%
  summarise(n = n(), .groups = 'drop')

# === COMBINE ALL MODES ===
complete_mex <- rbind(mode2_combined_mex, mode3_combined_mex, mode4_combined_mex, mode5_combined_mex, mode6_combined_mex)
complete_mex <- complete_mex %>%
  group_by(P5_14_merged) %>%
  summarise(n = sum(n), .groups = 'drop')

# Save Mexico City multimodal results
# write.csv(complete_mex, "data/2017/mode_combination_mexico_city_2017.csv", row.names = FALSE)

sum(complete_mex$n)+sum(summary_one_mex$n_valid)



#########
# === WEIGHTED ANALYSIS FOR MEXICO CITY ===
# Weighted two mode
mode2_weighted_mex <- two_mode_combined_mex %>%
  group_by(P5_14_merged) %>%
  summarise(weighted_n = sum(FACTOR), .groups = 'drop')

# Weighted three mode
mode3_weighted_mex <- three_mode_combined_mex %>%
  group_by(P5_14_merged) %>%
  summarise(weighted_n = sum(FACTOR), .groups = 'drop')

# Weighted four mode
mode4_weighted_mex <- four_mode_combined_mex %>%
  group_by(P5_14_merged) %>%
  summarise(weighted_n = sum(FACTOR), .groups = 'drop')

# Weighted five mode
mode5_weighted_mex <- five_mode_combined_mex %>%
  group_by(P5_14_merged) %>%
  summarise(weighted_n = sum(FACTOR), .groups = 'drop')

# Weighted six mode
mode6_weighted_mex <- six_mode_combined_mex %>%
  group_by(P5_14_merged) %>%
  summarise(weighted_n = sum(FACTOR), .groups = 'drop')

# Combine all weighted results
complete_weighted_mex <- rbind(mode2_weighted_mex, mode3_weighted_mex, mode4_weighted_mex, mode5_weighted_mex, mode6_weighted_mex)
complete_weighted_mex <- complete_weighted_mex %>%
  group_by(P5_14_merged) %>%
  summarise(weighted_n = sum(weighted_n), .groups = 'drop')

# Save weighted results for Mexico City
#write.csv(complete_weighted_mex, "data/2017/mode_combination_weighted_mexico_city_2017.csv", row.names = FALSE)

# === SINGLE MODE ANALYSIS FOR MEXICO CITY ===
# Process single mode trips with mode classification
single_mode_classified_mex <- one_mode_mex %>%
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
         P5_14_14 = ifelse(is.na(P5_14_14), NA, "Walking"),
         P5_14_15 = ifelse(is.na(P5_14_15), NA, "Metro"),
         P5_14_16 = ifelse(is.na(P5_14_16), NA, "Other"),
         P5_14_17 = ifelse(is.na(P5_14_17), NA, "Other"),
         P5_14_18 = ifelse(is.na(P5_14_18), NA, "Bus"),
         P5_14_19 = ifelse(is.na(P5_14_19), NA, "Other"),
         P5_14_20 = ifelse(is.na(P5_14_20), NA, "Other"))

single_mode_combined_mex <- single_mode_classified_mex %>%
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

single_mode_summary_mex <- single_mode_combined_mex %>%
  group_by(P5_14_merged) %>%
  summarise(n = n(), .groups = 'drop')

# Weighted single mode
single_mode_weighted_mex <- single_mode_combined_mex %>%
  group_by(P5_14_merged) %>%
  summarise(weighted_n = sum(FACTOR), .groups = 'drop')

# Save single mode results
#write.csv(single_mode_summary_mex, "data/2017/single_mode_mexico_city_2017.csv", row.names = FALSE)
#write.csv(single_mode_weighted_mex, "data/2017/single_mode_weighted_mexico_city_2017.csv", row.names = FALSE)

# === MODE SHARE CALCULATION ===
# Calculate mode shares for Mexico City
total_trips_mex <- sum(single_mode_summary_mex$n) + sum(complete_mex$n)
total_weighted_trips_mex <- sum(single_mode_weighted_mex$weighted_n) + sum(complete_weighted_mex$weighted_n)

# Create comprehensive mode share summary
all_modes_mex <- bind_rows(
  single_mode_summary_mex %>% mutate(type = "Single Mode"),
  complete_mex %>% mutate(type = "Multi Mode")
)

all_modes_weighted_mex <- bind_rows(
  single_mode_weighted_mex %>% rename(n = weighted_n) %>% mutate(type = "Single Mode"),
  complete_weighted_mex %>% rename(n = weighted_n) %>% mutate(type = "Multi Mode")
)

# Calculate mode shares
mode_share_mex <- all_modes_mex %>%
  mutate(share = n / total_trips_mex * 100) %>%
  arrange(desc(n))

mode_share_weighted_mex <- all_modes_weighted_mex %>%
  mutate(share = n / total_weighted_trips_mex * 100) %>%
  arrange(desc(n))

# Save mode share results
#write.csv(mode_share_mex, "data/2017/mode_share_mexico_city_2017.csv", row.names = FALSE)
#write.csv(mode_share_weighted_mex, "data/2017/mode_share_weighted_mexico_city_2017.csv", row.names = FALSE)
