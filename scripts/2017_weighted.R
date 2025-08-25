library(foreign)
library(tidyverse)

# Load 2017 trip data with weighting factor
trip_2017<- read.csv("data/2017/trip_2017.csv")

# Select P5_14 columns (transportation modes) and keep FACTOR column for weighting
trip_14 <- trip_2017 %>%
  select(starts_with("P5_14"), FACTOR)

# Replace 2 values with NA (as in original script)
trip_14[trip_14 == 2] <- NA

# One mode trips - weighted analysis
one_mode <- trip_14 %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 1)

n_total <- nrow(one_mode)
weighted_total <- sum(one_mode$FACTOR)

# Summary table for one mode - weighted
summary_tbl <- one_mode %>%
  summarise(across(starts_with("P5_14_"), ~ sum(FACTOR[!is.na(.)]))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "weighted_n") %>%
  arrange(desc(weighted_n))

# Two mode trips processing
two_mode<- trip_14 %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 2)

# get rid of walking (set P5_14_14 to NA)
two_mode<- two_mode %>%
  mutate(P5_14_14 = ifelse(is.na(P5_14_14), NA, NA))

# Process trips that became single mode after removing walking
two_one_combined_walking<- two_mode%>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 1)
summary_tbl_one_mode_plus <- two_one_combined_walking %>%
  summarise(across(starts_with("P5_14_"), ~ sum(FACTOR[!is.na(.)]))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "weighted_n") %>%
  arrange(desc(weighted_n))
summary_one<-rbind(summary_tbl, summary_tbl_one_mode_plus)%>%
  group_by(column) %>%
  summarise(weighted_n = sum(weighted_n))

# actual two mode processing - weighted
two_mode<-two_mode%>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 2)
two_mode<- two_mode%>%
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

# Build merged label per row: distinct, non-NA, joined with "_"
two_mode_combined <- two_mode %>%
  rowwise() %>%
  mutate(
    P5_14_merged = {
      x <- c_across(starts_with("P5_14_"))
      x <- unique(na.omit(trimws(x)))    # drop NA, trim, remove duplicates
      if (length(x) == 0) NA_character_  # all NA row stays NA
      else paste(sort(x), collapse = "_")# e.g., "Bus_Metro_Other"
    }
  ) %>%
  ungroup()

# Weighted summary for two mode
mode2_combined<- two_mode_combined %>%
  group_by(P5_14_merged) %>%
  summarise(weighted_n = sum(FACTOR), .groups = 'drop')

# Three mode trips - weighted
three_mode <- trip_14 %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 3)

# 3 mode trip doesn't involved pre-processing, as even though walk involved. Samples still at least utilized two transportation modes
three_mode<- three_mode%>%
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

three_mode_combined <- three_mode %>%
  rowwise() %>%
  mutate(
    P5_14_merged = {
      x <- c_across(starts_with("P5_14_"))
      x <- unique(na.omit(trimws(x)))    # drop NA, trim, remove duplicates
      if (length(x) == 0) NA_character_  # all NA row stays NA
      else paste(sort(x), collapse = "_")# e.g., "Bus_Metro_Other"
    }
  ) %>%
  ungroup()

mode3_combined<- three_mode_combined %>%
  group_by(P5_14_merged) %>%
  summarise(weighted_n = sum(FACTOR), .groups = 'drop')

# Four mode trips - weighted
four_mode <- trip_14 %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 4)
four_mode<-four_mode%>%
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

four_mode_combined <- four_mode %>%
  rowwise() %>%
  mutate(
    P5_14_merged = {
      x <- c_across(starts_with("P5_14_"))
      x <- unique(na.omit(trimws(x)))    # drop NA, trim, remove duplicates
      if (length(x) == 0) NA_character_  # all NA row stays NA
      else paste(sort(x), collapse = "_")# e.g., "Bus_Metro_Other"
    }
  ) %>%
  ungroup()
mode4_combined<- four_mode_combined %>%
  group_by(P5_14_merged) %>%
  summarise(weighted_n = sum(FACTOR), .groups = 'drop')

# Five mode trips - weighted
five_mode <- trip_14 %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 5)
five_mode<-five_mode%>%
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

five_mode_combined <- five_mode %>%
  rowwise() %>%
  mutate(
    P5_14_merged = {
      x <- c_across(starts_with("P5_14_"))
      x <- unique(na.omit(trimws(x)))    # drop NA, trim, remove duplicates
      if (length(x) == 0) NA_character_  # all NA row stays NA
      else paste(sort(x), collapse = "_")# e.g., "Bus_Metro_Other"
    }
  ) %>%
  ungroup()
mode5_combined<- five_mode_combined %>%
  group_by(P5_14_merged) %>%
  summarise(weighted_n = sum(FACTOR), .groups = 'drop')

# Six mode trips - weighted
six_mode <- trip_14 %>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 6)
six_mode<-six_mode%>%
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
six_mode_combined <- six_mode %>%
  rowwise() %>%
  mutate(
    P5_14_merged = {
      x <- c_across(starts_with("P5_14_"))
      x <- unique(na.omit(trimws(x)))    # drop NA, trim, remove duplicates
      if (length(x) == 0) NA_character_  # all NA row stays NA
      else paste(sort(x), collapse = "_")# e.g., "Bus_Metro_Other"
    }
  ) %>%
  ungroup()
mode6_combined<- six_mode_combined %>%
  group_by(P5_14_merged) %>%
  summarise(weighted_n = sum(FACTOR), .groups = 'drop')

# Combine all weighted results
complete_weighted<-rbind(mode2_combined, mode3_combined, mode4_combined, mode5_combined, mode6_combined)
complete_weighted<- complete_weighted %>%
  group_by(P5_14_merged) %>%
  summarise(weighted_n = sum(weighted_n), .groups = 'drop')

# Write weighted results
write.csv(complete_weighted, "data/2017/mode_combination_weighted_2017.csv", row.names = FALSE)

# Print comparison of totals
cat("Total weighted trips:", sum(complete_weighted$weighted_n), "\n")
cat("Original unweighted total from existing file:", sum(read.csv("data/2017/mode_combinationw2mod_more_2017.csv")$n), "\n")
