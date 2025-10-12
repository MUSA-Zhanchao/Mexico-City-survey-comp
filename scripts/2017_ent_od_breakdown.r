library(foreign)
library(tidyverse)

# Load 2017 data
trip_2017 <- read.csv("data/2017/trip_2017.csv")
hogar_2017 <- read.dbf("data/2017/THOGAR.DBF", as.is = TRUE)
dem_2017 <- read.dbf("data/2017/TSDEM.DBF", as.is = TRUE)

# Join household and demographic data
hogar_2017 <- hogar_2017 %>%
  select(ID_HOG, ENT)
dem_2017 <- dem_2017 %>%
  select(ID_SOC, ID_HOG)
dem_hogar_2017 <- dem_2017 %>%
  left_join(hogar_2017, by = "ID_HOG")

# Join trip data with household ENT (origin ENT)
complete_trip_2017 <- trip_2017 %>%
  left_join(dem_hogar_2017, by = "ID_SOC") %>%
  rename(origin_ent = ENT) %>%
  mutate(
    dest_ent = sprintf("%02d", as.numeric(P5_12_7))  # destination ENT code with leading zero
  )

# Filter for valid trips only (P5_3 == 1)
complete_trip_2017 <- complete_trip_2017 %>%
  filter(P5_3 == 1)

# City to city trips (origin 9 and destination 9)
city_to_city <- complete_trip_2017 %>%
  filter(origin_ent == "09" & dest_ent == "09") %>%
  select(starts_with("P5_14"), FACTOR)

# Suburb to suburb trips (origin 15 or 13 and destination 15 or 13)
suburb_to_suburb <- complete_trip_2017 %>%
  filter(origin_ent %in% c("13", "15") & dest_ent %in% c("13", "15")) %>%
  select(starts_with("P5_14"), FACTOR)

# Function to process mode data
process_mode_data <- function(trip_data, category_name) {
  # Replace 2 with NA (2 typically means "No" in the survey)
  trip_data[trip_data == 2] <- NA
  
  # === SINGLE MODE ANALYSIS ===
  one_mode <- trip_data %>%
    filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 1)
  
  # Classify single modes
  single_mode_classified <- one_mode %>%
    mutate(
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
      P5_14_11 = ifelse(is.na(P5_14_11), NA, "BRT"),
      P5_14_12 = ifelse(is.na(P5_14_12), NA, "Metro"),
      P5_14_13 = ifelse(is.na(P5_14_13), NA, "Metro"),
      P5_14_14 = ifelse(is.na(P5_14_14), NA, "Walking"),
      P5_14_15 = ifelse(is.na(P5_14_15), NA, "Metro"),
      P5_14_16 = ifelse(is.na(P5_14_16), NA, "Other"),
      P5_14_17 = ifelse(is.na(P5_14_17), NA, "Other"),
      P5_14_18 = ifelse(is.na(P5_14_18), NA, "Bus"),
      P5_14_19 = ifelse(is.na(P5_14_19), NA, "Other"),
      P5_14_20 = ifelse(is.na(P5_14_20), NA, "Other")
    )
  
  single_mode_combined <- single_mode_classified %>%
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
  
  single_mode_summary <- single_mode_combined %>%
    group_by(P5_14_merged) %>%
    summarise(n = n(), .groups = 'drop')
  
  single_mode_weighted <- single_mode_combined %>%
    group_by(P5_14_merged) %>%
    summarise(weighted_n = sum(FACTOR), .groups = 'drop')
  
  # === MULTI MODE ANALYSIS (2+ modes) ===
  multi_mode_list <- list()
  weighted_multi_mode_list <- list()
  
  for (num_modes in 2:6) {
    mode_data <- trip_data %>%
      filter(rowSums(!is.na(select(., starts_with("P5_14")))) == num_modes)
    
    if (nrow(mode_data) > 0) {
      # Remove walking if present in multi-mode trips
      mode_data <- mode_data %>%
        mutate(P5_14_14 = NA)
      
      # Re-filter after removing walking
      mode_data <- mode_data %>%
        filter(rowSums(!is.na(select(., starts_with("P5_14")))) >= 2)
      
      if (nrow(mode_data) > 0) {
        mode_classified <- mode_data %>%
          mutate(
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
            P5_14_11 = ifelse(is.na(P5_14_11), NA, "BRT"),
            P5_14_12 = ifelse(is.na(P5_14_12), NA, "Metro"),
            P5_14_13 = ifelse(is.na(P5_14_13), NA, "Metro"),
            P5_14_14 = ifelse(is.na(P5_14_14), NA, NA),
            P5_14_15 = ifelse(is.na(P5_14_15), NA, "Metro"),
            P5_14_16 = ifelse(is.na(P5_14_16), NA, "Other"),
            P5_14_17 = ifelse(is.na(P5_14_17), NA, "Other"),
            P5_14_18 = ifelse(is.na(P5_14_18), NA, "Bus"),
            P5_14_19 = ifelse(is.na(P5_14_19), NA, "Other"),
            P5_14_20 = ifelse(is.na(P5_14_20), NA, "Other")
          )
        
        mode_combined <- mode_classified %>%
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
        
        mode_summary <- mode_combined %>%
          group_by(P5_14_merged) %>%
          summarise(n = n(), .groups = 'drop')
        
        mode_weighted <- mode_combined %>%
          group_by(P5_14_merged) %>%
          summarise(weighted_n = sum(FACTOR), .groups = 'drop')
        
        multi_mode_list[[as.character(num_modes)]] <- mode_summary
        weighted_multi_mode_list[[as.character(num_modes)]] <- mode_weighted
      }
    }
  }
  
  # Combine all multi-mode results
  if (length(multi_mode_list) > 0) {
    multi_mode_summary <- bind_rows(multi_mode_list) %>%
      group_by(P5_14_merged) %>%
      summarise(n = sum(n), .groups = 'drop')
    
    multi_mode_weighted <- bind_rows(weighted_multi_mode_list) %>%
      group_by(P5_14_merged) %>%
      summarise(weighted_n = sum(weighted_n), .groups = 'drop')
  } else {
    multi_mode_summary <- tibble(P5_14_merged = character(), n = numeric())
    multi_mode_weighted <- tibble(P5_14_merged = character(), weighted_n = numeric())
  }
  
  # === COMBINE ALL RESULTS ===
  all_modes <- bind_rows(
    single_mode_summary %>% mutate(type = "Single Mode"),
    multi_mode_summary %>% mutate(type = "Multi Mode")
  )
  
  all_modes_weighted <- bind_rows(
    single_mode_weighted %>% rename(n = weighted_n) %>% mutate(type = "Single Mode"),
    multi_mode_weighted %>% rename(n = weighted_n) %>% mutate(type = "Multi Mode")
  )
  
  # Calculate totals
  total_trips <- sum(all_modes$n)
  total_weighted_trips <- sum(all_modes_weighted$n)
  
  # Calculate mode shares
  mode_share <- all_modes %>%
    mutate(
      share = n / total_trips * 100,
      category = category_name
    ) %>%
    arrange(desc(n))
  
  mode_share_weighted <- all_modes_weighted %>%
    mutate(
      share = n / total_weighted_trips * 100,
      category = category_name
    ) %>%
    arrange(desc(n))
  
  # Calculate transit split (Bus, Metro, BRT vs Other)
  transit_modes <- c("Bus", "Metro", "BRT", "Bus_Metro", "Bus_BRT", "Metro_BRT", "Bus_Metro_BRT")
  
  mode_share <- mode_share %>%
    rowwise() %>%
    mutate(
      is_transit = {
        if (length(P5_14_merged) == 0 || is.na(P5_14_merged)) {
          FALSE
        } else {
          mode_parts <- strsplit(P5_14_merged, "_")[[1]]
          all(mode_parts %in% c("Bus", "Metro", "BRT"))
        }
      }
    ) %>%
    ungroup()
  
  mode_share_weighted <- mode_share_weighted %>%
    rowwise() %>%
    mutate(
      is_transit = {
        if (length(P5_14_merged) == 0 || is.na(P5_14_merged)) {
          FALSE
        } else {
          mode_parts <- strsplit(P5_14_merged, "_")[[1]]
          all(mode_parts %in% c("Bus", "Metro", "BRT"))
        }
      }
    ) %>%
    ungroup()
  
  transit_split <- tibble(
    category = category_name,
    transit_trips = sum(mode_share$n[mode_share$is_transit]),
    transit_share = sum(mode_share$share[mode_share$is_transit]),
    non_transit_trips = sum(mode_share$n[!mode_share$is_transit]),
    non_transit_share = sum(mode_share$share[!mode_share$is_transit]),
    total_trips = total_trips
  )
  
  transit_split_weighted <- tibble(
    category = category_name,
    transit_trips = sum(mode_share_weighted$n[mode_share_weighted$is_transit]),
    transit_share = sum(mode_share_weighted$share[mode_share_weighted$is_transit]),
    non_transit_trips = sum(mode_share_weighted$n[!mode_share_weighted$is_transit]),
    non_transit_share = sum(mode_share_weighted$share[!mode_share_weighted$is_transit]),
    total_trips = total_weighted_trips
  )
  
  list(
    mode_share = mode_share,
    mode_share_weighted = mode_share_weighted,
    transit_split = transit_split,
    transit_split_weighted = transit_split_weighted
  )
}

# Process city to city trips
cat("Processing city to city trips...\n")
city_results <- process_mode_data(city_to_city, "City to City (ENT 09-09)")

# Process suburb to suburb trips
cat("Processing suburb to suburb trips...\n")
suburb_results <- process_mode_data(suburb_to_suburb, "Suburb to Suburb (ENT 13/15 - 13/15)")

# Combine results
combined_mode_share <- bind_rows(
  city_results$mode_share,
  suburb_results$mode_share
)

combined_mode_share_weighted <- bind_rows(
  city_results$mode_share_weighted,
  suburb_results$mode_share_weighted
)

combined_transit_split <- bind_rows(
  city_results$transit_split,
  suburb_results$transit_split
)

combined_transit_split_weighted <- bind_rows(
  city_results$transit_split_weighted,
  suburb_results$transit_split_weighted
)

# Create output directory if it doesn't exist
if (!dir.exists("data/2017/ent_od_breakdown")) {
  dir.create("data/2017/ent_od_breakdown", recursive = TRUE)
}

# Save results
write.csv(combined_mode_share, "data/2017/ent_od_breakdown/mode_share_2017.csv", row.names = FALSE)
write.csv(combined_mode_share_weighted, "data/2017/ent_od_breakdown/mode_share_weighted_2017.csv", row.names = FALSE)
write.csv(combined_transit_split, "data/2017/ent_od_breakdown/transit_split_2017.csv", row.names = FALSE)
write.csv(combined_transit_split_weighted, "data/2017/ent_od_breakdown/transit_split_weighted_2017.csv", row.names = FALSE)

# Print summary
cat("\n=== 2017 ENT OD BREAKDOWN SUMMARY ===\n")
cat("\nCity to City (ENT 09-09):\n")
print(city_results$transit_split)
cat("\nSuburb to Suburb (ENT 13/15 - 13/15):\n")
print(suburb_results$transit_split)

cat("\n=== WEIGHTED SUMMARY ===\n")
cat("\nCity to City (ENT 09-09) - Weighted:\n")
print(city_results$transit_split_weighted)
cat("\nSuburb to Suburb (ENT 13/15 - 13/15) - Weighted:\n")
print(suburb_results$transit_split_weighted)

cat("\nResults saved to data/2017/ent_od_breakdown/\n")
