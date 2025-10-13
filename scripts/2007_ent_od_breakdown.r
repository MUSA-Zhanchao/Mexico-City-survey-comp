library(foreign)
library(tidyverse)

# Load 2007 data
trip_2007 <- read.csv("data/2007/trip_2007.csv")
vivida <- read.dbf("data/2007/TVIVIENDA.DBF", as.is = TRUE)
hogar <- read.dbf("data/2007/tr_hogares.dbf", as.is = TRUE)
residents <- read.dbf("data/2007/tr_residentes.dbf", as.is = TRUE)

# Join household and demographic data to get origin ENT
vivi <- vivida %>%
  select(ID_VIV, ENT)

hogar_07 <- hogar %>%
  select(IDTR_HOGAR, IDTR_VIVIE)

vivi_hogar <- left_join(vivi, hogar_07, by = c("ID_VIV" = "IDTR_VIVIE"))

resident_07 <- residents %>%
  select(IDTR_RESID, IDTR_HOGAR)

vivi_hogar_resident <- left_join(vivi_hogar, resident_07, by = c("IDTR_HOGAR" = "IDTR_HOGAR"))

# Join trip data with household ENT (origin ENT)
complete_trip_2007 <- trip_2007 %>%
  left_join(vivi_hogar_resident, by = c("IDTR_RESID" = "IDTR_RESID")) %>%
  rename(origin_ent = ENT) %>%
  mutate(
    dest_ent = sprintf("%02d", as.numeric(CENTIDADDE))  # destination ENT code with leading zero
  )

# City to city trips (origin 9 and destination 9)
city_to_city <- complete_trip_2007 %>%
  filter(origin_ent == "09" & dest_ent == "09") %>%
  select(SORDENTRAN, NFACTOR)

# Suburb to suburb trips (origin 15 or 13 and destination 15 or 13)
suburb_to_suburb <- complete_trip_2007 %>%
  filter(origin_ent %in% c("13", "15") & dest_ent %in% c("13", "15")) %>%
  select(SORDENTRAN, NFACTOR)

# Function to process 2007 mode data
process_mode_data_2007 <- function(trip_data, category_name) {
  # Separate SORDENTRAN into individual trip segments
  trip_data <- trip_data %>%
    separate(SORDENTRAN, into = paste0("trip", 1:7), sep = 1:6, remove = FALSE) %>%
    mutate(across(starts_with("trip"), ~ na_if(., "0")))

  # === SINGLE MODE ANALYSIS ===
  one_mode <- trip_data %>%
    filter(is.na(trip2))

  # Classify single modes
  single_mode_classified <- one_mode %>%
    mutate(
      trip1_cat = case_when(
        trip1 %in% c("1", "2", "6") ~ "Metro",
        trip1 == "3" ~ "Metro",
        trip1 %in% c("4", "5", "7") ~ "Bus",
        trip1 == "8" ~ "Other",
        trip1 == "9" ~ "Walking",
        TRUE ~ "Other"
      )
    )

  single_mode_summary <- single_mode_classified %>%
    group_by(trip1_cat) %>%
    summarise(n = n(), .groups = 'drop')

  single_mode_weighted <- single_mode_classified %>%
    group_by(trip1_cat) %>%
    summarise(weighted_n = sum(NFACTOR), .groups = 'drop')

  # === MULTI MODE ANALYSIS (2+ modes) ===
  multi_mode <- trip_data %>%
    filter(!is.na(trip2))

  if (nrow(multi_mode) > 0) {
    # Categorize all trip segments
    multi_mode_classified <- multi_mode %>%
      mutate(
        trip1_cat = case_when(
          trip1 %in% c("1", "2", "6") ~ "Metro",
          trip1 == "3" ~ "Metro",
          trip1 %in% c("4", "5", "7") ~ "Bus",
          trip1 == "8" ~ "Other",
          trip1 == "9" ~ NA_character_,  # Remove walking from multi-mode
          TRUE ~ "Other"
        ),
        trip2_cat = case_when(
          trip2 %in% c("1", "2", "6") ~ "Metro",
          trip2 == "3" ~ "Metro",
          trip2 %in% c("4", "5", "7") ~ "Bus",
          trip2 == "8" ~ "Other",
          trip2 == "9" ~ NA_character_,  # Remove walking from multi-mode
          is.na(trip2) ~ NA_character_,
          TRUE ~ "Other"
        ),
        trip3_cat = case_when(
          trip3 %in% c("1", "2", "6") ~ "Metro",
          trip3 == "3" ~ "Metro",
          trip3 %in% c("4", "5", "7") ~ "Bus",
          trip3 == "8" ~ "Other",
          trip3 == "9" ~ NA_character_,  # Remove walking from multi-mode
          is.na(trip3) ~ NA_character_,
          TRUE ~ "Other"
        ),
        trip4_cat = case_when(
          trip4 %in% c("1", "2", "6") ~ "Metro",
          trip4 == "3" ~ "Metro",
          trip4 %in% c("4", "5", "7") ~ "Bus",
          trip4 == "8" ~ "Other",
          trip4 == "9" ~ NA_character_,  # Remove walking from multi-mode
          is.na(trip4) ~ NA_character_,
          TRUE ~ "Other"
        ),
        trip5_cat = case_when(
          trip5 %in% c("1", "2", "6") ~ "Metro",
          trip5 == "3" ~ "Metro",
          trip5 %in% c("4", "5", "7") ~ "Bus",
          trip5 == "8" ~ "Other",
          trip5 == "9" ~ NA_character_,  # Remove walking from multi-mode
          is.na(trip5) ~ NA_character_,
          TRUE ~ "Other"
        ),
        trip6_cat = case_when(
          trip6 %in% c("1", "2", "6") ~ "Metro",
          trip6 == "3" ~ "Metro",
          trip6 %in% c("4", "5", "7") ~ "Bus",
          trip6 == "8" ~ "Other",
          trip6 == "9" ~ NA_character_,  # Remove walking from multi-mode
          is.na(trip6) ~ NA_character_,
          TRUE ~ "Other"
        ),
        trip7_cat = case_when(
          trip7 %in% c("1", "2", "6") ~ "Metro",
          trip7 == "3" ~ "Metro",
          trip7 %in% c("4", "5", "7") ~ "Bus",
          trip7 == "8" ~ "Other",
          trip7 == "9" ~ NA_character_,  # Remove walking from multi-mode
          is.na(trip7) ~ NA_character_,
          TRUE ~ "Other"
        )
      )

    # Create combined mode string
    multi_mode_combined <- multi_mode_classified %>%
      rowwise() %>%
      mutate(
        mode_merged = {
          x <- c_across(ends_with("_cat"))
          x <- unique(na.omit(trimws(x)))
          if (length(x) == 0) NA_character_
          else paste(sort(x), collapse = "_")
        }
      ) %>%
      ungroup()

    multi_mode_summary <- multi_mode_combined %>%
      group_by(mode_merged) %>%
      summarise(n = n(), .groups = 'drop')

    multi_mode_weighted <- multi_mode_combined %>%
      group_by(mode_merged) %>%
      summarise(weighted_n = sum(NFACTOR), .groups = 'drop')
  } else {
    multi_mode_summary <- tibble(mode_merged = character(), n = numeric())
    multi_mode_weighted <- tibble(mode_merged = character(), weighted_n = numeric())
  }

  # === COMBINE ALL RESULTS ===
  # Rename columns to match
  single_mode_summary <- single_mode_summary %>%
    rename(mode = trip1_cat)
  single_mode_weighted <- single_mode_weighted %>%
    rename(mode = trip1_cat)
  multi_mode_summary <- multi_mode_summary %>%
    rename(mode = mode_merged)
  multi_mode_weighted <- multi_mode_weighted %>%
    rename(mode = mode_merged)

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

  # Calculate transit split (Bus, Metro vs Other)
  mode_share <- mode_share %>%
    rowwise() %>%
    mutate(
      is_transit = {
        if (length(mode) == 0 || is.na(mode)) {
          FALSE
        } else {
          mode_parts <- strsplit(mode, "_")[[1]]
          all(mode_parts %in% c("Bus", "Metro"))
        }
      }
    ) %>%
    ungroup()

  mode_share_weighted <- mode_share_weighted %>%
    rowwise() %>%
    mutate(
      is_transit = {
        if (length(mode) == 0 || is.na(mode)) {
          FALSE
        } else {
          mode_parts <- strsplit(mode, "_")[[1]]
          all(mode_parts %in% c("Bus", "Metro"))
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
city_results <- process_mode_data_2007(city_to_city, "City to City (ENT 09-09)")

# Process suburb to suburb trips
cat("Processing suburb to suburb trips...\n")
suburb_results <- process_mode_data_2007(suburb_to_suburb, "Suburb to Suburb (ENT 13/15 - 13/15)")

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
if (!dir.exists("data/2007/ent_od_breakdown")) {
  dir.create("data/2007/ent_od_breakdown", recursive = TRUE)
}

# Save results
write.csv(combined_mode_share, "data/2007/ent_od_breakdown/mode_share_2007.csv", row.names = FALSE)
write.csv(combined_transit_split, "data/2007/ent_od_breakdown/transit_split_2007.csv", row.names = FALSE)
