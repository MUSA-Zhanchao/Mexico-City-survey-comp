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
  select(IDTR_RESID, IDTR_HOGAR,CENTIDADOR)

vivi_hogar_resident <- left_join(vivi_hogar, resident_07, by = c("IDTR_HOGAR" = "IDTR_HOGAR"))


# Join trip data with household ENT (origin ENT)
complete_trip_2007 <- trip_2007 %>%
  left_join(vivi_hogar_resident, by = c("IDTR_RESID" = "IDTR_RESID")) %>%
  rename(origin_ent = CENTIDADOR) %>%
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

dedup_key <- function(v) {
  paste(sort(unique(na.omit(v))), collapse = "_")  # unique & order-insensitive
}


# city_to_city2007_single<- city_to_city %>%
#   separate(SORDENTRAN, into = paste0("trip", 1:7), sep = 1:6) %>%
#   mutate(across(starts_with("trip"), ~ na_if(., "0")))%>%
#   filter(is.na(trip2))%>%
#   group_by(trip1)%>%
#   summarise(
#     weighted_count = sum(NFACTOR)
#   )
# suburb_to_suburb2007_single<- suburb_to_suburb %>%
#   separate(SORDENTRAN, into = paste0("trip", 1:7), sep = 1:6) %>%
#   mutate(across(starts_with("trip"), ~ na_if(., "0")))%>%
#   filter(is.na(trip2))%>%
#   group_by(trip1)%>%
#   summarise(
#     weighted_count = sum(NFACTOR)
#   )

# write.csv(city_to_city2007_single, "data/city-city-suburb/2007_city_to_city_single_mode_summary.csv", row.names = FALSE)
# write.csv(suburb_to_suburb2007_single, "data/city-city-suburb/2007_suburb_to_suburb_single_mode_summary_weighted.csv", row.names = FALSE)
# write.csv(city_to_city2007_single, "data/city-city-suburb/2007_city_to_city_single_mode_summary_weighted.csv", row.names = FALSE)
# Function to classify trips into mode categories

classify_trips <- function(df) {
  # Parse SORDENTRAN into individual trip segments
  df_parsed <- df %>%
    separate(SORDENTRAN, into = paste0("trip", 1:7), sep = 1:6, remove = FALSE) %>%
    mutate(across(starts_with("trip"), ~ na_if(., "0")))

  # Categorize each trip segment
  df_categorized <- df_parsed %>%
    mutate(
      trip1_cat = case_when(
        trip1 %in% c("1", "2", "6") ~ "Metro",
        trip1 == "3" ~ "BRT",
        trip1 %in% c("4", "5", "7") ~ "Bus",
        is.na(trip1) ~ NA_character_,
        TRUE ~ "Other"
      ),
      trip2_cat = case_when(
        trip2 %in% c("1", "2", "6") ~ "Metro",
        trip2 == "3" ~ "BRT",
        trip2 %in% c("4", "5", "7") ~ "Bus",
        is.na(trip2) ~ NA_character_,
        TRUE ~ "Other"
      ),
      trip3_cat = case_when(
        trip3 %in% c("1", "2", "6") ~ "Metro",
        trip3 == "3" ~ "BRT",
        trip3 %in% c("4", "5", "7") ~ "Bus",
        is.na(trip3) ~ NA_character_,
        TRUE ~ "Other"
      ),
      trip4_cat = case_when(
        trip4 %in% c("1", "2", "6") ~ "Metro",
        trip4 == "3" ~ "BRT",
        trip4 %in% c("4", "5", "7") ~ "Bus",
        is.na(trip4) ~ NA_character_,
        TRUE ~ "Other"
      ),
      trip5_cat = case_when(
        trip5 %in% c("1", "2", "6") ~ "Metro",
        trip5 == "3" ~ "BRT",
        trip5 %in% c("4", "5", "7") ~ "Bus",
        is.na(trip5) ~ NA_character_,
        TRUE ~ "Other"
      ),
      trip6_cat = case_when(
        trip6 %in% c("1", "2", "6") ~ "Metro",
        trip6 == "3" ~ "BRT",
        trip6 %in% c("4", "5", "7") ~ "Bus",
        is.na(trip6) ~ NA_character_,
        TRUE ~ "Other"
      ),
      trip7_cat = case_when(
        trip7 %in% c("1", "2", "6") ~ "Metro",
        trip7 == "3" ~ "BRT",
        trip7 %in% c("4", "5", "7") ~ "Bus",
        is.na(trip7) ~ NA_character_,
        TRUE ~ "Other"
      )
    )

  # Get unique modes for each trip
  df_with_modes <- df_categorized %>%
    rowwise() %>%
    mutate(
      unique_modes = list(unique(na.omit(c(trip1_cat, trip2_cat, trip3_cat,
                                           trip4_cat, trip5_cat, trip6_cat, trip7_cat))))
    ) %>%
    ungroup()
}


# Process city to city trips
city_results <- classify_trips(city_to_city)

# Create a merged mode key for city trips
city_results <- city_results %>%
  rowwise() %>%
  mutate(
    mode_key = paste(sort(unique_modes), collapse = "_")
  ) %>%
  ungroup()

# Aggregate city trips by mode combination
city_mode_summary <- city_results %>%
  group_by(mode_key) %>%
  summarise(
    weighted_count = sum(NFACTOR),
    .groups = 'drop'
  ) %>%
  arrange(desc(weighted_count))


# Single mode city trips
city_single_mode <- city_results %>%
  filter(lengths(unique_modes) == 1) %>%
  rowwise() %>%
  mutate(mode = unique_modes[1]) %>%
  ungroup()

city_single_mode_summary <- city_single_mode %>%
  group_by(mode) %>%
  summarise(
    weighted_count = sum(NFACTOR),
    .groups = 'drop'
  ) %>%
  arrange(desc(weighted_count))


# Multimodal city trips (2 or more modes)
city_multimodal <- city_results %>%
  filter(lengths(unique_modes) >= 2)

city_multimodal_summary <- city_multimodal %>%
  group_by(mode_key) %>%
  summarise(
    weighted_count = sum(NFACTOR),
    .groups = 'drop'
  ) %>%
  arrange(desc(weighted_count))


# Process suburb to suburb trips
suburb_results <- classify_trips(suburb_to_suburb)

# Create a merged mode key for suburb trips
suburb_results <- suburb_results %>%
  rowwise() %>%
  mutate(
    mode_key = paste(sort(unique_modes), collapse = "_")
  ) %>%
  ungroup()

# Aggregate suburb trips by mode combination
suburb_mode_summary <- suburb_results %>%
  group_by(mode_key) %>%
  summarise(
    weighted_count = sum(NFACTOR),
    .groups = 'drop'
  ) %>%
  arrange(desc(weighted_count))


# Single mode suburb trips
suburb_single_mode <- suburb_results %>%
  filter(lengths(unique_modes) == 1) %>%
  rowwise() %>%
  mutate(mode = unique_modes[1]) %>%
  ungroup()

suburb_single_mode_summary <- suburb_single_mode %>%
  group_by(mode) %>%
  summarise(
    weighted_count = sum(NFACTOR),
    .groups = 'drop'
  ) %>%
  arrange(desc(weighted_count))

# Multimodal suburb trips (2 or more modes)
suburb_multimodal <- suburb_results %>%
  filter(lengths(unique_modes) >= 2)

suburb_multimodal_summary <- suburb_multimodal %>%
  group_by(mode_key) %>%
  summarise(
    weighted_count = sum(NFACTOR),
    .groups = 'drop'
  ) %>%
  arrange(desc(weighted_count))

sum(city_mode_summary$weighted_count)  # Total city to city trips (weighted)
sum(suburb_mode_summary$weighted_count)  # Total suburb to suburb trips (weighted)

# write.csv(city_mode_summary, "data/city-city-suburb/2007_city_to_city_mode_summary_weighted.csv", row.names = FALSE)
# write.csv(suburb_mode_summary, "data/city-city-suburb/2007_suburb_to_suburb_mode_summary_weighted.csv", row.names = FALSE)
