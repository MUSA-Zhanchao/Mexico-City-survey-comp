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

# Helper function for deduplication
dedup_key <- function(v) {
  paste(sort(unique(na.omit(v))), collapse = "_")
}

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


city_results <- classify_trips(city_to_city)

suburb_results <- classify_trips(suburb_to_suburb)

