library(foreign)
library(tidyverse)

#trip <- read.dbf("data/2007/tr_viajes.dbf", as.is = TRUE)
vivida<- read.dbf("data/2007/TVIVIENDA.DBF", as.is = TRUE)
#write.csv(vivida, "data/2007/vivida_2007.csv", row.names = FALSE)
vivi<- vivida%>%
  select(ID_VIV, ENT,MUN)
hogar<- read.dbf("data/2007/tr_hogares.dbf", as.is = TRUE)
hogar_07<- hogar%>%
  select(IDTR_HOGAR,IDTR_VIVIE)
vivi_hogar<- left_join(vivi, hogar_07, by = c("ID_VIV"= "IDTR_VIVIE"))
residents<- read.dbf("data/2007/tr_residentes.dbf", as.is = TRUE)
resident_07<- residents%>%
  select(IDTR_RESID, IDTR_HOGAR)
vivi_hogar_resident<- left_join(vivi_hogar, resident_07, by = c("IDTR_HOGAR"= "IDTR_HOGAR"))

trip_2007<- read.csv("data/2007/trip_2007.csv")
trip_2007<- left_join(trip_2007, vivi_hogar_resident, by = c( "IDTR_RESID"= "IDTR_RESID"))

trip_2007<- trip_2007%>%
  select(SORDENTRAN, ENT,MUN)

major_mex<-trip_2007%>%
  filter(ENT=="09")



library(tidyr)

library(purrr)

dedup_key <- function(v) {
  paste(sort(unique(na.omit(v))), collapse = "_")  # unique & order-insensitive
}


major_mex <- major_mex %>%
  separate(SORDENTRAN, into = paste0("trip", 1:7), sep = 1:6) %>%
  mutate(across(starts_with("trip"), ~ na_if(., "0")))

major_mex_single<- major_mex %>%
  filter(is.na(trip2))

single_mode_mex_c<-major_mex_single%>%
  group_by(trip1,MUN)%>%
  summarise(
    count = n(),
    .groups = 'drop'
  )

# Categorize transportation modes for spatial analysis
# Metro-only trips (no matter internal transfer)
metro_only_trips <- major_mex %>%
  mutate(
    # Categorize all trip segments
    trip1_cat = case_when(
      trip1 %in% c("1", "2", "6") ~ "Metro",
      trip1 == "3" ~ "Metro",
      trip1 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip1) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip2_cat = case_when(
      trip2 %in% c("1", "2", "6") ~ "Metro",
      trip2 == "3" ~ "Metro",
      trip2 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip2) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip3_cat = case_when(
      trip3 %in% c("1", "2", "6") ~ "Metro",
      trip3 == "3" ~ "Metro",
      trip3 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip3) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip4_cat = case_when(
      trip4 %in% c("1", "2", "6") ~ "Metro",
      trip4 == "3" ~ "Metro",
      trip4 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip4) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip5_cat = case_when(
      trip5 %in% c("1", "2", "6") ~ "Metro",
      trip5 == "3" ~ "Metro",
      trip5 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip5) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip6_cat = case_when(
      trip6 %in% c("1", "2", "6") ~ "Metro",
      trip6 == "3" ~ "Metro",
      trip6 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip6) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip7_cat = case_when(
      trip7 %in% c("1", "2", "6") ~ "Metro",
      trip7 == "3" ~ "Metro",
      trip7 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip7) ~ NA_character_,
      TRUE ~ "Other"
    )
  ) %>%
  rowwise() %>%
  mutate(
    # Get unique modes used in trip (excluding NA)
    unique_modes = list(unique(na.omit(c(trip1_cat, trip2_cat, trip3_cat, trip4_cat, trip5_cat, trip6_cat, trip7_cat))))
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
bus_only_trips <- major_mex %>%
  mutate(
    # Categorize all trip segments (reusing the same logic)
    trip1_cat = case_when(
      trip1 %in% c("1", "2", "6") ~ "Metro",
      trip1 == "3" ~ "Metro",
      trip1 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip1) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip2_cat = case_when(
      trip2 %in% c("1", "2", "6") ~ "Metro",
      trip2 == "3" ~ "Metro",
      trip2 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip2) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip3_cat = case_when(
      trip3 %in% c("1", "2", "6") ~ "Metro",
      trip3 == "3" ~ "Metro",
      trip3 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip3) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip4_cat = case_when(
      trip4 %in% c("1", "2", "6") ~ "Metro",
      trip4 == "3" ~ "Metro",
      trip4 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip4) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip5_cat = case_when(
      trip5 %in% c("1", "2", "6") ~ "Metro",
      trip5 == "3" ~ "Metro",
      trip5 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip5) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip6_cat = case_when(
      trip6 %in% c("1", "2", "6") ~ "Metro",
      trip6 == "3" ~ "Metro",
      trip6 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip6) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip7_cat = case_when(
      trip7 %in% c("1", "2", "6") ~ "Metro",
      trip7 == "3" ~ "Metro",
      trip7 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip7) ~ NA_character_,
      TRUE ~ "Other"
    )
  ) %>%
  rowwise() %>%
  mutate(
    # Get unique modes used in trip (excluding NA)
    unique_modes = list(unique(na.omit(c(trip1_cat, trip2_cat, trip3_cat, trip4_cat, trip5_cat, trip6_cat, trip7_cat))))
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
bus_metro_trips <- major_mex %>%
  mutate(
    # Categorize all trip segments
    trip1_cat = case_when(
      trip1 %in% c("1", "2", "6") ~ "Metro",
      trip1 == "3" ~ "Metro",
      trip1 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip1) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip2_cat = case_when(
      trip2 %in% c("1", "2", "6") ~ "Metro",
      trip2 == "3" ~ "Metro",
      trip2 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip2) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip3_cat = case_when(
      trip3 %in% c("1", "2", "6") ~ "Metro",
      trip3 == "3" ~ "Metro",
      trip3 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip3) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip4_cat = case_when(
      trip4 %in% c("1", "2", "6") ~ "Metro",
      trip4 == "3" ~ "Metro",
      trip4 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip4) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip5_cat = case_when(
      trip5 %in% c("1", "2", "6") ~ "Metro",
      trip5 == "3" ~ "Metro",
      trip5 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip5) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip6_cat = case_when(
      trip6 %in% c("1", "2", "6") ~ "Metro",
      trip6 == "3" ~ "Metro",
      trip6 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip6) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip7_cat = case_when(
      trip7 %in% c("1", "2", "6") ~ "Metro",
      trip7 == "3" ~ "Metro",
      trip7 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip7) ~ NA_character_,
      TRUE ~ "Other"
    )
  ) %>%
  rowwise() %>%
  mutate(
    # Get unique modes used in trip (excluding NA)
    unique_modes = list(unique(na.omit(c(trip1_cat, trip2_cat, trip3_cat, trip4_cat, trip5_cat, trip6_cat, trip7_cat))))
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
spatial_distribution_ent09 <- major_mex %>%
  distinct(MUN) %>%
  left_join(metro_only_by_mun, by = "MUN") %>%
  left_join(bus_only_by_mun, by = "MUN") %>%
  left_join(bus_metro_by_mun, by = "MUN") %>%
  mutate(
    metro_only_count = ifelse(is.na(metro_only_count), 0, metro_only_count),
    bus_only_count = ifelse(is.na(bus_only_count), 0, bus_only_count),
    bus_metro_count = ifelse(is.na(bus_metro_count), 0, bus_metro_count),
  )


# Export results
write.csv(spatial_distribution_ent09, "municipal_level_analysis/export/ent09_trip_categories_07.csv", row.names = FALSE)
