library(foreign)
library(tidyverse)

#trip <- read.dbf("data/2007/tr_viajes.dbf", as.is = TRUE)
vivida<- read.dbf("data/2007/TVIVIENDA.DBF", as.is = TRUE)
#write.csv(vivida, "data/2007/vivida_2007.csv", row.names = FALSE)
vivi<- vivida%>%
  select(ID_VIV, ENT)
hogar<- read.dbf("data/2007/tr_hogares.dbf", as.is = TRUE)
hogar_07<- hogar%>%
  select(IDTR_HOGAR,IDTR_VIVIE)
vivi_hogar<- left_join(vivi, hogar_07, by = c("ID_VIV"= "IDTR_VIVIE"))
residents<- read.dbf("data/2007/tr_residentes.dbf", as.is = TRUE)
resident_07<- residents%>%
  select(IDTR_RESID, IDTR_HOGAR)
vivi_hogar_resident<- left_join(vivi_hogar, resident_07, by = c("IDTR_HOGAR"= "IDTR_HOGAR"))

# Load 2007 trip data with weighting factor
trip_2007<- read.csv("data/2007/trip_2007.csv")
trip_2007<- left_join(trip_2007, vivi_hogar_resident, by = c( "IDTR_RESID"= "IDTR_RESID"))

trip_2007<- trip_2007%>%
  select(SORDENTRAN, ENT, NFACTOR)

# Filter for non-Mexico City trips (ENT != "09")
non_major_mex<-trip_2007%>%
  filter(ENT!="09")

library(tidyr)
library(purrr)

dedup_key <- function(v) {
  paste(sort(unique(na.omit(v))), collapse = "_")  # unique & order-insensitive
}

# Process trip data - keep NFACTOR column for weighting
non_major_mex <- non_major_mex %>%
  separate(SORDENTRAN, into = paste0("trip", 1:7), sep = 1:6) %>%
  mutate(across(starts_with("trip"), ~ na_if(., "0")))

# Single mode trips - weighted analysis
non_major_mex_single<- non_major_mex %>%
  filter(is.na(trip2))

single_mode_non_mex_weighted<-non_major_mex_single%>%
  group_by(trip1)%>%
  summarise(
    weighted_count = sum(NFACTOR)
  )

single_mode_non_mex_weighted<-single_mode_non_mex_weighted%>%
  mutate(trip1=case_when(
    trip1 == "a" ~ "A",
    trip1 == "b" ~ "B", 
    trip1 == "c" ~ "C",
    TRUE ~ trip1))

single_mode_non_mex_weighted<-single_mode_non_mex_weighted%>%
  group_by(trip1)%>%
  summarise(weighted_count = sum(weighted_count))

# Two mode trips - weighted analysis
trip2_or_more_non_mex<- non_major_mex %>%
  filter(!is.na(trip2) & is.na(trip3))

trip2_summarised_non_mex<-trip2_or_more_non_mex%>%
  group_by(trip1, trip2)%>%
  summarise(
    weighted_count = sum(NFACTOR)
  )%>%
  arrange(desc(weighted_count))

combo_2_non_mex<- trip2_summarised_non_mex %>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(weighted_count = sum(weighted_count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2"), sep = "_", fill = "right")

combo_2_non_mex<-combo_2_non_mex%>%
  filter(!is.na(trip2))

combo_2_non_mex<-combo_2_non_mex%>%
  mutate(
    trip1 = case_when(
      trip1 %in% c("1", "2", "6") ~ "Metro",
      trip1 == "3" ~ "BRT",
      trip1 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip1) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip2 = case_when(
      trip2 %in% c("1", "2", "6") ~ "Metro",
      trip2 == "3" ~ "BRT",
      trip2 %in% c("4", "5","7") ~ "Bus",
      is.na(trip2) ~ NA_character_,
      TRUE ~ "Other"
    )
  )

combo_2_non_mex<-combo_2_non_mex%>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(weighted_count = sum(weighted_count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2"), sep = "_", fill = "right")

# Three mode trips - weighted analysis
trip3_or_more_non_mex<- non_major_mex %>%
  filter(!is.na(trip3) & is.na(trip4))

trip3_summarised_non_mex<-trip3_or_more_non_mex%>%
  group_by(trip1, trip2, trip3)%>%
  summarise(
    weighted_count = sum(NFACTOR)
  )%>%
  arrange(desc(weighted_count))

combo_3_non_mex <- trip3_summarised_non_mex %>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2, trip3))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(weighted_count = sum(weighted_count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2", "trip3"), sep = "_", fill = "right")

combo_3_non_mex<-combo_3_non_mex%>%
  filter(!is.na(trip2))

combo_3_non_mex<-combo_3_non_mex%>%
  mutate(
    trip1 = case_when(
      trip1 %in% c("1", "2", "6") ~ "Metro",
      trip1 == "3" ~ "BRT",
      trip1 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip1) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip2 = case_when(
      trip2 %in% c("1", "2", "6") ~ "Metro",
      trip2 == "3" ~ "BRT",
      trip2 %in% c("4", "5","7") ~ "Bus",
      is.na(trip2) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip3 = case_when(
      trip3 %in% c("1", "2", "6") ~ "Metro",
      trip3 == "3" ~ "BRT",
      trip3 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip3) ~ NA_character_,
      TRUE ~ "Other"
    )
  )

combo_3_non_mex<-combo_3_non_mex%>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2, trip3))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(weighted_count = sum(weighted_count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2", "trip3"), sep = "_", fill = "right")

# Four mode trips - weighted analysis
trip4_or_more_non_mex<- non_major_mex %>%
  filter(!is.na(trip4)&is.na(trip5))

trip4_summarised_non_mex<-trip4_or_more_non_mex%>%
  group_by(trip1, trip2, trip3, trip4)%>%
  summarise(
    weighted_count = sum(NFACTOR)
  )%>%
  arrange(desc(weighted_count))

combo_4_non_mex<- trip4_summarised_non_mex %>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2, trip3, trip4))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(weighted_count = sum(weighted_count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2", "trip3", "trip4"), sep = "_", fill = "right")

combo_4_non_mex<-combo_4_non_mex%>%
  filter(!is.na(trip2))

combo_4_non_mex<-combo_4_non_mex%>%
  mutate(
    trip1 = case_when(
      trip1 %in% c("1", "2", "6") ~ "Metro",
      trip1 == "3" ~ "BRT",
      trip1 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip1) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip2 = case_when(
      trip2 %in% c("1", "2", "6") ~ "Metro",
      trip2 == "3" ~ "BRT",
      trip2 %in% c("4", "5","7") ~ "Bus",
      is.na(trip2) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip3 = case_when(
      trip3 %in% c("1", "2", "6") ~ "Metro",
      trip3 == "3" ~ "BRT",
      trip3 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip3) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip4 = case_when(
      trip4 %in% c("1", "2", "6") ~ "Metro",
      trip4 == "3" ~ "BRT",
      trip4 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip4) ~ NA_character_,
      TRUE ~ "Other"
    )
  )

combo_4_non_mex<-combo_4_non_mex%>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2, trip3, trip4))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(weighted_count = sum(weighted_count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2", "trip3", "trip4"), sep = "_", fill = "right")

# Five mode trips - weighted analysis
trip5_or_more_non_mex<- non_major_mex %>%
  filter(!is.na(trip5) & is.na(trip6))

trip5_summarised_non_mex<-trip5_or_more_non_mex%>%
  group_by(trip1, trip2, trip3, trip4, trip5)%>%
  summarise(
    weighted_count = sum(NFACTOR)
  )%>%
  arrange(desc(weighted_count))

combo_5_non_mex<- trip5_summarised_non_mex %>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2, trip3, trip4, trip5))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(weighted_count = sum(weighted_count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2", "trip3", "trip4", "trip5"), sep = "_", fill = "right")

combo_5_non_mex<-combo_5_non_mex%>%
  filter(!is.na(trip2))

combo_5_non_mex<-combo_5_non_mex%>%
  mutate(
    trip1 = case_when(
      trip1 %in% c("1", "2", "6") ~ "Metro",
      trip1 == "3" ~ "BRT",
      trip1 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip1) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip2 = case_when(
      trip2 %in% c("1", "2", "6") ~ "Metro",
      trip2 == "3" ~ "BRT",
      trip2 %in% c("4", "5","7") ~ "Bus",
      is.na(trip2) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip3 = case_when(
      trip3 %in% c("1", "2", "6") ~ "Metro",
      trip3 == "3" ~ "BRT",
      trip3 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip3) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip4 = case_when(
      trip4 %in% c("1", "2", "6") ~ "Metro",
      trip4 == "3" ~ "BRT",
      trip4 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip4) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip5 = case_when(
      trip5 %in% c("1", "2", "6") ~ "Metro",
      trip5 == "3" ~ "BRT",
      trip5 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip5) ~ NA_character_,
      TRUE ~ "Other"
    )
  )

combo_5_non_mex<-combo_5_non_mex%>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2, trip3, trip4, trip5))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(weighted_count = sum(weighted_count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2", "trip3", "trip4", "trip5"), sep = "_", fill = "right")

# Six mode trips - weighted analysis
trip6_or_more_non_mex<- non_major_mex %>%
  filter(!is.na(trip6))

trip6_summarised_non_mex<-trip6_or_more_non_mex%>%
  group_by(trip1, trip2, trip3, trip4, trip5, trip6)%>%
  summarise(
    weighted_count = sum(NFACTOR)
  )%>%
  arrange(desc(weighted_count))

combo_6_non_mex<- trip6_summarised_non_mex %>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2, trip3, trip4, trip5, trip6))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(weighted_count = sum(weighted_count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2", "trip3", "trip4", "trip5", "trip6"), sep = "_", fill = "right")

combo_6_non_mex<-combo_6_non_mex%>%
  filter(!is.na(trip2))

combo_6_non_mex<-combo_6_non_mex%>%
  mutate(
    trip1 = case_when(
      trip1 %in% c("1", "2", "6") ~ "Metro",
      trip1 == "3" ~ "BRT",
      trip1 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip1) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip2 = case_when(
      trip2 %in% c("1", "2", "6") ~ "Metro",
      trip2 == "3" ~ "BRT",
      trip2 %in% c("4", "5","7") ~ "Bus",
      is.na(trip2) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip3 = case_when(
      trip3 %in% c("1", "2", "6") ~ "Metro",
      trip3 == "3" ~ "BRT",
      trip3 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip3) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip4 = case_when(
      trip4 %in% c("1", "2", "6") ~ "Metro",
      trip4 == "3" ~ "BRT",
      trip4 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip4) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip5 = case_when(
      trip5 %in% c("1", "2", "6") ~ "Metro",
      trip5 == "3" ~ "BRT",
      trip5 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip5) ~ NA_character_,
      TRUE ~ "Other"
    ),
    trip6 = case_when(
      trip6 %in% c("1", "2", "6") ~ "Metro",
      trip6 == "3" ~ "BRT",
      trip6 %in% c("4", "5", "7") ~ "Bus",
      is.na(trip6) ~ NA_character_,
      TRUE ~ "Other"
    )
  )

combo_6_non_mex<-combo_6_non_mex%>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2, trip3, trip4, trip5, trip6))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(weighted_count = sum(weighted_count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2", "trip3", "trip4", "trip5", "trip6"), sep = "_", fill = "right")

library(dplyr)

combine_trip_tables_weighted <- function(..., max_trips = 6, unordered_within_row = FALSE) {
  trip_cols_all <- paste0("trip", seq_len(max_trips))
  dfs <- list(...)

  standardize_one <- function(x) {
    trip_cols <- intersect(names(x), trip_cols_all)
    if (!"weighted_count" %in% names(x)) stop("Each data frame must have a 'weighted_count' column.")

    x <- x %>%
      mutate(across(all_of(trip_cols), as.character),   # keep existing NA as-is
             weighted_count = as.numeric(weighted_count))

    missing <- setdiff(trip_cols_all, names(x))
    if (length(missing)) x[missing] <- NA_character_

    x %>% select(all_of(trip_cols_all), weighted_count)
  }

  big <- dfs %>% map(standardize_one) %>% bind_rows()

  if (unordered_within_row) {
    big %>%
      mutate(key = apply(select(., all_of(trip_cols_all)), 1,
                         function(r) paste(sort(na.omit(r)), collapse = "|"))) %>%
      group_by(key) %>%
      summarise(weighted_count = sum(weighted_count, na.rm = TRUE), .groups = "drop") %>%
      separate(key, into = trip_cols_all, sep = "\\|", fill = "right", remove = TRUE) %>%
      arrange(desc(weighted_count))
  } else {
    big %>%
      group_by(across(all_of(trip_cols_all))) %>%
      summarise(weighted_count = sum(weighted_count, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(weighted_count))
  }
}

combined_non_mex_weighted <- combine_trip_tables_weighted(combo_2_non_mex, combo_3_non_mex, combo_4_non_mex, combo_6_non_mex, combo_5_non_mex, max_trips = 6)

write.csv(combined_non_mex_weighted, "data/2007/multimodal_trip_combined_non_mex_weighted_2007.csv", row.names = FALSE)
write.csv(single_mode_non_mex_weighted, "data/2007/single_mode_non_mex_weighted_2007.csv", row.names = FALSE)