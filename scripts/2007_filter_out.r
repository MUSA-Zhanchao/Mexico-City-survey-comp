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

trip_2007<- read.csv("data/2007/trip_2007.csv")
trip_2007<- left_join(trip_2007, vivi_hogar_resident, by = c( "IDTR_RESID"= "IDTR_RESID"))

trip_2007<- trip_2007%>%
  select(SORDENTRAN, ENT)

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
  group_by(trip1)%>%
  summarise(
    count = n()
  )

# 2 mode
trip2_or_more_mex<- major_mex %>%
  filter(!is.na(trip2) & is.na(trip3))

trip2_summarised_mex<-trip2_or_more_mex%>%
  group_by(trip1, trip2)%>%
  summarise(
    count = n()
  )%>%
  arrange(desc(count))
combo_2_mex<- trip2_summarised_mex %>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2"), sep = "_", fill = "right")

combo_2_1_mex<-combo_2_mex%>%
  filter(is.na(trip2))%>%
  group_by(trip1)%>%
  summarise(
    count = sum(count)
  )
single_mode_mex_plus<-rbind(single_mode_mex_c, combo_2_1_mex)
single_mode_mex_c<-single_mode_mex_plus%>%
  group_by(trip1)%>%
  summarise(
    count = sum(count)
  )

combo_2_mex<-combo_2_mex%>%
  filter(!is.na(trip2))

combo_2_mex<-combo_2_mex%>%
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
combo_2_mex<-combo_2_mex%>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2"), sep = "_", fill = "right")


trip3_or_more_mex<- major_mex %>%
  filter(!is.na(trip3) & is.na(trip4))

trip3_summarised_mex<-trip3_or_more_mex%>%
  group_by(trip1, trip2, trip3)%>%
  summarise(
    count = n()
  )%>%
  arrange(desc(count))

sum(trip3_summarised_mex$count)

combo_3_mex <- trip3_summarised_mex %>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2, trip3))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2", "trip3"), sep = "_", fill = "right")


combo_3_mex<-combo_3_mex%>%
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
combo_3_mex<-combo_3_mex%>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2, trip3))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2", "trip3"), sep = "_", fill = "right")



trip4_or_more_mex<- major_mex %>%
  filter(!is.na(trip4)&is.na(trip5))

trip4_summarised_mex<-trip4_or_more_mex%>%
  group_by(trip1, trip2, trip3, trip4)%>%
  summarise(
    count = n()
  )%>%
  arrange(desc(count))

combo_4_mex<- trip4_summarised_mex %>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2, trip3, trip4))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2", "trip3", "trip4"), sep = "_", fill = "right")


combo_4_mex<-combo_4_mex%>%
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
combo_4_mex<-combo_4_mex%>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2, trip3, trip4))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2", "trip3", "trip4"), sep = "_", fill = "right")


trip5_or_more_mex<- major_mex %>%
  filter(!is.na(trip5) & is.na(trip6))
trip5_summarised_mex<-trip5_or_more_mex%>%
  group_by(trip1, trip2, trip3, trip4, trip5)%>%
  summarise(
    count = n()
  )%>%
  arrange(desc(count))
combo_5_mex<- trip5_summarised_mex %>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2, trip3, trip4, trip5))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2", "trip3", "trip4", "trip5"), sep = "_", fill = "right")


combo_5_mex<-combo_5_mex%>%
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
combo_5_mex<-combo_5_mex%>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2, trip3, trip4, trip5))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2", "trip3", "trip4", "trip5"), sep = "_", fill = "right")

trip6_or_more_mex<- major_mex %>%
  filter(!is.na(trip6))

trip6_summarised_mex<-trip6_or_more_mex%>%
  group_by(trip1, trip2, trip3, trip4, trip5, trip6)%>%
  summarise(
    count = n()
  )%>%
  arrange(desc(count))
combo_6_mex<- trip6_summarised_mex %>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2, trip3, trip4, trip5, trip6))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2", "trip3", "trip4", "trip5", "trip6"), sep = "_", fill = "right")

combo_6_mex<-combo_6_mex%>%
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
combo_6_mex<-combo_6_mex%>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2, trip3, trip4, trip5, trip6))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2", "trip3", "trip4", "trip5", "trip6"), sep = "_", fill = "right")

library(dplyr)

combine_trip_tables <- function(..., max_trips = 6, unordered_within_row = FALSE) {
  trip_cols_all <- paste0("trip", seq_len(max_trips))
  dfs <- list(...)

  standardize_one <- function(x) {
    trip_cols <- intersect(names(x), trip_cols_all)
    if (!"count" %in% names(x)) stop("Each data frame must have a 'count' column.")

    x <- x %>%
      mutate(across(all_of(trip_cols), as.character),   # keep existing NA as-is
             count = as.numeric(count))

    missing <- setdiff(trip_cols_all, names(x))
    if (length(missing)) x[missing] <- NA_character_

    x %>% select(all_of(trip_cols_all), count)
  }

  big <- dfs %>% map(standardize_one) %>% bind_rows()

  if (unordered_within_row) {
    big %>%
      mutate(key = apply(select(., all_of(trip_cols_all)), 1,
                         function(r) paste(sort(na.omit(r)), collapse = "|"))) %>%
      group_by(key) %>%
      summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
      separate(key, into = trip_cols_all, sep = "\\|", fill = "right", remove = TRUE) %>%
      arrange(desc(count))
  } else {
    big %>%
      group_by(across(all_of(trip_cols_all))) %>%
      summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(count))
  }
}

combined_mex <- combine_trip_tables(combo_2_mex, combo_3_mex, combo_4_mex, combo_6_mex, combo_5_mex, max_trips = 6)

sum(combined_mex$count)+ sum(single_mode_mex_c$count)

#write.csv(combined_mex, "data/2007/mutimodal_trip_combined_mex_2007.csv", row.names = FALSE)
#write.csv(single_mode_mex_c, "data/2007/single_mode_mex_2007.csv", row.names = FALSE)
