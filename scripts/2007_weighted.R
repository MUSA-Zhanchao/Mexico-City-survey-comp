library(foreign)
library(tidyverse)

# Load 2007 trip data with weighting factor
trip_2007<- read.csv("data/2007/trip_2007.csv")

library(tidyr)
library(purrr)

dedup_key <- function(v) {
  paste(sort(unique(na.omit(v))), collapse = "_")  # unique & order-insensitive
}

# Process trip data - keep NFACTOR column for weighting
trip_2007 <- trip_2007 %>%
  separate(SORDENTRAN, into = paste0("trip", 1:7), sep = 1:6) %>%
  mutate(across(starts_with("trip"), ~ na_if(., "0")))

# Single mode trips - weighted analysis
single_mode<- trip_2007 %>%
  filter(is.na(trip2))

single_mode<-single_mode%>%
  group_by(trip1)%>%
  summarise(
    weighted_count = sum(NFACTOR)
  )
single_mode<-single_mode%>%
  mutate(trip1=case_when(
    trip1 == "a" ~ "A",
    trip1 == "b" ~ "B",
    trip1 == "c" ~ "C",
    TRUE ~ trip1))
single_mode<-single_mode%>%
  group_by(trip1)%>%
  summarise(weighted_count = sum(weighted_count))

# Two mode trips - weighted analysis
trip2_or_more<- trip_2007 %>%
  filter(!is.na(trip2) & is.na(trip3))

trip2_summarised<-trip2_or_more%>%
  group_by(trip1, trip2)%>%
  summarise(
    weighted_count = sum(NFACTOR)
  )%>%
  arrange(desc(weighted_count))

combo_2<- trip2_summarised %>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(weighted_count = sum(weighted_count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2"), sep = "_", fill = "right")

# Combined constant adjustment - need to handle this proportionally
# Original combined_2_1 = 10+69+438+17214+26+9+3 = 17769 (unweighted count)
# We need to find the weighted equivalent by looking at trips that became single mode
trip2_to_single <- combo_2 %>%
  filter(is.na(trip2))%>%
  select(-trip2)
single_mode_complete<-rbind(single_mode, trip2_to_single)
single_mode_complete<-single_mode_complete%>%
  group_by(trip1)%>%
  summarise(weighted_count = sum(weighted_count))
#write.csv(single_mode_complete, "data/2007/single_mode_weighted_complete_2007.csv", row.names = FALSE)



combo_2<-combo_2%>%
  filter(!is.na(trip2))

combo_2<-combo_2%>%
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

combo_2<-combo_2%>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(weighted_count = sum(weighted_count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2"), sep = "_", fill = "right")

# Three mode trips - weighted analysis
trip3_or_more<- trip_2007 %>%
  filter(!is.na(trip3) & is.na(trip4))

trip3_summarised<-trip3_or_more%>%
  group_by(trip1, trip2, trip3)%>%
  summarise(
    weighted_count = sum(NFACTOR)
  )%>%
  arrange(desc(weighted_count))

combo <- trip3_summarised %>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2, trip3))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(weighted_count = sum(weighted_count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2", "trip3"), sep = "_", fill = "right")

# Weighted equivalent of Combined_3_1 = 5+1932 = 1937
#weighted_Combined_3_1 <- 1937 * mean(trip_2007$NFACTOR)

#combo<-combo%>%
#  filter(!is.na(trip2))

combo_3<-combo%>%
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

combo_3<-combo_3%>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2, trip3))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(weighted_count = sum(weighted_count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2", "trip3"), sep = "_", fill = "right")

# Four mode trips - weighted analysis
trip4_or_more<- trip_2007 %>%
  filter(!is.na(trip4)&is.na(trip5))

trip4_summarised<-trip4_or_more%>%
  group_by(trip1, trip2, trip3, trip4)%>%
  summarise(
    weighted_count = sum(NFACTOR)
  )%>%
  arrange(desc(weighted_count))

# Weighted equivalent of combined_4_1 = 131
# weighted_combined_4_1 <- 131 * mean(trip_2007$NFACTOR)

combo_4<- trip4_summarised %>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2, trip3, trip4))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(weighted_count = sum(weighted_count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2", "trip3", "trip4"), sep = "_", fill = "right")

#combo_4<-combo_4%>%
#  filter(!is.na(trip2))

combo_4<-combo_4%>%
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

combo_4<-combo_4%>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2, trip3, trip4))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(weighted_count = sum(weighted_count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2", "trip3", "trip4"), sep = "_", fill = "right")

# Five mode trips - weighted analysis
trip5_or_more<- trip_2007 %>%
  filter(!is.na(trip5) & is.na(trip6))

trip5_summarised<-trip5_or_more%>%
  group_by(trip1, trip2, trip3, trip4, trip5)%>%
  summarise(
    weighted_count = sum(NFACTOR)
  )%>%
  arrange(desc(weighted_count))

combo_5<- trip5_summarised %>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2, trip3, trip4, trip5))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(weighted_count = sum(weighted_count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2", "trip3", "trip4", "trip5"), sep = "_", fill = "right")

# Weighted equivalent of combined_5_1 = 8
#weighted_combined_5_1 <- 8 * mean(trip_2007$NFACTOR)

#combo_5<-combo_5%>%
#  filter(!is.na(trip2))

combo_5<-combo_5%>%
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

combo_5<-combo_5%>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2, trip3, trip4, trip5))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(weighted_count = sum(weighted_count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2", "trip3", "trip4", "trip5"), sep = "_", fill = "right")

# Six mode trips - weighted analysis
trip6_or_more<- trip_2007 %>%
  filter(!is.na(trip6))

trip6_summarised<-trip6_or_more%>%
  group_by(trip1, trip2, trip3, trip4, trip5, trip6)%>%
  summarise(
    weighted_count = sum(NFACTOR)
  )%>%
  arrange(desc(weighted_count))

combo_6<- trip6_summarised %>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2, trip3, trip4, trip5, trip6))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(weighted_count = sum(weighted_count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2", "trip3", "trip4", "trip5", "trip6"), sep = "_", fill = "right")

#combo_6<-combo_6%>%
#  filter(!is.na(trip2))

combo_6<-combo_6%>%
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

combo_6<-combo_6%>%
  rowwise() %>%
  mutate(key = dedup_key(c(trip1, trip2, trip3, trip4, trip5, trip6))) %>%
  ungroup() %>%
  group_by(key) %>%
  summarise(weighted_count = sum(weighted_count), .groups = "drop") %>%
  separate(key, into = c("trip1", "trip2", "trip3", "trip4", "trip5", "trip6"), sep = "_", fill = "right")

# Use the same combining function from the original script
library(dplyr)
library(purrr)
library(tidyr)

combine_trip_tables <- function(..., max_trips = 6, unordered_within_row = FALSE) {
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

# Combine all weighted results
combined_weighted <- combine_trip_tables(combo_2, combo_3, combo_4, combo_6, combo_5, max_trips = 6)

combined_weighted <- combined_weighted %>%
  select(-trip5,-trip6)
# Save weighted results
write.csv(combined_weighted, "data/2007/multimodal_trip_combined_weighted_2007.csv", row.names = FALSE)

# Print comparison
cat("=== WEIGHTED 2007 ANALYSIS SUMMARY ===\n")
cat("Weighted total trips:", round(weighted_total), "\n")
cat("Mean expansion factor (NFACTOR):", round(mean(trip_2007$NFACTOR), 2), "\n")
cat("Unweighted total from original script:", 232317, "\n")
cat("Expansion ratio:", round(weighted_total / 232317, 2), "\n")

# Save single mode weighted results too
#write.csv(single_mode, "data/2007/single_mode_weighted_2007.csv", row.names = FALSE)
