library(foreign)
library(tidyverse)

# Load 2017 data
trip_2017 <- read.csv("data/2017/trip_2017.csv")
# hogar_2017 <- read.dbf("data/2017/THOGAR.DBF", as.is = TRUE)
# dem_2017 <- read.dbf("data/2017/TSDEM.DBF", as.is = TRUE)
#
# # Join household and demographic data
# hogar_2017 <- hogar_2017 %>%
#   select(ID_HOG, ENT)
# dem_2017 <- dem_2017 %>%
#   select(ID_SOC, ID_HOG)
# dem_hogar_2017 <- dem_2017 %>%
#   left_join(hogar_2017, by = "ID_HOG")

# Join trip data with household ENT (origin ENT)
trip_2017 <- trip_2017 %>%
  rename(origin_ent = P5_7_7,
         dest_ent = P5_12_7)

# Filter for valid trips only (P5_3 == 1)
trip_2017 <- trip_2017 %>%
  filter(P5_3 == 1)

# City to city trips (origin 9 and destination 9)
city_to_city <- trip_2017 %>%
  filter(origin_ent == "9" & dest_ent == "9") %>%
  select(starts_with("P5_14"), FACTOR)

# Suburb to suburb trips (origin 15 or 13 and destination 15 or 13)
suburb_to_suburb <- trip_2017 %>%
  filter(origin_ent %in% c("13", "15") & dest_ent %in% c("13", "15")) %>%
  select(starts_with("P5_14"), FACTOR)

#turns all 2 to NA
city_to_city[city_to_city == 2] <- NA
suburb_to_suburb[suburb_to_suburb == 2] <- NA

# mode 1
city_1<- city_to_city%>%
  filter(rowSums(!is.na(select(., starts_with("P5_14_"))))==1)
n_city_1 <- nrow(city_1)

summary_tbl_city_1 <- city_1 %>%
  summarise(across(starts_with("P5_14_"), ~ sum(!is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "n_valid") %>%
  arrange(desc(n_valid))
summary_tbl_city_1 <- summary_tbl_city_1 %>%
  mutate(mode = case_when(
    column == "P5_14_01" ~ "Drive",
    column == "P5_14_02" ~ "Bus",
    column == "P5_14_03" ~ "Taxi",
    column == "P5_14_04" ~ "Taxi",
    column == "P5_14_05" ~ "Metro",
    column == "P5_14_06" ~ "Bus",
    column == "P5_14_07" ~ "Bicycle",
    column == "P5_14_08" ~ "Bus",
    column == "P5_14_09" ~ "Moto",
    column == "P5_14_10" ~ "Bus",
    column == "P5_14_11" ~ "BRT",
    column == "P5_14_12" ~ "Metro",
    column == "P5_14_13" ~ "Metro",
    column == "P5_14_14" ~ "Walk",
    column == "P5_14_15" ~ "Metro",
    column == "P5_14_16" ~ "Taxi",
    column == "P5_14_17" ~ "Taxi",
    column == "P5_14_18" ~ "Bus",
    column == "P5_14_19" ~ "Other",
    column == "P5_14_20" ~ "Other",
    TRUE ~ "Unknown"
  ))%>%
  group_by(mode)%>%
  summarise(n=sum(n_valid))%>%
  arrange(desc(n))%>%
  mutate(percentage = n / n_city_1 * 100)

city_to_city_2<-  city_to_city%>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 2)
# get rid of walking
city_to_city2<- city_to_city_2 %>%
  mutate(P5_14_14 = ifelse(is.na(P5_14_14), NA, NA))
city_to_city_mode1_plus<- city_to_city2%>%
  filter(rowSums(!is.na(select(., starts_with("P5_14")))) == 1)
summary_tbl_one_mode_plus <- city_to_city_mode1_plus %>%
  summarise(across(starts_with("P5_14_"), ~ sum(!is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "n_valid") %>%
  arrange(desc(n_valid))
summary_tbl_one_mode_plus <- summary_tbl_one_mode_plus %>%
  mutate(mode = case_when(
    column == "P5_14_01" ~ "Drive",
    column == "P5_14_02" ~ "Bus",
    column == "P5_14_03" ~ "Taxi",
    column == "P5_14_04" ~ "Taxi",
    column == "P5_14_05" ~ "Metro",
    column == "P5_14_06" ~ "Bus",
    column == "P5_14_07" ~ "Bicycle",
    column == "P5_14_08" ~ "Bus",
    column == "P5_14_09" ~ "Moto",
    column == "P5_14_10" ~ "Bus",
    column == "P5_14_11" ~ "BRT",
    column == "P5_14_12" ~ "Metro",
    column == "P5_14_13" ~ "Metro",
    column == "P5_14_14" ~ "Walk",
    column == "P5_14_15" ~ "Metro",
    column == "P5_14_16" ~ "Taxi",
    column == "P5_14_17" ~ "Taxi",
    column == "P5_14_18" ~ "Bus",
    column == "P5_14_19" ~ "Other",
    column == "P5_14_20" ~ "Other",
    TRUE ~ "Unknown"
  ))%>%
  group_by(mode)%>%
  summarise(n=sum(n_valid))%>%
  arrange(desc(n))%>%
  mutate(percentage = n / n_city_1 * 100)
summary_one<-rbind(summary_tbl_city_1, summary_tbl_one_mode_plus)%>%
  group_by(mode) %>%
  summarise(n = sum(n))%>%
  arrange(desc(n))
