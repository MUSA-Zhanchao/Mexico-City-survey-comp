library(sf)
library(tidyverse)

## Load shapefiles
mex_09_07<- st_read("mapping/Mex_Municipio_09_07.geojson")
mex_09_17<- st_read("mapping/Mex_Municipio_09.geojson")
mex_13_07<- st_read("mapping/Mex_Municipio_13_07.geojson")
mex_13_17<- st_read("mapping/Mex_Municipio_13.geojson")
mex_15_07<- st_read("mapping/Mex_Municipio_15_07.geojson")
mex_15_17<- st_read("mapping/Mex_Municipio_15.geojson")


## load data
data_09_07 <- read_csv("municipal_level_analysis/export/ent09_trip_categories_07.csv")
data_09_17 <- read_csv("municipal_level_analysis/export/ent09_trip_categories_17.csv")
data_13_17 <- read_csv("municipal_level_analysis/export/ent13_trip_categories_17.csv")
data_15_17 <- read_csv("municipal_level_analysis/export/ent15_trip_categories_17.csv")
data_15_07 <- read_csv("municipal_level_analysis/export/ent15_trip_categories_07.csv")


## Merge data with shapefiles
map_09_07 <- mex_09_07 %>%
  left_join(data_09_07, by = c("CVE_MUN" = "MUN"))
data_09_17<- data_09_17%>%
  mutate(MUN = paste0("09", MUN))
map_09_17 <- mex_09_17 %>%
  left_join(data_09_17, by = c("ID" = "MUN"))

data_13_17<- data_13_17%>%
  mutate(MUN = paste0("13", MUN))
map_13_17 <- mex_13_17 %>%
  left_join(data_13_17, by = c("ID" = "MUN"))
data_15_17<- data_15_17%>%
  mutate(MUN = paste0("15", MUN))
map_15_17 <- mex_15_17 %>%
  left_join(data_15_17, by = c("ID" = "MUN"))

map_15_07 <- mex_15_07 %>%
  left_join(data_15_07, by = c("CVE_MUN" = "MUN"))

## pre-processing
complete_17 <- rbind(map_09_17, map_13_17, map_15_17)
complete_17<- complete_17 %>%
  filter(!is.na(metro_only_count))

complete_07 <- rbind(map_09_07, map_15_07)
complete_07<- complete_07 %>%
  filter(!is.na(metro_only_count))


## 2007 ready to use
metro_07 <- complete_07 %>%
  mutate(n_cat = ntile(metro_only_count, 5)) %>%   # split into 5 equal quantile bins
  group_by(n_cat) %>%
  mutate(label_range = paste0(min(metro_only_count), " – ", max(metro_only_count))) %>%
  ungroup() %>%
  mutate(
    n_cat = factor(n_cat, levels = sort(unique(n_cat))),
    label_range = factor(label_range, levels = unique(label_range[order(n_cat)]))
  )

bus_07 <- complete_07 %>%
  mutate(n_cat = ntile(bus_only_count, 5)) %>%   # split
  group_by(n_cat) %>%
  mutate(label_range = paste0(min(bus_only_count), " – ", max(bus_only_count))) %>%
  ungroup() %>%
  mutate(
    n_cat = factor(n_cat, levels = sort(unique(n_cat))),
    label_range = factor(label_range, levels = unique(label_range[order(n_cat)]))
  )
bus_metro_07 <- complete_07 %>%
  mutate(n_cat = ntile(bus_metro_count, 5)) %>% 
  group_by(n_cat) %>%
  mutate(label_range = paste0(min(bus_metro_count), " – ", max(bus_metro_count))) %>%
  ungroup() %>%
  mutate(
    n_cat = factor(n_cat, levels = sort(unique(n_cat))),
    label_range = factor(label_range, levels = unique(label_range[order(n_cat)]))
  )


## 2017 ready to use
metro_17 <- complete_17 %>%
  mutate(n_cat = ntile(metro_only_count, 6)) %>%  
  group_by(n_cat) %>%
  mutate(label_range = paste0(min(metro_only_count), " – ", max(metro_only_count))) %>%
  ungroup() %>%
  mutate(
    n_cat = factor(n_cat, levels = sort(unique(n_cat))),
    label_range = factor(label_range, levels = unique(label_range[order(n_cat)]))
  )
bus_17 <- complete_17 %>%
  mutate(n_cat = ntile(bus_only_count, 5)) %>%
  group_by(n_cat) %>%
  mutate(label_range = paste0(min(bus_only_count), " – ", max(bus_only_count))) %>%
  ungroup() %>%
  mutate(
    n_cat = factor(n_cat, levels = sort(unique(n_cat))),
    label_range = factor(label_range, levels = unique(label_range[order(n_cat)]))
  )
bus_metro_17 <- complete_17 %>%
  mutate(n_cat = ntile(bus_metro_count, 5)) %>%
  group_by(n_cat) %>%
  mutate(label_range = paste0(min(bus_metro_count), " – ", max(bus_metro_count))) %>%
  ungroup() %>%
  mutate(
    n_cat = factor(n_cat, levels = sort(unique(n_cat))),
    label_range = factor(label_range, levels = unique(label_range[order(n_cat)]))
  )

# st_write(metro_07, "metro_only_07.geojson")
# st_write(bus_07, "bus_only_07.geojson")
# st_write(bus_metro_07, "bus_metro_07.geojson")
# st_write(metro_17, "metro_only_17.geojson")
# st_write(bus_17, "bus_only_17.geojson")
# st_write(bus_metro_17, "bus_metro_17.geojson")
