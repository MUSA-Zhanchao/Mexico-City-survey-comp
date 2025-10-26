city_to_city2007<-read.csv("data/city-city-suburb/2007_city_to_city_mode_summary.csv")
city_to_city2017<-read.csv("data/city-city-suburb/2017_city_to_city_mode_breakdown.csv")
suburb_to_suburb2007<-read.csv("data/city-city-suburb/2007_suburb_to_suburb_mode_summary.csv")
suburb_to_suburb2017<-read.csv("data/city-city-suburb/suburb_mode_summary_2017.csv")

library(tidyverse)
#filter walk in 2017
city_to_city2017<-city_to_city2017 %>%
  filter(mode!="Walk")
city_to_city2017_sum<- sum(city_to_city2017$n)
city_to_city2007_sum<- sum(city_to_city2007$count)
suburb_to_suburb2017<-suburb_to_suburb2017 %>%
  filter(mode!="Walk")
suburb_to_suburb2017_sum<- sum(suburb_to_suburb2017$n)
suburb_to_suburb2007_sum<- sum(suburb_to_suburb2007$count)

high_capacity_suburb2017<- read.csv("data/city-city-suburb/2017-suburb-high-capacity.csv")
high_capacity_suburb2007<- read.csv("data/city-city-suburb/2007-suburb-high-capacity.csv")

high_capacity_city2017<- read.csv("data/city-city-suburb/2017-city-high-capacity.csv")
high_capacity_city2007<- read.csv("data/city-city-suburb/2007_high_capacity-city.csv")

high_capacity_city2017<- high_capacity_city2017 %>%
  mutate(percentage = round(n / city_to_city2017_sum * 100),2)
high_capacity_city2007<- high_capacity_city2007 %>%
  mutate(percentage = round(count / city_to_city2007_sum * 100),2)
high_capacity_suburb2017<- high_capacity_suburb2017 %>%
  mutate(percentage = round(n / suburb_to_suburb2017_sum * 100),2)
high_capacity_suburb2007<- high_capacity_suburb2007 %>%
  mutate(percentage = round(count / suburb_to_suburb2007_sum * 100),2)
# Plotting
maxy9 <- max(high_capacity_city2017$percentage, na.rm = TRUE)
ggplot(
  high_capacity_city2017,
  aes(x = reorder(mode, percentage), y = percentage, fill = mode)
) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(percentage, "%")), hjust = -0.1, size = 3) +
  coord_flip(clip = "off") +  # let text draw outside the panel
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, maxy9 * 1.12),                 # add headroom
    expand = expansion(mult = c(0.02, 0.12))    # extra space on the right
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "High Capacity Transit Usage for City-to-City Trips in 2017",
    x = "Mode of Transportation",
    y = "Percentage of Trips (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 20, 5.5, 5.5)     # a bit more right margin
  )
maxy13 <- max(high_capacity_suburb2017$percentage, na.rm = TRUE)
ggplot(
  high_capacity_suburb2017,
  aes(x = reorder(mode, percentage), y = percentage, fill = mode)
) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(percentage, "%")), hjust = -0.1, size = 3) +
  coord_flip(clip = "off") +  # let text draw outside the panel
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, maxy13 * 1.12),                 # add headroom
    expand = expansion(mult = c(0.02, 0.12))    # extra space on the right
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "High Capacity Transit Usage for Suburb-to-Suburb Trips in 2017",
    x = "Mode of Transportation",
    y = "Percentage of Trips (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 20, 5.5, 5.5)     # a bit more right margin
  )

maxy15 <- max(high_capacity_city2007$percentage, na.rm = TRUE)
ggplot(
  high_capacity_city2007,
  aes(x = reorder(mode_key, percentage), y = percentage, fill = mode_key)
) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(percentage, "%")), hjust = -0.1, size = 3) +
  coord_flip(clip = "off") +  # let text draw outside the panel
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, maxy15 * 1.12),                 # add headroom
    expand = expansion(mult = c(0.02, 0.12))    # extra space on the right
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "High Capacity Transit Usage for City-to-City Trips in 2007",
    x = "Mode of Transportation",
    y = "Percentage of Trips (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 20, 5.5, 5.5)     # a bit more right margin
  )
maxy17 <- max(high_capacity_suburb2007$percentage, na.rm = TRUE)
ggplot(
  high_capacity_suburb2007,
  aes(x = reorder(mode_key, percentage), y = percentage, fill = mode_key)
) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(percentage, "%")), hjust = -0.1, size = 3) +
  coord_flip(clip = "off") +  # let text draw outside the panel
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, maxy17 * 1.12),                 # add headroom
    expand = expansion(mult = c(0.02, 0.12))    # extra space on the right
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "High Capacity Transit Usage for Suburb-to-Suburb Trips in 2007",
    x = "Mode of Transportation",
    y = "Percentage of Trips (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 20, 5.5, 5.5)     # a bit more right margin
  )

single_2007_city<- read.csv("data/city-city-suburb/2007_city_to_city_single_mode_summary.csv")
single_2007_suburb<- read.csv("data/city-city-suburb/2007_suburb_to_suburb_single_mode_summary.csv")
single_2017_city<- read.csv("data/city-city-suburb/2017_city_to_city_single_mode_summary.csv")
single_2017_suburb<- read.csv("data/city-city-suburb/2017_suburbtosuburb_single_mode_summary.csv")

single_2017_city<- single_2017_city %>%
  filter(mode!="Walk")%>%
  mutate(percentage = n / city_to_city2017_sum *100)%>%
  mutate(percentage = round(percentage,2))
single_2007_city<- single_2007_city %>%
  mutate(percentage = count / city_to_city2007_sum* 100)%>%
  mutate(percentage = round(percentage,2))
single_2017_suburb<- single_2017_suburb %>%
  filter(mode!="Walk")%>%
  mutate(percentage = n / suburb_to_suburb2017_sum* 100)%>%
  mutate(percentage = round(percentage,2))
single_2007_suburb<- single_2007_suburb %>%
  mutate(percentage = count / suburb_to_suburb2007_sum* 100)%>%
  mutate(percentage = round(percentage,2))
maxy1 <- max(single_2017_city$percentage, na.rm = TRUE)
ggplot(
  single_2017_city,
  aes(x = reorder(mode, percentage), y = percentage, fill = mode)
) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(percentage, "%")), hjust = -0.1, size = 3) +
  coord_flip(clip = "off") +  # let text draw outside the panel
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, maxy1 * 1.12),                 # add headroom
    expand = expansion(mult = c(0.02, 0.12))    # extra space on the right
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Single-Mode Trips for City-to-City Trips in 2017",
    x = "Mode of Transportation",
    y = "Percentage of Trips (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 20, 5.5, 5.5)     # a bit more right margin
  )
maxy2 <- max(single_2007_city$percentage, na.rm = TRUE)
ggplot(
  single_2007_city,
  aes(x = reorder(trip1, percentage), y = percentage, fill = trip1)
) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(percentage, "%")), hjust = -0.1, size = 3) +
  coord_flip(clip = "off") +  # let text draw outside the panel
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, maxy2 * 1.12),                 # add headroom
    expand = expansion(mult = c(0.02, 0.12))    # extra space on the right
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Single-Mode Trips for City-to-City Trips in 2007",
    x = "Mode of Transportation",
    y = "Percentage of Trips (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 20, 5.5, 5.5)     # a bit more right margin
  )
maxy3 <- max(single_2017_suburb$percentage, na.rm = TRUE)
ggplot(
  single_2017_suburb,
  aes(x = reorder(mode, percentage), y = percentage, fill = mode)
) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(percentage, "%")), hjust = -0.1, size = 3) +
  coord_flip(clip = "off") +  # let text draw outside the panel
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, maxy3 * 1.12),                 # add headroom
    expand = expansion(mult = c(0.02, 0.12))    # extra space on the right
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Single-Mode Trips for Suburb-to-Suburb Trips in 2017",
    x = "Mode of Transportation",
    y = "Percentage of Trips (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 20, 5.5, 5.5)     # a bit more right margin
  )
maxy4 <- max(single_2007_suburb$percentage, na.rm = TRUE)
ggplot(
  single_2007_suburb,
  aes(x = reorder(trip1, percentage), y = percentage, fill = trip1)
) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(percentage, "%")), hjust = -0.1, size = 3) +
  coord_flip(clip = "off") +  # let text draw outside the panel
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, maxy4 * 1.12),                 # add headroom
    expand = expansion(mult = c(0.02, 0.12))    # extra space on the right
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Single-Mode Trips for Suburb-to-Suburb Trips in 2007",
    x = "Mode of Transportation",
    y = "Percentage of Trips (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 20, 5.5, 5.5)     # a bit more right margin
  )
