library(tidyverse)
library(lubridate)
library(ggrepel)
library(patchwork)
library(scales)

tables<-read.csv("data/weighted_summary.csv")

trip2007<- tables%>%
  filter(!is.na(count))
trip2007<- trip2007 %>%
  filter(x!= "Walk")%>%
  select(x, count)%>%
  mutate(percent= round(count/sum(count)*100,2))

maxy <- max(trip2007$percent, na.rm = TRUE)

ggplot(trip2007, aes(x = reorder(x, percent), y = percent, fill = x)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(percent, "%")), hjust = -0.1, size = 3) +
  coord_flip(clip = "off") +  # let text draw outside the panel
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, maxy * 1.12),                 # add headroom
    expand = expansion(mult = c(0.02, 0.12))    # extra space on the right
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = " Weighted Percentage of Trips by Mode in 2007",
       x = "Mode of Transportation",
       y = "Percentage of Trips (%)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 20, 5.5, 5.5)     # a bit more right margin
  )

trip2017<- tables%>%
  filter(!is.na(count_1))
trip2017<- trip2017 %>%
  filter(x!= "Walk")%>%
  select(x, count_1)%>%
  mutate(percent= round(count_1/sum(count_1)*100,2))

maxy2 <- max(trip2017$percent, na.rm = TRUE)
ggplot(trip2017, aes(x = reorder(x, percent), y = percent, fill = x)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(percent, "%")), hjust = -0.1, size = 3) +
  coord_flip(clip = "off") +  # let text draw outside the panel
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, maxy2 * 1.12),                 # add headroom
    expand = expansion(mult = c(0.02, 0.12))    # extra space on the right
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = " Weighted Percentage of Trips by Mode in 2017",
       x = "Mode of Transportation",
       y = "Percentage of Trips (%)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 20, 5.5, 5.5)     # a bit more right margin
  )

metro<- tables%>%
  filter(x=="Metro & Light rail")%>%
  select(multi_label,count2,multi_la_17,count_2)
metro<- metro %>%
  mutate(percent_2007= round(count2/sum(count2)*100,2),
         percent_2017= round(count_2/sum(count_2)*100,2))

maxy3 <- max(metro$percent_2007, na.rm = TRUE)
maxy4 <- max(metro$percent_2017, na.rm = TRUE)

ggplot(metro,
       aes(x = reorder(multi_label, percent_2007),
           y = percent_2007,
           fill = multi_label)) +
  geom_col() +
  geom_text(aes(label = percent_2007),
            hjust = -0.1, size = 2.8) +  # smaller text
  coord_flip(clip = "off") +
  scale_y_continuous(         # assumes data are 0–1
    limits = c(0, maxy3 * 1.12),
    expand = expansion(mult = c(0.02, 0.14))    # a bit more room for labels
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Weighted Percentage of  Multimodal Metro & Light Rail Trips in 2007",
       x = "Mode",
       y = "Percentage of Trips") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 28, 5.5, 5.5)     # extra right margin for text
  )

ggplot(metro,
       aes(x = reorder(multi_la_17, percent_2017),
           y = percent_2017,
           fill = multi_la_17)) +
  geom_col() +
  geom_text(aes(label = percent_2017),
            hjust = -0.1, size = 2.8) +  # smaller text
  coord_flip(clip = "off") +
  scale_y_continuous(         # assumes data are 0–1
    limits = c(0, maxy4 * 1.12),
    expand = expansion(mult = c(0.02, 0.14))    # a bit more room for labels
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Weighted Percentage of  Multimodal Metro & Light Rail Trips in 2017",
       x = "Mode",
       y = "Percentage of Trips") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 28, 5.5, 5.5)     # extra right margin for text
  )

BRT<- tables%>%
  filter(x=="BRT")%>%
  select(multi_label,count2,multi_la_17,count_2)
BRT<- BRT %>%
  mutate(percent_2007= round(count2/sum(count2)*100,2),
         percent_2017= round(count_2/sum(count_2)*100,2))
maxy5 <- max(BRT$percent_2007, na.rm = TRUE)
maxy6 <- max(BRT$percent_2017, na.rm = TRUE)

ggplot(BRT,
       aes(x = reorder(multi_label, percent_2007),
           y = percent_2007,
           fill = multi_label)) +
  geom_col() +
  geom_text(aes(label = percent_2007),
            hjust = -0.1, size = 2.8) +  # smaller text
  coord_flip(clip = "off") +
  scale_y_continuous(         # assumes data are 0–1
    limits = c(0, maxy5 * 1.12),
    expand = expansion(mult = c(0.02, 0.14))    # a bit more room for labels
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Weighted Percentage of Multimodal BRT Trips in 2007",
       x = "Mode",
       y = "Percentage of Trips") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 28, 5.5, 5.5)     # extra right margin for text
  )
ggplot(BRT,
       aes(x = reorder(multi_la_17, percent_2017),
           y = percent_2017,
           fill = multi_la_17)) +
  geom_col() +
  geom_text(aes(label = percent_2017),
            hjust = -0.1, size = 2.8) +  # smaller text
  coord_flip(clip = "off") +
  scale_y_continuous(         # assumes data are 0–1
    limits = c(0, maxy6 * 1.12),
    expand = expansion(mult = c(0.02, 0.14))    # a bit more room for labels
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Weighted Percentage of Multimodal BRT Trips in 2017",
       x = "Mode",
       y = "Percentage of Trips") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 28, 5.5, 5.5)     # extra right margin for text
  )

bus<- tables%>%
  filter(x=="Non BRT bus")%>%
  select(multi_label,count2,multi_la_17,count_2)%>%
  filter(!is.na(count2))

bus<- bus %>%
  mutate(percent_2007= round(count2/sum(count2)*100,2),
         percent_2017= round(count_2/sum(count_2)*100,2))
maxy7 <- max(bus$percent_2007, na.rm = TRUE)
maxy8 <- max(bus$percent_2017, na.rm = TRUE)

ggplot(bus,
       aes(x = reorder(multi_label, percent_2007),
           y = percent_2007,
           fill = multi_label)) +
  geom_col() +
  geom_text(aes(label = percent_2007),
            hjust = -0.1, size = 2.8) +  # smaller text
  coord_flip(clip = "off") +
  scale_y_continuous(         # assumes data are 0–1
    limits = c(0, maxy7 * 1.12),
    expand = expansion(mult = c(0.02, 0.14))    # a bit more room for labels
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Weighted Percentage of Multimodal Non BRT Bus Trips in 2007",
       x = "Mode",
       y = "Percentage of Trips") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 28, 5.5, 5.5)     # extra right margin for text
  )

ggplot(bus,
       aes(x = reorder(multi_la_17, percent_2017),
           y = percent_2017,
           fill = multi_la_17)) +
  geom_col() +
  geom_text(aes(label = percent_2017),
            hjust = -0.1, size = 2.8) +  # smaller text
  coord_flip(clip = "off") +
  scale_y_continuous(         # assumes data are 0–1
    limits = c(0, maxy8 * 1.12),
    expand = expansion(mult = c(0.02, 0.14))    # a bit more room for labels
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Weighted Percentage of Multimodal Non BRT Bus Trips in 2017",
       x = "Mode",
       y = "Percentage of Trips") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 28, 5.5, 5.5)     # extra right margin for text
  )

high_cap<-read.csv("data/high_capacity_weighted.csv")

maxy9 <- max(high_cap$X2007, na.rm = TRUE)
maxy10 <- max(high_cap$X2017, na.rm = TRUE)

ggplot(high_cap,
       aes(x = reorder(type, X2007),
           y = X2007,
           fill = type)) +
  geom_col() +
  geom_text(aes(label = X2007),
            hjust = -0.1, size = 2.8) +  # smaller text
  coord_flip(clip = "off") +
  scale_y_continuous(         # assumes data are 0–1
    limits = c(0, maxy9 * 1.12),
    expand = expansion(mult = c(0.02, 0.14))    # a bit more room for labels
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Weighted High Capacity Transit Trips in 2007",
       x = "Mode",
       y = "Percent of Trips") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 28, 5.5, 5.5)     # extra right margin for text
  )

ggplot(high_cap,
       aes(x = reorder(type, X2017),
           y = X2017,
           fill = type)) +
  geom_col() +
  geom_text(aes(label = X2017),
            hjust = -0.1, size = 2.8) +  # smaller text
  coord_flip(clip = "off") +
  scale_y_continuous(         # assumes data are 0–1
    limits = c(0, maxy10 * 1.12),
    expand = expansion(mult = c(0.02, 0.14))    # a bit more room for labels
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Weighted High Capacity Transit Trips in 2017",
       x = "Mode",
       y = "Percent of Trips") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 28, 5.5, 5.5)     # extra right margin for text
  )
