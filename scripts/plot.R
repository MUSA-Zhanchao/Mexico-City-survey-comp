library(tidyverse)
library(lubridate)
library(ggrepel)
library(patchwork)
library(scales)

tables<- read.csv("data/Mode_Choice_Comparison_Summary.csv")

percent_2007<-tables%>%
  filter(!is.na(X2007))

percent_2007 <- percent_2007 %>%
  mutate(X2007 = parse_number(X2007))

ggplot(percent_2007, aes(x = reorder(x, X2007), y = X2007, fill = x)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Percentage of Trips by Mode in 2007",
       x = "Mode of Transportation",
       y = "Percentage of Trips (%)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  geom_text(aes(label = paste0(X2007, "%")), hjust = -0.1, size= 3) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(5.5, 20, 5.5, 5.5),
        legend.position = "none")

maxy <- max(percet_2007$X2007, na.rm = TRUE)

ggplot(percet_2007, aes(x = reorder(x, X2007), y = X2007, fill = x)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(X2007, "%")), hjust = -0.1, size = 3) +
  coord_flip(clip = "off") +  # let text draw outside the panel
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, maxy * 1.12),                 # add headroom
    expand = expansion(mult = c(0.02, 0.12))    # extra space on the right
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Percentage of Trips by Mode in 2007",
       x = "Mode of Transportation",
       y = "Percentage of Trips (%)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 20, 5.5, 5.5)     # a bit more right margin
  )

percent<-tables%>%
  filter(!is.na(X2017))
percent <- percent %>%
  mutate(X2017 = parse_number(X2017))

# special accomodation
percent<- percent %>%
  filter(x != "Walk")

percent<-percent%>%
  mutate(X2017= X2017/sum(X2017)*100)
percent<-percent%>%
  mutate(X2017= round(X2017,2))
maxy2 <- max(percent$X2017, na.rm = TRUE)
ggplot(percent, aes(x = reorder(x, X2017), y = X2017, fill = x)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(X2017, "%")), hjust = -0.1, size = 3) +
  coord_flip(clip = "off") +  # let text draw outside the panel
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, maxy2 * 1.12),                 # add headroom
    expand = expansion(mult = c(0.02, 0.12))    # extra space on the right
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Percentage of Trips by Mode in 2017",
       x = "Mode of Transportation",
       y = "Percentage of Trips (%)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 20, 5.5, 5.5)     # a bit more right margin
  )

metro<-tables%>%
  filter(x== "Metro & Light rail")

metro_2007<- metro %>%
  filter(!is.na(count2)) %>%
  mutate(percent_metro_2007 = count2/sum(count2))

maxy3 <- max(metro_2007$percent_metro_2007, na.rm = TRUE)

ggplot(metro_2007,
       aes(x = reorder(multi_label, percent_metro_2007),
           y = percent_metro_2007,
           fill = multi_label)) +
  geom_col() +
  geom_text(aes(label = scales::percent(percent_metro_2007, accuracy = 0.1)),
            hjust = -0.1, size = 2.8) +  # smaller text
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = scales::percent_format(),          # assumes data are 0–1
    limits = c(0, maxy3 * 1.12),
    expand = expansion(mult = c(0.02, 0.14))    # a bit more room for labels
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Percentage of  Multimodal Metro & Light Rail Trips in 2007",
       x = "Metro & Light Rail Lines",
       y = "Percentage of Trips") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 28, 5.5, 5.5)     # extra right margin for text
  )


metro_2017<- metro %>%
  filter(!is.na(count_2)) %>%
  mutate(percent_metro_2017 = count_2/sum(count_2))

maxy4 <- max(metro_2017$percent_metro_2017, na.rm = TRUE)

ggplot(metro_2017,
       aes(x = reorder(multi_la_17, percent_metro_2017),
           y = percent_metro_2017,
           fill = multi_label)) +
  geom_col() +
  geom_text(aes(label = scales::percent(percent_metro_2017, accuracy = 0.1)),
            hjust = -0.1, size = 2.8) +  # smaller text
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = scales::percent_format(),          # assumes data are 0–1
    limits = c(0, maxy4 * 1.12),
    expand = expansion(mult = c(0.02, 0.14))    # a bit more room for labels
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Percentage of  Multimodal Metro & Light Rail Trips in 2017",
       x = "Metro & Light Rail Lines",
       y = "Percentage of Trips") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 28, 5.5, 5.5)     # extra right margin for text
  )

brt_2017<- tables %>%
  filter(x== "BRT") %>%
  filter(!is.na(count_2)) %>%
  mutate(percent_brt_2017 = count_2/sum(count_2))

maxy5 <- max(brt_2017$percent_brt_2017, na.rm = TRUE)

ggplot(brt_2017,
       aes(x = reorder(multi_la_17, percent_brt_2017),
           y = percent_brt_2017,
           fill = multi_label)) +
  geom_col() +
  geom_text(aes(label = scales::percent(percent_brt_2017, accuracy = 0.1)),
            hjust = -0.1, size = 2.8) +  # smaller text
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = scales::percent_format(),          # assumes data are 0–1
    limits = c(0, maxy5 * 1.12),
    expand = expansion(mult = c(0.02, 0.14))    # a bit more room for labels
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Percentage of  Multimodal BRT Trips in 2017",
       x = "BRT Lines",
       y = "Percentage of Trips") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 28, 5.5, 5.5)     # extra right margin for text
  )

brt_2007<- tables %>%
  filter(x== "BRT") %>%
  filter(!is.na(count2)) %>%
  mutate(percent_brt_2007 = count2/sum(count2))

maxy6 <- max(brt_2007$percent_brt_2007, na.rm = TRUE)

ggplot(brt_2007,
       aes(x = reorder(multi_label, percent_brt_2007),
           y = percent_brt_2007,
           fill = multi_label)) +
  geom_col() +
  geom_text(aes(label = scales::percent(percent_brt_2007, accuracy = 0.1)),
            hjust = -0.1, size = 2.8) +  # smaller text
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = scales::percent_format(),          # assumes data are 0–1
    limits = c(0, maxy6 * 1.12),
    expand = expansion(mult = c(0.02, 0.14))    # a bit more room for labels
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Percentage of  Multimodal BRT Trips in 2007",
       x = "BRT Lines",
       y = "Percentage of Trips") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 28, 5.5, 5.5)     # extra right margin for text
  )

non_brt_2007<- tables %>%
  filter(x== "Non BRT bus") %>%
  filter(!is.na(count2)) %>%
  mutate(percent_non_brt_2007 = count2/sum(count2))
maxy7 <- max(non_brt_2007$percent_non_brt_2007, na.rm = TRUE)
ggplot(non_brt_2007,
       aes(x = reorder(multi_label, percent_non_brt_2007),
           y = percent_non_brt_2007,
           fill = multi_label)) +
  geom_col() +
  geom_text(aes(label = scales::percent(percent_non_brt_2007, accuracy = 0.1)),
            hjust = -0.1, size = 2.8) +  # smaller text
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = scales::percent_format(),          # assumes data are 0–1
    limits = c(0, maxy7 * 1.12),
    expand = expansion(mult = c(0.02, 0.14))    # a bit more room for labels
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Percentage of  Multimodal Non BRT Bus Trips in 2007",
       x = "Non BRT Bus Lines",
       y = "Percentage of Trips") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 28, 5.5, 5.5)     # extra right margin for text
  )

non_brt_2017<- tables %>%
  filter(x== "Non BRT bus") %>%
  filter(!is.na(count_2)) %>%
  mutate(percent_non_brt_2017 = count_2/sum(count_2))
maxy8 <- max(non_brt_2017$percent_non_brt_2017, na.rm = TRUE)
ggplot(non_brt_2017,
       aes(x = reorder(multi_la_17, percent_non_brt_2017),
           y = percent_non_brt_2017,
           fill = multi_label)) +
  geom_col() +
  geom_text(aes(label = scales::percent(percent_non_brt_2017, accuracy = 0.1)),
            hjust = -0.1, size = 2.8) +  # smaller text
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = scales::percent_format(),          # assumes data are 0–1
    limits = c(0, maxy8 * 1.12),
    expand = expansion(mult = c(0.02, 0.14))    # a bit more room for labels
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Percentage of  Multimodal Non BRT Bus Trips in 2017",
       x = "Non BRT Bus Lines",
       y = "Percentage of Trips") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 28, 5.5, 5.5)     # extra right margin for text
  )

volume<- read.csv("binary-distri.csv")
volume<- volume %>%
  mutate(volume_2007= volume_2007/ 100)
maxy8 <- max(volume$volume_2007, na.rm = TRUE)
ggplot(volume,
       aes(x = reorder(type_2007, volume_2007),
           y = volume_2007,
           fill = type_2007)) +
  geom_col() +
  geom_text(aes(label = scales::percent(volume_2007, accuracy = 0.1)),
            hjust = -0.1, size = 2.8) +  # smaller text
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = scales::percent_format(),          # assumes data are 0–1
    limits = c(0, maxy8 * 1.12),
    expand = expansion(mult = c(0.02, 0.14))    # a bit more room for labels
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Multimodal bus vs single ride in 2007",
       x = "type",
       y = "Percentage of Trips") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 28, 5.5, 5.5)     # extra right margin for text
  )


volume<- volume %>%
  mutate(volume_2017= volume_2017/ 100)
maxy9 <- max(volume$volume_2017, na.rm = TRUE)
ggplot(volume,
       aes(x = reorder(type_2017, volume_2017),
           y = volume_2017,
           fill = type_2017)) +
  geom_col() +
  geom_text(aes(label = scales::percent(volume_2017, accuracy = 0.1)),
            hjust = -0.1, size = 2.8) +  # smaller text
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = scales::percent_format(),          # assumes data are 0–1
    limits = c(0, maxy9 * 1.12),
    expand = expansion(mult = c(0.02, 0.14))    # a bit more room for labels
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Multimodal bus vs single ride in 2017",
       x = "type",
       y = "Percentage of Trips") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 28, 5.5, 5.5)     # extra right margin for text
  )
