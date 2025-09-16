library(tidyverse)
library(lubridate)
library(ggrepel)
library(patchwork)
library(scales)

table<-read.csv("data/mex.csv")

percent_2007<-table%>%
  filter(!is.na(X2007))

percent_2007 <- percent_2007 %>%
  mutate(X2007 = parse_number(X2007))

maxy <- max(percent_2007$X2007, na.rm = TRUE)

ggplot(percent_2007, aes(x = reorder(x, X2007), y = X2007, fill = x)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(X2007, "%")), hjust = -0.1, size = 3) +
  coord_flip(clip = "off") +  # let text draw outside the panel
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, maxy * 1.12),                 # add headroom
    expand = expansion(mult = c(0.02, 0.12))    # extra space on the right
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Percentage of Trips by Mode in 2007 (Mexico City)",
       x = "Mode of Transportation",
       y = "Percentage of Trips (%)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 20, 5.5, 5.5)     # a bit more right margin
  )


percent<-table%>%
  filter(!is.na(X2017))
percent <- percent %>%
  mutate(X2017 = parse_number(X2017))

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
  labs(title = "Percentage of Trips by Mode in 2017 (Mexico City)",
       x = "Mode of Transportation",
       y = "Percentage of Trips (%)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 20, 5.5, 5.5)     # a bit more right margin
  )

metro<-table%>%
  filter(x== "Metro & Light rail")

metro_2007<- metro %>%
  filter(!is.na(count2)) %>%
  mutate(percent_metro_2007 = round(count2/sum(count2),4))

maxy3 <- max(metro_2007$percent_metro_2007, na.rm = TRUE)

ggplot(metro_2007,
       aes(x = reorder(multi_label_2007, percent_metro_2007),
           y = percent_metro_2007,
           fill = multi_label_2007)) +
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
  labs(title = "Percentage of  Multimodal Metro Trips in 2007 (Mexico City)",
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
           fill = multi_la_17)) +
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
  labs(title = "Percentage of Multimodal Metro Trips in 2017 (Mexico City)",
       x = "Metro & Light Rail Lines",
       y = "Percentage of Trips") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 28, 5.5, 5.5)     # extra right margin for text
  )

BRT_2017<- table %>%
  filter(x== "BRT") %>%
  filter(!is.na(count_2)) %>%
  mutate(percent_brt_2017 = round(count_2/sum(count_2),4))

maxy5 <- max(BRT_2017$percent_brt_2017, na.rm = TRUE)

ggplot(BRT_2017,
       aes(x = reorder(multi_la_17, percent_brt_2017),
           y = percent_brt_2017,
           fill = multi_la_17)) +
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
  labs(title = "Percentage of  Multimodal BRT Trips in 2017 (Mexico City)",
       x = "BRT Lines",
       y = "Percentage of Trips") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 28, 5.5, 5.5)     # extra right margin for text
  )

brt_2007<- table %>%
  filter(x== "BRT") %>%
  filter(!is.na(count2)) %>%
  mutate(percent_brt_2007 = count2/sum(count2))

maxy6 <- max(brt_2007$percent_brt_2007, na.rm = TRUE)
ggplot(brt_2007,
       aes(x = reorder(multi_label_2007, percent_brt_2007),
           y = percent_brt_2007,
           fill = multi_label_2007)) +
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
  labs(title = "Percentage of Multimodal BRT Trips in 2007 (Mexico City)",
       x = "BRT Lines",
       y = "Percentage of Trips") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 28, 5.5, 5.5)     # extra right margin for text
  )

non_brt_2007<- table %>%
  filter(x== "Non BRT bus") %>%
  filter(!is.na(count2)) %>%
  mutate(percent_non_brt_2007 = count2/sum(count2))
maxy7 <- max(non_brt_2007$percent_non_brt_2007, na.rm = TRUE)
ggplot(non_brt_2007,
       aes(x = reorder(multi_label_2007, percent_non_brt_2007),
           y = percent_non_brt_2007,
           fill = multi_label_2007)) +
  geom_col() +
  geom_text(aes(label = scales::percent(percent_non_brt_2007, accuracy= 0.1)),
            hjust = -0.1, size = 2.8) +
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = scales::percent_format(),          # assumes data are 0–1
    limits = c(0, maxy7 * 1.12),
    expand = expansion(mult = c(0.02, 0.14))    #
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Percentage of Multimodal Non-BRT Bus Trips in 2007
       (Mexico City)",
       x = "Non BRT Bus Lines",
       y = "Percentage of Trips") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 28, 5.5, 5.5)     # extra right margin for text
  )
non_brt_2017<- table %>%
  filter(x== "Non BRT bus") %>%
  filter(!is.na(count_2)) %>%
  mutate(percent_non_brt_2017 = count_2/sum(count_2))
maxy8 <- max(non_brt_2017$percent_non_brt_2017)
ggplot(non_brt_2017,
       aes(x = reorder(multi_la_17, percent_non_brt_2017),
           y = percent_non_brt_2017,
           fill = multi_label_2007)) +
  geom_col() +
  geom_text(aes(label = scales::percent(percent_non_brt_2017, accuracy= 0.1)),
            hjust = -0.1, size = 2.8) +
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = scales::percent_format(),          # assumes data are 0–1
    limits = c(0, maxy8 * 1.12),
    expand = expansion(mult = c(0.02, 0.14))    #
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Percentage of Multimodal Non-BRT Bus Trips in 2017
       (Mexico City)",
       x = "Non BRT Bus Lines",
       y = "Percentage of Trips") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 28, 5.5, 5.5)     # extra right margin for text
  )
highcap<-read.csv("data/high-cap-mex.csv")

maxy9 <- max(highcap$X2007, na.rm = TRUE)
ggplot(
  highcap,
  aes(x = reorder(x, X2007), y = X2007, fill = x)
) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(X2007, "%")), hjust = -0.1, size = 3) +
  coord_flip(clip = "off") +  # let text draw outside the panel
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, maxy9 * 1.12),                 # add headroom
    expand = expansion(mult = c(0.02, 0.12))    # extra space on the right
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Percentage of Trips by High-Capacity Mode in 2007 (Mexico City)",
    x = "Mode of Transportation",
    y = "Percentage of Trips (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 20, 5.5, 5.5)     # a bit more right margin
  )
maxy10 <- max(highcap$X2017, na.rm = TRUE)
ggplot(
  highcap,
  aes(x = reorder(x, X2017), y = X2017, fill =
        x)
) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(X2017, "%")), hjust = -
              0.1, size = 3) +
  coord_flip(clip = "off") +  # let text draw outside the panel
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, maxy10 * 1.12),                 # add
    expand = expansion(mult = c(0.02, 0.12))    # extra space on the right
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Percentage of Trips by High-Capacity Mode in 2017 (Mexico
    City)",
    x = "Mode of Transportation",
    y = "Percentage of Trips (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 20, 5.5,
                         5.5)     # a bit more right margin
  )
