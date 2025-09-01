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
