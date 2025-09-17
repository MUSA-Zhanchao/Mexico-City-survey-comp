library(sf)
library(tidyverse)
library(foreign)

municipio_09<-st_read("mapping/MEX_Municipio_09.geojson")
municipio_13<-st_read("mapping/MEX_Municipio_13.geojson")
municipio_15<-st_read("mapping/MEX_Municipio_15.geojson")

hogar_2017<- read.dbf("data/2017/THOGAR.DBF", as.is = TRUE)

hogar_mun_15<- hogar_2017 %>%
  filter(ENT == "15") %>%
  group_by(MUN)%>%
  summarise(n = n())

hogar_mun_15<- hogar_mun_15%>%
  mutate(MUN = paste0("15", MUN))

complete_mun_15<- left_join(municipio_15, hogar_mun_15, by = c("ID" = "MUN"))

complete_mun_15<- complete_mun_15 %>%
  filter(!is.na(n))





hogar_mun_13<- hogar_2017 %>%
  filter(ENT == "13") %>%
  group_by(MUN)%>%
  summarise(n = n())
hogar_mun_13<- hogar_mun_13%>%
  mutate(MUN = paste0("13", MUN))
complete_mun_13<- left_join(municipio_13, hogar_mun_13, by = c("ID" = "MUN"))
complete_mun_13<- complete_mun_13 %>%
  filter(!is.na(n))

hogar_mun_09<- hogar_2017 %>%
  filter(ENT == "09") %>%
  group_by(MUN)%>%
  summarise(n = n())
hogar_mun_09<- hogar_mun_09%>%
  mutate(MUN = paste0("09", MUN))
complete_mun_09<- left_join(municipio_09, hogar_mun_09, by = c("ID" = "MUN"))
complete_mun_09<- complete_mun_09 %>%
  filter(!is.na(n))
complete<-rbind(complete_mun_09,complete_mun_15,complete_mun_13)



complete <- complete %>%
  mutate(n_cat = ntile(n, 5)) %>%   # split into 5 equal quantile bins
  group_by(n_cat) %>%
  mutate(label_range = paste0(min(n), " – ", max(n))) %>%
  ungroup() %>%
  mutate(
    n_cat = factor(n_cat, levels = sort(unique(n_cat))),
    label_range = factor(label_range, levels = unique(label_range[order(n_cat)]))
  )

ggplot(complete, aes(fill = n_cat)) +
  geom_sf() +
  scale_fill_manual(
    values = c(
      "1" = "#0081a7",
      "2" = "#00afb9",
      "3" = "#fdfcdc",
      "4" = "#fed9b7",
      "5" = "#f07167"
    ),
    labels = levels(complete$label_range), # use ordered raw ranges
    guide = guide_legend(title = "Sample Size")
  ) +
  labs(
    title = "Sample size of the Survey (2017)",
  ) +
  theme(
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 14),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.subtitle = element_text(size = 13, face = "italic"),
    plot.title = element_text(size = 35, hjust = 0.5, face = "bold"),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "grey", fill = NA, linewidth = 0.8)
  )



##### 2007


vivida<- read.dbf("data/2007/TVIVIENDA.DBF", as.is = TRUE)

vivida_mun_15<- vivida %>%
  filter(ENT == 15) %>%
  group_by(MUN)%>%
  summarise(n = n())

vivida_mun_15<- vivida_mun_15%>%
  mutate(MUN = paste0("15", MUN))
complete_mun_15_07<- left_join(municipio_15, vivida_mun_15, by = c("ID" = "MUN"))
complete_mun_15_07<- complete_mun_15_07 %>%
  filter(!is.na(n))

vivida_mun_13<- vivida %>%
  filter(ENT == 13) %>%
  group_by(MUN)%>%
  summarise(n = n())
vivida_mun_13<- vivida_mun_13%>%
  mutate(MUN = paste0("13", MUN))
complete_mun_13_07<- left_join(municipio_13, vivida_mun_13, by= c("ID" = "MUN"))
complete_mun_13_07<- complete_mun_13_07 %>%
  filter(!is.na(n))

vivida_mun_09<- vivida %>%
  filter(ENT == "09") %>%
  group_by(MUN)%>%
  summarise(n = n())
vivida_mun_09<- vivida_mun_09%>%
  mutate(MUN = paste0("09", MUN))
complete_mun_09_07<- left_join(municipio_09, vivida_mun_09, by = c("ID" = "MUN"))
complete_mun_09<- complete_mun_09_07 %>%
  filter(!is.na(n))
complete_07<-rbind(complete_mun_09_07,complete_mun_13_07,complete_mun_15_07)
complete_07 <- complete_07 %>%
  mutate(n_cat = ntile(n, 5)) %>%   # split into 5 equal quantile bins
  group_by(n_cat) %>%
  mutate(label_range = paste0(min(n), " – ", max(n))) %>%
  ungroup() %>%
  mutate(
    n_cat = factor(n_cat, levels = sort(unique(n_cat))),
    label_range = factor(label_range, levels = unique(label_range[order(n_cat)]))
  )

ggplot(complete_07, aes(fill = n_cat)) +
  geom_sf() +
  scale_fill_manual(
    values = c(
      "1" = "#0081a7",
      "2" = "#00afb9",
      "3" = "#fdfcdc",
      "4" = "#fed9b7",
      "5" = "#f07167"
    ),
    labels = levels(complete_07$label_range), # use ordered raw ranges
    guide = guide_legend(title = "Sample Size")
  ) +
  labs(
    title = "Sample size of the Survey (2007)",
  ) +
  theme(
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 14),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.subtitle = element_text(size = 13, face = "italic"),
    plot.title = element_text(size = 35, hjust = 0.5,
                              face = "bold"),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "grey", fill = NA, linewidth =
                                  0.8)
  )

