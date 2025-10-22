city_to_city2007<-read.csv("data/city-city-suburb/2007_city_to_city_mode_summary.csv")
city_to_city2017<-read.csv("data/city-city-suburb/2017_city_to_city_mode_breakdown.csv")
suburb_to_suburb2007<-read.csv("data/city-city-suburb/2007_suburb_to_suburb_mode_summary.csv")
suburb_to_suburb2017<-read.csv("data/city-city-suburb/suburb_mode_summary_2017.csv")

library(tidyverse)
#filter walk in 2017
city_to_city2017<-city_to_city2017 %>%
  filter(mode!="Walk")
# sum(city_to_city2017$n)
# sum(city_to_city2007$count)
suburb_to_suburb2017<-suburb_to_suburb2017 %>%
  filter(mode!="Walk")
# sum(suburb_to_suburb2017$n)
# sum(suburb_to_suburb2007$count)

# single mode, the mode don't have _
city_to_city2017_single<-city_to_city2017 %>%
  filter(!str_detect(mode, "_")) %>%
  group_by(mode) %>%
  summarise(n = sum(n))


city_to_city2007_single<-city_to_city2007 %>%
  filter(!str_detect(mode_key, "_")) %>%
  group_by(mode_key) %>%
  summarise(n = sum(count))

suburb_to_suburb2017_single<-suburb_to_suburb2017 %>%
  filter(!str_detect(mode, "_")) %>%
  group_by(mode) %>%
  summarise(n = sum(n))
suburb_to_suburb2007_single<-suburb_to_suburb2007 %>%
  filter(!str_detect(mode_key, "_")) %>%
  group_by(mode_key) %>%
  summarise(n = sum(count))

ggplot()+
  geom_bar(data=city_to_city2007_single, aes(x=mode_key, y=n, fill= mode_key), stat="identity", position="dodge")+
  labs(title="City to City Single Mode Trips Comparison 2007",
       x="Transportation Mode",
       y="Number of Trips",
       fill="Year")+
  theme_minimal()
ggplot()+
  geom_bar(data=city_to_city2017_single, aes(x=mode, y=n, fill= mode), stat="identity", position="dodge")+
  labs(title="City to City Single Mode Trips Comparison 2017",
       x="Transportation Mode",
       y="Number of Trips",
       fill="Year")+
  theme_minimal()


city_to_city2017_multi<-city_to_city2017 %>%
  filter(str_detect(mode, "_"))
city_to_city2007_multi<-city_to_city2007 %>%
  filter(str_detect(mode_key, "_"))    

