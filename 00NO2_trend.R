#NO2 Trend
library(tidyverse)
library(xts)
library(lubridate)
library(broom)
library(gridExtra)
library(modelr)
options(digits = 3)
load("~/Documents/Luftqualitaet/Analysen/BW_station.RData")
levels(BW_stations_NO2_tbl$name)<- levels(BW_stations_NO2_tbl$name) %>% 
  recode("Rt_"="Rt_pomol","Rtl"="Rt_leder",
         "Lbg_4"="Lbg_weimar","Frei"= "Freiburg",
         "Heid"="Heidelbg","Heil"="Heilbrn") 
BW_stations_NO2_tbl$NO2 <- BW_stations_NO2_tbl$NO2 %>% na.locf()
BW_stations_NO2_tbl$station <- BW_stations_NO2_tbl$station %>% as_factor()
jahrs <- 365*24*60*60 # Jahr in sec
# working with broom
df_15 <-BW_stations_NO2_tbl %>% group_by(name) %>% 
  do(tidy(lm(NO2 ~ datetime,data =.),conf.int= TRUE)) %>%
  filter(term == "datetime") %>%
  select(name,estimate,conf.low,conf.high)%>%
  mutate(estimate = estimate*jahrs,
         conf.low = conf.low*jahrs,
         conf.high= conf.high*jahrs)%>% 
  as_tibble() %>%
  arrange(., estimate) #%>%
df_15 %>%  ggplot(.,aes(x = reorder(name,estimate),y = estimate,ymin =conf.high,ymax= conf.low))+
  geom_errorbar()+
  geom_point()+
  ggtitle(" Reduzierung NO2 -Immissionen
      Jahresrate 2000 bis 2020")+
  labs(y = "NO2 [μg/m3]pro Jahr", x = "Messstation") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
