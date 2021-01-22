# KOMP confounding
library(tidyverse)
library(lubridate)
library(broom)
library(xts)
load("/Users/alfloeffler/Documents/Luftqualitaet/Analysen/NO2/BW_17_stationen.RData")
summary(BW_station_data)
load("~/Documents/Luftqualitaet/Analysen/NO2/BW_stations_komp_WG.RData")
BW_stations_komp_WG %>% summary()
BW_data <- BW_stations_komp_WG
DATEN <- BW_data%>% 
  mutate(yearID = BW_data$datetime  %>% 
           format_ISO8601(precision = "y") %>%as.numeric() )
DATEN <- DATEN %>% filter(yearID %in% 2014:2019)
DATEN$NO2 <- DATEN$NO2 %>% replace_na(mean(.,na.rm= TRUE))
DATEN$NO_ <- DATEN$NO_ %>% replace_na(mean(.,na.rm= TRUE))
DATEN$O3 <- DATEN$O3 %>% replace_na(mean(.,na.rm= TRUE))
summary(DATEN)

# Auswahl der Daten von 2015 bis 2020
Daten_15_20 <- BW_station_data %>% 
  dplyr::select(name,yearID,NO_,NO2) %>%
  filter(yearID %in% 2015:2020) 
head(Daten_15_20,2)
 single_slope <- Daten_15_20 %>% filter(name == "Alb") %>% 
  lm(NO2 ~ NO_,data = .) %>% 
  .$coef %>%
  .[2]
# Datensätze mit NA eliminieren

Daten_15_20 <- Daten_15_20 %>% na.omit()


Regr_no2.no <-Daten_15_20 %>% group_by(name)%>% 
  summarise(slp = lm(NO2 ~ NO_)$coeff[2],
                  intcpt = lm (NO2 ~ NO_)$coeff[1])

NROW(Daten_15_20)
Korr <- Daten_15_20 %>% group_by(name) %>% 
  summarise (Korrelation =cor(NO2,NO_))
Korr %>% arrange(Korrelation) # zwischen 0.1 und 0.62
#weitere Korrelationen
#NO2 & O3
Daten_NO2_O3 <- BW_station_data %>% 
  dplyr::select(yearID,name,datetime,NO2,O3) %>% na.omit()
Korr_NO2_O3 <- Daten_NO2_O3 %>% group_by(name)%>%
  summarise (Korrelation =cor(NO2,O3)) %>% arrange(Korrelation)
# NO2 & Temp
NROW(DATEN)#393454
# Datensätze mit defektem Temperaturfühler eliminieren
DATEN_temp <- DATEN %>% filter (Temp < 45) 
DATEN_temp <- DATEN_temp %>% na.omit()
Korrelationen <- DATEN_temp %>% group_by(name)%>%
  summarise(Kor_no2.temp= cor(NO2,Temp),Kor_no.temp = cor(NO_,Temp),
            Kor_o3.temp = cor(O3,Temp))

DATEN_temp %>% filter(name == "Rt_") %>% 
  summarise (cor(O3,Temp))



