library(tidyverse)
library(lubridate)
load("BW_stations_NO2_tbl.RData")

BW_stations_NO2_tbl %>% head()
BW_stations_NO2_tbl$name %>% as_factor()# Alb Brn Can Egg Frei Friedri Heid Heil Lbg_4 Lbg_Friedr Nck Odw Rt_ Rtl Sws
# Teillisten erstellen

station_names <- c("Alb","Brn","Can","Egg","Frei","Friedri","Heid","Heil","Kar",
                   "Lbg_4","Lbg_Friedr","Man","Nck","Odw","Rt_","Rtl","Sws")
rurallist <- c ("Sws","Alb","Odw")
traficlist <- c("Nck","Lbg_Friedr","Rtl")
urbanlist <- c ("Brn","Can","Egg","Frei","Friedri","Heid","Heil","Kar","Lbg_4","Man","Rt_")
# Randwerte Odw eliminieren
BW_stations_NO2_tbl %>% filter(name == "Odw") %>% head()
BW_rural_NO2_tbl <- BW_stations_NO2_tbl[which(BW_stations_NO2_tbl$name %in% rurallist),]
BW_trafic_NO2_tbl <-BW_stations_NO2_tbl [which(BW_stations_NO2_tbl$name %in% traficlist),]
BW_urban_NO2_tbl <- BW_stations_NO2_tbl [which(BW_stations_NO2_tbl$name %in% urbanlist),]


#================================================================
save(BW_stations_NO2_tbl, file = "BW_station_NO2.RData")
save(BW_rural_NO2_tbl, file = "rural_NO2.RData")
save(BW_trafic_NO2_tbl, file = "trafic_NO2.RData")
save(BW_urban_NO2_tbl, file = "urban_NO2_tbl")


# Test- overview
ggplot(BW_stations_NO2_tbl%>% filter (datetime > ymd("2014-01-01")), aes ( x = datetime, y = NO2))+
  geom_smooth(method = "lm",aes(color = name))+
  geom_smooth(method = "auto",aes(col =name))+
  ggtitle ( "NO2 Trends 17 Stationen
            2014 bis 2020",
            subtitle = "Mittelwerte und Regression")+
  labs(x ="", y= "NO2[µg/m³]")

ggplot(BW_rural_NO2_tbl%>% filter(datetime > ymd("2001-02-01")), aes ( x = datetime, y = NO2))+
  geom_smooth(method = "lm",aes(color = name))+
  geom_smooth(method = "auto",aes(col =name))+
  ggtitle ( "NO2 Trends laendl Hintergrund
            2001/4 bis 2020",
  subtitle = "Mittelwerte und Regression")+
  labs(x ="", y= "NO2 [µg/m³]")
ggplot(BW_trafic_NO2_tbl, aes ( x = datetime, y = NO2))+
  geom_smooth(method = "lm",aes(color = name))+
  geom_smooth(method = "auto",aes(col =name))+
  ggtitle ( "NO2 Trends verkehrsnah",
            subtitle = "Mittelwerte und Regression")+
  labs(x ="", y= "NO2[μg/m3]")
ggplot(BW_urban_NO2_tbl, aes ( x = datetime, y = NO2))+
  geom_smooth(method = "lm",aes(color = name))+
  geom_smooth(method = "auto",aes(col =name))+
  ggtitle ( "NO2 Trends staedt. Hintergrund",
            subtitle = "Mittelwerte und Regression")+
  labs(x ="", y= "NO2[µg/m³]")
BW_urban_NO2_tbl%>% filter (name != "Heid") %>% filter(name != "Lbg_4")%>%
ggplot( aes ( x = datetime, y = NO2))+
  geom_smooth(method = "lm",aes(color = name))+
  #geom_smooth(method = "auto",aes(col =name))+
  ggtitle ( "NO2 Trends staedt. Hintergrund
            Mittelwerte und Regression",
            subtitle = " Bernhausen,Bad Cannstatt,Eggenstein,
 Freiburg, Friedrichshafen,Heilbronn,Reutlingen")+
  labs(x ="", y= "NO2[µg/m³]")
# Bestimmung der Regressionsparameter 

