URL <- "https://petolau.github.io/Forecast-electricity-consumption-with-similar-day-approach-in-R/"
browseURL(URL)

library(tidyverse)
library(data.table)
library(ggmap)
library(grid)
library(gridExtra)
load("~/Documents/Luftqualitaet/Daten/BW/BW.RData")
summary(BW.all_data)
# alle metadaten einlesen 
# Meta_data <- data.table::fread("meta/all_sites.csv")
#"SITE_ID"     "INDUSTRY"  "SUB_INDUSTRY"          "LAT" "LNG"       "SQ_M"   
#         6        Commercial    Shopping Center/Mall  34.783 -106.8952    15006.81
load("~/Documents/Luftqualitaet/Analysen/Stationsdaten.RData")
summary(Stationsdaten)
View(Stationsdaten)
Meta_station <- Stationsdaten %>% 
  dplyr::select(ID = "Stationsnummer",
                "Messstelle",
                "Komponente",
                LAT = "lat" ,
                LNG = "long")
Meta_data_BW <- Meta_station %>% as.data.table()
str(Meta_data_BW)
names(Meta_data_BW)
kable(Meta_data_BW)
# Lade NO2 Messwerte       
load("~/Documents/Luftqualitaet/02Analysen/BW_stations_NO2_tbl.RData")
BW_stations_NO2_tbl$station <- as_factor(BW_stations_NO2_tbl$station)
BW_stations_NO2_tbl$name <- as_factor(BW_stations_NO2_tbl$name)
summary(BW_stations_NO2_tbl)
BW_stations_NO2_tbl$name
station_names <- c("Alb","Brn","Can","Egg","Frei","Friedri","Heid","Heil","Kar",
                   "Lbg_4","Lbg_Friedr","Man","Nck","Odw","Rt_","Rtl","Sws")
