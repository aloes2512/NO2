#Stationskarte
library(tidyverse)
library(sp)
library(raster)
library(ggmap)
load("~/Documents/Luftqualitaet/Analysen/NO2/Stationsliste_2.RData")
lonlat <- Stationsliste %>% dplyr::select("longitude","latitude")
# Google Cloud Platform zur Erstellung von API im Adressbuch gespeichert
findkey # Lookup in gitignore
#register_google(findkey) # Ist API unter der Geolocation angmeldet wurde
# Kartendarstellung
Meta_station <- Stationsliste %>% 
  dplyr::select(ID = "Stationsnummer",
                "Messstelle",
                 longitude,
                 latitude 
                )
# Karte von Google herunterladem
map <- get_googlemap( c(lon = 8.5 , lat = 48.55), zoom = 8,type = "roadmap")
map_BW <- map
ggmap(map_BW,maptype = "roadmap") + 
  geom_point(data = Meta_station, aes(x =longitude, y = latitude),
             size = 7, shape = 20,col = "red") + 
  geom_point(data = lonlat,aes(x = longitude,y = latitude),
             size = 5, alpha = 0.5,shape = 12,col = "black")+
  ggtitle ("Baden Württemberg
untersuchte Messstationen")+
  theme(axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))
# Nur Stuttgart
map_Stgt <- get_map(c(lon = 9.179, lat = 48.782), zoom = 15)
map_Stgt_innen.15 <- map_Stgt
map_Stgt.14 <- get_map(c(lon = 9.179, lat = 48.782), zoom = 14)
# citation ggmap :  D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R
# Journal, 5(1), 144-161.
ggmap(map_Stgt.14,maptype = "watercolor")+
  geom_point(data = Meta_station, aes(x =longitude, y = latitude),
             size = 7, shape = 20,col = "red")+
  geom_point(data = lonlat,aes(x = longitude,y = latitude),
             size = 7, shape = 12,col = "black")
