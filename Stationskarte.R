#Stationskarte
library(tidyverse)
library(ggmap)
load("~/projects/NO2/BW_stations_list.RData")
Stationsliste %>% head()
lonlat <- Stationsliste %>% dplyr::select("longitude","latitude")
# Google Cloud Platform zur Erstellung von API im Adressbuch gespeichert
#findkey in Notizen# Lookup in gitignore
# # Ist API unter der Geolocation angemeldet wurde
# Kartendarstellung
Meta_station <- Stationsliste %>% 
  dplyr::select(ID = "Station",
                "Messstelle",
                 longitude,
                 latitude 
                )
# Karte von Google herunterladen 
#zunächst registrieren mit register_google("key")
register_google("AIzaSyAlBb7-UOG2KxBihE3G8u354Ren32_hjYI")
map <- get_googlemap( c(lon = 8.5 , lat = 48.55), zoom = 8,type = "roadmap")
map_BW <- map
BW_Uebersichtskarte <- ggmap(map_BW,maptype = "roadmap") + 
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
ggsave(BW_Uebersichtskarte, filename = "plts/BW_Uebersichtskarte.png")
print(BW_Uebersichtskarte)
# Nur Stuttgart
# Innenstadt
map_Stgt.14 <- get_map(c(lon = 9.179, lat = 48.782), zoom = 14)
# citation ggmap :  D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R
# Journal, 5(1), 144-161.
Stgt_Innenstadt_Stationen <- ggmap(map_Stgt.14,maptype = "watercolor")+
  geom_point(data = Meta_station, aes(x =longitude, y = latitude),
             size = 7, shape = 20,col = "red")+
  geom_point(data = lonlat,aes(x = longitude,y = latitude),
             size = 7, shape = 12,col = "black")+
  ggtitle(" LUBW Messtationen Stuttgart 
          Innenstadt")+
  theme(axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))
ggsave(Stgt_Innenstadt_Stationen, filename = "plts/Stgt_Innenstadt_Stationen.png")
print(Stgt_Innenstadt_Stationen)
# Stuttgart gesamt
map_Stgt_13 <- get_map(c(lon = 9.179, lat = 48.782), zoom = 13)
Stuttgart_Gesamt <-ggmap(map_Stgt_13,maptype = "watercolor")+
  geom_point(data = Meta_station, aes(x =longitude, y = latitude),
             size = 7, shape = 20,col = "red")+
  geom_point(data = lonlat,aes(x = longitude,y = latitude),
             size = 7, shape = 12,col = "black")+
  ggtitle(" LUBW Messtationen 
    Stadt Stuttgart")+
  theme(axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))
ggsave(Stuttgart_Gesamt, filename = "plts/Stuttgart_Gesamt.png")
print(Stuttgart_Gesamt)
# Großraum Stuttgart
# Kartendarstellung
map <- get_map( c(lon = 9.179 , lat = 48.783), zoom = "auto")
BW_sw <- ggmap(map) + 
  #geom_point(data = Meta_station, aes(x = longitude, y = latitude),
  #           size = 6,shape = 1,col = "red") + 
  geom_point(data = lonlat,aes(x = longitude,y = latitude),
             size = 2, shape = 10,col = "black")+
  ggtitle ("Region Stuttgart
14 Messtationen")+
  theme(axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))
ggsave(BW_sw, filename = "plts/BW_sw.png")
print(BW_sw) 
