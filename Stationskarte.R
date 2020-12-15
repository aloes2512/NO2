#Stationskarte
library(tidyverse)
library(sp)
library(raster)
library(ggmap)
load("~/Documents/Luftqualitaet/Analysen/NO2/Stationsliste_2.RData")
lonlat <- Stationsliste_decimal %>% dplyr::select("longitude","latitude")
# first try: "https://rspatial.org/raster/spatial/3-vectordata.html"
URL <- "https://rspatial.org/raster/spatial/3-vectordata.html"
browseURL(URL)
pts <- SpatialPoints(lonlat)
class(pts)
showDefault(pts) # bbox  min :longitude 7.764504; latitude 47.659764
                 # bbox  max: longitude 9.486118; latitude 49.475063
crdref <- CRS('+proj=longlat +datum=WGS84 +towgs84=1000000,0,0')
pts <- SpatialPoints(lonlat, proj4string=crdref)
pts
df <- Stationsliste_decimal %>% dplyr::select(Station,Messstelle) 
df$ID <- 1: NROW(df)
df <- df %>%
  dplyr::select(ID,Station,Messstelle)
ptsdf <- SpatialPointsDataFrame(pts, data=df)
str(ptsdf)
showDefault(ptsdf)
plot(ptsdf)
# Google Cloud Platform zur Erstellung von API im Adressbuch gespeichert
findkey # Lookup in gitignore
#register_google(findkey) # Ist API unter der Geolocation angmeldet wurde
# Kartendarstellung
Meta_station <- Stationsliste_decimal %>% 
  dplyr::select(ID = "Station",
                "Messstelle",
                 longitude,
                 latitude 
                )
# Karte von Google herunterladem
map <- get_map( c(lon = 8.5 , lat = 48.55), zoom = 8)
map_BW <- map
ggmap(map_BW,maptype = "hybrid") + 
  geom_point(data = Meta_station, aes(x =longitude, y = latitude),
             size = 3, alpha = .3,shape = 20,col = "red") + 
  ggtitle ("Baden Württemberg
Untersuchte Messtationen")+
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
