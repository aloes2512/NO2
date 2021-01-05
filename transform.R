# https://geocompr.robinlovelace.net/intro.html
library(raster)
library(sp)
library(rgdal)
library(tidyverse)
library(spData)
library(leaflet)
load("~/Documents/Luftqualitaet/Analysen/luft-qualitaet/Stationsliste.RData")
Stationsliste
UTM_Stationsliste <- Stationsliste %>% dplyr::select(Ost_UTM,Nord_UTM)
write_csv(UTM_Stationsliste,file = "UTM_Koordinaten.csv")
NEONurl <- "https://www.neonscience.org/resources/learning-hub/tutorials/raster-data-r"
browseURL(NEONurl)
# load raster in an R object called 'DEM'
DEM <- raster("~/downloads/NEON-DS-Field-Site-Spatial-Data/SJER/DigitalTerrainModel/SJER2013_DTM.tif")
# look at the raster attributes. 
DEM
DEM@extent # same units as coordinate reference system
#view coordinate reference system
DEM@crs #  +proj=utm +zone=11 +datum=WGS84 +units=m +no_defs 
        #coordinate reference system
# calculate and save the min and max values of the raster to the raster object
DEM <- setMinMax(DEM) # set min and max values of pixels #values     : 228.1, 518.66  (min, max)
# which cells ??
cellStats(DEM, min) #228.1
cellStats(DEM, max) # 518.6
# range
cellStats(DEM,range) # does not work for large file
plot(DEM)
# ausdehnung des Raser Bereichs
DEM@extent %>% class() # "Extent" attr(,"package") >> "raster"
# hiszogramm of values
# the distribution of values in the raster
hist(DEM, main="Distribution of elevation values", 
     col= "purple", 
     maxpixels=22000000)
# create a plot of our raster with moree than 100000 cells
image(DEM)
# specify the range of values that you want to plot in the DEM
# just plot pixels between 250 and 300 m in elevation
image(DEM, zlim=c(200,500),main="Elevation values  200 to 500m")
# we can specify the colors too
colors <- terrain.colors(10)
image(DEM, zlim=c(250,500), main="Digital Elevation Model (DEM)", col=colors)
# add a color map with 5 colors
col=terrain.colors(5)
# add breaks to the colormap (6 breaks = 5 segments)
brk <- c(250, 300, 350, 400, 450, 500)
plot(DEM,zlim=c(250,500), col=col, breaks=brk, main="DEM with more breaks")
# example 
popup = c("Robin", "Jakub", "Jannes")
leaflet() %>%
  addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") %>%
  addMarkers(lng = c(-3, 23, 11),
             lat = c(52, 53, 49), 
             popup = popup,)
library(geosphere)
Lon <- c(1:9/1000, 1:9/100, 1:9/10, 1:90*2)
Lat <- c(1:9/1000, 1:9/100, 1:9/10, 1:90)
cbind(Lon, Lat)
dcos <- distCosine(c(0,0), cbind(Lon, Lat))
dhav <- distHaversine(c(0,0), cbind(Lon, Lat))
dvsp <- distVincentySphere(c(0,0), cbind(Lon, Lat)) 
par(mfrow=(c(1,2)))
plot(log(dcos), dcos-dhav, col='red', ylim=c(-1e-05, 1e-05), 
     xlab="Log 'Law of Cosines' distance (m)",
      ylab="Law of Cosines minus Haversine distance") 
plot(log(dhav), dhav-dvsp, col='blue',
                xlab="Log Haversine distance (m)",
     ylab="Vincenty Sphere minus Haversine distance")
