library(tidyverse)
library(readr)
library(readxl)
library(knitr)
library(ggmap)
library(gridExtra)
library(grid)
BW_Stationsdaten_LUBW <- read_delim("~/Google Drive/BW_dat/BW_Stationsdaten_LUBW.csv",
";", escape_double = FALSE, col_types = cols(`Inbetriebnahme Datum` = col_date(format = "%d.%m.%Y"),
`Stilllege Datum` = col_date(format = "%d.%m.%Y")),
trim_ws = TRUE, skip = 23)
Stuttgart_Stationsdaten_LUBW <- read_delim("~/Google Drive/BW_dat/Stuttgart_Stationsdaten_LUBW.csv",
";", escape_double = FALSE, col_types = cols(`Inbetriebnahme Datum` = col_date(format = "%D.%m.%Y"),
`Stilllege Datum` = col_date(format = "%d.%m.%Y")),
trim_ws = TRUE, skip = 14)
#Stuttgart_Stationsdaten_LUBW$Ort <- Stuttgart_Stationsdaten_LUBW$Ort %>% as_factor
Stuttgart_Stationsdaten <- Stuttgart_Stationsdaten_LUBW
Kar_Stationen_LUBW <- read_delim("~/Google Drive/BW_dat/Kar_Stationen_LUBW.csv",
";", escape_double = FALSE, col_types = cols(`Inbetriebnahme Datum` = col_date(format = "%d.%m.%Y"),
`Stilllege Datum` = col_date(format = "%d.%m.%Y")),
trim_ws = TRUE)
#Kar_Stationen_LUBW$Ort <- Kar_Stationen_LUBW$Ort %>% as_factor()
Kar_Stationen <- Kar_Stationen_LUBW
#BW_Stationsdaten_LUBW$Ort <- BW_Stationsdaten_LUBW$Ort %>% as_factor()
BW_Stationsdaten <- BW_Stationsdaten_LUBW %>% dplyr::filter ( !Ort == "Karlsruhe" & !Ort == "Stuttgart")

BW_Stationsdaten_LUBW$Messstelle <- BW_Stationsdaten_LUBW$Messstelle %>% as_factor()
BW_Stationsdaten_LUBW %>% summary()
BW_Stationsdaten %>% summary()                                                           
Stationsdaten <- bind_rows(BW_Stationsdaten,Stuttgart_Stationsdaten,Kar_Stationen)
Stationsdaten$Ort <-Stationsdaten$Ort %>% as_factor()

Stationsdaten$Messstelle <- Stationsdaten$Messstelle  %>% as_factor()
Stationsdaten$Komponente <- as_factor(Stationsdaten$Komponente )
Stationsdaten$`Aktiv (Y/N)` <- Stationsdaten$`Aktiv (Y/N)`%>% as_factor()
Stationsdaten$Stationsnummer <- as_factor(Stationsdaten$Stationsnummer)
Stationsdaten$Einheit <- as_factor(Stationsdaten$Einheit)
Stationsdaten$Straße <- as_factor(Stationsdaten$Straße)
summary(Stationsdaten)
save(Stationsdaten, file = "Stationsdaten.RData")
# Teillisten
Stationsdaten_tbl <- Stationsdaten %>% dplyr::select(Stationsnummer,Messstelle,Ost,Nord)
Stationsliste <- Stationsdaten %>% group_by(Stationsnummer) %>% summarise(Messstelle = first(Messstelle),Ost_UTM =first(Ost),Nord_UTM=first(Nord))
class(Stationsliste)
save(Stationsliste, file = "Stationsliste.RData")
# Koordinaten als csv2 exportieren
Stationskoordinaten <- Stationsliste %>% dplyr::select(Station= "Stationsnummer",LON = "Ost_UTM",LAT = "Nord_UTM")
write.csv2(Stationskoordinaten,file = "Stationskoordinaten.csv")
#open the csv file in a simple TEXT EDITOR , copy 
browseURL("https://www.engineeringtoolbox.com/utm-latitude-longitude-d_1370.html")
# convert UTM csv file to textfile and add column zone (all values = 32)
#copy textfile online converter in engineering toolbox and convert
# copy and paste data
Liste_UTM_degrees <-read.csv("Liste_UTM_degrees.txt")
Stationskoordinaten <-cbind(Stationskoordinaten,Liste_UTM_degrees) 
Stationskoordinaten <- Stationskoordinaten %>% dplyr::select(-c(LON,LAT))
Stationsliste <- inner_join(Stationsliste,Stationskoordinaten,by = c("Stationsnummer" = "Station")) %>% dplyr::select(-c(northing,easting))
Stationsliste$zone <- NULL
# ======================================================
# als pdf exportieren
library(gridExtra)
pdf("Stationsliste.pdf", height = 11, width = 8.5 ) # für DIN A4
grid.table(Stationsliste)
dev.off()
# =====================================================
NROW(Stationsliste) #29
rurallist <- c ("4467","47650","76118")  #  'Sws'  'Alb' 'Odw'                                              
traficlist <- c("76361","76359","76364") #  'Nck' 'Lbg_Friedr' 'Rtl'
Stationsliste %>% str()
trafic_stations <- Stationsliste %>% filter(Stationsnummer %in% traficlist)  
rural_stations <- Stationsliste %>% filter(Stationsnummer %in% rurallist)
urban_stations <- anti_join(Stationsliste,rural_stations) %>% anti_join(trafic_stations)


#=====================================================
save(urban_tbl,file = "staedt_stations_list.RData")
save(trafic_tbl, file = "Verkehrs_stations_list.RData")
save(rural_tbl,file = "rural_stations_list.RData" )
save(Stationsliste, file = "BW_stations_list.RData")
#=====================================================
#=======================================================
## Stationsdaten in Dezimalgrad angeben für Darstellung mit Google maps API
# convert UTM to Decimal long & lat
url_conv <- "https://www.engineeringtoolbox.com/utm-latitude-longitude-d_1370.html"
browseURL(url_conv)
load("~/Documents/Luftqualitaet/Analysen/Stationsdaten.RData")
dec_coord <- Stationsdaten %>% 
  dplyr::select(Stationsnummer,Messstelle,Nord,Ost) %>% 
  group_by(Stationsnummer,Messstelle) %>% nest()
#Bernhausen
dec_coord$data[[1]]$lat <- 48.677223
dec_coord$data[[1]]$long <-9.226859
#Eggenstein
dec_coord$data[[2]]$Nord #5436100
dec_coord$data[[2]]$Ost  #456600
dec_coord$data[[2]]$lat <- 49.07623
dec_coord$data[[2]]$long <-8.405729
# Freiburg
dec_coord$data[[3]]$Nord #5317100
dec_coord$data[[3]]$Ost  #412800
dec_coord$data[[3]]$lat <- 48.001247
dec_coord$data[[3]]$long <-7.830992 
#Friedrichshafen
dec_coord$data[[4]]$Nord #5278600
dec_coord$data[[4]]$Ost  #412800
dec_coord$data[[4]]$lat <- 48.001247
dec_coord$data[[4]]$long <-7.830992 
# Heidelberg
dec_coord$data[[5]]$Nord #5474100
dec_coord$data[[5]]$Ost #476500
dec_coord$data[[5]]$lat <- 49.419124
dec_coord$data[[5]]$long <-8.675982

# Heilbronn
dec_coord$Messstelle[[6]]
dec_coord$data[[6]]$Nord #5445800
dec_coord$data[[6]]$Ost #516300
dec_coord$data[[6]]$lat <-49.164799
dec_coord$data[[6]]$long <- 9.223591
#Ludwigsburg
dec_coord$Messstelle[[7]]
dec_coord$data[[7]]$Nord #5416200
dec_coord$data[[7]]$Ost #512600
dec_coord$data[[7]]$lat <-48.898618
dec_coord$data[[7]]$long <- 9.171918 
#Ludwigsburg Friedrichstraße
dec_coord$Messstelle[[8]]
dec_coord$data[[8]]$Nord #5415100
dec_coord$data[[8]]$Ost #513900
dec_coord$data[[8]]$lat <-48.888695
dec_coord$data[[8]]$long <- 9.189618 
#Mannheim-Mitte
dec_coord$Messstelle[[9]]
dec_coord$data[[9]]$Nord #5480400
dec_coord$data[[9]]$Ost #462100
dec_coord$data[[9]]$lat <-49.475063
dec_coord$data[[9]]$long <- 8.476839  
# Odenwald
dec_coord$Messstelle[[10]]
dec_coord$data[[10]]$Nord #5479100
dec_coord$data[[10]]$Ost #482100
dec_coord$data[[10]]$lat <-49.464289
dec_coord$data[[10]]$long <- 8.752968  
#Reutlingen
dec_coord$Messstelle[[11]]
dec_coord$data[[11]]$Nord #5370600
dec_coord$data[[11]]$Ost #515300
dec_coord$data[[11]]$lat <-48.488338
dec_coord$data[[11]]$long <-9.207068
# Reutlingen Lederstraße-Ost
dec_coord$Messstelle[[12]]
dec_coord$data[[12]]$Nord #5370700
dec_coord$data[[12]]$Ost #515500
dec_coord$data[[12]]$lat <-48.489233
dec_coord$data[[12]]$long <-9.209778
#Schwäbische Alb
dec_coord$Messstelle[[13]]
dec_coord$data[[13]]$Nord #5354700
dec_coord$data[[13]]$Ost #515300
dec_coord$data[[13]]$lat <-48.345294
dec_coord$data[[13]]$long <-9.206488
#Schwarzwald-Süd
dec_coord$Messstelle[[14]]
dec_coord$data[[14]]$Nord #5295900
dec_coord$data[[14]]$Ost #407500
dec_coord$data[[14]]$lat <-47.809803
dec_coord$data[[14]]$long <-7.764504
#Stuttgart Am Neckartor
dec_coord$Messstelle[[15]]
dec_coord$data[[15]]$Nord #5403900
dec_coord$data[[15]]$Ost #514000
dec_coord$data[[15]]$lat <-48.78794
dec_coord$data[[15]]$long <-9.190599 
#Stuttgart Arnulf-Klett-Platz
dec_coord$Messstelle[[16]]
dec_coord$data[[16]]$Nord #5403300
dec_coord$data[[16]]$Ost #513200
dec_coord$data[[16]]$lat <-48.78256
dec_coord$data[[16]]$long <-9.179689 
#Stuttgart Hauptstätter Straße
dec_coord$Messstelle[[17]]
dec_coord$data[[17]]$Nord #5401500
dec_coord$data[[17]]$Ost #512500
dec_coord$data[[17]]$lat <-48.766381
dec_coord$data[[17]]$long <-9.170105 
#Stuttgart Hauptstätter Straße-West
dec_coord$Messstelle[[18]]
dec_coord$data[[18]]$Nord #5401400
dec_coord$data[[18]]$Ost #512500
dec_coord$data[[18]]$lat <-48.765482
dec_coord$data[[18]]$long <-9.170102 
#Stuttgart Hohenheimer Straße
dec_coord$Messstelle[[19]]
dec_coord$data[[19]]$Nord #5401700
dec_coord$data[[19]]$Ost #513500
dec_coord$data[[19]]$lat <-48.76816 
dec_coord$data[[19]]$long <-9.18372 
#Stuttgart Siemensstraße
dec_coord$Messstelle[[20]]
dec_coord$data[[20]]$Nord #5406400
dec_coord$data[[20]]$Ost #512800
dec_coord$data[[20]]$lat <-48.810455 
dec_coord$data[[20]]$long <-9.17434 
# Stuttgart Stadtgarten
dec_coord$Messstelle[[21]]
dec_coord$data[[21]]$Nord #5403100
dec_coord$data[[21]]$Ost #512600
dec_coord$data[[21]]$lat <-48.810459
dec_coord$data[[21]]$long <-9.171616 
#Stuttgart Waiblinger Straße
dec_coord$Messstelle[[22]]
dec_coord$data[[22]]$Nord #5405600
dec_coord$data[[22]]$Ost #516100
dec_coord$data[[22]]$lat <-48.803182 
dec_coord$data[[22]]$long <-9.219256  
#Stuttgart-Bad Cannstatt
dec_coord$Messstelle[[23]]
dec_coord$data[[23]]$Nord #5406200
dec_coord$data[[23]]$Ost #516800
dec_coord$data[[23]]$lat <-48.808561 
dec_coord$data[[23]]$long <-9.228813 
#Stuttgart-Hafen
dec_coord$Messstelle[[24]]
dec_coord$data[[24]]$Nord #5399900
dec_coord$data[[24]]$Ost #519500
dec_coord$data[[24]]$lat <-48.751808 
dec_coord$data[[24]]$long <-9.265287 
# Stuttgart-Zuffenhausen
dec_coord$Messstelle[[25]]
dec_coord$data[[25]]$Nord #5408000
dec_coord$data[[25]]$Ost #512600
dec_coord$data[[25]]$lat <-48.824853 
dec_coord$data[[25]]$long <-9.171665
#Karlsruhe Reinhold-Frank-Straße
dec_coord$Messstelle[[26]]
dec_coord$data[[26]]$Nord #5428500
dec_coord$data[[26]]$Ost #455100
dec_coord$data[[26]]$lat <-49.00776 
dec_coord$data[[26]]$long <-8.386033
#Karlsruhe-Mitte
dec_coord$Messstelle[[27]]
dec_coord$data[[27]]$Nord #5428700
dec_coord$data[[27]]$Ost #457400
dec_coord$data[[27]]$lat <-49.009722 
dec_coord$data[[27]]$long <-8.417461 
#Karlsruhe-Nordwest
dec_coord$Messstelle[[28]]
dec_coord$data[[28]]$Nord #5430800
dec_coord$data[[28]]$Ost #452800
dec_coord$data[[28]]$lat <-49.028277 
dec_coord$data[[28]]$long <-8.354317  
#Karlsruhe-West
dec_coord$Messstelle[[29]]
dec_coord$data[[29]]$Nord #5428600
dec_coord$data[[29]]$Ost #452700
dec_coord$data[[29]]$lat <-49.00848
dec_coord$data[[29]]$long <-8.353206   
#=========================
summary(dec_coord)

dec_coord$Stationsnummer <- dec_coord$Stationsnummer%>% as_factor()
Stationsdaten$Stationsnummer <- Stationsdaten$Stationsnummer%>% as_factor()
Stationsdaten %>% head()

Stationsdaten <-Stationsdaten %>% 
  right_join(Stat_Coord)
save(Stationsdaten,file = "~/Documents/Luftqualitaet/Analysen/Stationsdaten.RData")
