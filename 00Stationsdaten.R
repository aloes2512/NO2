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
Stuttgart_Stationsdaten_LUBW$Ort <- Stuttgart_Stationsdaten_LUBW$Ort %>% as_factor
Stuttgart_Stationsdaten <- Stuttgart_Stationsdaten_LUBW
Kar_Stationen_LUBW <- read_delim("~/Google Drive/BW_dat/Kar_Stationen_LUBW.csv",
";", escape_double = FALSE, col_types = cols(`Inbetriebnahme Datum` = col_date(format = "%d.%m.%Y"),
`Stilllege Datum` = col_date(format = "%d.%m.%Y")),
trim_ws = TRUE)
Kar_Stationen_LUBW$Ort <- Kar_Stationen_LUBW$Ort %>% as_factor()
Kar_Stationen <- Kar_Stationen_LUBW
BW_Stationsdaten_LUBW$Ort <- BW_Stationsdaten_LUBW$Ort %>% as_factor()
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
write_csv2(Stationsdaten,file= "Stationsdaten.csv")
save(Stationsdaten, file = "Stationsdaten.RData")
# Teillisten
Stationsdaten_tbl <- Stationsdaten %>% dplyr::select(Stationsnummer,Messstelle,Ost,Nord)
Stationsliste <- Stationsdaten %>% group_by(Stationsnummer) %>% summarise(Messstelle = first(Messstelle),Ost_UTM =first(Ost),Nord_UTM=first(Nord))
class(Stationsliste)
# Koordinaten als csv2 exportieren
Stationskoordinaten <- Stationsliste %>% dplyr::select(Station= "Stationsnummer",LON = "Ost_UTM",LAT = "Nord_UTM")
Stationskoordinaten$zone <- 32
write.csv2(Stationskoordinaten,file = "Stationskoordinaten.csv")
#open the csv file in a simple TEXT EDITOR , copy 
browseURL("https://www.engineeringtoolbox.com/utm-latitude-longitude-d_1370.html")
# convert UTM csv file to textfile and add column zone (all values = 32)
#copy textfile online converter in engineering toolbox and convert
# copy and paste data
Stationskoordinaten <-read_csv("Liste_UTM_degrees.txt")
Stationskoordinaten $zone <- NULL
dim(Stationsliste) #29 4
dim(Stationskoordinaten)
Stationsliste <-bind_cols(Stationsliste,Stationskoordinaten) %>%
  dplyr::select(-c(Nord_UTM,Ost_UTM ))
write.csv2(Stationsliste,file = "Stationsliste.csv" )
save(Stationsliste, file = "Stationsliste.RData")
# ======================================================
# als pdf exportieren
library(gridExtra)
pdf("Stationskoordinaten.pdf", height = 11, width = 8.5 ) # für DIN A4
pdf("Stationsliste.pdf", height = 11, width = 8.5 ) # für DIN A4
grid.table(Stationsliste)
grid.table(Stationskoordinaten)
dev.off()
# =====================================================
NROW(Stationsliste) #29
rurallist <- c ("4467","47650","76118")  #  'Sws'  'Alb' 'Odw'                                              
traficlist <- c("76361","76359","76364") #  'Nck' 'Lbg_Friedr' 'Rtl'
Stationsliste %>% str()
trafic_stations <- Stationsliste %>% filter(Station %in% traficlist)  
rural_stations <- Stationsliste %>% filter(Station %in% rurallist)
urban_stations <- anti_join(Stationsliste,rural_stations) %>% anti_join(trafic_stations)


#=====================================================
save(urban_stations,file = "urban_stations_list.RData")
save(trafic_stations, file = "trafic_stations_list.RData")
save(rural_stations,file = "rural_stations_list.RData" )
save(Stationsliste, file = "BW_stations_list.RData")
#=====================================================

