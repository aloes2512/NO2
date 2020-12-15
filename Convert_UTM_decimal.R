# UTM to lat long converter
load("/Users/alfloeffler/projects/02Analysen/RData/Stationsliste.RData")
Stationsliste %>% head()
Koordinaten_Stationen <- Stationsliste %>% 
  dplyr::select(northing=Nord_UTM,easting= Ost_UTM) %>% mutate(zone =32)
write.csv(Koordinaten_Stationen,file = "UTM_Koordinaten_Stationen.csv")
# Write the data as a textfile with column names "northing,easting,zone"
#open the csv file in a simple TEXT EDITOR , copy 
browseURL("https://www.engineeringtoolbox.com/utm-latitude-longitude-d_1370.html")
#and paste into "convert a list" push button convert the result is a csv file containing both values
Stationsliste_decimal  <- read_csv("UTM_Koordinaten_Stationen.csv") %>%
  dplyr::select(longitude, latitude) %>% bind_cols(Stationsliste) %>%
  dplyr::select(Station= Stationsnummer,Messstelle,longitude,latitude)
save(Stationsliste_decimal,
     file = "/Users/alfloeffler/documents/Luftqualitaet/Analysen/NO2/Stationsliste_2.RData")

