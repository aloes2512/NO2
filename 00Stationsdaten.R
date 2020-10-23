library(tidyverse)
library(readr)
library(readxl)
library(knitr)

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
summary(Stationsdaten)
save(Stationsdaten, file = "Stationsdaten.RData")
Stationsdaten_tbl <- Stationsdaten %>% dplyr::select(Stationsnummer,Messstelle,Ost,Nord)
Stationsdaten$Stationsnummer <-Stationsdaten$Stationsnummer %>% as_factor()

Stationsliste <- Stationsdaten %>% group_by(Stationsnummer) %>% summarise(Messstelle = first(Messstelle),Ost_UTM =first(Ost),Nord_UTM=first(Nord))

save(Stationsliste, file = "Stationsliste.RData")
Stationsliste %>% kable() 
# als pdf exportieren


#library(grid)
d <- head(Stationsliste)
g <- tableGrob(d)
plot(g)
grid.newpage()
ggsave(g, )

# =====================================================
NROW(Stationsliste) #29
rurallist <- c ("4467","47650","76118")  #  'Sws'  'Alb' 'Odw'                                              
traficlist <- c("76361","76359","76364") #  'Nck' 'Lbg_Friedr' 'Rtl'
Stationsliste %>% str()
Stationsliste %>% filter(Stationsnummer == "76364")  
Stationsliste$Stationsnummer 
rural_stations <- vector("list", length = length(rurallist))
rural_tbl <- tibble()
names(rural_stations) <- rurallist
for (nmb in c ("4467","47650","76118")) { 
  rural_stations[[nmb]] <- Stationsliste %>% filter(Stationsnummer == nmb) 
  rural_tbl <- bind_rows( rural_tbl,rural_stations[[nmb]])
  }
trafic_stations <- vector("list", length = length(traficlist))
names(trafic_stations) <- traficlist
trafic_tbl <- tibble()
for (nmb in traficlist) { 
  trafic_stations[[nmb]] <- Stationsliste %>% filter(Stationsnummer == nmb) 
  trafic_tbl <- bind_rows( trafic_tbl,trafic_stations[[nmb]])
}
urban_tbl <- anti_join(Stationsliste,rural_tbl) %>% anti_join(trafic_tbl)


#=====================================================
save(urban_tbl,file = "staedt_stations_list.RData")
save(trafic_tbl, file = "Verkehrs_stations_list.RData")
save(rural_tbl,file = "rural_stations_list.RData" )
save(Stationsliste, file = "BW_stations_list.RData")
#=====================================================
kable(Stationsliste)
Stationsliste %>% filter (Stationsnummer == 4443)
AbbListe <- Stationsliste %>% filter ()