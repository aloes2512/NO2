# Beispiel Heizg_daten
library(xts)
load("~/projects/NO2/Heizg_data.RData")
load("~/Documents/Luftqualitaet/Daten/BW_Rdat/BW_station.RData")
BW_stations_NO2_tbl %>% summary()
BW_stations_NO2_tbl$name %>% levels() # 15 Stationen
Heizungsdaten_15_19 %>% head() # Daten Bad Cannstatt, Bernhausen, Reutlingen
Heizungsdaten_15_19 %>% summary()
NO2_data <- BW_stations_NO2_tbl %>% filter(name %in% c("Can","Brn","Rt_"))
stat_names <- c("Can","Brn","Rt_")

Grdz_Can <- Heizungsdaten_15_19 %>% 
  .[["Can"]] %>% mutate(name = "Can")
Grdz_Brn <- Heizungsdaten_15_19 %>% 
  .[["Can"]] %>% mutate(name = "Brn")
Grdz_Rt <- Heizungsdaten_15_19 %>% 
  .[["Can"]] %>% mutate(name = "Rt_")

Grdz_data <- bind_rows(Grdz_Brn,Grdz_Can,Grdz_Rt)
NO2_Grdz_data <- inner_join(NO2_data,Grdz_data, by = c("name","datetime")) 
NO2_Grdz_data$NO2 <- NO2_Grdz_data$NO2 %>% na.locf()
NO2_Grdz_data$Gradzahl<- NO2_Grdz_data$Gradzahl %>% na.locf()
cor(NO2_Grdz_data$NO2,NO2_Grdz_data$Gradzahl)
NO2_Grdz_data <-NO2_Grdz_data %>% 
  mutate(z_NO2= (NO2-mean(NO2,na.rm = T))/sd(NO2),
         z_Grdz = (Gradzahl-mean(Gradzahl,na.rm = T))/sd(Gradzahl))
cor(NO2_Grdz_data$z_NO2,NO2_Grdz_data$z_Grdz)
range(NO2_Grdz_data$z_NO2) #  -1.548825  8.416762
NO2_Grdz_data <- NO2_Grdz_data %>% mutate(rd_Grdz = round(z_Grdz))
NO2_Grdz_data %>%
  ggplot() + 
  stat_qq(aes(sample=z_NO2)) + 
  facet_wrap(~rd_Grdz)
NO2_Grdz_data %>%
  ggplot() + 
  stat_qq(aes(sample=z_Grdz)) + 
  facet_wrap(~rd_Grdz)
