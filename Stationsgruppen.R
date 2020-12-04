load("RData/BW_stations_NO2_tbl.RData")
BW_stations_NO2_tbl %>% head()
BW_stations_NO2_tbl$name <- BW_stations_NO2_tbl$name %>% as_factor()
# Teillisten erstellen
station_names <- c("Alb","Brn","Can","Egg","Frei","Friedri","Heid","Heil","Kar",
                   "Lbg_4","Lbg_Friedr","Man","Nck","Odw","Rt_","Rtl","Sws")
BW
rurallist <- c ("Sws","Alb","Odw")
traficlist <- c("Nck","Lbg_Friedr","Rtl")
urbanlist <- c ("Brn","Can","Egg","Frei","Friedri","Heid","Heil","Kar","Lbg_4","Man","Rt_")

BW_rural_NO2_tbl <- BW_stations_NO2_tbl %>% filter (name %in% rurallist)
BW_trafic_NO2_tbl <-BW_stations_NO2_tbl %>% filter (name %in% traficlist)
BW_urban_NO2_tbl <- BW_stations_NO2_tbl %>% filter (name %in% urbanlist)


#================================================================
save(BW_stations_NO2_tbl, file = "RData/BW_station_NO2.RData")
save(BW_rural_NO2_tbl, file = "RData/rural_NO2.RData")
save(BW_trafic_NO2_tbl, file = "RData/trafic_NO2.RData")
save(BW_urban_NO2_tbl, file = "RData/urban_NO2_tbl")


# Test- overview
ggplot(BW_stations_NO2_tbl, aes ( x = datetime, y = NO2))+
  geom_smooth(method = "lm",aes(color = name))+
  geom_smooth(method = "auto",aes(col =name))+
  ggtitle ( "NO2 Trends 17 Stationen",
            subtitle = "Mittelwerte und Regression")+
  labs(x ="", y= "NO2[ug/m3]")
ggplot(BW_rural_NO2_tbl, aes ( x = datetime, y = NO2))+
  geom_smooth(method = "lm",aes(color = name))+
  geom_smooth(method = "auto",aes(col =name))+
  ggtitle ( "NO2 Trends laendl Hintergrund",
  subtitle = "Mittelwerte und Regression")+
  labs(x ="", y= "NO2[ug/m3]")
ggplot(BW_trafic_NO2_tbl, aes ( x = datetime, y = NO2))+
  geom_smooth(method = "lm",aes(color = name))+
  geom_smooth(method = "auto",aes(col =name))+
  ggtitle ( "NO2 Trends verkehrsnah",
            subtitle = "Mittelwerte und Regression")+
  labs(x ="", y= "NO2[ug/m3]")
ggplot(BW_urban_NO2_tbl, aes ( x = datetime, y = NO2))+
  geom_smooth(method = "lm",aes(color = name))+
  geom_smooth(method = "auto",aes(col =name))+
  ggtitle ( "NO2 Trends staedt. Hintergrund",
            subtitle = "Mittelwerte und Regression")+
  labs(x ="", y= "NO2[ug/m3]")
