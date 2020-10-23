# apply Trend _analysis procedure to NO2 Data
#parms : station , name, datetime, year, NO2
library(lubridate)
load("~/documents/Luftqualitaet/Analysen/Stationsdaten.RData")
 Stationen <- Stationsdaten  %>% dplyr::select(station=Stationsnummer,Ort,Ost,Nord)
load("~/documents/Luftqualitaet/Analysen/BW_station.RData")
summary(BW_stations_NO2_tbl)
head(BW_stations_NO2_tbl)
NO2_BW <- left_join(Stationen,BW_stations_NO2_tbl, by = "station")
NO2_BW %>% head()
# Stationen eliminieren die nicht in name
no_name <- NO2_BW %>% is.na()
NO2_BW <- NO2_BW [!no_name,]

# Trend Uebersicht an einzelnen Stationen
brn <- NO2_BW %>% filter ( name == "Brn")
brn %>% ggplot(aes(x = datetime,y= NO2))+
                 geom_smooth()+
  geom_smooth(method = "lm")

brn_mod <- lm(NO2 ~ datetime, data = brn)
brn %>%
  add_predictions(brn_mod) %>%
  ggplot(aes(datetime, pred)) +
  geom_line(col = "red") +
  ggtitle("Linear trend + ")
nck <- NO2_BW %>% filter( name == "Nck")
# Regressionsmodel
nck_mod <- lm(NO2 ~ datetime,data = nck)
nck <- nck %>%
  add_predictions(nck_mod)
nck <- nck %>%  mutate(Jahr = floor_date (datetime, "year")) %>% filter(Jahr >= ymd("2004-01-01"))

nck %>%  ggplot(aes(x = datetime,y = pred)) +
  geom_line(col = "red") +
  geom_smooth(method = "auto",aes(x = datetime, y = NO2),col = "green")+
  ggtitle("NO2 Neckartor",
          subtitle = "Mittlere Werte & linearer Trend")
nck %>% head()
nck_jahr <- nck %>% group_by(Jahr,name,station) %>% summarise(NO2_y = mean(NO2,na.rm = TRUE))
ggplot(nck_jahr, aes(x = Jahr, y = NO2_y))+
  geom_smooth()
# model fitting function
NO2_model <- function(df) {
  lm(NO2_y ~ Jahr, data = df)
}
NO2_model(nck_jahr)
nck_jahr$Jahr <- nck_jahr$Jahr %>% str_extract("^[0-9]{4}") %>% as.numeric()
nck_mod_y <- lm (NO2_y ~ Jahr,nck_jahr)
nck_jahr <- nck_jahr %>% add_predictions(nck_mod_y)
#Darstellung Jahreswerte
 ggplot(nck_jahr, aes(x = Jahr, y = pred))+
  geom_line(col = "red")+
   geom_point(aes(y= NO2_y))

# Daten auswählen und gruppieren
 NO2_BW <- NO2_BW %>% dplyr::select (name,station, datetime,NO2)%>%
   mutate(Jahr = floor_date (datetime, "year"))
 NO2_BW_Jahr <- NO2_BW %>% group_by(name,station,Jahr) %>%
   summarise(NO2_y = mean(NO2,na.rm = TRUE))
NO2_BW_Jahr$Jahr <- NO2_BW_Jahr$Jahr %>% as.character() %>% str_extract("^[0-9]{4}") %>% as.numeric()
NO2_BW_Jahr %>% head()
ggplot(NO2_BW_Jahr,aes( x = Jahr, y = NO2_y))+
geom_point(aes(col = name))
