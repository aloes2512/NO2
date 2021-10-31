library(xts)
library(tidyverse)
library(lubridate)
library(broom)
library(gridExtra)
library(modelr)
library(stringr)
options(digits = 3)
BW_Rdat_path <- "~/Documents/Luftqualitaet/Daten/BW_Rdat/"
# 17 Stationen mehrere Parameter
load("~/Documents/Luftqualitaet/Daten/BW_Rdat/BW_station.RData")
summary(BW_station_data$Freiburg)
# 15 Stationen NO2  + Zeitparameter
load(file.path(BW_Rdat_path,"BW_stations_NO2_tbl.RData"))
head(BW_stations_NO2_tbl,2)
     levels(BW_stations_NO2_tbl$name)
BW_stations_NO2_tbl%>% filter(name== "Friedri")
# Berechnung der Regressionsgeraden
Nck_NO2_lm <- BW_stations_NO2_tbl %>%  filter (name == "Nck")%>%
summarise(name,augment(lm(NO2 ~datetime,data = .))) 
head(Nck_NO2_lm)
Nck_NO2_lm %>%  ggplot(aes(x = datetime))+
  geom_line(aes(y= .fitted),col ="black")+
  geom_smooth(aes(y = .resid),col = "red",linetype = 2)+
  geom_smooth(aes(y = NO2),col = "red",linetype = 1)+
  ggtitle("NO2-Immissionen Am Neckartor",
  subtitle = " Mittelwerte(rot),
Trend(schw.Linie) & 
  Abweichung vom Trend (rot gestrichelt)")+
  labs( x = "", y = "NO2[μg/m3]")
# als Funktion
detrend_station <- function(df, nm) {
  loc_df <- df %>% filter( (name == nm)) %>%
    summarise(name,augment(lm(NO2~ datetime, data = .)))
  loc_df %>% ggplot(aes(x = datetime))+
    geom_line(aes(y= .fitted),col ="black")+
    geom_smooth(aes(y = .resid),col = "red",linetype = 2)+
    geom_smooth(aes(y = NO2),col = "red",linetype = 1)+
    labs( x = "", y = "NO2[μg/m3]")+
    ggtitle(paste("NO2-Immissionen",nm),
    subtitle = " Mittelwerte(rot),
Trend(schw.Linie) & 
Abweichung vom Trend (rot gestrichelt)")
}
Nck_NO2_detrend <- Nck_NO2_lm %>%dplyr::select(name,datetime,NO2_dtrd=.resid)
detrend_station(BW_stations_NO2_tbl,nm = "Rt_leder")
detrend_station(BW_stations_NO2_tbl,nm = "Can")
detrend_station(BW_stations_NO2_tbl,nm = "Rt_pomol")
detrend_station(BW_stations_NO2_tbl,nm = "Heilbrn")
detrend_station(BW_stations_NO2_tbl,nm = "Brn")

# transfer R$DS example to NO2 data
# built a nested dataframe
rural <- c("Alb","Odw","Sws")
trafic <- c("Lbg_Friedr","Nck","Rt_leder")
urbanbackgrd<-BW_stations_NO2_tbl$name%>%levels()%>% setdiff(c(rural,trafic))

NO2_by_rural <- filter(BW_stations_NO2_tbl,name %in% rural)%>% 
  group_by(name,station) %>% nest()
NO2_by_trafic <- filter(BW_stations_NO2_tbl,name %in% trafic)%>% 
  group_by(name,station) %>% nest()
NO2_urban_backgrd<-  filter(BW_stations_NO2_tbl,name %in% urbanbackgrd)%>%
  group_by(name,station)%>%
  nest()

#nested data frame, each row is a group
NO2_urban_backgrd
# apply function to every dataframe
## make function
name_model <- function(df){
  lm(NO2 ~ datetime, data = df)
}
## apply function to nested list
NO2_bckgrd_models <- map(NO2_bckgrd$data,name_model)

## make it part of the original dataframe
NO2_bckgrd_models <- NO2_bckgrd %>% 
  mutate(models.no2 = map(data,name_model))
NO2_rural_models  <- NO2_by_rural%>%
  mutate(models.no2 = map(data,name_model))
NO2_trafic_models <- NO2_by_trafic%>%
  mutate(models.no2 = map(data,name_model))
# add residuals with each model,data pair
NO2_station_resid <- NO2_bckgrd_models %>% 
  mutate(resids = map2(data, models.no2, add_residuals) )
NO2_rural_resid <- NO2_rural_models %>%
  mutate(resids = map2(data, models.no2, add_residuals) )
NO2_trafic_resid <- NO2_trafic_models %>% 
  mutate(resids = map2(data, models.no2, add_residuals) )
NO2_bckgrd_resid <-  NO2_bckgrd_models %>%
  mutate(resids = map2(data, models.no2, add_residuals) )
NO2_bckgrd_predct <- NO2_bckgrd_models %>%
  mutate(prdct = map2(data,models.no2,add_predictions))
NO2_rural_predct <- NO2_rural_models %>%
  mutate(prdct = map2(data,models.no2,add_predictions))
NO2_trafic_predct <- NO2_trafic_models %>%
  mutate(prdct = map2(data,models.no2,add_predictions))

# to get dataframe for plotting and further analysis
NO2_rural_resid %>% unnest(resids)%>%
  ggplot(aes(x = datetime))+
  geom_smooth(aes(y = resid),col = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(~name)
NO2_trafic_resid%>% unnest(resids)%>%
  ggplot(aes(x = datetime))+
  geom_smooth(aes(y = resid),col = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(~name)
NO2_bckgrd_resid %>% unnest(resids)%>%
  ggplot(aes(x = datetime))+
  geom_smooth(aes(y = resid),col = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(~name)
# NO2 predictions
NO2_bckgrd_predct %>% unnest(prdct)%>%
  ggplot(aes(x = datetime))+
  geom_line(aes(y = pred),col = "black")+
  geom_smooth(aes(x =datetime,y = resid),col= "red",
              data = NO2_bckgrd_resid %>% unnest(resids))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs( x = "", y = "NO2[μg/m3]")+
  ggtitle("NO2 - Immissionen 
  Trend (schwarz), 
  Abweichung vom Trend(rot)",
          subtitle = "staedt. Hintergrund")+
  facet_wrap(~name)
overview <- function(df_prdct,df_resd){
  df_prdct %>% unnest(prdct)%>%
    ggplot(aes(x = datetime))+
    geom_line(aes(y = pred),col = "black")+
    geom_smooth(aes(x =datetime,y = resid),col= "red",
                data = df_resd %>% unnest(resids))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    labs( x = "", y = "NO2[μg/m3]")+
    ggtitle("NO2 - Immissionen 
  Trend (schwarz), 
  Abweichung vom Trend(rot)")+
    facet_wrap(~name)
}
overview(NO2_bckgrd_predct,NO2_bckgrd_resid)
overview(NO2_rural_predct,NO2_rural_resid)
overview(NO2_trafic_predct,NO2_trafic_resid)
