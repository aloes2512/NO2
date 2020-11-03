# NO2_Heizungseffekt
library(tidyverse)
library(lubridate)
library(modelr)
library(xts)
load("/Users/alfloeffler/Documents/Luftqualitaet/Daten/BW/BW.RData")
Can_NO2 <- BW.all_data$Stgt_data$Can.no2
mean(Can_NO2$NO2,na.rm = TRUE) # 32.07829
Can_Temp <- BW.all_data$Stgt_data$Can.temp
mean(Can_Temp$Temp,na.rm = TRUE) # 11.09
HeizDaten_Can <-Can_Temp %>% filter (Temp < 15) %>%
  summarise (Gesamt_stunden =NROW(Can_Temp) ,Hz_stunden = n(),Anteil_Hzg = Hz_stunden / NROW(Can_Temp),
             Temp_mittel_Hzg =mean(Temp, na.rm = TRUE),
             Gradzahl_Hzg = 20-Temp_mittel_Hzg)

write.csv2(HeizDaten_Can, file = "HeizDatenCan.csv",row.names = F)
Can_data <- left_join(Can_NO2,Can_Temp)
Can_dat_15_19 <- Can_data %>% filter (datetime > ymd("2015-01-01")&datetime < ymd("2019-12-31"))
Can_dat_15_19_Heizg <- Can_dat_15_19 %>% mutate(Grdz = ifelse(Temp <15,20-Temp,0))
Can_dat_15_19_Heizg$NO2 <- Can_dat_15_19_Heizg$NO2 %>% na.locf()
Can_dat_15_19_Heizg$Temp <- Can_dat_15_19_Heizg$Temp %>% na.locf()
Can_dat_15_19_Heizg %>% names() #"station"  "datetime" "NO2"      "Temp"     "Grdz" 
names(Can_dat_15_19_Heizg) <- c("station","datetime","NO2","Temp","Grdz")
# NO2 Trend
x_min <- ymd("2015-07-01")%>% as.POSIXct()
x_max <- ymd("2019-06-30") %>% as.POSIXct()
Can_NO2_model <- lm (NO2 ~ datetime +1, data = Can_dat_15_19_Heizg)
Can_Temp_model <- lm(Grdz ~ datetime +1,data = Can_dat_15_19_Heizg )
Can_dat_15_19_Heizg <- Can_dat_15_19_Heizg %>% 
  add_predictions(Can_NO2_model)%>% 
  add_residuals(Can_NO2_model)

  names(Can_dat_15_19_Heizg) <- c("station","datetime","NO2","Temp","Grdz",
                                  "pred.no2","resid.no2")
Can_dat_15_19_Heizg <- Can_dat_15_19_Heizg %>% 
  add_predictions(Can_Temp_model) %>%
  add_residuals(Can_Temp_model) %>% dplyr::select(-station)
names(Can_dat_15_19_Heizg) <-c("datetime","NO2","Temp","Grdz",
                               "pred.no2","resid.no2","pred.temp","resid.temp")
  Can_dat_15_19_Heizg %>% ggplot(aes(x=datetime, y = NO2))+
  geom_smooth(col = "red", linetype = 2)+
  geom_smooth(method = "lm",col = "red", linetype = 1)+
  geom_smooth(col = "blue", linetype =2, aes( x= datetime, y =Grdz))+
  geom_smooth(method = "lm",col = "blue", linetype =1, aes( x= datetime, y =Grdz))+
  coord_cartesian(xlim = c(x_min,x_max))+
  ggtitle("NO2-Immissionen(rot) & Heizbedarf(blau)
          Mittelwerte und linearer Trend
          Bad Cannstatt, Gnesenerstr")+
  labs( x="", y ="NO2[μg/m3] / Gradzahl")
Can_dat_15_19_Heizg %>% filter ( Temp < 15) %>%
  ggplot(aes(Grdz,NO2))+
  geom_point(size = 0.01)+
  geom_smooth(method = "lm",col = "red")+
  ggtitle("NO2-Immissionen ~ Heizwaermebedarf
          Bad Cannstatt (Heiztage 2015 bis 2019)")+
  labs( x = "Heizwärmebedarf", y = " NO2 [μg/m3]")
lm(NO2 ~ Grdz,data =Can_dat_15_19_Heizg %>% filter (Grdz >0 ))

Can_dat_15_19_Heizg %>% names()# "datetime" "NO2" "Temp" "Grdz""pred.no2" 
                               # "resid.no2"  "pred.temp"  "resid.temp"
Can_dat_15_19_Heizg %>% ggplot(aes(x=datetime, y = resid.no2))+
  geom_smooth(col = "red", linetype = 2)+
  geom_smooth(method = "lm",col = "red", linetype = 1)+
  geom_smooth(col = "blue", linetype =2, aes( x= datetime, 
                                              y =resid.temp))+
  geom_smooth(method = "lm",col = "blue", linetype =1, 
              aes( x= datetime, y =resid.temp))+
  coord_cartesian(xlim = c(x_min,x_max))+
  ggtitle("NO2-Immissionen(rot) & Heizbedarf(blau)
          Abweichungen vom Trend
       Bad Cannstatt, Gnesenerstr")+
 labs( x="", y ="NO2[μg/m3] / Gradzahl")
res.no2 <-Can_dat_15_19_Heizg$resid.no2 %>% as_vector()
res.temp <- Can_dat_15_19_Heizg$resid.temp
length(res.no2)
length(res.temp)
is.na(res.temp) %>% sum()# 18
res.temp <- res.temp %>% na.locf() %>% as_vector()
cov(Can_dat_15_19_Heizg$resid.no2,Can_dat_15_19_Heizg$resid.temp)
Can_res_dat <- tibble(no2 = res.no2,
                      temp= res.temp,
                      grdz = Can_dat_15_19_Heizg$Grdz)
summary(Can_res_dat%>% filter(grdz >0))
Can_res_dat <- Can_res_dat %>% mutate (distance = (no2-grdz)^2)
Can_res_dat %>% summarise (Abstand = sqrt(sum(distance,na.rm=T)/NROW(.)))
#Can gam Model
Can_NO2_model_loess <- loess(NO2 ~ datetime +1, data = Can_dat_15_19_Heizg,
                            span = 0.1,model = T,degree=1)
Can_Temp_model_loess <- loess(Grdz ~ datetime +1,data = Can_dat_15_19_Heizg )
