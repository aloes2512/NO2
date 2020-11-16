# NO2_Heizungseffekt
library(tidyverse)
library(lubridate)
library(modelr)
library(xts)
library(forecast)
library(gam)

load("/Users/alfloeffler/Documents/Luftqualitaet/Daten/BW/BW.RData")
summary(BW.all_data)
names(BW.all_data$Stgt_data) # "Can.no2"  "Can.no"   "Can.temp" "Can.wg"   "Can.wr" 
Stgt_names <- BW.all_data$Stgt_data %>% names()
length(Stgt_names)
list_names <- paste0("Stg.",Stgt_names)
Stgt_list <- vector("list",length = length(Stgt_names))
names(Stgt_list) <- list_names  # "Stg.Can.no2"  "Stg.Can.no"   "Stg.Can.temp" "Stg.Can.wg"  .......
for ( nm in Stgt_names) {
    stnm <- paste0("Stg.",nm)  
   Stgt_list[[stnm]]<- BW.all_data$Stgt_data[[nm]]
}
Stgt_list
# Heizdaten
load("Heizg_data.RData")
summary(Heizungsdaten_15_19)
Heizungsdaten_15_19$Can %>% head(2)
# Stationen
Can_NO2 <- BW.all_data$Stgt_data$Can.no2
mean(Can_NO2$NO2,na.rm = TRUE) # 32.07829
Can_Temp <- BW.all_data$Stgt_data$Can.temp
mean(Can_Temp$Temp,na.rm = TRUE) # 11.09
Can_Temp %>% filter (Temp <15) %>% summarise(mean(Temp,na.rm = TRUE))
Can_grdz <- Heizungsdaten_15_19$Can # datetime Gradzahl
# NO2,Temp,Grdz zusammenfassen
Can_data_15_19 <- left_join(Can_NO2,Can_Temp) %>% right_join(Can_grdz)
Can_data_15_19 %>% names() #"station""datetime""NO2""Temp""Gradzahl" 
NROW(Can_data_15_19) #41951
sum(is.na(Can_data_15_19)) #877
Can_data_15_19 %>% ggplot(aes(x =datetime,y = Gradzahl))+
  geom_point(size = 0.001,alpha = 0.5)

# NO2 Trend nur von 15-07-01 bis 19-06-30
x_min <- ymd("2015-07-01")%>% as.POSIXct()
x_max <- ymd("2019-06-30") %>% as.POSIXct()
Can_NO2_model.lm <- lm (NO2 ~ datetime +1, data = Can_data_15_19)
Can_Temp_model.lm <- lm(Gradzahl ~ datetime +1,data = Can_data_15_19 )

Can_data_15_19_Heizg_pred <- Can_data_15_19 %>% 
  add_predictions(Can_NO2_model.lm)%>% 
  add_residuals(Can_NO2_model.lm)
head(Can_data_15_19_Heizg_pred)
names(Can_data_15_19_Heizg_pred) <- c("station","datetime","NO2","Temp","Gradzahl",
                                  "no2.pred","no2.resid")
Can_data_15_19_Heizg_pred <- Can_data_15_19_Heizg_pred %>% 
  add_predictions(Can_Temp_model.lm) %>%
  add_residuals(Can_Temp_model.lm) %>% dplyr::select(-station)
names(Can_data_15_19_Heizg_pred) <-c("datetime","NO2","Temp","Grdz",
                               "no2.pred","no2.resid","Temp.pred","Temp.resid")
Can_data_15_19_Heizg_pred %>% ggplot(aes(x=datetime, y = NO2))+
  geom_smooth(col = "red", linetype = 2)+
  geom_smooth(method = "lm",col = "red", linetype = 1)+
  geom_smooth(col = "blue", linetype =2, aes( x= datetime, y =Grdz))+
  geom_smooth(method = "lm",col = "blue", linetype =1, aes( x= datetime, y =Grdz))+
  coord_cartesian(xlim = c(x_min,x_max))+
  ggtitle("NO2-Immissionen(rot) & Heizbedarf(blau)
          Mittelwerte und linearer Trend
          Bad Cannstatt, Gnesenerstr",
          subtitle = "mit gam geglättet")+
  labs( x="", y ="NO2[μg/m3] / Gradzahl")
#QQ Can plot Heizwaermebedarf NO2 Immissionen
Can_data_15_19_Heizg_pred %>% filter ( Temp < 15) %>%
  ggplot(aes(Grdz,NO2))+
  geom_point(size = 0.01)+
  geom_smooth(method = "lm",col = "red")+
  ggtitle("NO2-Immissionen ~ Heizwaermebedarf
          Bad Cannstatt (Heiztage 2015 bis 2019)")+
  labs( x = "Heizwärmebedarf", y = " NO2 [μg/m3]")

lm(NO2 ~ Grdz,data =Can_data_15_19_Heizg_pred %>% filter (Grdz >0 ))

Can_data_15_19_Heizg_pred %>% names()# "datetime" "NO2" "Temp" "Grdz""no2.pred" 
                               # "no2.resid"  "Temp.pred"  "Temp.resid"
Can_data_15_19_Heizg_pred %>% ggplot(aes(x=datetime, y = no2.resid))+
  geom_smooth(col = "red", linetype = 2)+
  geom_smooth(method = "lm",col = "red", linetype = 1)+
  geom_smooth(col = "blue", linetype =2, aes( x= datetime, 
                                              y =Temp.resid))+
  geom_smooth(method = "lm",col = "black", linetype =1, 
              aes( x= datetime, y =Temp.resid))+
  coord_cartesian(xlim = c(x_min,x_max))+
  ggtitle("NO2-Immissionen(rot) & Heizbedarf(blau)
    Abweichungen vom linearen Trend(schwarz)
       Bad Cannstatt, Gnesenerstr",
          subtitle = "mit gam geglättet")+
 labs( x="", y ="NO2[μg/m3] / Gradzahl")

#===============
res.no2 <-Can_data_15_19_pred$no2.resid %>% na.locf %>% as_vector()
res.temp <- Can_data_15_19_pred$Temp.resid %>% na.locf %>% as_vector()
length(res.no2)
length(res.temp)
is.na(res.temp) %>% sum()# 0

cov(res.no2,res.temp)
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
#Can_dat moving average
Can_15_19_Temp_ma<- ma(Can_dat_15_19_xts$Temp,order = 24*30*3)
Can_15_19_NO2_ma<- ma(Can_dat_15_19_xts$NO2,order = 24*30*3)
Can_15_19_NO2_ma
Can_dat_15_19_Heizg <- Can_dat_15_19_Heizg %>% mutate(MonatsTemp =Can_15_19_Temp_ma,
                                                      NO2_mittel = Can_15_19_NO2_ma)
Can_dat_15_19_Heizg %>% ggplot(aes(x = datetime, y= 20 -MonatsTemp))+
  geom_line(col= "blue")+
  geom_smooth(aes(y = Grdz),col ="blue")+
  geom_line(aes(y= NO2_mittel),col = "purple")+
  geom_smooth(method = "gam",aes(y=NO2),col = "red")+
  coord_cartesian(xlim = c(x_min,x_max))
#=============================================
# Analyse Hintergrund Bernhausen
Brn_NO2 <- BW.all_data$Brn_data$Brn.NO2
Brn_Temp <- BW.all_data$Brn_data$Brn.Temp
Brn_15_19_Grdz <-Heizungsdaten_15_19$Brn
Brn_data_15_19 <-left_join(Brn_NO2,Brn_Temp) %>% right_join(Brn_15_19_Grdz)
NROW(Brn_data_15_19) #43774
Brn_data_15_19 %>% names() #"station"  "datetime" "NO2"      "Temp"     "Gradzahl" 
#QQ Brn plot Heizwaermebedarf NO2 Immissionen
Brn_data_15_19 %>% filter ( Temp < 15 ) %>%
  ggplot(aes(Gradzahl,NO2))+
  geom_point(size = 0.001,alpha = 0.5)+
  geom_smooth(method = "lm",col = "red")+
  ggtitle("NO2-Immissionen ~ Heizwaermebedarf(Gradzahl)
          Bernhausen (Heiztage 2015 bis 2019)")+
  labs( x = "Heizwärmebedarf(Gradzahl)", y = " NO2 [μg/m3]")+
  coord_cartesian(ylim = c(0,100))
lm(NO2 ~ Gradzahl, data = Brn_data_15_19 ) #Intercept 19.36 Slope 0.7698
# make xts
index_Brn_data_15_19 <- Brn_data_15_19$datetime
Brn_data_15_19_core <- Brn_data_15_19[,c(-1,-2)]
Brn_data_15_19_xts <- xts(Brn_data_15_19_core,order.by = index_Brn_data_15_19)
# Built various models
Brn_NO2_model <- lm (NO2 ~ datetime +1, data = Brn_data_15_19)
Brn_Temp_model <- lm(Gradzahl ~ datetime +1,data = Brn_data_15_19)
# add predictions & residuals to Brn Data
Brn_data_15_19 <- Brn_data_15_19 %>% 
  add_predictions(Brn_NO2_model)%>% 
  add_residuals(Brn_NO2_model) 
names(Brn_data_15_19) <- names(Brn_data_15_19) %>% replace(c(6,7),c("no2.pred","no2.resid"))
Brn_data_15_19 <- Brn_data_15_19 %>%
  add_predictions(Brn_Temp_model) %>% add_residuals(Brn_Temp_model)
names(Brn_data_15_19) <-names(Brn_data_15_19) %>% replace(c(8,9),c("Temp.pred","Temp.resid"))
# Übersicht Bernhausen
Brn_data_15_19 %>% ggplot(aes(x=datetime, y = NO2))+
  geom_smooth(col = "red", linetype = 2)+
  geom_smooth(method = "lm",col = "red", linetype = 1)+
  geom_smooth(col = "blue", linetype =2, aes( x= datetime, y =Gradzahl))+
  geom_smooth(method = "lm",col = "blue", linetype =1, aes( x= datetime, y =Gradzahl))+
  coord_cartesian(xlim = c(x_min,x_max))+
  ggtitle("NO2-Immissionen(rot) & Heizbedarf(blau)
          Mittelwerte und linearer Trend
          Bernhausen, Heubergstr")+
  labs( x="", y ="NO2[μg/m3] / Gradzahl")
# Bernhausen linearer Trend eliminiert
Brn_data_15_19 %>% ggplot(aes(x=datetime, y = no2.resid))+
  geom_smooth(col = "red", linetype = 2)+
  geom_smooth(method = "lm",col = "red", linetype = 1)+
  geom_smooth(col = "blue", linetype =2, aes( x= datetime, 
                                              y =Temp.resid))+
  geom_smooth(method = "lm",col = "black", linetype =1, 
              aes( x= datetime, y =Temp.resid))+
  coord_cartesian(xlim = c(x_min,x_max))+
  ggtitle("NO2-Immissionen(rot) & Heizbedarf(blau)
    Abweichungen vom linearen Trend(schwarz)
       Bernhausen, Heubergstr",
          subtitle = "mit gam geglättet")+
  labs( x="", y ="NO2[μg/m3] / Gradzahl")
# Reutlingen
Rt_NO2 <- BW.all_data$Rt_data$Rt.no2
Rt_Temp <- BW.all_data$Rt_data$Rt.temp
Rt_15_19_Grdz <-Heizungsdaten_15_19$Rt
Rt_data_15_19 <-left_join(Rt_NO2,Rt_Temp) %>% right_join(Rt_15_19_Grdz)
NROW(Rt_data_15_19) #41950
Rt_data_15_19 %>% names() #"station"  "datetime" "NO2"      "Temp"     "Gradzahl" 
#QQ Rt plot Heizwaermebedarf NO2 Immissionen
Rt_data_15_19 %>% filter ( Temp < 15 ) %>%
  ggplot(aes(Gradzahl,NO2))+
  geom_point(size = 0.001,alpha = 0.5)+
  geom_smooth(method = "lm",col = "red")+
  ggtitle("NO2-Immissionen ~ Heizwaermebedarf(Gradzahl)
         Reutlingen (Heiztage 2015 bis 2019)")+
  labs( x = "Heizwärmebedarf(Gradzahl)", y = " NO2 [μg/m3]")+
  coord_cartesian(ylim = c(0,100))
lm(NO2 ~ Gradzahl, data = Rt_data_15_19 ) #Intercept 19.36 Slope 0.7698
# make xts
index_Rt_data_15_19 <- Rt_data_15_19$datetime
Rt_data_15_19_core <- Rt_data_15_19[,c(-1,-2)]
Rt_data_15_19_xts <- xts(Rt_data_15_19_core,order.by = index_Rt_data_15_19)
# Built various models
Rt_NO2_model <- lm (NO2 ~ datetime +1, data = Rt_data_15_19)
Rt_Temp_model <- lm(Gradzahl ~ datetime +1,data = Rt_data_15_19)
# add predictions & residuals to Rt Data
Rt_data_15_19 <- Rt_data_15_19 %>% 
  add_predictions(Rt_NO2_model)%>% 
  add_residuals(Rt_NO2_model) 
names(Rt_data_15_19) <- names(Rt_data_15_19) %>% replace(c(6,7),c("no2.pred","no2.resid"))
Rt_data_15_19 <- Rt_data_15_19 %>%
  add_predictions(Rt_Temp_model) %>% add_residuals(Rt_Temp_model)
names(Rt_data_15_19) <-names(Rt_data_15_19) %>% replace(c(8,9),c("Temp.pred","Temp.resid"))
# Übersicht Reutlingen
Rt_data_15_19 %>% ggplot(aes(x=datetime, y = NO2))+
  geom_smooth(col = "red", linetype = 2)+
  geom_smooth(method = "lm",col = "red", linetype = 1)+
  geom_smooth(col = "blue", linetype =2, aes( x= datetime, y =Gradzahl))+
  geom_smooth(method = "lm",col = "blue", linetype =1, aes( x= datetime, y =Gradzahl))+
  coord_cartesian(xlim = c(x_min,x_max))+
  ggtitle("NO2-Immissionen(rot) & Heizbedarf(blau)
          Mittelwerte und linearer Trend
         Reutlingen, Friedrichstrasser")+
  labs( x="", y ="NO2[μg/m3] / Gradzahl")
# Reutlingen linearer Trend eliminiert
Rt_data_15_19 %>% ggplot(aes(x=datetime, y = no2.resid))+
  geom_smooth(col = "red", linetype = 2)+
  geom_smooth(method = "lm",col = "red", linetype = 1)+
  geom_smooth(col = "blue", linetype =2, aes( x= datetime, 
                                              y =Temp.resid))+
  geom_smooth(method = "lm",col = "black", linetype =1, 
              aes( x= datetime, y =Temp.resid))+
  coord_cartesian(xlim = c(x_min,x_max))+
  ggtitle("NO2-Immissionen(rot) & Heizbedarf(blau)
    Abweichungen vom linearen Trend(schwarz)
       Reutlingen Friedrichstrasse",
          subtitle = "mit gam geglättet")+
  labs( x="", y ="NO2[μg/m3] / Gradzahl")
Rt_data_15_19 %>% filter ( Temp < 15 ) %>%
  ggplot(aes(Gradzahl,NO2))+
  geom_point(size = 0.001,alpha = 0.5)+
  geom_smooth(method = "lm",col = "red")+
  ggtitle("NO2-Immissionen ~ Heizwaermebedarf(Gradzahl)
          Reutlingen (Heiztage 2015 bis 2019)")+
  labs( x = "Heizwärmebedarf(Gradzahl)", y = " NO2 [μg/m3]")+
  coord_cartesian(ylim = c(0,100))
lm(NO2 ~ Gradzahl, data = Rt_data_15_19 ) #Intercept 19.36 Slope 0.7698

