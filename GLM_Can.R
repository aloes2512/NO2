#Generalized linear Regression BOOK "Generalized.....
library(tidyverse)
library(readxl)
library(lubridate)
library(broom)
library(mgcv)
library(xts)
library(mgcViz)
library(gridExtra)
library(qgam)
install.packages("codetools")
library(codetools)
load("~/Documents/Luftqualitaet/Daten/BW/BW.RData")
 # BW.all_data
summary(BW.all_data)
load("~/Documents/Luftqualitaet/Analysen/Stationsdaten.RData")
summary(Stationsdaten)


# ========================================================
# Can Daten in tbl zusammenfassen
BW.all_data$Stg.Can_data %>% summary()
Can_tbl <- inner_join(BW.all_data[["Stg.Can_data"]][["Stg.Can.no2"]],
                      BW.all_data$Stg.Can_data$Stg.Can.no)
Can_tbl <- inner_join(Can_tbl,BW.all_data$Stg.Can_data$Stg.Can.temp)
Can_data_tbl <- Can_tbl %>% mutate(Heizb = ifelse((20-Temp) >0, 20-Temp,0))
Can_data_tbl%>% filter (Temp < 15) %>% NROW(.) # 114020
prozHzg <-NROW(Can_data_tbl%>% filter(Temp<15))/Can_data_tbl%>%  NROW(.)
WW_grdz <- 20 -mean(Can_data_tbl$Temp,na.rm=TRUE)*prozHzg
Can_data_tbl <- Can_tbl %>% mutate(Heizb = ifelse((20-Temp) >0, 20-Temp+WW_grdz,WW_grdz))
Can_data_tbl %>% summary()
# Subset of data
Can_data_15_19 <-Can_data_tbl %>% filter(datetime >= ymd("2015-01-01")) 

# Lineares Regressionsmodel
Can_lmodel_15_19 <- Can_data_15_19 %>%
  lm(formula = ( NO2 ~ 1+ datetime+ NO+Heizb),na.action =na.exclude )
Can_lin_parms <-tidy(Can_lmodel_15_19)
names(Can_lmodel_15_19) #[1] "coefficients"  "residuals"     "effects"       "rank"          "fitted.values" "assign"
# [7] "qr"            "df.residual"   "xlevels"       "call"          "terms"         "model"
# Overview Can_data
Can_data_15_19 %>%
  ggplot(aes(x =datetime,y=NO2))+
  geom_line(mapping =aes(y = predict(Can_lmodel_15_19)),alpha = 0.1 )+
  geom_smooth(mapping =aes(y = predict(Can_lmodel_15_19)),col ="red")+
  geom_smooth(mapping =aes(y = NO),col = "green")+
  ggtitle("NO2/NO - Immissionen (rot/grün)
   (lineares Modell) geglättete Darstellung ",
          subtitle = "Bad Cannstatt")+
  labs( x ="", y = "NO2/NO [μg/m3]")
# Daten ohne Trend' == residuen
Can_data_15_19$NO2 <- Can_data_15_19$NO2 %>% replace_na(mean(.,na.rm =T))
Can_data_15_19$NO <- Can_data_15_19$NO %>% replace_na(mean(.,na.rm =T))
Can_data_15_19$Temp <- Can_data_15_19$Temp %>% replace_na(mean(.,na.rm =T))
Can_data_15_19$Heizb <- Can_data_15_19$Heizb %>% replace_na(mean(.,na.rm =T))
Can_15_19_completed <- tibble(datetime = Can_data_15_19$datetime,
                              NO2_data =Can_data_15_19$NO2,
                            NO_data = Can_data_15_19$NO ,
                            Temp_data = Can_data_15_19$Temp,
                            Heizb_data =Can_data_15_19$Heizb
                            )
Can_15_19_completed %>% head(2)
#  mgcViz example b <- gam(y ~ s(x1) + s(x2) + x3, data = dat, method = "REML")
Can_modl <- mgcv::gam(data = Can_15_19_completed, 
                      formula = NO2_data ~ s(NO_data,Heizb_data,k =10),
                      method = "REML",sm = 20)
Can_modl <- getViz(Can_modl)
plt <-plot(Can_modl,20)
plt1 <-plt + l_fitRaster() + l_fitContour() + l_points()
plt1+ 
  l_rug()+
  l_points()
print(plt1,allTerms =TRUE,pages =1)
# Alternativer code für mgcViz
Can_15_19_completed %>% head()
dat2 <- tibble( x1 = Can_15_19_completed$datetime%>% as.numeric(),
                x2 = Can_15_19_completed$NO_data,
                x3 = Can_15_19_completed$Heizb_data)
dat2$y <- Can_15_19_completed$NO2_data
bb <- mgcv::gam(y ~ s(x1, x2, k=10) + x3, data = dat2, method = "REML",sm =10)
bb <- getViz(bb, nsim = 20)
plot(sm(bb, 1)) + l_fitRaster() + l_fitContour() + l_points()
plot(pterm(bb, 1)) + l_ciPoly() + l_fitLine() # effect of parameter x3
print(plot(getViz(bb),allTerms = TRUE),pages =2)

Can_15_19_completed %>% 
  pivot_longer(cols = c(NO2_data,NO_data,Temp_data,Heizb_data),names_to = "Werte")%>%
  ggplot(data = .,mapping =aes(x= Werte, y =value))+
  geom_boxplot()

Can_15_19 <- list(Varbl = Can_15_19_completed)
# Calculate Models and residuals                  
Can_15_19_lm_NO2 <-  lm ( data = Can_15_19_completed,formula =NO2_data ~ 1+ datetime, na.action = na.exclude)
Can_15_19_NO2_resid <-tibble (NO2_resid =Can_15_19_lm_NO2$residuals )
Can_15_19_lm_NO <-  lm ( data = Can_15_19_completed,formula =NO_data ~ 1+ datetime, na.action = na.exclude)
Can_15_19_NO_resid <-tibble(NO_resid =Can_15_19_lm_NO$residuals )
Can_15_19_lm_NO2$model %>% dim() # 41952
Can_15_19_NO2_resid %>% dim() # [1] 41952     1
Can_15_19_NO2_detrend <-Can_15_19_lm_NO2$model %>% bind_cols(Can_15_19_NO2_resid)
Can_15_19_NO2_detrend %>% ggplot(aes(x =datetime, y = NO2_resid))+
  geom_smooth(col = "red")
Can_15_19_NO_resid %>% dim()
Can_15_19_NO_detrend <-Can_15_19_lm_NO$model %>% bind_cols(Can_15_19_NO_resid)
Can_15_19_NO_detrend %>% ggplot(aes(x =datetime, y = NO_resid))+
  geom_smooth(col = "green")+
  geom_smooth(data =Can_15_19_NO2_detrend,aes(x =datetime, y = NO2_resid),col ="red")
Can_15_19_lm_Heizb <- lm (data= Can_data_15_19,formula = Heizb ~ 1+ datetime, na.action = na.exclude)
Can_15_19_Heizb_resid <- tibble(Heizb_resid = Can_15_19_lm_Heizb$residuals)
Can_15_19_Heizb_detrend <-Can_15_19_lm_Heizb$model %>% bind_cols(Can_15_19_Heizb_resid)
Can_15_19_detrend <- Can_15_19_NO2_detrend %>% right_join(Can_15_19_NO_detrend)%>% right_join(Can_15_19_Heizb_detrend)
Can_15_19_detrend <-Can_15_19_detrend %>% dplyr::select(-c(NO2_resid,NO_resid,Heizb_resid)) %>% 
  mutate(datetime =floor_date(datetime,unit = "hour") %>% 
           format("%Y-%m-%d %H")) 

Can_15_19_NO_detrend %>% ggplot(aes(x =datetime, y = NO_resid))+
  geom_smooth(col = "green")+
  geom_smooth(data =Can_15_19_NO2_detrend,aes(x =datetime, y = NO2_resid),col ="red")+
  geom_smooth(data = Can_15_19_Heizb_detrend , aes(x =datetime, y = Heizb_resid),col = "black")+
  labs (x = "", y = "NO,NO2[μg/m3],Heizb [ ° ] ")+
  ggtitle("Saisonaler Verlauf (Abweichung vom  Trend)
          Immissionen NO (grün), NO2(rot) & Heizbedarf",
          subtitle = "Bad Cannstatt 2015 bis 2019")

# Gam mgcv package
#check for missing values in timeseries
is.na(Can_15_19_detrend ) %>% sum() # 0
help("mgcv-package")


# repl_na first as fitted values will not contained for missing values
Can_15_19_detrend %>% head(2)
Can_15_19_detrend <-Can_15_19_detrend %>% mutate(Nr = 1:NROW(.))
Can_15_19_detrend %>% head(2)
Can_gamodel <-Can_15_19_detrend %>%
  mgcv::gam(NO2_data ~ s(Nr, k=10)+s(NO_data,k= 3)+Heizb, method = "REML",data = .)
dim(Can_gamodel$model) #[1] 173446      3
plot(Can_gamodel)
Can_15_19_detrend  %>% ggplot(aes(x = datetime))+
  geom_smooth(mapping =aes(y = predict(Can_gamodel)),col ="red")+
  geom_smooth(mapping = aes(y = NO_data),col = "green")+
  ggtitle ("NO2/NO-Immissionen Bad Cannstatt 15/19
           Abweichung vom Trend",
           subtitle= "GAM Filter/Smooth mit k =3 (rot)
  NO (grün) mit automatischen Filterkonstanten")+
  labs (x = "Jahr", y = "NO2/NO")
#b <- gam(y ~ s(x1) + s(x2) + x3, data = dat, method = "REML")



# Beschränkung auf 1 Jahr 2019
Can_data_19 <- Can_tbl %>% filter(datetime >= ymd("2019-01-01"))
Can_data_19 <- Can_data_19 %>% mutate(Heizb = ifelse((20-Temp) >0, 20-Temp,0))
Can_data_19%>% filter (Temp < 15) %>% NROW(.) # 3776
prozHzg <-NROW(Can_data_19%>% filter(Temp<15))/Can_data_19%>%  NROW(.)
WW_grdz <- 20 -mean(Can_data_19$Temp,na.rm=TRUE)*prozHzg
Can_data_19 <- Can_data_19 %>% mutate(Heizb = ifelse((20-Temp) >0, 20-Temp+WW_grdz,WW_grdz))
Can_data_19$NO2 <- replace_na(Can_data_19$NO2, mean(Can_data_19$NO2,na.rm= TRUE))
Can_data_19$NO <- replace_na(Can_data_19$NO, mean(Can_data_19$NO,na.rm= TRUE))
Can_data_19 %>% summary()
Can_gamodel_NO2_19 <- Can_data_19 %>% 
  mgcv::gam(NO2 ~ s(datetime %>% as.numeric(), k=10),method = "REML",data =.)
Can_data_19 %>% ggplot(aes(x = datetime))+
  geom_smooth(mapping =aes(y = predict(Can_gamodel_NO2_19 )),col ="red",data=Can_data_19)+
  geom_smooth(mapping = aes(y = NO),col = "green")+
  geom_smooth(mapping = aes(y= Heizb),data = Can_data_19)+
  ggtitle ("NO2/NO-Immissionen 
  Heizbedarf Bad Cannstatt 2019",
           subtitle= " GAM Filter k =20 ´
  NO2 (rot), NO (grün), Heizbedarf (blau)")+
  labs (x = "Monat", y = "NO2/NO [μg/m3]")
# Boxplot
Can_data_15_19_komp <-Can_data_15_19 %>% pivot_longer(cols = c(-station,-datetime),names_to = "Komp") 
Can_data_15_19_komp$Komp <-Can_data_15_19_komp$Komp %>% as_factor()  
Can_data_15_19_komp %>% 
ggplot(aes( x = Komp,y = value))+
  geom_boxplot()+
  ggtitle("Immissionen Bad Cannstatt
  Werteverteilung NO2 / NO / Temp / Heizbedarf")+
  labs( x ="", y = "NO2/NO [μg/m3], Temp/Heizb [°C ]")
#' Darstellung mit mgcViz'
my_dat<- data.frame(NO = Can_15_19$Varbl$NO_data, 
                    Heizb = Can_15_19$Varbl$Heizb_data, 
                    Temp = Can_15_19$Varbl$Temp_data)
my_dat$NO2 <- Can_15_19$Varb$NO2_data
my_gam <-gam(NO2 ~ s(NO) + s(Heizb) + Temp, data = my_dat, method = "REML")
b_can <- getViz(my_gam, nsim = 20)
o_can <- plot(sm(b_can,1))
o_can + l_fitLine(col = "red")+ #  layer add lines graphical parameters as with geom_line
  l_ciLine(col = "blue") + # adds confidence intervalls 95%
l_rug() + #geom_rug {ggplot2}
  l_points() #' graphical arguments from ggpoint'
# 2D smootheffects  "b <- gam(y ~ s(x1, x2) + x3, data = dat, method = "REML")"
no.hzg_can <- gam(NO2 ~ s(Heizb, NO)+Temp, data = my_dat, method = "REML")
# b <- getViz(b)
no.hzg_can <- getViz(no.hzg_can)
# plot(sm(b, 1)) + l_fitRaster() + l_fitContour() + l_points()
plot(sm(no.hzg_can,1)) + l_fitRaster()+l_fitContour()+l_points()
#plot(pterm(b, 1)) + l_ciPoly() + l_fitLine()
plot(pterm(no.hzg_can,1))+ l_ciPoly()+ l_fitLine(linetype = 2, col = "red")+
  ggtitle ("NO2- Zusatz Immissionen 
  Funktion ~ Aussentemperatur
  Bad Cannstatt 2015 bis 2019")+
  labs( y = "NO2-Anteil f(Temp)")


