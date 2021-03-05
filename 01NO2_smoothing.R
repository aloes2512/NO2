# Anwendung NO2_smoothing auf alle Messstellen
#vectorization is preferred over for-loops since it results in shorter and clearer code
URL <-"https://noamross.github.io/gams-in-r-course/chapter1"

browseURL(URL)
library(tidyverse)
library(lubridate)
library(xts)
library(dslabs)
library(mgcv)
library(broom)
library(stats)
datapath <-"~/Documents/Luftqualitaet/Daten/BW/"
# Lade Daten von ausgewählten Stationen
load(paste0(datapath,"BW.RData"))


save(BW.all_data, file = paste0(datapath,"BW.RData"))

BW.all_data%>% summary()
names(BW.all_data)
Can_NO2 <- BW.all_data %>% .$Stg.Can %>% .$no2%>%
  filter(NO2 != is.na(NO2))
  #mutate(NO2= na.locf(NO2),tm = as.numeric(datetime))
Can_Temp <- BW.all_data$Stg.Can$temp %>% 
  filter(Temp != is.na(Temp))
   #mutate(Temp= na.locf(Temp,fromLast=TRUE))
Can_dat <- inner_join(Can_NO2,Can_Temp)
Can_dat <- Can_dat %>% mutate(Heizg = if_else(Temp<15, 20 -Temp,0))
Can_Heizg_mdl <-Can_dat%>%  gam(Heizg ~ s(as.numeric(datetime), k = 40),data = .)
Can_NO2_mdl <-Can_dat%>%  gam(NO2 ~ s(as.numeric(datetime), k = 40),data = .)
Can_dat$NO2_fit <-Can_NO2_mdl$fitted.values 
Can_dat$Heizg_fit <-Can_Heizg_mdl$fitted.values 
summary(Can_dat)
NO2_qnt <-stats::quantile(Can_dat$NO2_fit)
Heizg_qnt <-stats::quantile(Can_dat$Heizg_fit)
Can_qnt <- tibble ( NO2_qnt,Heizg_qnt)
Can_qnt$perc <- c("Min","25%","Median","75%","Max")
cal_fac <- function(Can_qnt){
  fac <- (Can_qnt$NO2_qnt[[4]]-Can_qnt$NO2_qnt[[2]])/
    (Can_qnt$Heizg_qnt[[4]]-Can_qnt$Heizg_qnt[[2]]) 
  return(fac)
}
 fct <- cal_fac(Can_qnt = Can_qnt)%>% unclass() # 0.955412
 WWimm <- Heizg_qnt[[3]]*.2*fct
 Can_dat <- Can_dat%>% mutate(Heizimm = Heizg_fit*fct+WWimm)
cor(Can_dat$NO2_fit,Can_dat$Heizimm)#[1] 0.6405011
Can_dat %>% filter(datetime > ymd("2000-06-30")&datetime < ymd("2019-07-01"))%>%
  ggplot(aes( x = datetime))+
  #geom_point(aes( y= NO2), size = 0.00001,alpha = 0.9)+
  geom_line(aes( y = NO2_fit),col = "red")+
  geom_line(aes(y = Heizimm),col = "black")+
  ggtitle(" NO2-Immissionen gemittelt (rot)
          Bad Cannstatt",
          subtitle= "anteilige  Heizgsimmissionen (schwarz)" )+
  labs (x = "",y = "NO2 [μg/m3]" )
# Sws daten
BW.all_data %>% .$Sws
Sws_NO2 <- BW.all_data %>% .$Sws %>% .$NO2%>%
  filter(NO2 != is.na(NO2))

Sws_Temp <- BW.all_data$Sws$Temp %>% 
  filter(!is.na(Temp))
Sws_dat <- inner_join(Sws_NO2,Sws_Temp)
Sws_dat <- Sws_dat %>% 
  mutate(Heizg = if_else(Temp<15, 20 -Temp,0))
Sws_Heizg_mdl <-Sws_dat%>%  gam(Heizg ~ s(as.numeric(datetime), k = 40),data = .)
Sws_NO2_mdl <-Sws_dat%>%  gam(NO2 ~ s(as.numeric(datetime), k = 40),data = .)
Sws_dat$NO2_fit <-Sws_NO2_mdl$fitted.values 
Sws_dat$Heizg_fit <-Sws_Heizg_mdl$fitted.values 
summary(Sws_dat)
# Bestimmung prop zwischen Gradzahl und Immissionen
Sws_qnt <- tibble ( NO2_qnt =stats::quantile(Sws_dat$NO2_fit),
                    Heizg_qnt=stats::quantile(Sws_dat$Heizg_fit))
Sws_qnt$perc <- c("Min","25%","Median","75%","Max")
fct <- cal_fac(Can_qnt = Sws_qnt)%>% unclass() #[1] 0.2103684
# Immissionen zu WW Erzeugung
WWimm <- Heizg_qnt[[3]]*.2*fct #[1] 0.3663953
Sws_dat <- Sws_dat%>% mutate(Heizimm = Heizg_fit*fct+WWimm)
Sws_dat %>% filter(datetime > ymd("2000-06-30")&datetime < ymd("2019-07-01"))%>%
  ggplot(aes( x = datetime))+
  geom_line(aes( y = NO2_fit),col = "red")+
  geom_line(aes(y = Heizimm),col = "black")+
  ggtitle(" NO2-Immissionen gemittelt (rot)
  Schwarzwald Süd Jul/2006 - Jun/2019",
          subtitle= "anteilige  Heizgsimmissionen (schwarz)" )+
  labs (x = "",y = "NO2 [μg/m3]" )
Sws_fit<-Sws_dat %>% summarise(NO2_Mittel= median(NO2_fit),
                      NO2_Heizg = median(Heizimm))
summary(Sws_fit)# NO2_Mittel 4.569  NO2_Heizg 2.798
cor(Sws_dat$NO2_fit,Sws_dat$Heizimm) #[1] 0.8703135
#' Reutlingen Pomologie'
RT.pm_NO2 <- BW.all_data$Rt.pom$no2 %>%
  filter(NO2 != is.na(NO2))
  #mutate(NO2= na.locf(NO2),tm = as.numeric(datetime))
RT.pm_Temp <- BW.all_data$Rt.pom$temp %>%
  filter(Temp != is.na(Temp))
  
RT.pm_dat <- inner_join(RT.pm_NO2,RT.pm_Temp)
RT.pm_dat <- RT.pm_dat %>% 
  mutate(Heizg = if_else(Temp<15, 20 -Temp,0))
RT.pm_Heizg_mdl <-RT.pm_dat%>%  gam(Heizg ~ s(as.numeric(datetime), k = 40),data = .)
RT.pm_NO2_mdl <-RT.pm_dat%>%  gam(NO2 ~ s(as.numeric(datetime), k = 40),data = .)
RT.pm_dat$NO2_fit <-RT.pm_NO2_mdl$fitted.values 
RT.pm_dat$Heizg_fit <-RT.pm_Heizg_mdl$fitted.values 
summary(RT.pm_dat)
#=====
# Bestimmung prop zwischen Gradzahl und Immissionen
RT.pm_qnt <- tibble ( NO2_qnt =stats::quantile(RT.pm_dat$NO2_fit),
                    Heizg_qnt=stats::quantile(RT.pm_dat$Heizg_fit))
RT.pm_qnt$perc <- c("Min","25%","Median","75%","Max")
fct <- cal_fac(Can_qnt = RT.pm_qnt)%>% unclass() # [1] 0.8067057
WWimm <- RT.pm_qnt$Heizg_qnt[[3]]*.2*fct
RT.pm_dat <- RT.pm_dat%>% mutate(Heizimm = Heizg_fit*fct+WWimm)
# Darstellung als Plot
RT.pm_dat %>% filter(datetime > ymd("2000-06-30")&datetime < ymd("2019-07-01"))%>%
  ggplot(aes( x = datetime))+
  geom_line(aes( y = NO2_fit),col = "red")+
  geom_line(aes(y = Heizimm),col = "black")+
  ggtitle(" NO2-Immissionen gemittelt (rot)
          Reutlingen Pomologie",
          subtitle= "anteilige  Heizgsimmissionen (schwarz)
          6/2006 bis 6/2019" )+
  labs (x = "",y = "NO2 [μg/m3]" )
RT.pm_dat %>% summarise(NO2_Mittel= median(NO2_fit),
                      NO2_Heizg = median(Heizimm))#24.8      8.45
cor(RT.pm_dat$NO2_fit,RT.pm_dat$Heizimm) #[1] 0.9278111
# Bernhausen
Brn_Temp <- BW.all_data$Brn$Brn.Temp%>%
  filter(Temp != is.na(Temp))
Brn_NO2 <- BW.all_data$Brn$NO2%>% 
  filter(NO2 != is.na(NO2))
Brn_dat <- inner_join(Brn_NO2,Brn_Temp)
Brn_dat <- Brn_dat %>% 
  mutate(Heizg = if_else(Temp<15, 20 -Temp,0))
Brn_Heizg_mdl <-Brn_dat%>%  gam(Heizg ~ s(as.numeric(datetime), k = 40),data = .)
Brn_NO2_mdl <- mgcv::gam(NO2~ s(as.numeric(datetime), k= 40),data = Brn_dat)
Brn_dat$NO2_fit <- Brn_NO2_mdl$fitted.values
Brn_dat$Heizg_fit <-Brn_Heizg_mdl$fitted.values 
summary(Brn_dat)
# Bestimmung prop zwischen Gradzahl und Immissionen
Brn_qnt <- tibble ( NO2_qnt =stats::quantile(Brn_dat$NO2_fit),
                      Heizg_qnt=stats::quantile(Brn_dat$Heizg_fit))
Brn_qnt$perc <- c("Min","25%","Median","75%","Max")
fct <- cal_fac(Can_qnt = Brn_qnt)%>% unclass() # [1] 1.524818
WWimm <- Brn_qnt$Heizg_qnt[[3]]*.2*fct # [1] [1] 3.020678
Brn_dat <- Brn_dat%>% mutate(Heizimm = Heizg_fit*fct+WWimm)
# Darstellung als plot
Brn_dat %>% filter(datetime > ymd("2000-06-30")&datetime < ymd("2019-07-01"))%>%
  ggplot(aes( x = datetime))+
  geom_line(aes( y = NO2_fit),col = "red")+
  geom_line(aes(y = Heizimm),col = "black")+
  ggtitle(" NO2-Immissionen gemittelt (rot)
          Bernhausen",
          subtitle= "anteilige  Heizgsimmissionen (schwarz)
          06/2000 bis 06/2019" )+
  labs (x = "",y = "NO2 [μg/m3]" )
Brn_dat %>% summarise(NO2_Mittel= mean(NO2_fit),
                        NO2_Heizg = mean(Heizimm))#30.8  18.1
cor(Brn_dat$NO2_fit,Brn_dat$Heizimm)
Brn_dat %>% 
  filter(datetime >= ymd("2014-01-01")&datetime <ymd ("2018-12-31"))%>%
  ggplot(aes(x = datetime))+
  geom_line(aes(y =NO2.fit))+
  geom_line(aes(y =Temp.fit),col = "red")

# Schwäbische Alb
Alb_NO2 <- BW.all_data %>% .$Alb %>% .$NO2%>%
  filter(!is.na(NO2))
Alb_Temp <- BW.all_data$Alb$Temp %>% 
  filter(!is.na(Temp))

Alb_dat <- inner_join(Alb_NO2,Alb_Temp)
Alb_dat <- Alb_dat %>% mutate(Heizg = if_else(Temp<15, 20 -Temp,0))
Alb_Heizg_mdl <-Alb_dat%>%  gam(Heizg ~ s(as.numeric(datetime), k = 40),data = .)
Alb_NO2_mdl <-Alb_dat%>%  gam(NO2 ~ s(as.numeric(datetime), k = 40),data = .)
Alb_dat$NO2_fit <-Alb_NO2_mdl$fitted.values 
Alb_dat$Heizg_fit <-Alb_Heizg_mdl$fitted.values 
summary(Alb_dat)
NO2_qnt <-stats::quantile(Alb_dat$NO2_fit)
Heizg_qnt <-stats::quantile(Alb_dat$Heizg_fit)
Alb_qnt <- tibble ( NO2_qnt,Heizg_qnt)
Alb_qnt$perc <- c("Min","25%","Median","75%","Max")
cal_fac <- function(Alb_qnt){
  fac <- (Alb_qnt$NO2_qnt[[4]]-Alb_qnt$NO2_qnt[[2]])/
    (Alb_qnt$Heizg_qnt[[4]]-Alb_qnt$Heizg_qnt[[2]]) 
  return(fac)
}
fct <- cal_fac(Alb_qnt = Alb_qnt)%>% unclass() # 0.3892805
WWimm <- Heizg_qnt[[3]]*.2*fct # 0.9158427
Alb_dat <- Alb_dat%>% mutate(Heizimm = Heizg_fit*fct+WWimm)

Alb_dat %>% filter(datetime > ymd("2000-06-30")&datetime < ymd("2019-07-01"))%>%
  ggplot(aes( x = datetime))+
  #geom_point(aes( y= NO2), size = 0.00001,alpha = 0.9)+
  geom_line(aes( y = NO2_fit),col = "red")+
  geom_line(aes(y = Heizimm),col = "black")+
  ggtitle(" NO2-Immissionen gemittelt (rot)
          Schwäbische Alb",
          subtitle= "anteilige  Heizgsimmissionen (schwarz)
          06/2000 bis 06/2019" )+
  labs (x = "",y = "NO2 [μg/m3]" )
summary(Alb_dat)
Alb_dat_select <-Alb_dat %>% filter(datetime > ymd("2000-06-30")&datetime < ymd("2019-07-01"))
cor(Alb_dat_select$NO2_fit,Alb_dat_select$Heizimm)  #[1] 0.9480838
tail(Alb_dat_select)
Alb_dat %>% summarise(NO2_Mittel= mean(NO2_fit),
                      NO2_Heizg = mean(Heizimm))
