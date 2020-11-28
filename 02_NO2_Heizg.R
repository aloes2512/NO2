# Smoothing 
library(tidyverse)
library(lubridate)
library(modelr)
library(caret)
library(xts)
# Temp Daten
load("~/Documents/Luftqualitaet/Analysen/NO2/Dat/BW.RData")
BW.all_data%>% summary()
load("~/Documents/Luftqualitaet/Daten/BW_Rdat/BW_station.RData")
BW_stations_NO2_tbl %>% summary()
BW_stations_NO2_tbl$name %>% levels()
load("~/Documents/Luftqualitaet/Daten/BW_Rdat/Stgt_NO2_data.RData")
Stgt_NO2 %>% summary()
Can_NO2<- Stgt_NO2 %>% filter (Messstelle== "Stuttgart-Bad_Cannstatt")
Can_data <- left_join(BW.all_data$Stgt_data$Can.no2,BW.all_data$Stgt_data$Can.temp)
mean(Can_data$NO2,na.rm = T) #32.1
sum (is.na(Can_data$NO2)) # 7368
 Can_data$NO2 <- Can_data$NO2 %>% na.locf()
 sum (is.na(Can_data$Temp)) # 1387
 mean(Can_data$Temp,na.rm = T)
 Can_data$Temp <- Can_data$Temp %>% na.locf()
 
Can_data <- Can_data %>% mutate(Grdz = ifelse(Temp < 15,20-Temp,0))
Can_data %>% head(2)
# Model  NO2 -data
x_min <- ymd("2015-06-30") %>% as.POSIXct()
x_max <- ymd("2019-06-30") %>% as.POSIXct()
NO2_mod <- lm(NO2 ~ datetime,data = Can_data)
Can_data <- Can_data %>% add_predictions(NO2_mod) %>%
  add_residuals(NO2_mod)
Can_data_15_19 <- Can_data  %>% filter (datetime >= ymd("2015-06-30") &
                        datetime <= ymd("2019-06-30")) 
Can_data_15_19 %>% ggplot(aes(x = datetime, y = NO2))+
  geom_smooth(aes(x=datetime, y = Grdz),col ="blue")+

  geom_smooth(method = "gam",col= "red",aes(x= datetime, y = resid))+
  ggtitle("NO2 [Mittel minus linearer Trend] (rot)
Gradzahl[ Mittlwert  aus 1-h Werten ] (blau)
          Bad Cannstatt 6/2015 bis 6/2019")+
  labs( x ="", y = "NO2 [mikrogramm/m3]....Temp [°C]")+
  coord_cartesian(xlim = c(x_min,x_max))

res <- Can_data$resid
grd  <- Can_data$Grdz
no2  <- Can_data$NO2
can_anova <- tibble(res,grdz=grd,no2)
Can_anova <- can_anova %>% filter (grdz >0)
lm(res ~ grdz, data = Can_anova )# -8.6 + 0.9 *grdz
lm(res ~ grdz +1,data = can_anova) # -7.1360  0.8 *grdz
Can_anova %>% ggplot(aes(grdz,res)) + 
geom_point(size = 0.01,col ="grey",shape = 2)+
  geom_smooth(method="lm",col = "blue")+
  geom_smooth(method = "lm",aes(grdz,no2),col= "red")
sd(res)
sd(no2) #19.4
# ks smoothing
fit_Can_ks <- with(Can_data %>% filter (datetime > ymd("2015-01-01")&datetime < ymd("2015-01-31")),
                   ksmooth(datetime,NO2,kernel ="normal"),span = 24*60)

Can_data %>% filter (datetime > ymd("2015-01-01")&datetime < ymd("2015-01-31"))%>% 
  mutate(smooth = fit_Can_ks$y) %>%
  ggplot(aes(x =datetime,y=smooth))+
  geom_line()
# loess smoothing
Can_data_2015_19 <- Can_data %>% filter(datetime <= ymd("2019-12-31")&
                                       datetime >= ymd("2015-01-01"))

total_hours <- length(Can_data_2015_19$datetime) #26281
span <- 365*24/total_hours # 
fit <- loess(NO2 ~ datetime%>%as.numeric(), degree=2, span = span, data=Can_data_2015_19)
fit$fitted
Can_data_2015_19 <-Can_data_2015_19 %>% mutate(fit.no2 =fit$fitted)
Can_data_2015_19 %>% ggplot(aes(x=datetime, y = fit.no2))+
  geom_line()+
  geom_smooth(aes(y = Grdz))
Can_data_2015_19 %>% head(2)
# filter using moving average
# https://cran.r-project.org/web/packages/smooth/vignettes/sma.html
index_Can_data <-Can_data_2015_19$datetime

