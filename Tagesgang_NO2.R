# correlate with periodic signals
library(tidyverse)
library(modelr)
library(xts)
library(broom)
library(lubridate)
load("~/Documents/Luftqualitaet/Analysen/BW_station.RData")
levels(BW_stations_NO2_tbl$name)<- levels(BW_stations_NO2_tbl$name) %>% 
  recode("Rt_"="Rt_pomol","Rtl"="Rt_leder",
         "Lbg_4"="Lbg_weimar","Frei"= "Freiburg",
         "Heid"="Heidelbg","Heil"="Heilbrn") 
BW_stations_NO2_tbl$station <- as_factor(BW_stations_NO2_tbl$station)
# Fehlende NO2 Werte durch Folgewert ersetzen
BW_stations_NO2_tbl<-BW_stations_NO2_tbl %>% mutate(NO2 = na.locf(.$NO2,fromLast= TRUE))
#Einfuegen zusäzlicher Variablen, Monat, KWoche,WTag, Stunde
BW_stations_NO2_tbl<- BW_stations_NO2_tbl %>%
  mutate(Monat = as.numeric(floor_date(datetime,unit= "month")%>%format("%m")))#
BW_stations_NO2_tbl<- BW_stations_NO2_tbl %>%  
mutate(KWoche = as.numeric(floor_date(datetime,unit= "week")%>%format("%U")))
BW_stations_NO2_tbl<- BW_stations_NO2_tbl %>%   
mutate(WTag =as.numeric(floor_date(datetime,unit= "day")%>%format("%w")))
BW_stations_NO2_tbl<- BW_stations_NO2_tbl %>% 
  mutate(Stunde =as.numeric(floor_date(datetime,unit= "hour")%>%format("%H")))
head(BW_stations_NO2_tbl)
dim(BW_stations_NO2_tbl)#[1] 2385207       8
# erstelle grouped list
tm.list = list( tm.period <- c("Stunde","WTag","KWoche","Monat"),
                tm.code <- c("%H","%w","%U","%m"))
NO2_stations_grouped <-BW_stations_NO2_tbl %>% 
  group_by(name,station) %>% nest()
saveRDS(NO2_stations_grouped, file = "NO2_stations_grouped.rds")

#===============
NO2_Nck_Stunde <- NO2_stations_grouped %>% 
  filter( name == "Nck") %>% unnest(data)%>%
  group_by(Stunde) %>%
  summarise(NO2_stunde_mean = mean(NO2,na.rm = TRUE)) %>%
  mutate(varStunde = NO2_stunde_mean- mean(NO2_stunde_mean))%>%
  arrange(Stunde)
#________________
H.period.means <- function (grp_df,nm){
  df <- grp_df %>% filter(name == nm) %>% unnest(data)%>%
    group_by(Stunde)%>%
    summarise(NO2_h = mean(NO2,na.rm = TRUE))%>%
    mutate(name = nm,varH = NO2_h- mean(NO2_h))%>%
    arrange(Stunde)
  return(df)
}
WT.period.means <-function (grp_df,nm){
  df <- grp_df %>% filter(name == nm) %>% unnest(data)%>%
    group_by(WTag)%>%
    summarise(NO2_h = mean(NO2,na.rm = TRUE))%>%
    mutate(name = nm,varH = NO2_h- mean(NO2_h))%>%
    arrange(WTag)
  return(df)
}
NO2_Can_Stunde <-H.period.means(NO2_stations_grouped,"Can")
NO2_Nck_Stunde <- H.period.means(NO2_stations_grouped,"Nck")
NO2_Alb_Stunde <- H.period.means(NO2_stations_grouped,"Alb")
NO2_Sws_Stunde <- H.period.means(NO2_stations_grouped,"Sws")
NO2_Rtlg_Stunde <- H.period.means(NO2_stations_grouped,"Rt_pomol")
# Wochengang
NO2_Sws_WTag <- WT.period.means(NO2_stations_grouped,"Sws")
 Plt.h <- function(df){
  p<- ggplot(df,aes(Stunde,varH))+
    geom_point(col= "red",size =1)+
    geom_histogram(dat = df,aes(y= varH),stat = "identity",col = "grey")+
    ggtitle("     Tagesgang 
  Mittlere Abweichungen NO2",
            subtitle = paste(df$name," 2000 bis 2020"))+
    labs(x = "Stunde", y ="Abweichg. NO2 [μg/m3]")
  return(p)
  }
Plt.h(NO2_Nck_Stunde)
Plt.h(NO2_Can_Stunde)
Plt.h(NO2_Alb_Stunde)
Plt.h (NO2_Sws_Stunde)
Plt.h (NO2_Rtlg_Stunde)
  #----------------
# Wochengang
Plt.tg <- function(df){
  p<- ggplot(df,aes(WTag,varH))+
    geom_point(col= "red",size =1)+
    geom_histogram(dat = df,aes(y= varH),stat = "identity",col = "grey")+
    ggtitle("     Wochengang 
  Mittlere Abweichungen NO2",
            subtitle = paste(df$name," 2000 bis 2020"))+
    labs(x = "Sonntag(0) bis Samstag(6)", y ="Abweichg. NO2 [μg/m3]")
  return(p)
}
NO2_Sws_WTag <- WT.period.means(NO2_stations_grouped,"Sws")
Plt.tg (NO2_Sws_WTag)
NO2_Nck_WTag <- WT.period.means(NO2_stations_grouped,"Nck")
Plt.tg (NO2_Nck_WTag)
NO2_Can_WTag <- WT.period.means(NO2_stations_grouped,"Can")
Plt.tg(NO2_Can_WTag)
NO2_Rtlg_WTag <- WT.period.means(NO2_stations_grouped,"Rt_pomol")
Plt.tg(NO2_Rtlg_WTag)
#-------------------
NO2_Nck_KWoche <-NO2_stations_grouped %>% 
  filter( name == "Nck") %>% unnest(data)%>%
  group_by(KWoche) %>%
  summarise(NO2_KW_mean = mean(NO2,na.rm = TRUE)) %>%
  mutate(varKW = NO2_KW_mean- mean(NO2_KW_mean)) %>% 
  arrange(KWoche)
#----------------
NO2_Nck_Monat <- NO2_stations_grouped %>% 
  filter( name == "Nck") %>% unnest(data)%>%
  group_by(Monat) %>%
  summarise(NO2_Monats_mean = mean(NO2,na.rm = TRUE)) %>%
  mutate(varMonat = NO2_Monats_mean- mean(NO2_Monats_mean),name = "Nck") %>% 
  arrange(Monat)
# Tagesgang
head(NO2_Nck_Stunde)  
NO2_Nck_Stunde%>%
  ggplot(aes(Stunde))+
  geom_histogram(aes(x =Stunde,y= varStunde),stat = "identity",col = "grey")+
  geom_point(aes(y =varStunde),col= "red",size =1)+
  ggtitle("     Tagesgang 
  Mittlere Abweichungen NO2",
          subtitle = "  Am Neckartor 2003 bis 2020")+
  labs(x = "Stunde", y ="Abweichg. NO2 [μg/m3]")
#Wochengang
head(NO2_Nck_wday)
NO2_Nck_wday%>%
  ggplot(aes(WTag))+
  geom_histogram(aes(x =WTag,y= varDay),stat = "identity",col = "grey")+
  geom_point(aes(y =varDay),col= "red",size =1)+
  ggtitle("     Wochengang 
  Mittlere Abweichungen NO2",
          subtitle = "  Am Neckartor 2003 bis 2020")+
  labs(x = "Sonntag bis Samstag", y ="Abweichg. NO2 [μg/m3]")  
# Kalenderwochen
head(NO2_Nck_KWoche)
NO2_Nck_KWoche %>%
  ggplot(aes(KWoche))+
  geom_histogram(aes(x =KWoche,y= varKW),stat = "identity",col = "grey")+
  geom_point(aes(y =varKW),col= "red",size =1)+
  ggtitle("     Kalenderwochen 
  Mittlere Abweichungen NO2",
          subtitle = "  Am Neckartor 2003 bis 2020")+
  labs(x = "Kalenderwochen", y ="Abweichg. NO2 [μg/m3]")
# Jahresgang Monate
head(NO2_Nck_Monat)
NO2_Nck_Monat%>%
  ggplot(aes(Monat))+
  geom_histogram(aes(x =Monat,y= varMonat),stat = "identity",col = "grey")+
  geom_point(aes(y =varMonat),col= "red",size =1)+
  ggtitle("     Monate 
  Mittlere Abweichungen NO2",
          subtitle = "  Am Neckartor 2003 bis 2020")+
  labs(x = "Januar(1) bis Dezember(12)", y ="Abweichg. NO2 [μg/m3]")

#theme(axis.text.x = element_text(angle = 90, hjust = 1))+




# ====================


Nck_NO2_h.tbl <- NO2_Stundenmittel(BW_stations_NO2_tbl,"Nck")
Alb_NO2_h.tbl <- NO2_Stundenmittel(BW_stations_NO2_tbl,"Alb")
Can_NO2_h.tbl <- NO2_Stundenmittel(BW_stations_NO2_tbl,"Can")
Rtl_NO2_h.tbl<- NO2_Stundenmittel(BW_stations_NO2_tbl,"Rt")
tagesverlauf_plot <- function(df) {
  subttl <-paste(as.character(df$station[[1]]),"Station",as.character(df$name[[1]]))
  df %>% group_by(hour) %>% summarise(name,Std_mittel = mean(NO2,na.rm = TRUE)-mean(df$NO2,na.rm = TRUE)) %>%
    ggplot(aes(x = hour%>% as.integer(), y = Std_mittel))+
    geom_point()+
    geom_smooth(aes(x = hour%>% as.integer, y= Std_mittel),col = "red",se = FALSE)+
    ggtitle("Tagesverlauf NO2-Immissionen
    langj.Abweichg. v. Mittelwert pro Stunde",
            subtitle =subttl)+
    labs( x= "Stunde", y = "Std.mittel NO2 [μg/m3]")
  
}
tagesverlauf_plot(Nck_NO2_h.tbl)
range(Nck_NO2_h.tbl$datetime)#"2003-12-23 15:00:00 UTC" "2020-06-30 21:00:00 UTC"
tagesverlauf_plot(Can_NO2_h.tbl)
range(Can_NO2_h.tbl$datetime)#"2000-01-01 01:00:00 UTC" "2019-10-14 23:00:00 UTC"
tagesverlauf_plot(Alb_NO2_h.tbl)
range(Alb_NO2_h.tbl$datetime)
# kleineren Zeitraum
Nck_NO2_h_15_20 <-  NO2_Stundenmittel(BW_stations_NO2_tbl%>%
                                        filter(datetime > ymd_h("2015-01-01 00")&
                                                 datetime <= ymd_h("2020-06-29 24")),"Nck")
                                        
STD_mittel <-Nck_NO2_h_15_20 %>% 
  group_by(hour) %>% 
  summarise(Std_mittel =mean(NO2))%>% .[,2]%>% as_vector()
length(STD_mittel)#24
hour_vector <- rep(STD_mittel,times= (NROW(Nck_NO2_h_15_20))/24 )
Nck_NO2_h_15_20$Std_mittel<- hour_vector
Nck_NO2_h_15_20 %>% summary()
Nck_NO2_h_15_20 %>% ggplot(aes(x=datetime))+
  geom_smooth(aes(x=datetime,y= NO2-Std_mittel),col = "red",linetype = 2)+
  geom_smooth(method = "lm",
              mapping =aes(x = datetime,y= NO2-Std_mittel),
              col = "black",linetype =2)+
  geom_smooth(method = "lm",
              mapping =aes(x = datetime,y= NO2),
              col = "black",linetype =1)+
  geom_smooth(aes(x=datetime,y= NO2),col = "red")+
  
  ggtitle("NO2- Immissionen % Stundenmittel",
          subtitle=" Am Neckartor 2015 bis 2020")
#===================
#NO2 Tagesmittel
NO2_Tagesmittel <- function(df,nm){
  df_loc <- df %>% filter(name == nm) %>%
    mutate(name =nm,WT = wday(datetime),WDay = floor_date(datetime,unit="1 days")%>% format("%A ")%>% as_factor())
  return(df_loc)
}




# ======================
# linear model
## define model
NO2_model <-  function(df) { 
  lm(NO2 ~ datetime, data = df)
}
NO2_stations_model <- NO2_stations_grouped %>%
  mutate(model = map(Data, NO2_model))

Alb_NO2_h.tbl <-BW_stations_NO2_tbl %>% 
  filter(name =="Alb")%>% 
  mutate(hour= floor_date(datetime,unit="1 hours")%>% format("%H ")%>% as_factor())

Nck_NO2_h_15_20 %>% .$Std_mittel %>% range() # 47.70852 86.64773
Nck_NO2_h_15_20 %>% .$Std_mittel %>% sd() # 11.47
Nck_NO2_h_15_20 %>% 
  mutate(Diff= NO2-Std_mittel)%>%
  summarise(Stdev = var(NO2),
            Stdev_minus_Var_h = var(Diff),
            var(Std_mittel))
 Coeff <-Nck_NO2_h_15_20 %>% lm(NO2~ datetime,data = .) %>% .$coefficients
jahrs <- 365*24*60*60

 Coeff[1] <- Coeff[1]-
   24*60*60*Coeff[2]*diff.Date(c(ymd_hms("2014-12-31 23 59 59"),ymd("1970-01-01")))
 Coeff[2]<- Coeff[2]*jahrs
 NROW(Nck_NO2_h_15_20) # 48168 h
 x <- seq(1,by= 1,length.out = NROW(Nck_NO2_h_15_20))

y <- Coeff[1]+ x*Coeff[2]/(365*24)
Nck_NO2_h_15_20$regr <- y
Nck_NO2_h_15_20 <- Nck_NO2_h_15_20 %>% mutate(NO2_detrend = NO2-regr) %>% 
  dplyr::select(-regr)
detrend_plot <-Nck_NO2_h_15_20 %>% ggplot(aes(x = datetime))+
  geom_smooth(aes(y = NO2_detrend),col = "red",linetype =3)+
  ggtitle("NO2-Immissionen Am Neckartor",
        subtitle ="Abweichung vom 5-Jahre-Trend")+
  labs(x= "", y = "NO2[μg/m3] detrend")
detrend_plot+ geom_smooth (aes(x =datetime,y= NO2_detrend-Std_mittel),col = "red")  
# Wochenmittel
NO2_KW <- function(df){
  df_loc <- df %>%
    mutate(Week = format(datetime,"%W")%>% as_factor())
  return(df_loc)
}
NO2_Kalendwoche(BW_stations_NO2_tbl,"Alb")
KW_plot  <-function(df,nm)  {df %>% group_by(name) %>% nest() %>%
  mutate( data = map(data,NO2_KW)) %>%
 filter(name == nm) %>% .$data %>% as.data.frame()%>%
  mutate(NO2_Mittel= mean(NO2,na.rm=TRUE))%>%
  group_by(Week) %>% 
  summarise(NO2_week = mean(NO2,na.rm = TRUE)-NO2_Mittel)%>%
 ggplot(aes(Week%>% as.integer(),NO2_week))+
  geom_point()+
  geom_smooth(method = "gam",col = "red",se = FALSE)+
  labs(x = "Kalenderwoche", y="Δ(NO2)  [μg/m3]")+
  ggtitle("NO2 Abweichung Wochenmittel
          in 16 Jahren",
          subtitle = paste("Station",nm))
}
KW_plot(BW_stations_NO2_tbl,"Can")
KW_plot(BW_stations_NO2_tbl,"Nck")
