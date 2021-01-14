# Beispiel Heizg_daten
library(xts)
library(tidyverse)
library(lubridate)
library(broom)
library(gridExtra)
options(digits = 3)
load("~/Documents/Luftqualitaet/Analysen/NO2/Heizg_data.RData")
load("~/Documents/Luftqualitaet/Analysen/BW_station.RData")
BW_stations_NO2_tbl %>% summary()
BW_stations_NO2_tbl$name %>% levels() # 15 Stationen
Heizungsdaten_15_19 %>% head() # Daten Bad Cannstatt, Bernhausen, Reutlingen
Heizungsdaten_15_19 %>% summary()
NO2_data <- BW_stations_NO2_tbl %>% filter(name %in% c("Can","Brn","Rt_"))
stat_urban <- c("Can","Brn","Rt_")
stat_trafic <- c("Lbg_Friedr","Rtl","Nck" )
NO2_trafic <-BW_stations_NO2_tbl %>% filter(name %in% stat_trafic)

Grdz_Can <- Heizungsdaten_15_19 %>% 
  .[["Can"]] %>% mutate(name = "Can")
Grdz_Brn <- Heizungsdaten_15_19 %>% 
  .[["Brn"]] %>% mutate(name = "Brn")
Grdz_Rt <- Heizungsdaten_15_19 %>% 
  .[["Rt"]] %>% mutate(name = "Rt_")

Grdz_data <- bind_rows(Grdz_Brn,Grdz_Can,Grdz_Rt)
summary(Grdz_data)
NO2_Grdz_data <- inner_join(NO2_data,Grdz_data, by = c("name","datetime")) 
NO2_Grdz_data$NO2 <- NO2_Grdz_data$NO2 %>% na.locf()
NO2_Grdz_data$Gradzahl<- NO2_Grdz_data$Gradzahl %>% na.locf()
cor(NO2_Grdz_data$NO2,NO2_Grdz_data$Gradzahl)
NO2_Grdz_data$name <- as_factor(NO2_Grdz_data$name)
NO2_Grdz_data$station <- as_factor(NO2_Grdz_data$station)

summary(NO2_Grdz_data)
# Vergleich Verteilung mit Normalverteilung
NO2_Grdz_data %>%
  ggplot() + 
  stat_qq(aes(sample=NO2),distribution = stats::qnorm) + 
  facet_wrap(~name)
NO2_Grdz_data %>%
  ggplot() + 
  geom_histogram(aes(NO2), bins = 40) + 
  ggtitle("Haeufigkeitsverteilung der NO2 Messungen
          Poissonverteilung")+
  facet_wrap(~name)
NO2_Grdz_data %>%
  ggplot() + 
  geom_freqpoly(aes(NO2), bins = 40) + 
  ggtitle("Haeufigkeitsverteilung der NO2 Messungen
          Poissonverteilung")+
  facet_wrap(~name)
range(NO2_Grdz_data$NO2) # 0 bis 176
NO2_Grdz_data %>% filter (Gradzahl > 5) %>%
  ggplot() + 
  geom_freqpoly(aes(Gradzahl), bins = 40) + 
  ggtitle("Haeufigkeitsverteilung des Heizbedarfs
          dargestellt als Gradzahlen")+
  facet_wrap(~name)
range(NO2_Grdz_data$datetime)#"2015-01-01 01:00:00 UTC" "2019-10-14 23:00:00 UTC"
NO2_Grdz_data %>% filter (Gradzahl > 5) %>%
  ggplot() + 
  geom_point(aes(Gradzahl,NO2),size = 0.1, alpha = 0.5) + 
  geom_smooth(method = "lm", aes(Gradzahl,NO2),col = "red")+
  ggtitle("NO2 als Funktion des Heizbedarfs 
          im städtischen Hintergrund", 
          subtitle = "Regressionsgerade (rot)" )+
  labs(x= "Heizbedarf (Gradzahl)", y = " NO2 [μg/m3]")+
  facet_wrap(~name)

NO2_Grdz_data %>% ggplot(aes(datetime,Gradzahl))+
  geom_smooth(method = "lm",aes(datetime,Gradzahl))+
  geom_point(aes(datetime,Gradzahl),size = 0.01,alpha = 0.01)+
  ggtitle ("Heizwaermebedarf als Gradzahl
  einschl. Energie f. WW-bedarf")+
  facet_wrap(~ name)
#======================================
# Verkehrsnahe Stationen
NO2_trafic <- NO2_trafic %>% filter (datetime > ymd("2015-01-01"))
NO2_trafic %>% ggplot()+
  geom_point(aes(datetime,NO2),size = 0.01,alpha= 0.1) + 
 geom_smooth(method = "lm",aes(datetime,NO2),col = "red")+
  labs( x = "",y = "NO2 [μg/m3]")+
  ggtitle(" NO2 1-h Messwerte verkehrsnahe Stationen
      Ludwigsburg Friedr.Str,Stgt Am Neckartor,
      Reutlingen Lederstrasse",
          subtitle = "Trend als Regressionsgerade (rot)")+
  facet_wrap(~name)
NO2_trafic$station <- NO2_trafic$station %>% as_factor()

#====================

#Umrechnung Jahreswerte
td <- difftime(ymd_h("1970-01-01 00"),ymd_h("2015-01-01 00"),
               units="secs")
jahrs <- 365*24*60*60
# use broom to visualise model

NO2_Grdz_data %>% group_by(name) %>% as.tbl() %>%
  do(tidy(lm(NO2 ~ datetime,data =.),conf.int= TRUE)) %>%
  filter(term == "datetime") %>%
  select(name,estimate,conf.low,conf.high)%>%
  mutate(estimate = estimate*jahrs,
         conf.low = conf.low*jahrs,
         conf.high= conf.high*jahrs) %>%
  ggplot(aes(name,y = estimate,ymin =conf.low,ymax= conf.high))+
  geom_errorbar()+
  geom_point()+
  ggtitle("Jahresrate Veränderung NO2 - Immissionen",
   subtitle = "Hintergrund: Bernhausen,Cannstatt,Reutlingen")+
  labs(y = "NO2 [μg/m3]pro Jahr", x = "Messstation")
NO2_Grdz_data %>% group_by(name) %>% as.tbl() %>%
  do(tidy(lm(Gradzahl ~ datetime,data =.),conf.int= TRUE)) %>%
  filter(term == "datetime") %>%
  select(name,estimate,conf.low,conf.high)%>%
  mutate(estimate = estimate*jahrs,
         conf.low = conf.low*jahrs,
         conf.high= conf.high*jahrs) %>%
  ggplot(aes(name,y = estimate,ymin =conf.low,ymax= conf.high))+
  geom_errorbar()+
  geom_point()+
  ggtitle("Jahresrate 15/19 Veränderung Heizbedarf",
          subtitle = "Hintergrund: Bernhausen,Cannstatt,Reutlingen")+
  labs(y = " Gradzahl[°C ] pro Jahr", x = "Messstation") 
# NO2 Trafic regression
NO2_trafic$NO2 <- NO2_trafic$NO2 %>% na.locf()
NO2_trafic %>% group_by(name) %>% as.tbl() %>%
  do(tidy(lm(NO2 ~ datetime,data =.),conf.int= TRUE)) %>%
filter(term == "datetime") %>%
  select(name,estimate,conf.low,conf.high)%>%
  mutate(estimate = estimate*jahrs,
         conf.low = conf.low*jahrs,
         conf.high= conf.high*jahrs) %>%
  ggplot(aes(name,y = estimate,ymin =conf.low,ymax= conf.high))+
  geom_errorbar()+
  geom_point()
df <-bind_rows(NO2_trafic %>% group_by(name) %>% as.tbl() %>%
  do(tidy(lm(NO2 ~ datetime,data =.),conf.int= TRUE)) %>%
  filter(term == "datetime") %>%
  select(name,estimate,conf.low,conf.high),NO2_Grdz_data %>% group_by(name) %>% as.tbl() %>%
    do(tidy(lm(NO2 ~ datetime,data =.),conf.int= TRUE)) %>%
    filter(term == "datetime") %>%
    select(name,estimate,conf.low,conf.high))%>%
  mutate(estimate = -estimate*jahrs,
         conf.low = -conf.low*jahrs,
         conf.high= -conf.high*jahrs) %>% as_tibble()
 df <-arrange(df, desc(estimate))
df[,1] <- c("1 Nck","2 Lbg_friedr","3 Can", "4 Brn")
df %>%   ggplot(aes(name,y = estimate,ymin =conf.low,ymax= conf.high))+
  geom_errorbar()+
  geom_point()+
  ggtitle("Jahresrate 15/19 Red.NO2 Immissionen",
          subtitle = "Städt. Hintergrund/ verkehrsnah")+
  labs(y = "NO2 [μg/m3]pro Jahr", x = "Messstation") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+

df %>% ggplot()
ggplot()
grid.table(df)
levels(BW_stations_NO2_tbl$name) <-BW_stations_NO2_tbl$name %>% levels()%>% 
  recode("Lbg_4" ="Lbg_Weimar","Rt_" = "Rt_pomol","Rtl" ="Rt_leder",
         "Friedri"= "Friedrichshafen","Heid" ="Heidelbg","Heil"="Heilbron")

BW_stations_NO2_tbl$datetime %>% range() 
df_15 <-BW_stations_NO2_tbl %>% group_by(name) %>% 
  do(tidy(lm(NO2 ~ datetime,data =.),conf.int= TRUE)) %>%
  filter(term == "datetime") %>%
  select(name,estimate,conf.low,conf.high)%>%
  mutate(estimate = estimate*jahrs,
         conf.low = conf.low*jahrs,
         conf.high= conf.high*jahrs)%>% 
  as_tibble() %>%
arrange(., estimate) 
# for visualisation compare datasciecebook page 185
df_15 %>% ggplot(aes(x = reorder(name,desc(estimate)),
                     y = estimate,ymin =conf.high,ymax= conf.low),label = name)+
  geom_errorbar()+
  geom_point(show.legend = TRUE)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  #coord_flip()+
  ggtitle("Jahresrate  NO2 -Immissionen
   Reduzierung 2000 bis 2020")+
  labs(y = "NO2 [μg/m3]pro Jahr", x = "Messstation") 
BW_stations_NO2_tbl <- BW_stations_NO2_tbl %>% 
  mutate(NO2 = na.locf(NO2),station =as_factor(station)) %>%
  select(name = "name",NO2 = "NO2",datetime,station)

# Verkehrsnahe Stationen
stat_trafic <- c("Nck","Rtl","Lbg_Friedr")
stat_rural <- c("Alb","Odw","Sws")
BW_stations_NO2_tbl %>% 
  filter(name %in% stat_trafic) %>%
  ggplot(aes(x=datetime,y = NO2))+
  geom_smooth(col = "red")+
  geom_smooth(method= "lm",linetype = 2, col = "darkred")+
  #geom_point(size = 0.00001, alpha = 0.5)+
  facet_wrap(~name)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs( x = "", y= "NO2[μg/m3]")+
  ggtitle( "  NO2- Immissionen 
  Trends und Mittelwerte",
          subtitle = "  verkehrsnahe Messungen
  Lbg.Friedrichstr.,Am Neckartor,Rtl.Lederstr. ")
BW_stations_NO2_tbl %>% 
  filter(name %in% c("Can","Brn","Rt_")) %>%
  ggplot(aes(x=datetime,y = NO2))+
  geom_smooth(col = "red")+
  geom_smooth(method= "lm",linetype = 2, col = "darkred")+
  #geom_point(size = 0.00001, alpha = 0.5)+
  facet_wrap(~name)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs( x = "", y= "NO2[μg/m3]")+
  ggtitle( "  NO2- Immissionen 
  Trends und Mittelwerte",
           subtitle = "  städt. Hintergrund-Messungen
  Stgt Bad Cannstatt,Bernhausen,Rtl.Pomologie ")
levels(BW_stations_NO2_tbl$name)
BW_stations_NO2_tbl %>% 
  filter(name %in% stat_urban) %>%
  ggplot(aes(x=datetime,y = NO2))+
  geom_smooth(col = "red")+
  geom_smooth(method= "lm",linetype = 2, col = "darkred")+
  #geom_point(size = 0.00001, alpha = 0.5)+
  facet_wrap(~name)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs( x = "", y= "NO2[μg/m3]")+
  ggtitle( "  NO2- Immissionen 
  Trends und Mittelwerte",
           subtitle = "  städt. Hintergrund-Messungen
  Stgt Bad Cannstatt,Bernhausen,Rtl.Pomologie ")
levels(BW_stations_NO2_tbl$name)
BW_stations_NO2_tbl %>% 
  filter(name %in% c("Alb","Sws")) %>%
  ggplot(aes(x=datetime,y = NO2))+
  geom_smooth(col = "red")+
  geom_smooth(method= "lm",linetype = 2, col = "darkred")+
  #geom_point(size = 0.00001, alpha = 0.5)+
  facet_wrap(~name)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs( x = "", y= "NO2[μg/m3]")+
  ggtitle( "  NO2- Immissionen 
  Trends und Mittelwerte",
           subtitle = "  ländl. Hintergrund-Messungen
  Schw.-Alb,Schwarzwald Süd")
