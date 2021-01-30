library(tidyverse)
library(lubridate)
library(readxl)
dir()
load("~/Documents/Luftqualitaet/Daten/BW/Stations_data.RData")
# Overview of data
summary(my_stations)
head(my_stations,2) # names data model
head(my_stations$names,3) #[1] "Odw.no2" "Sws.no2" "Alb.no2"

head(my_stations$data$Odw.no2,3) #Station datetime   NO2 residuals
head(my_stations$model$Odw.no2,1) # $coefficients
                                  # (Intercept)     datetime
                                  #1.639619e+01 -4.958935e-09 
NROW(my_stations$model$Odw.no2$residuals) #91990 Rows Time difference  96422 hours
                                          # 4432 missing datasets
my_stations$model$Odw.no2$fitted.values %>% head()  #    1        2        3        4        5        6 
                                                    #11.70162 11.70160 11.70159 11.70157 11.70155 11.70153 
list_names <- my_stations$names

#=========================================================
## Grafische Darstellungen
plot.fun <- function( df) {
  plt<-df %>%
    as_tibble() %>%
    ggplot(aes(x = datetime,y = NO2))+
    geom_point(size = 0.1,alpha = 0.1)+
    geom_smooth(data = df, method = "auto",col = "red",span = 0.01)+
    labs ( x = "", y = "NO2 [ug/m3]")+
    ggtitle(paste("Station",df[1,1],df[1,4]),
            subtitle = "Trend (Mehr- Jahresmittelwerte) rote Linie")
  return(plt)
}
all_plts <-my_stations$data %>% map(plot.fun)
print(all_plts)
all_plts[[1]]
names(all_plts) <- list_names
all_plts %>% walk(print)
# Speichern im WD mit Namen "trend.png"
# Graph im WD/ Abbldg/ speichern
save.path <- paste0(getwd(),"/Abbldg/")
if(!dir.exists(save.path)){
  dir.create(save.path)
}
for (nm in list_names){
  ggsave(filename = paste0(save.path,nm,"trend.png"),plot = all_plts[[nm]])
}
#===============================================================
# Darstellung der Regressionsgeraden
nm
ggplot(my_stations$data[[nm]], aes (x = datetime, y = NO2)) +
  geom_point(size = 0.1, alpha = 0.1)+
  geom_abline( slope =my_stations$model[[nm]]$coefficients[2],
               intercept = my_stations$model[[nm]]$coefficients[1],
               col = "red")+
  ggtitle( paste("NO2  (12 Jahres - Trend) ", my_stations$data[[nm]][1,1]),
           subtitle = "Regressionsgerade rote Linie") +
  labs( x = "year", y = "NO2 ")
plot.fun2 <- function( data,par) {
  slp = par[[2]]
  intcp <- par[[1]]
  data %>%
    ggplot(aes(x = datetime,y = NO2))+
    geom_point(size  = 0.1, alpha = 0.1 ,shape = 3)+
    geom_abline( slope =slp,
                 intercept = intcp,
                 col = "red")+
    labs ( x = "", y = "NO2 [ug/m3]")+
    ggtitle(paste("Station",nm),
            subtitle = "Regressionsgerade rote Linie")
  
}
reg.plots <-  vector ("list",length = length(list_names))
parms <- vector ( "list", length = length(list_names))
names(reg.plots) <- list_names
names(parms) <- list_names
for(nm in list_names) {
  dfr[[nm]] <-  my_stations$data[[nm]]
  parms[[nm]] <- my_stations$model[[nm]]$coefficients
  reg.plots[[nm]] <- plot.fun2(dfr[[nm]],parms[[nm]])
}
reg.plots %>% walk(print)
# =====================================================================================
#====Vergleich der Jahreswerte
my_stations$data$Odw.no2 %>% head()
nm <- "Odw.no2"
my_stations.year <- vector("list", length= length(list_names))
names(my_stations.year) <- list_names
# Kalenderjahre als Faktor
for  (nm in list_names) {
my_stations$data[[nm]]$fullyear <- my_stations$data[[nm]]$datetime %>% 
  floor_date("year") %>% 
  str_extract("^.{4}") %>% as.numeric()
}
# Jahreswerte als Mittelwerte
for (nm in list_names) {
my_stations.year[[nm]] <- my_stations$data[[nm]] %>% 
       group_by(fullyear) %>% 
       summarise(NO2.year.mean = mean(NO2), residual.year.mean = mean(residuals),n())
}
my_stations.year[[nm]] %>% head()

# Darstellung der Jahrewerte
reg_year.plot <- vector ( "list", length = length (list_names))
names(reg_year.plot) <- list_names
df <- vector("list", length = length(list_names))
names(df) <- list_names
for (nm in list_names) { 
  #df[[nm]] <-select(my_stations.year[[nm]],fullyear , NO2 = "NO2.year.mean")
 
  reg_year.plot[[nm]]<- ggplot(my_stations.year[[nm]], aes (x = fullyear , y = NO2.year.mean) ) +
  geom_point()+
  geom_smooth(col = "red", se = FALSE)+
  geom_smooth(method = "lm", col = "blue")+
  labs ( x = "Kalenderjahr", y = "NO2 Jahresmittel")+
  ggtitle(paste( nm, "Jahresmittel" ),
          subtitle = "Trend und Regressionsgerade")
}
reg_year.plot %>% walk(print)
for (nm in list_names){
  ggsave(filename = paste0(save.path,nm,"Jahrestrend.png"),plot =  reg_year.plot[[nm]])
}
# Daten aus Internet sind unter Daten/BRD/NO2_Emissionen_BRD.xls abgelegt
emissions <-  read_excel("~/Documents/Luftqualitaet/Daten/BRD/NO2_Emissionen_BRD.xls")
names(emissions) 
# Darstllung der Jahrestonnen
ggplot(emissions, aes(Jahr,D_Gesamt))+
  geom_smooth(aes(Jahr,D_Gesamt))+
  geom_smooth(aes(Jahr,BWGesamt))+
  geom_smooth(aes(x = Jahr, y = D_Verkehr),col = "red")+
  geom_smooth(method = "lm",aes(x = Jahr, y = D_Verkehr),col = "red",linetype = 5 )+
  geom_smooth(method = "lm",aes(x = Jahr, y = BWVerkehr),col = "red" ,linetype = 5 )+
  labs( y = "NO2 [Tausend t]")+
  ggtitle (" Entwicklung der NO2 Emissionen BRD und BW",
           subtitle = " Gesamt (blau) Verkehr (rot)
           gestrichelt = Regressionsgerade")
ggplot(emissions, aes(Jahr,BWGesamt))+
  geom_smooth(aes(Jahr,BWGesamt))+
  geom_smooth(aes(x = Jahr, y = BWVerkehr),col = "red" ,linetype = 5 )+
  geom_smooth(aes(x = Jahr, y = BW_Diesel_Pkw),col = "black" ,linetype = 5)+
  labs( y = "NO2 [Tausend t]")+
  ggtitle (" Entwicklung der NO2 Emissionen BW",
           subtitle = " Gesamt (blau)        
            Verkehr (rot)
            Pkw Diesel gestrichelt (schwarz)")

# Auswahl der Variablen
selemissions <- emissions %>% dplyr::select(D_Gesamt,D_Verkehr,BWGesamt,BWVerkehr)
# Prozent Darstellung
proz <- function(vec) {vec/vec[1]*100}
selemissions.pr <- selemissions %>% map_dfr(proz)
selemissions.pr$Jahr <- emissions$Jahr
head(selemissions,1)
ggplot(selemissions.pr, aes(x = Jahr, y = D_Gesamt))+
  geom_smooth()+
  geom_smooth(aes(x=Jahr, y = D_Verkehr),col = "red")+
  geom_smooth(aes(y= BWGesamt), col = "green")+
  geom_smooth(aes(y = BWVerkehr), col ="purple")+
  labs( y = " Prozent von 1990")+
  ggtitle(" NO2 Gesamt- und Verkehrsemissionen
          Relative Entwicklung 1990 bis 2017",
          subtitle = "         BRD: Gesamt (blau) Verkehr (rot)
          BW: Gesamt (gruen) Verkehr (violet)")
ggplot(selemissions.pr, aes(x = Jahr, y = D_Gesamt))+
  geom_point(col = "blue",shape = 4)+
  geom_smooth(method = "lm",mapping = aes(x = Jahr, y = D_Gesamt),col = "blue")+
  geom_smooth(method ="lm",mapping = aes(x=Jahr, y = D_Verkehr),col = "red")+
  geom_smooth(method ="lm",mapping =aes(x = Jahr,y= BWGesamt), col = "green")+
  geom_smooth(method ="lm",mapping =aes(x = Jahr, y = BWVerkehr), col ="purple")+
  labs( y = " Prozent von 1990")+
  ggtitle(" NO2 Gesamt- und Verkehrsemissionen
          Relative Entwicklung 1990 bis 2017",
          subtitle = " Regressionsgeraden 
          BRD: Gesamt (blau) Verkehr (rot)
          BW: Gesamt (gruen) Verkehr (violet)")
# Vergleich Trend Verkehrs Emissionen BW Immissionen
emissions2000 <- emissions %>% filter(Jahr >= 2000)
stplot <- vector ("list", length = length (list_names))
names(stplot)<- list_names

for (nm in list_names) { 
  stplot[[nm]] <- reg_year.plot[[nm]] +
    geom_smooth(data = emissions2000, method = "lm",aes(x= Jahr,y = BWVerkehr),col = "red", linetype = 4)+
    ggtitle (paste("NO2 Trends: Verkehrs - Emissionen BW /
                   Immissionen",nm))+
    labs (x= "Kalenderjahr", y= "NO2 [ug/m3] | [kt]")
  }
for (nm in list_names){
  stplot[[nm]] <- stplot[[nm]]+coord_cartesian(xlim= range(my_stations.year[[nm]]$fullyear))
}

stplot %>% walk(print)
# Gesamtemissionen - Verkehrsemissionen 
head(emissions)
relative_emissions <- emissions %>% 
  filter (emissions$Jahr>=2000) %>% 
  mutate(D_differ = D_Gesamt-D_Verkehr,BW_differ = BWGesamt-BWVerkehr)%>%
  map_dfr(proz) 
relative_emissions$Jahr <- 2000:2017
head(relative_emissions)
ggplot(relative_emissions, aes( x = Jahr, y = D_Gesamt))+
  geom_point(aes( x = Jahr, y = D_differ))+
  geom_smooth(aes( x = Jahr, y = D_differ),col = "red")+
  geom_smooth(method =  "lm", aes( x = Jahr, y = D_differ),col = "red", linetype = 4, se = F)+
  ggtitle(" NO2 BRD (Trend in %) : Gesamt-minus Verkehrsemissionen")+
  labs( y = "NO2% von 2000 ")
