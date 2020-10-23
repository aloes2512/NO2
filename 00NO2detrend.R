install.packages("tidyverse")
library(tidyverse)
library(lubridate)
load("~/Documents/Gesundheit/Luftqualitaet/Analysen/NO2_detrend.RData")     # Daten vom 2020-04-25
load("~/Documents/Gesundheit/Luftqualitaet/Analysen/luft-qualitaet/.RData") # Daten vom 2020-04-28
summary(no2_detrend)
no2_detrend$Odw.no2
# Datei bereinigen : Datens??tze mit fehlenden NO2 Werten eliminieren
# =======================================================
# #Erzeugung einer "named list" vgl R4ds Seite 402
list_names <-names(no2_detrend) # [1] "Odw.no2"    "Sws.no2"    "Alb.no2"    "Brn.no2"    "Rt.no2"     "Can.no2"    "Lbg.no2"    "Lbg.Fr.no2" "Nck.no2"
my_data <- vector("list",length = length(list_names))
names(my_data) <- list_names
#Zeilen mit fehlenden NO2 eliminieren
for (nm in list_names){
  my_data[[nm]] <- no2_detrend[[nm]][!is.na(no2_detrend[[nm]]$NO2),]
  my_data[[nm]]$Name <- rep(str_replace(nm,".no2",""), NROW(my_data[[nm]]))
}
NROW(no2_detrend) #9
head(no2_detrend,1) # tibble  Odw.no2 mit: Station datetime  NO2 residuals
# Alle Daten in einer Liste my_stations abspeichern
my_stations <- tibble(names = list_names,
                      data = no2_detrend)
my_stations[[2]] # Zum sammeln aller NO2 Daten alle Daten pro Station
# Lineares Model auf die Datens??tze jeder Station anwenden
##----------------
stat.model <- function (df) {
  lm(NO2 ~ datetime, data = df)
}

my_stations <- my_stations %>%
  mutate( model = map(data,stat.model)) # my_stations$data ist Liste von df

names(my_stations) #"names" "data"  "model"
head(my_stations)
my_stations$data[[1]] %>% head()
# Ergebnisse in Liste my_stations
my_stations$model[["Odw.no2"]] # parameter der lm`s`
NROW(my_stations$model$Odw.no2$residuals) #91990 Time difference of 96422 hours
my_stations$model$Odw.no2$fitted.values %>% head()
## Beispiele der ausgewerteten Daten
summary(my_stations$model$Nck.no2$residuals) #   Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
                                            #  19.515  -32.582   -4.841    0.000   27.857  306.885
head(my_stations$model$Nck.no2$coefficients)  #  (Intercept)   datetime (= slope)
                                              # 2.401625e+02 -1.108000e-07
# =========================================================
# sichern der Modelparameter aus linearer Regression
save(my_stations, file = "Stations_data.RData")
#========================================================
load("Stations_data.RData")
#=========================================================
## Grafische Darstellungen
plot.fun <- function( df) {df %>%
    as_tibble() %>%
    ggplot(aes(x = datetime,y = NO2))+
    geom_point(size = 0.1,alpha = 0.1)+
    geom_smooth(data = df, method = "auto",col = "red",span = 0.01)+
    labs ( x = "", y = "NO2 [ug/m3]")+
    ggtitle(paste("Station",df[1,1],df[1,4]),
            subtitle = "Trend (Mehr- jahresmittelwerte) rote Linie")

}

plots <- vector ("list",length = length(my_stations$names))
dfrms <- vector ("list",length = length(my_stations$names))
names(plots) <- my_stations$names
names(dfrms) <- my_stations$names
for (nm in my_stations$names) {
  dfrms[[nm]] <- my_stations$data[[nm]]
  plots[[nm]] <- plot.fun(dfrms[[nm]])
}
plots %>% walk(print)
# Speichern im WD mit Namen "trend.png"
# Graph speichern
save.path <- paste0(getwd(),"/Abbldg/")
if(!dir.exists(save.path)){
  dir.create(save.path)
}
for (nm in list_names){
  ggsave(filename = paste0(save.path,nm,"trend.png"),plot = plots[[nm]])
}
# Darstellung der Regressionsgeraden

ggplot(my_stations$data$Odw.no2, aes (x = datetime, y = NO2)) +
  geom_point(size = 0.1, alpha = 0.1)+
  geom_abline( slope =my_stations$model$Odw.no2$coefficients[2],
              intercept = my_stations$model$Odw.no2$coefficients[1],
              col = "red")+
  ggtitle( paste("NO2  (12 Jahres - Trend) ", my_stations$data$Odw.no2[1,4]),
           subtitle = "Regressionsgerade rote Linie") +
  labs( x = "year", y = "NO2 ")

#_____________________
plot.fun2 <- function( df1,df2) {
    df1 %>%
    ggplot(aes(x = datetime,y = NO2))+
    geom_point(size = 0.1,alpha = 0.1)+
    geom_abline( slope =df2[[nm]]$coefficients[2],
                 intercept =df2[[nm]] $coefficients[1],
                 col = "red")
    labs ( x = "", y = "NO2 [ug/m3]")+
    ggtitle(paste("Station",df[1,1],df[1,4]),
            subtitle = "Trend (Mehr- jahresmittelwerte) rote Linie")
}
df <- vector ( "list" , length = length(my_stations$names))
plots2 <- vector ("list",length = length(my_stations$names))#"Odw.no2"    "Sws.no2"    "Alb.no2"    "Brn.no2"    "Rt.no2"    #
                                                            # "Can.no2"    "Lbg.no2"    "Lbg.Fr.no2" "Nck.no2"
dfrms2 <- vector ("list",length = length(my_stations$names))
names(plots2) <- my_stations$names
names(dfrms2) <- my_stations$names
names(df) <- my_stations$names
for (nm in my_stations$names) {
  df[[nm]] <- my_stations$data[[nm]]
  dfrms2[[nm]] <- my_stations$model[[nm]]$coefficients
  plots2[[nm]] <- plot.fun2(df1 = df[[nm]],df2 =dfrms2[[nm]])
}

sd(my_stations$model$Odw.no2$residuals) # 9.35
mean(my_stations$data$Odw.no2$NO2) #10.84
# definiere  Funktion

# Werte uberpr??fen
summary(my_stations$model)
my_stations$model$Odw.no2$coefficients
my_stations$model$Odw.no2$residuals %>% length() # [1] 91990
no2_data$Odw.no2$datetime %>% length() # [1] 105216
sum(no2_data$Odw.no2$NO2 %>% is.na()) # 13226
my_stations$data # List of data  9 stations

# gesamte Liste pr??fen

summary(my_stations$model$Nck.no2)
Nck_df <- tibble( datetime = my_stations$data$Nck.no2$datetime,
                  NO2 = my_stations$data$Nck.no2$NO2,
                  resid = my_stations$model$Nck.no2$residuals)
ggplot(Nck_df)+
  geom_point(data=Nck_df,aes (x= datetime, y = resid),  alpha = 0.1, size = 0.1)+
  geom_smooth(method = "gam", aes(x= datetime, y = resid),color = "red")+
  ggtitle(" NO2 1-h Werte minus 15 Jahre Trend Stuttgart
          Am Neckartor",
          subtitle = "Residuen `GAM`")+
  labs ( x = "", y = "resid(NO2) [ug/m3]")
#==========================================
# Vorschlag Florian
list_names <-names(my_stations$data)
my.plots <- vector("list", length = length(list_names)) # length = 9
for (nm in list_names){
  my.plots[[nm]] <- ggplot(my_stations$data[[nm]], aes (x= datetime, y = NO2 ))+
    geom_point(size = 0.1,alpha= 0.1)+
    ggtitle (paste0("Station:",nm,no2_data[[nm]][1,1]))+
    geom_smooth(method = "lm",col ="red",mapping =  aes( x = datetime, y = NO2))+
    labs ( x = "", y = "NO2")
}
# Ausgabe in R- Studio
my.plots %>% walk(print)
# Mapping on List BW_data

my_data[["Odw.no2"]] %>% str()
# Darstellung Regressionsgerade

int <- vector ("double",length= length(list_names))
names(int) <- list_names
slp <- vector ("double",length= length(list_names))
names(slp) <- list_names

for (nm in list_names){
  int[nm] <- my_stations$model[[nm]]$coefficients[1]
  slp[nm] <- my_stations$model[[nm]]$coefficients[2]
}
reg.par <- tibble(names = list_names,
                  intersect = int,
                  slope = slp)
reg.par <- reg.par %>% mutate(no2.2000 = as.numeric(ymd_hms("2000-01-01 00:00:00"))*slope+ intersect)
year_nr <- seq(0,19,1)
names(year_nr) <- c(paste0("200",0:9),paste0("20",10:19))
reg.par <- reg.par %>% mutate(yearslope = slope * 60*60*24*365)
vergleich.jahrwerte <- vector ( "list" , length = length(list_names))
names(vergleich.jahrwerte) <- list_names
jahreswerte.no2 <- vector ("double",length = length(names(year_nr)))

jahreswerte.no2 <- tibble( x= year_nr,
                          no2 = reg.par$no2.2000[[nm]] + year_nr*reg.par$yearslope[[nm]])
for( nm in list_names) {
  vergleich.jahrwerte[[nm]] <- tibble(x = year_nr,
                                      no2 = reg.par$no2.2000[[nm]] + year_nr*reg.par$yearslope[[nm]])
}
vergleich.jahrwerte
regr.plot <- vector ("list", length= length(list_names))
for (nm in list_names) {
regr.plot[[nm]] <- ggplot(vergleich.jahrwerte[[nm]],aes(x,no2))+geom_point()
}
regr.plot %>% walk(print)





