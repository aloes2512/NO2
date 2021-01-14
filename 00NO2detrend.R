install.packages("tidyverse")
library(tidyverse)
library(lubridate)
library(xts)
# Reutlingen Lederstrasse Ost ergänzen
library(readxl)
library(broom)
# ===============================
load("~/Documents/Gesundheit/Luftqualitaet/Analysen/NO2_detrend.RData")     # Daten teilweise aktualisiert5
no2_detrend$Odw.no2 %>% head()
# =======================================================
# #Erzeugung einer "named list" vgl R4ds Seite 402
list_names <-names(no2_detrend) # [1] "Odw.no2"    "Sws.no2"    
                                #"Alb.no2"    "Brn.no2"    "Rt.no2"     "Can.no2"    
#                               "Lbg.no2"    "Lbg.Fr.no2" "Nck.no2" "Rt_leder.no2"
NO2_data <- vector("list",length = length(list_names))
names(NO2_data) <- list_names
#Zeilen mit fehlenden NO2 eliminieren !!! Wichtig für Anwendung GAM ""
for (nm in list_names){
  NO2_data[[nm]] <- no2_detrend[[nm]][!is.na(no2_detrend[[nm]]$NO2),]
  NO2_data[[nm]]$Name <- rep(str_replace(nm,".no2",""), NROW(NO2_data[[nm]]))
}
NROW(NO2_data) #10
head(NO2_data,1) # tibble  Odw.no2 mit: Station datetime  NO2 residuals
# Alle Daten in einer Liste NO2_data abspeichern
NO2_data <- tibble(names = list_names,
                      data = NO2_data)
NO2_data$names <- as_factor(NO2_data$names) #Levels: Odw.no2 Sws.no2 Alb.no2 Brn.no2 
#                               Rt.no2 Can.no2 Lbg.no2 Lbg.Fr.no2 Nck.no2 Rt_leder.no2
summary(NO2_data)
NO2_data$data[[1]]
NO2_data[[2]] # Zum sammeln aller NO2 Daten alle Daten pro Station
# Lineares Model auf die Datens??tze jeder Station anwenden
##----------------
no2.model <- function (df) {
  lm(NO2 ~ datetime, data = df)
}

NO2_data <- NO2_data %>%    # NO2_data ist Liste mit Namen und dataframes df 
  mutate( model = map(data,no2.model)) # my_stations$data ist  df aus Liste
NO2_data$data %>% names()
names(NO2_data) #"names" "data"  "model"
NO2_data$data[[10]] %>% tail()
# Ergebnisse in Liste NO2_data
NO2_data$model[["Odw.no2"]] # parameter der lm`s`
NROW(NO2_data$model$Odw.no2$residuals) #91990 Time difference of 96422 hours
NO2_data$model$Odw.no2$fitted.values %>% head()
## Beispiele der ausgewerteten Daten
summary(NO2_data$model$Nck.no2$residuals) #   Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
NO2_resid <- tibble ( datetime = )                                           #  19.515  -32.582   -4.841    0.000   27.857  306.885
(NO2_data$model$Nck.no2$coefficients)  #  (Intercept)   datetime (= slope)
                                              # 2.401625e+02 -1.108000e-07
summary(NO2_data$model$Rt_leder.no2$residuals)
# =========================================================
# sichern der Modelparameter aus linearer Regression
save(NO2_data, file = "Stations_data_lm.RData")
#========================================================
load("Stations_data_lm.RData")
#=========================================================
## Grafische Darstellungen
# Mittelwerte GAM
plot.fun <- function( df) {df %>%
    as_tibble() %>%
    ggplot(aes(x = datetime,y = NO2))+
    geom_point(size = 0.1,alpha = 0.3)+
    geom_smooth(data = df, method = "auto",col = "red",span = 0.01)+
    labs ( x = "", y = "NO2[µg/m³]")+
    ggtitle(paste("Station",df[1,5],"Nummer",df[1,1]),
            subtitle = "Mehr-jahresmittelwerte GAM (rote Linie)")
}
NO2_plots <- NO2_data %>%
  mutate (plot0 = map(data, plot.fun))
plots <- NO2_plots$plot0
plots %>% walk(print)

# Graph speichern
save.path <- paste0(getwd(),"/Abbldg/")
if(!dir.exists(save.path)){
  dir.create(save.path)
}
# Speichern im WD mit Namen "GAM.png"
for (nm in list_names){
  ggsave(filename = paste0(save.path,nm,"GAM.png"),plot = plots[[nm]])
}


# Darstellung der Regressionsgeraden
#_____________________
plot.fun2 <- function( df) {df %>%
    as_tibble() %>%
    ggplot(aes(x = datetime,y = NO2))+
    #geom_point(size = 0.1,alpha = 0.3)+
    geom_smooth(data = df, method = "lm",col = "red")+
    labs ( x = "", y = "NO2[µg/m³]")+
    ggtitle(paste("Station",df[1,5],"Nummer",df[1,1]),
            subtitle = "Regressionsgerade (rote Linie)")
}

NO2_lm_plots <- NO2_data %>% mutate (plot2 = map(data, plot.fun2))
NO2_lm_plots %>% walk(print)

ggplot(NO2_data$data$Can.no2 , aes (x = datetime, y = NO2)) +
  geom_point(size = 0.1, alpha = 0.1)+
  geom_abline( slope =NO2_data$model$Can.no2$coefficients[2],
              intercept = NO2_data$model$Can.no2$coefficients[1],
              col = "red")+
  ggtitle( paste("NO2  (12 Jahres - Trend) ", NO2_data$data$Can.no2[1,5]),
           subtitle = "Regressionsgerade rote Linie") +
  labs( x = "year", y = "NO2 ")


df <- vector ( "list" , length = length(NO2_data$names))
plots2 <- vector ("list",length = length(NO2_data$names))#"Odw.no2"    "Sws.no2"    "Alb.no2"    "Brn.no2"    "Rt.no2"    #
                                                            # "Can.no2"    "Lbg.no2"    "Lbg.Fr.no2" "Nck.no2"
dfrms2 <- vector ("list",length = length(NO2_data$names))
names(plots2) <- NO2_data$names
names(dfrms2) <- NO2_data$names
names(df) <- NO2_data$names
for (nm in NO2_data$names) {
  df[[nm]] <- NO2_data$data[[nm]]
  dfrms2[[nm]] <- NO2_data$model[[nm]]$coefficients
  plots2[[nm]] <- plot.fun2(df1 = df[[nm]],df2 =dfrms2[[nm]])
}
plots2 %>% walk(print)
for (nm in list_names){
  ggsave(filename = paste0(save.path,nm,"trend.png"),plot = plots[[nm]])
}
sd(NO2_data$model$Odw.no2$residuals) # 9.35
mean(NO2_data$data$Odw.no2$NO2) #10.84
# definiere  Funktion
Regr_NO2 <- dfrms2 %>% unlist() %>% as.data.frame()
Regr_NO2$parm <- c("Intercept","Slope") %>% as_factor()
names(Regr_NO2) <- c("value","parm")
Regr_NO2$names <-Regr_NO2%>% rownames() %>% as_factor
Regr_NO2 %>% pivot_wider(names_from = parm, values_from = value)
Intercpt <- Regr_NO2$names %>% str_detect("Intercept")
Regr_NO2_Inter <-Regr_NO2 %>% filter(Intercpt) %>% dplyr::select(names,value) %>% as_tibble()
Regr_NO2_slope <- Regr_NO2 %>% filter(!Intercpt) %>% dplyr::select(names,value) %>% as_tibble()
names(Regr_NO2_slope) <- c("Station","Slope")
NO2_Regr_prm <-bind_cols(Regr_NO2_slope,Regr_NO2_Inter$value)
names(NO2_Regr_prm) <- c("Station","Slope","Intersect")
# Inbetriebnahme und Stilllegedatum der Stationen

# Werte uberpruefen
summary(NO2_data$model)
NO2_data$model$Odw.no2$coefficients
NO2_data$model$Odw.no2$residuals %>% length() # [1] 91990
no2_data$Odw.no2$datetime %>% length() # [1] 105216
sum(no2_data$Odw.no2$NO2 %>% is.na()) # 13226
NO2_data$data # List of data  9 stations

# gesamte Liste pr??fen

summary(NO2_data$model$Nck.no2)
Nck_df <- tibble( datetime = NO2_data$data$Nck.no2$datetime,
                  NO2 = NO2_data$data$Nck.no2$NO2,
                  resid = NO2_data$model$Nck.no2$residuals)
ggplot(Nck_df)+
  geom_point(data=Nck_df,aes (x= datetime, y = resid),  alpha = 0.1, size = 0.1)+
  geom_smooth(method = "gam", aes(x= datetime, y = resid),color = "red")+
  ggtitle(" NO2 1-h Werte minus 15 Jahre Trend Stuttgart
          Am Neckartor",
          subtitle = "Residuen `GAM`")+
  labs ( x = "", y = "resid(NO2) [ug/m3]")
#==========================================
# Vorschlag Florian
list_names <-names(NO2_data$data)
my.plots <- vector("list", length = length(list_names)) # length = 9
for (nm in list_names){
  my.plots[[nm]] <- ggplot(NO2_data$data[[nm]], aes (x= datetime, y = NO2 ))+
    geom_point(size = 0.1,alpha= 0.1)+
    ggtitle (paste0("Station:",nm,no2_data[[nm]][1,1]))+
    geom_smooth(method = "lm",col ="red",mapping =  aes( x = datetime, y = NO2))+
    labs ( x = "", y = "NO2")
}
# Ausgabe in R- Studio
my.plots %>% walk(print)
# Mapping on List BW_data

NO2_data[["Odw.no2"]] %>% str()
# Darstellung Regressionsgerade

int <- vector ("double",length= length(list_names))
names(int) <- list_names
slp <- vector ("double",length= length(list_names))
names(slp) <- list_names

for (nm in list_names){
  int[nm] <- NO2_data$model[[nm]]$coefficients[1]
  slp[nm] <- NO2_data$model[[nm]]$coefficients[2]
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





