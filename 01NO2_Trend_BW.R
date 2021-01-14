# NO2_Trend_BW
library(lubridate)
library(tidyverse)
library(modelr)
library(xts)
library(readxl)
library(broom)
# NO2 Daten laden und formatieren
load("~/projects/02Analysen/RData/BW_stations_NO2_tbl.RData")
summary(BW_stations_NO2_tbl)
BW_stations_NO2_tbl$name <- BW_stations_NO2_tbl$name %>% as_factor()
BW_stations_NO2_tbl %>% head(2)
BW_stations_NO2_tbl$name %>% levels() # 15 Stationen
BW_stations_NO2_tbl %>% filter (name == "Brn") %>%
  lm ( formula = NO2 ~ datetime) %>% tidy()
# define function datasciencebook 5.12
# Zeitfaktor
ty <- 60*60*24*365
my_slp <- function(df) {
  mod <- lm(df$NO2~ df$datetime) %>% tidy()
  slp <- mod[2,2]*ty
}
my_intcpt <- function(df) {
  mod <- lm (df$NO2 ~ df$datetime) %>% tidy()
  intcpt <- mod[1,2]
}
df_Brn <- BW_stations_NO2_tbl %>% filter(name == "Brn")
df_Brn$datetime %>% first
slp<- my_slp(df_Brn)
intcpt <- my_intcpt(df_Brn)

BW_stations_slp_y <-BW_stations_NO2_tbl %>% group_by(name) %>%
  do(my_slp(.))
BW_stations_intcpt_1970 <- BW_stations_NO2_tbl %>% group_by(name)%>%
  do(my_intcpt(.))
colnames(BW_stations_intcpt_1970) <- c("station", "NO2_1970")
colnames(BW_stations_slp_y) <- c("station", "slope")
BW_stations_model <- right_join(BW_stations_slp_y,
                                BW_stations_intcpt_1970,
                                by = "station") %>%
                      mutate(NO2_2010 = NO2_1970+ slope*40,
                             NO2_2020 = NO2_1970+ slope*50)%>%
                      mutate(Aenderung_proz_10_20 = -slope*10/NO2_2010*100)%>%
                      mutate(Aenderung_10_20 = -slope*10)%>% arrange(slope)
levels(BW_stations_model$station)<- c("Schwb.Alb","Bernhausen","Bad Cannstatt",
                                            "Eggenstein","Freiburg","Friedr.hf",
                                            "Heidelberg","Heilbronn","Ludw.bg Weim.str",
                                            "Ludw.bg Friedr.str","Am Neckartor","Odenwald",
                                            "Reutlg.Friedr.str","Reutlingen Lederstr.",
                                            "Schwarzw.Süd")
write_csv2(BW_stations_model,"~/documents/Luftqualitaet/Analysen/Berichte/RegrModell.csv")
head(BW_stations_model)
