#update udo data
library(readxl)
library(tidyverse)
library(lubridate)
Rt_4470_Temp_00_20 <- read_excel("~/Documents/Luftqualitaet/Daten/Reutlingen/Reutlingen_raw/Rt_4470_Temp_00_20.xlsx",
col_types = c("numeric", "text", "text",
"text", "text", "text", "text",
"text"), skip = 10)

Rt_4470_Temp_00_20 <- Rt_4470_Temp_00_20 %>% na.omit()
head(Rt_4470_Temp_00_20)
Rt_4470_Temp_00_20$Wert <- Rt_4470_Temp_00_20$Wert %>% str_replace(",",".")%>% as.numeric()
Rt_4470_Temp_00_20$datetime <- Rt_4470_Temp_00_20$"Datum / Uhrzeit" %>% 
  ymd_hm() %>%
  floor_date("hours")
tb<-Rt_4470_Temp_00_20 %>% as_tibble()%>% 
  dplyr::select(Station =Stationsnummer,
                name=Messstelle,
                datetime= `Datum / Uhrzeit`,
                Temp = Wert)
h_mean <-function (tb) {
  tb$b <- tb[[4]]
  tb$hour <- tb[[3]] %>% ymd_hm() %>% floor_date("hours")
  res <- tb %>% group_by(hour) %>% summarise (Wert = mean (b))
  return(res)
}
Rt_Temp <- h_mean(tb)
# load existing data
load("/Users/alfloeffler/Documents/Luftqualitaet/Analysen/NO2/BW_17_stationen.RData")

summary(BW_station_data)
BW_station_data[["Rt_"]]$Temp
Rt_Temp
RT_Temp <-inner_join(BW_station_data[["Rt_"]]$Temp,Rt_Temp, by = c("datetime"="hour"))%>% dplyr::select(station,name,datetime,Temp= Wert)
BW_station_data[["Rt_"]]$Temp <-RT_Temp
save(BW_station_data, file = "/Users/alfloeffler/Documents/Luftqualitaet/Analysen/NO2/BW_17_stationen.RData")
