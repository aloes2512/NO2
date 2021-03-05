library(lubridate)
library(tidyverse)
library(readxl)
basepath<-"~/documents/Luftqualitaet/Daten/BW"
list.files(basepath)
load(file.path(basepath,"BW_list_tbl.RData"))
BW_list_tbl%>% names
Stgt_Temp <- BW_list_tbl%>% .$Stg_Schwz %>% dplyr::select(-NO2)
BW_list_tbl$Stg_Nck%>% summary()
BW_list_tbl$Stg_Klt%>% summary
BW_list_tbl$Stg_Hpt%>% summary
BW_list_tbl$Stg_Hoh%>% summary
BW_list_tbl$Stg_Stadtg%>% summary
Can_4452_Temp_00_20 <- read_excel("~/Documents/Luftqualitaet/Daten/BW/Can_4452_Temp_00_20.xlsx",
col_types = c("skip", "skip", "skip",
"text", "text", "skip", "skip"),
skip = 10)

Can_4452_Temp_00_20%>% head(2)
Can_Temp<- Can_4452_Temp_00_20%>% mutate(station= as_factor("4452"),
                              name= as_factor("Can"),
                              datetime= ymd_hm(`Datum / Uhrzeit`),
                              Temp= as.numeric(str_replace(Wert,",",".")))
Can_Temp <-Can_Temp %>% dplyr::select(station,name,datetime,Temp)
Can_4452_NO2_00_20 <- read_excel("~/Documents/Luftqualitaet/Daten/BW/Can_4452_NO2_00_20.xlsx",
                                  col_types = c("skip", "skip", "skip",
                                                "text", "text", "skip", "skip"),
                                  skip = 10)
Can_NO2<-Can_4452_NO2_00_20%>% mutate(station= as_factor("4452"),
                         name= as_factor("Can"),
                         datetime= ymd_hm(`Datum / Uhrzeit`),
                         NO2= as.numeric(str_replace(Wert,",",".")))

Can_NO2<-Can_NO2%>% dplyr::select(station,name,datetime,NO2)
BW_list_tbl$Stg_Can<-Can_NO2%>% right_join(Can_Temp)
BW_list_tbl$Lbg_weimar<-BW_list_tbl$Lbg_weimar%>%mutate(station = as_factor(station),
                                name= as_factor("Lbg_weimar"))%>%
  right_join(dplyr::select(Can_Temp,datetime,Temp))
BW_list_tbl$Lbg_Friedr<-BW_list_tbl$Lbg_Friedr%>%mutate(station = as_factor(station),
                                name= as_factor("Lbg_Friedr"))%>%
  left_join(dplyr::select(Can_Temp,datetime,Temp))
RT_Temp<-read_excel("~/Documents/Luftqualitaet/Daten/BW/RT_4470_Temp_03_21.xlsx",
           col_types = c("skip", "skip", "skip",
                         "text", "text", "skip", "skip"),
           skip = 10)
RT_Temp$Wert%>% is.na()%>% sum()
RT_Temp<-RT_Temp%>% na.omit()%>% 
  mutate(Temp=str_replace(Wert,",","."))%>% 
  mutate(Temp=as.numeric(Temp),datetime= ymd_hm(`Datum / Uhrzeit`))%>%
  dplyr::select(-c(Wert,`Datum / Uhrzeit`))
BW_list_tbl$Rt_pomol<-BW_list_tbl$Rt_pomol%>% right_join(RT_Temp)
BW_list_tbl$Rt_leder<-BW_list_tbl$Rt_leder%>% right_join(RT_Temp)
BW_list_tbl$Sws
Sws_Temp <-
 read_excel("~/Documents/Luftqualitaet/Daten/BW/table20042021143407992.xlsx",
            col_types = c("text", "text", "skip",
                          "text", "text", "skip", "skip", "skip"),
            skip = 10)
Sws_Temp<-Sws_Temp%>% mutate(station=as_factor("4467"),
                   name=as_factor("Sws"),
                   datetime = ymd_hm(`Datum / Uhrzeit`),
                   Temp = as.numeric(str_replace(Wert,",",".")))%>%
  dplyr::select(station,name,datetime,Temp)%>% na.omit()
BW_list_tbl$Sws<-BW_list_tbl$Sws %>% right_join(Sws_Temp)

Odw_Temp<- read_excel("~/Documents/Luftqualitaet/Daten/BW/Odw_76118_Temp_00_11.xlsx",
                      col_types = c("text", "text", "skip",
                                    "text", "text", "skip", "skip"),
                      skip = 10)
Odw_Temp%>%head(2)
Odw_Temp<-Odw_Temp%>% mutate( name =as_factor("Odw"),
                   datetime= ymd_hm(`Datum / Uhrzeit`),
                   Temp= as.numeric(str_replace(Wert,",",".")))%>%
                     dplyr::select(name,datetime,Temp)
BW_list_tbl$Odw<-BW_list_tbl$Odw %>% right_join(Odw_Temp)%>% na.omit
BW_list_tbl$Stg_Schwz
BW_list_tbl$Stg_Klt<-BW_list_tbl$Stg_Klt%>% mutate(station= as_factor(station),
                              name=as_factor("Stg_Klt"))
BW_list_tbl$Stg_Hpt<-BW_list_tbl$Stg_Hpt%>% mutate(station= as_factor(station),
                                                   name=as_factor("Stg_Hpt"))  
BW_list_tbl$Stg_Hoh <-BW_list_tbl$Stg_Hoh%>%mutate(station= as_factor(station),
                              name=as_factor("Stg_Hoh"))  
BW_list_tbl$Stg_Stadtg<-BW_list_tbl$Stg_Stadtg%>%mutate(station= as_factor(station),
                             name=as_factor("Stg_Stadtg"))  
BW_list_tbl$Stg_Schwz <-BW_list_tbl$Stg_Schwz%>%
  mutate(station= as_factor("Schwz"),
         name=as_factor("Stg_Schwz"))
summary(BW_list_tbl)  
save(BW_list_tbl,file = file.path(basepath,"BW_list_tbl.RData"))
load(file.path(basepath,"BW_list_tbl.RData"))
summary(BW_list_tbl)
names(BW_list_tbl)# 20 Stations
BW_NO2_Temp_tbl<- tibble()
for(nm in names(BW_list_tbl)){
  BW_NO2_Temp_tbl<-bind_rows(BW_NO2_Temp_tbl,BW_list_tbl[[nm]])
}
BW_NO2_Temp_tbl%>% tail()
save(BW_NO2_Temp_tbl,file = file.path(basepath,"BW_NO2_Temp_tbl.RData"))
