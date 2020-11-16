#Aktualisierte Daten
Stgt_Haupt_NO_15_19 <- read_excel("~/Documents/Luftqualitaet/Daten/Stgt_dat/Stgt_Haupt_NO_16_19.xlsx",
                                  col_types = c("numeric", "text", "text",
                                                "text", "numeric", "text", "text",
                                                "text"))
Stgt_Haupt_NO_15_19 <- Stgt_Haupt_NO_15_19 %>%
  dplyr::select(station = Stationsnummer,Ort=Messstelle,datetime = "Datum / Uhrzeit", NO =Wert)%>%
  mutate(datetime = ymd_hm(datetime))

Stgt_Haupt_NO2_15_19 <- read_excel("~/Documents/Luftqualitaet/Daten/Stgt_dat/Stgt_Haupt_NO2_15_19.xlsx",
                                   col_types = c("numeric", "text", "text",
                                                 "text", "numeric", "text", "text",
                                                 "text"))
Stgt_Haupt_NO2_15_19 <- Stgt_Haupt_NO2_15_19 %>%
  dplyr::select(station = Stationsnummer,Ort=Messstelle,datetime = "Datum / Uhrzeit", NO2 =Wert)%>%
  mutate(datetime = ymd_hm(datetime)%>% format("%Y-%m-%d %H"))
Stg.Hpt_data <- list(Hpt.no2=Stgt_Haupt_NO_15_19,
                     Hpt.no =Stgt_Haupt_NO2_15_19)
BW.all_data[["Stg.Hpt_data"]] <-Stg.Hpt_data
BW.all_data[["Stgt.Hpt.no"]] <- NULL
BW.all_data[["Stgt.Hpt.no2"]] <- NULL

#Aktualisierung Am Neckartor
Stgt_Nck_NO_03_20 <- read_excel("~/Documents/Luftqualitaet/Daten/Stgt_dat/Stgt_Nck_NO_03_20.xlsx",
                                col_types = c("numeric", "text", "text",
                                              "text", "numeric", "text", "text"))
Stgt_Nck_NO_03_20 <-Stgt_Nck_NO_03_20 %>%
  dplyr::select(station = Stationsnummer,Ort=Messstelle,datetime = "Datum / Uhrzeit", NO =Wert)%>%
  mutate(datetime = ymd_hm(datetime)%>% format("%Y-%m-%d %H"),Komp = names(Stgt_Nck_NO_03_20)[[4]])
Stgt_Nck_NO2_03_20 <- read_excel("~/Documents/Luftqualitaet/Daten/Stgt_dat/Stgt_Nck_NO2_03_20.xlsx",
                                 col_types = c("numeric", "text", "text",
                                               "text", "numeric", "text", "text"))
Stgt_Nck_NO2_03_20 <-Stgt_Nck_NO2_03_20 %>%
  dplyr::select(station = Stationsnummer,Ort=Messstelle,datetime = "Datetime", NO2 ="Wert")%>%
  mutate(datetime = ymd_hm(datetime)%>% format("%Y-%m-%d %H"))
Stgt_Nck_NO2_03_20 <-Stgt_Nck_NO2_03_20 %>%   mutate(Komp = names(Stgt_Nck_NO2_03_20)[[4]])
Stgt.Nck_data <- list(Nck.no2=Stgt_Nck_NO2_03_20,
                      Nck.no=Stgt_Nck_NO_03_20)
BW.all_data[["Stg.Nck_data"]] <- Stgt.Nck_data
BW.all_data[["Stgt.Nck.no"]] <- NULL
BW.all_data[["Stgt.Nck.no2"]] <- NULL
BW.all_data[["Stgt.Hpt_data"]] <- NULL


BW.all_data$Stgt_data %>% summary()
Stg.Can_data <- list(Stg.Can.no2 =BW.all_data$Stgt_data$Can.no2,
                     Stg.Can.no =BW.all_data$Stgt_data$Can.no,
                     Stg.Can.temp =BW.all_data$Stgt_data$Can.temp,
                     Stg.Can.wr = BW.all_data$Stgt_data$Can.wr,
                     Stg.Can.wg = BW.all_data$Stgt_data$Can.wg)
Stg.Klt_data <- list (Stg.Klt.no2 = BW.all_data$Stgt_data$Klt.no2,
                      Stg.Klt.no =BW.all_data$Stgt_data$Klt.no,
                      Stg.Klt.o3 =BW.all_data$Stgt_data$Klt.o3,
                      Stg.Klt.co = BW.all_data$Stgt_data$Klt.co)
BW.all_data[["Stg.Can_data"]] <- Stg.Can_data
BW.all_data[["Stg.Klt_data"]] <- Stg.Klt_data