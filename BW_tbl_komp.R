load("BW_17_stationen.RData")
summary(BW_station_data)
station_names <-names(BW_station_data)
BW_station_data[[stnm]]$NO_
BW_stations_NO2_tbl <- tibble()
for (stnm in station_names) {
  if (NROW (BW_station_data[[stnm]]$NO2)> 1){
    df <- BW_station_data[[stnm]]$NO2
    BW_stations_NO2_tbl <- bind_rows(BW_stations_NO2_tbl,df)
  }
}
head(BW_stations_NO2_tbl)
NROW(BW_stations_NO2_tbl) #[1] 2385207
# NO hinzufügen
BW_stations_NO_tbl <- tibble()
for (stnm in station_names) {
  if (NROW (BW_station_data[[stnm]]$NO_)> 1){
    df <- BW_station_data[[stnm]]$NO_
    BW_stations_NO_tbl <- bind_rows(BW_stations_NO_tbl,df)
  }
}
NROW(BW_stations_NO_tbl)#2304904
BW_stations_komp <-inner_join(BW_stations_NO2_tbl,BW_stations_NO_tbl, by = c("station","datetime","name"))
# O3 hinzufügen
BW_stations_O3_tbl <- tibble()
for (stnm in station_names) {
if (NROW (BW_station_data[[stnm]]$O3)> 1){
df <- BW_station_data[[stnm]]$O3
BW_stations_O3_tbl <- bind_rows(BW_stations_O3_tbl,df)
}
}
NROW(BW_stations_O3_tbl)#1703734
BW_stations_komp <-inner_join(BW_stations_komp,BW_stations_O3_tbl, by = c("station","datetime","name"))
# Temp hinzufügen
BW_stations_Temp_tbl <- tibble()
for (stnm in station_names) {
if (NROW (BW_station_data[[stnm]]$Temp)> 1){
df <- BW_station_data[[stnm]]$Temp 
df$Temp <- as.numeric(df$Temp)
BW_stations_Temp_tbl <- bind_rows(BW_stations_Temp_tbl,df)
}
}
BW_stations_komp <-inner_join(BW_stations_komp,BW_stations_Temp_tbl, by = c("station","datetime","name"))
BW_stations_komp$name <- as_factor(BW_stations_komp$name)

summary(BW_stations_komp)
# WG hinzufügen
BW_stations_WG_tbl <- tibble()
for (stnm in station_names) {
if (NROW (BW_station_data[[stnm]]$WG)> 1){
df <- BW_station_data[[stnm]]$WG
df$WG <- as.numeric(df$WG)
BW_stations_WG_tbl <- bind_rows(BW_stations_WG_tbl,df)
}
}
BW_stations_WG_tbl$name <- as_factor(BW_stations_WG_tbl$name)
BW_stations_komp_WG <- inner_join(BW_stations_komp,BW_stations_WG_tbl)
levels(BW_stations_komp_WG$name)
save(BW_stations_komp, file = "BW_stations_komp.RData")
save(BW_stations_komp_WG, file = "BW_stations_komp_WG.RData")
