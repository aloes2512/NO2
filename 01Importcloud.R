# Import von cloud
library(tidyverse)
library(lubridate)
library(xts)
library(readr)
library(readxl)

cloud_path <- "/Users/alfloeffler/Google Drive/BW_dat"
cloud_files <- list.files(cloud_path)
cloud_files # Google drive

read.data <- function(my.path,names= TRUE) {
  read_csv2(my.path,
             col_names = names,
             locale = locale(date_format = "%Y.%m.%d",
                             time_format = "%H:%M:%S",
                             decimal_mark = ",",
                             grouping_mark = "",
                             tz = "CET"),
             trim_ws = TRUE)
}

# correct missing datetime
repl.na.datetime = function (df) {
  start <- df$datetime[[1]] %>% ymd_hms()
  time2 <- df$datetime[[2]] %>% ymd_hms()
  intv <- time2 - start
  df$datetime <- seq(from =start, by = intv, length.out = NROW(df))
  return(df)
}
comp_names <- c ("NO2","NO_","O3","Temp","Nied","WG","WR") # fehlen,"GLS","PM10","PM2.5", "CO"
station_names <- c("Alb","Brn","Can","Egg","Frei","Friedri","Heid","Heil","Kar",
                   "Lbg_4","Lbg_Friedr","Man","Nck","Odw","Rt_","Rtl","Sws")
comp_list <- vector("list",length = length(comp_names))
names(comp_list) <- comp_names
BW_station_data <- vector ("list", length = length(station_names))
names(BW_station_data) <- station_names

# IMPORT ALL
for (stnm in names(BW_station_data)){
  station.exist <- str_detect(cloud_files,stnm) %>% as.vector()
  stat.select <- cloud_files[station.exist]
for (nm in names(comp_list)){
            comp.exist <- str_detect(stat.select,nm) %>% as.vector()
            if (sum(comp.exist)==1) {
            comp.file <- stat.select[comp.exist]
            df <- read.data(file.path(cloud_path,comp.file)) %>% dplyr::select(c(1,4,5))
            names(df) <- c("station","datetime",nm)
            df$name <-stnm
            comp_list[[nm]] <- repl.na.datetime(df)}
            else {comp_list[[nm]] <- paste(nm,"no_data")}
                     }
           BW_station_data[[stnm]] <- comp_list
  }

summary(BW_station_data)
BW_station_data[["Sws"]] %>% head()
save(BW_station_data,file = "BW_17_stationen.RData")



