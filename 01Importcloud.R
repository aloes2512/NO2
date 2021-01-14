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

save(BW_station_data,file = "BW_17_stationen.RData")

# Beispiel Sws
BW_station_data[["Sws"]] %>% head(10) %>% names()
NO2 <- BW_station_data[["Sws"]]$NO2 %>% 
  as.data.frame()
NROW(NO2) #173159     
NO  <- BW_station_data[["Sws"]]$NO_ %>% 
  as.data.frame() %>% 
  filter(datetime <= last(NO2$datetime))
O3 <- BW_station_data[["Sws"]]$O3 
NROW(O3) # 173159
Sws_data <- bind_cols(NO2,NO = NO$NO_,O3 = O3$O3) %>% 
  dplyr::select(station,name,datetime,NO2,NO,O3)
Sws_data_long <- Sws_data %>% pivot_longer(cols = c(NO2,NO),names_to = "Komp")
str(Sws_data_long) 
summary(Sws_data_long)
ggplot(Sws_data)+
  geom_point(mapping = aes(x = NO,y= NO2),size = 0.1,alpha= 0.5)+
  geom_smooth(method = "lm",mapping = aes(x = NO,y= NO2))
ggplot(Sws_data)+
  geom_smooth(method = "lm",mapping = aes(x= datetime,y = NO2))+
  facet_wrap(~c(NO2,NO))
    
