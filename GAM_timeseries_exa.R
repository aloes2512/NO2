# exploring electrical meter data
URL <- "https://petolau.github.io/Forecast-electricity-consumption-with-similar-day-approach-in-R/"
browseURL(URL)
browseURL("https://cran.r-project.org/doc/manuals/r-patched/R-admin.html")
# Introduction to data.table
browseURL("https://campus.datacamp.com/courses/data-table-data-manipulation-r-tutorial-archived/chapter-one-datatable-novice?ex=1")

library(readr)
library(data.table)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(grid)
library(forecast)
library(ggmap)

register_google("AIzaSyA_I6bmTgaNCZT9gGf2kh0Rl9zuRz9kDOw") # Ist API unter der Geolocation angmeldet wurde
# alle metadaten einlesen
Meta_data <- data.table::fread("meta/all_sites.csv")
# Dimension SQ_M
Meta_data <-as.data.frame(Meta_data ) %>% mutate(SQ_M =  SQ_FT*0.09290304)%>% 
  dplyr::select(-c(SQ_FT, TIME_ZONE, TZ_OFFSET)) %>% as.data.table()
Meta_data %>% head(1) #[1] "SITE_ID"     "INDUSTRY"  "SUB_INDUSTRY"          "LAT" "LNG"       "SQ_M"   
#         6        Commercial    Shopping Center/Mall  34.783 -106.8952    15006.81
# Uebersicht Industrie / Anzahl der Sub_Industrien als df
Meta_summary <-Meta_data %>% group_by(INDUSTRY,SUB_INDUSTRY) %>% summarise (N= n())
summary(Meta_summary)
grid.table(Meta_summary, cols= c("INDUSTRY","SUB_INDUSTRY","N"))
# Uebersicht alternativ mit data.table
qplot(1:5, 1:5, geom = "blank") + theme_bw() + 
  theme(line = element_blank(), text = element_blank()) +
  annotation_custom(grob = tableGrob(Meta_data[, .N, by = .(INDUSTRY, SUB_INDUSTRY)]))
# Data table als df
Meta_frame <- Meta_data%>% as.data.frame()
Meta_frame %>% head(1)
# Darstellung in Karte
map <- get_map(location = c(lon = -95.3632715, lat = 29.7632836), zoom = 4)
ggmap(map) + 
  geom_point(data = Meta_data, aes(x = LNG, y = LAT, color = INDUSTRY),
             size = 5, alpha = .6) #+ 
  theme(axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"))
                   
names(Meta_data) == names(Meta_frame)
names(Meta_frame) # [1] "SITE_ID" "INDUSTRY" "SUB_INDUSTRY" "LAT"  "LNG"  "SQ_M
# Overview Customers SQ_M
Meta_data %>% ggplot(mapping = aes(SQ_M))+
geom_histogram(binwidth = 10000, aes( col = INDUSTRY))
ggplot(Meta_data,mapping =  aes(x = SQ_M, colour = INDUSTRY, fill = INDUSTRY)) + 
  geom_density(alpha=0.55) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"))+
  labs(title = " Density (per sq_m of sub industries)")
Meta_frame%>% ggplot (aes(SQ_M)) + 
  geom_freqpoly(binwidth = 10000,mapping = aes(col = INDUSTRY) )
Meta_frame %>% ggplot(aes(SQ_M)) + 
  geom_density(alpha = 0.5,mapping = aes(col = INDUSTRY,fill = INDUSTRY))+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"))+
  labs(title = " Density (per sq_m of sub industries")
# ======================================================
# Daten auf benötigte Variable beschrämken
names(Meta_data) <-c("ID","INDUSTRY","SUB_INDUSTRY","LAT","LNG","SQ_M")
names(Meta_frame) <- names(Meta_data)
head(Meta_data,2)  == head(Meta_frame,2) %>% as.data.table()
head(Meta_frame,2)# "ID","INDUSTRY","SUB_INDUSTRY","LAT","LNG","SQ_M"
# add timeseries data / measurements per ID directory ~/.../NO2/csv"
files <- list.files("csv/",pattern = "*.csv") #[1] "all_sites.csv"    "HeizDatenCan.csv"

DT <- rbindlist(lapply(files, function(x) cbind(fread(paste0("csv/",x)),
                                                file= gsub(".csv", "", x))), # reads from.../NO2/...
                                                fill = TRUE) # fills blanks in rows with unequal length
names(DT) <- c( "timestamp" ,    "dttm_utc",      "value", "estimated","anomaly",  "ID")
head(DT,2)#     1325376600    2012-01-01 00:10:00 105.7895      0         NA        10
dim(DT) # [1] 10531288        6
DT$ID <- DF$ID %>% as_factor()
DF <- DT %>% as.data.frame()
# prepare Meta_data to merge with DT
names(Meta_data)
names(Meta_frame) 
Meta_data$ID <- Meta_data$ID %>% as_factor()
Meta_frame$ID <- as_factor (Meta_frame$ID)
head(Meta_data,2)
# Extract mean, median, sum (consumption)
ID_stats <- DT[, .(Mean = mean(value), Median = median(value),
                   Sum = sum(value)), .(ID)]
ID_stats[, ID:= as.factor(ID_stats[["ID"]])]

#merge with Meta_data with ID_stats
data_m <- merge(ID_stats, Meta_data, by = "ID")
dim (data_m)
names(data_m)#"ID"| "Mean" |"Median"| "Sum"| "INDUSTRY"| "SUB_INDUSTRY"| "LAT" | "LNG" |"SQ_M"  
sub_sum <- data_m[, .(mean(Mean)),by = .(SUB_INDUSTRY)]
head(sub_sum,2)
# use right_join to merge
DF_merge <-as.data.frame(ID_stats)  %>% right_join(Meta_frame, by = c("ID"))
names(DF_merge)#"ID"|"Mean"|"Median"|"Sum"|"INDUSTRY"|"SUB_INDUSTRY"|"LAT"|"LNG"|"SQ_M" 
DF_SUB_merge <- DF_merge %>% group_by(SUB_INDUSTRY) %>% summarise ( V1 = mean(Mean))
#Bar plot of mean load by sub-industries:
ggplot(sub_sum, aes(x = reorder(SUB_INDUSTRY, V1), y = V1, # takes SUB_INDUSTRY and orders by V!
                    fill = reorder(SUB_INDUSTRY, V1))) +
  geom_bar(stat = "identity", width = 0.8) +
  labs(x = "", y = "Mean Load (kW)",
       title = "Mean load by subindustries",
       fill = "SUB_INDUSTRY") +
  theme(title = element_text(size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
# Barplot from df- data DF_SUB_merge
ggplot (DF_SUB_merge,aes( x = reorder(SUB_INDUSTRY,V1),y = V1,
                                     fill = reorder(SUB_INDUSTRY, V1)))+
  geom_bar(stat = "identity", width = 0.8) +
  labs(x = "", y = "Mean Load (kW)",
       title = "Mean load by subindustries",
       fill = "SUB_INDUSTRY")+
  theme(title = element_text(size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
#===========================================
ggplot(data_m[, .(SQ_M, Median, INDUSTRY)], aes(x = SQ_M, y = Median)) +
  geom_point(aes(colour = INDUSTRY, shape = INDUSTRY), size = 4, alpha = 0.8) +
  geom_smooth(method = lm, color = "yellow1", se = TRUE) +
  scale_shape_manual(values = c(15,16,17,18)) +
  scale_color_manual(values=c("salmon", "dodgerblue2", "springgreen3", "plum3")) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"))
#================
#' Darstellung mit df'
DF_merge %>% names()
ggplot(DF_merge, aes(x = SQ_M, y = Median))+
  geom_point(aes(colour = INDUSTRY, shape = INDUSTRY), size = 4, alpha = 0.8) +
  geom_smooth(method = lm, color = "yellow1", se = TRUE) +
  scale_shape_manual(values = c(15,16,17,18)) +
  scale_color_manual(values=c("salmon", "dodgerblue2", "springgreen3", "plum3")) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"))
  
# correlation between median load and square meters
cor(data_m[, SQ_M], data_m[, Median]) # 0.4505678
# use Dframes
data_m %>% head(1)%>% as.data.frame()==DF_merge %>% head(1)
# Prepare dataset to forecast timeseries load
# ================
DT[, date_time := ymd_hms(DT[["dttm_utc"]])]
DT[, date := as.Date(DT[["dttm_utc"]], "%Y-%m-%d")]
DT[, ':='(timestamp = NULL, estimated = NULL, anomaly = NULL,dttm_utc= NULL)]
DT %>% head()
str(DT)
count_ID <- DT[,.N,.(ID)]
full <- count_ID[N == max(N), .(ID)]
nrow(full) # 43
full[,ID]
DT <- DT[ID %in% full[,ID]]
unique(DT[,ID])
num_date <- DT[ID == 100, .N, .(date)]
num_date
table(num_date[,N])
DT <- DT[!date %in% num_date[c(2,366), date]]
ggplot(DT[ID == 99, .(value, date)], aes(date, value)) +
  geom_line(alpha= 0.4) +
  theme(panel.border = element_blank(), panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"), 
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "Load (kW)")
