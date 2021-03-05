BW_path<-"~/documents/Luftqualitaet/Daten/BW"
file.path(BW_path,"BW_list_tbl.RData")
load(file.path(BW_path,"BW_list_tbl.RData"))          
summary(BW_list_tbl)
summary(BW_list_tbl$Stg_Schwz)# keine WG
# aktuelle Daten mit WG
Halbstd_Werte_Stuttgart_Mitte_SZ_2000 <- read_xlsx("~/Documents/Luftqualitaet/Daten/Stuttgart/Halbstd-Werte-Stuttgart-Mitte-SZ_1987-2020.xlsx",
skip = 8,sheet =14)%>% dplyr::select(c(1,2,3,7,15))

Halbstd_Werte_Stuttgart_Mitte_SZ_2000<-Halbstd_Werte_Stuttgart_Mitte_SZ_2000%>% 
  mutate(station = as_factor("xxx"),name= as_factor("SZ"),datetime =  Datum)%>% 
  dplyr::select(station,name,datetime,
                NO2 =`NO2 (µg/m³)`,
                Temp=`Temp. (°C)...3`,
                WG= `WG (m/s)...7`)
SZ_2000<-Halbstd_Werte_Stuttgart_Mitte_SZ_2000

summary(SZ_2000)
#2001
Halbstd_Werte_Stuttgart_Mitte_SZ_2001 <- read_xlsx("~/Documents/Luftqualitaet/Daten/Stuttgart/Halbstd-Werte-Stuttgart-Mitte-SZ_1987-2020.xlsx",
                                                   skip = 8,sheet =15)%>% dplyr::select(c(1,2,3,7,16))

Halbstd_Werte_Stuttgart_Mitte_SZ_2001<-Halbstd_Werte_Stuttgart_Mitte_SZ_2001%>% 
  mutate(station = as_factor("xxx"),name= as_factor("SZ"),datetime =  Datum)%>% 
  dplyr::select(station,name,datetime,
                NO2 =`NO2 (µg/m³)`,
                Temp=`Temp. (°C)...3`,
                WG= `WG (m/s)...7`)
SZ_2001<-Halbstd_Werte_Stuttgart_Mitte_SZ_2001

summary(SZ_2001)
#2002
Halbstd_Werte_Stuttgart_Mitte_SZ_2002 <- read_xlsx("~/Documents/Luftqualitaet/Daten/Stuttgart/Halbstd-Werte-Stuttgart-Mitte-SZ_1987-2020.xlsx",
                                                   skip = 8,sheet =16)%>% dplyr::select(c(1,2,3,7,17))


Halbstd_Werte_Stuttgart_Mitte_SZ_2002<-Halbstd_Werte_Stuttgart_Mitte_SZ_2002%>% 
  mutate(station = as_factor("xxx"),name= as_factor("SZ"),datetime =  Datum)%>% 
  dplyr::select(station,name,datetime,
                NO2 =`NO2 (µg/m³)`,
                Temp=`Temp. (°C)...3`,
                WG= `WG (m/s)...7`)
SZ_2002<-Halbstd_Werte_Stuttgart_Mitte_SZ_2002

summary(SZ_2002)

#2003
Halbstd_Werte_Stuttgart_Mitte_SZ_2003 <- read_xlsx("~/Documents/Luftqualitaet/Daten/Stuttgart/Halbstd-Werte-Stuttgart-Mitte-SZ_1987-2020.xlsx",
                                                   skip = 8,sheet =17)%>% dplyr::select(c(1,2,3,7,17))


Halbstd_Werte_Stuttgart_Mitte_SZ_2003<-Halbstd_Werte_Stuttgart_Mitte_SZ_2003%>% 
  mutate(station = as_factor("xxx"),name= as_factor("SZ"),datetime =  Datum)%>% 
  dplyr::select(station,name,datetime,
                NO2 =`NO2 (µg/m³)`,
                Temp=`Temp. (°C)...3`,
                WG= `WG (m/s)...7`)
SZ_2003<-Halbstd_Werte_Stuttgart_Mitte_SZ_2003

summary(SZ_2003)
#2004
Halbstd_Werte_Stuttgart_Mitte_SZ_2004 <- read_xlsx("~/Documents/Luftqualitaet/Daten/Stuttgart/Halbstd-Werte-Stuttgart-Mitte-SZ_1987-2020.xlsx",
                                                   skip = 8,sheet =18)%>% dplyr::select(c(1,2,3,7,17))


Halbstd_Werte_Stuttgart_Mitte_SZ_2004<-Halbstd_Werte_Stuttgart_Mitte_SZ_2004%>% 
  mutate(station = as_factor("xxx"),name= as_factor("SZ"),datetime =  Datum)%>% 
  dplyr::select(station,name,datetime,
                NO2 =`NO2 (µg/m³)`,
                Temp=`Temp. (°C)...3`,
                WG= `WG (m/s)...7`)
SZ_2004<-Halbstd_Werte_Stuttgart_Mitte_SZ_2004

summary(SZ_2004)
#2005
Halbstd_Werte_Stuttgart_Mitte_SZ_2005 <- read_xlsx("~/Documents/Luftqualitaet/Daten/Stuttgart/Halbstd-Werte-Stuttgart-Mitte-SZ_1987-2020.xlsx",
                                                   skip = 8,sheet =19)%>% dplyr::select(c(1,2,3,7,17))


Halbstd_Werte_Stuttgart_Mitte_SZ_2005<-Halbstd_Werte_Stuttgart_Mitte_SZ_2005%>% 
  mutate(station = as_factor("xxx"),name= as_factor("SZ"),datetime =  Datum)%>% 
  dplyr::select(station,name,datetime,
                NO2 =`NO2 (µg/m³)`,
                Temp=`Temp. (°C)...3`,
                WG= `WG (m/s)...7`)
SZ_2005<-Halbstd_Werte_Stuttgart_Mitte_SZ_2005

summary(SZ_2005)
#2006
Halbstd_Werte_Stuttgart_Mitte_SZ_2006 <- read_xlsx("~/Documents/Luftqualitaet/Daten/Stuttgart/Halbstd-Werte-Stuttgart-Mitte-SZ_1987-2020.xlsx",
                                                   skip = 8,sheet =20)%>% dplyr::select(c(1,2,3,7,17))


Halbstd_Werte_Stuttgart_Mitte_SZ_2006<-Halbstd_Werte_Stuttgart_Mitte_SZ_2006%>% 
  mutate(station = as_factor("xxx"),name= as_factor("SZ"),datetime =  Datum)%>% 
  dplyr::select(station,name,datetime,
                NO2 =`NO2 (µg/m³)`,
                Temp=`Temp. (°C)...3`,
                WG= `WG (m/s)...7`)
SZ_2006<-Halbstd_Werte_Stuttgart_Mitte_SZ_2006

summary(SZ_2006)
#2007
Halbstd_Werte_Stuttgart_Mitte_SZ_2007 <- read_xlsx("~/Documents/Luftqualitaet/Daten/Stuttgart/Halbstd-Werte-Stuttgart-Mitte-SZ_1987-2020.xlsx",
                                                   skip = 8,sheet =21)%>% dplyr::select(c(1,2,3,7,17))


Halbstd_Werte_Stuttgart_Mitte_SZ_2007<-Halbstd_Werte_Stuttgart_Mitte_SZ_2007%>% 
  mutate(station = as_factor("xxx"),name= as_factor("SZ"),datetime =  Datum)%>% 
  dplyr::select(station,name,datetime,
                NO2 =`NO2 (µg/m³)`,
                Temp=`Temp. (°C)...3`,
                WG= `WG (m/s)...7`)
SZ_2007<-Halbstd_Werte_Stuttgart_Mitte_SZ_2007

summary(SZ_2007)
#2008
Halbstd_Werte_Stuttgart_Mitte_SZ_2008 <- read_xlsx("~/Documents/Luftqualitaet/Daten/Stuttgart/Halbstd-Werte-Stuttgart-Mitte-SZ_1987-2020.xlsx",
                                                   skip = 8,sheet =22)%>% dplyr::select(c(1,2,3,7,17))


Halbstd_Werte_Stuttgart_Mitte_SZ_2008<-Halbstd_Werte_Stuttgart_Mitte_SZ_2008%>% 
  mutate(station = as_factor("xxx"),name= as_factor("SZ"),datetime =  Datum)%>% 
  dplyr::select(station,name,datetime,
                NO2 =`NO2 (µg/m³)`,
                Temp=`Temp. (°C)...3`,
                WG= `WG (m/s)...7`)
SZ_2008<-Halbstd_Werte_Stuttgart_Mitte_SZ_2008

summary(SZ_2008)
#2009
Halbstd_Werte_Stuttgart_Mitte_SZ_2009 <- read_xlsx("~/Documents/Luftqualitaet/Daten/Stuttgart/Halbstd-Werte-Stuttgart-Mitte-SZ_1987-2020.xlsx",
                                                   skip = 8,sheet =23)%>% dplyr::select(c(1,2,3,7,17))


Halbstd_Werte_Stuttgart_Mitte_SZ_2009<-Halbstd_Werte_Stuttgart_Mitte_SZ_2009%>% 
  mutate(station = as_factor("xxx"),name= as_factor("SZ"),datetime =  Datum)%>% 
  dplyr::select(station,name,datetime,
                NO2 =`NO2 (µg/m³)`,
                Temp=`Temp. (°C)...3`,
                WG= `WG (m/s)...7`)
SZ_2009<-Halbstd_Werte_Stuttgart_Mitte_SZ_2009

summary(SZ_2009)
#2010
Halbstd_Werte_Stuttgart_Mitte_SZ_2010 <- read_xlsx("~/Documents/Luftqualitaet/Daten/Stuttgart/Halbstd-Werte-Stuttgart-Mitte-SZ_1987-2020.xlsx",
                                                   skip = 8,sheet =24)%>% dplyr::select(c(1,2,3,7,17))


Halbstd_Werte_Stuttgart_Mitte_SZ_2010<-Halbstd_Werte_Stuttgart_Mitte_SZ_2010%>% 
  mutate(station = as_factor("xxx"),name= as_factor("SZ"),datetime =  Datum)%>% 
  dplyr::select(station,name,datetime,
                NO2 =`NO2 (µg/m³)`,
                Temp=`Temp. (°C)...3`,
                WG= `WG (m/s)...7`)
SZ_2010<-Halbstd_Werte_Stuttgart_Mitte_SZ_2010

summary(SZ_2010)
#2011
Halbstd_Werte_Stuttgart_Mitte_SZ_2011 <- read_xlsx("~/Documents/Luftqualitaet/Daten/Stuttgart/Halbstd-Werte-Stuttgart-Mitte-SZ_1987-2020.xlsx",
                                                   skip = 8,sheet =25)%>% dplyr::select(c(1,2,3,7,17))


Halbstd_Werte_Stuttgart_Mitte_SZ_2011<-Halbstd_Werte_Stuttgart_Mitte_SZ_2011%>% 
  mutate(station = as_factor("xxx"),name= as_factor("SZ"),datetime =  Datum)%>% 
  dplyr::select(station,name,datetime,
                NO2 =`NO2 (µg/m³)`,
                Temp=`Temp. (°C)...3`,
                WG= `WG (m/s)...7`)
SZ_2011<-Halbstd_Werte_Stuttgart_Mitte_SZ_2011

summary(SZ_2011)
#2012
Halbstd_Werte_Stuttgart_Mitte_SZ_2012 <- read_xlsx("~/Documents/Luftqualitaet/Daten/Stuttgart/Halbstd-Werte-Stuttgart-Mitte-SZ_1987-2020.xlsx",
                                                   skip = 8,sheet =26)%>% dplyr::select(c(1,2,3,7,17))


Halbstd_Werte_Stuttgart_Mitte_SZ_2012<-Halbstd_Werte_Stuttgart_Mitte_SZ_2012%>% 
  mutate(station = as_factor("xxx"),name= as_factor("SZ"),datetime =  Datum)%>% 
  dplyr::select(station,name,datetime,
                NO2 =`NO2 (µg/m³)`,
                Temp=`Temp. (°C)...3`,
                WG= `WG (m/s)...7`)
SZ_2012<-Halbstd_Werte_Stuttgart_Mitte_SZ_2012

summary(SZ_2012)
#2013
Halbstd_Werte_Stuttgart_Mitte_SZ_2013 <- read_xlsx("~/Documents/Luftqualitaet/Daten/Stuttgart/Halbstd-Werte-Stuttgart-Mitte-SZ_1987-2020.xlsx",
                                                   skip = 8,sheet =27)%>% dplyr::select(c(1,2,3,7,17))


Halbstd_Werte_Stuttgart_Mitte_SZ_2013<-Halbstd_Werte_Stuttgart_Mitte_SZ_2013%>% 
  mutate(station = as_factor("xxx"),name= as_factor("SZ"),datetime =  Datum)%>% 
  dplyr::select(station,name,datetime,
                NO2 =`NO2 (µg/m³)`,
                Temp=`Temp. (°C)...3`,
                WG= `WG (m/s)...7`)
SZ_2013<-Halbstd_Werte_Stuttgart_Mitte_SZ_2013

summary(SZ_2013)
#2014
Halbstd_Werte_Stuttgart_Mitte_SZ_2014 <- read_xlsx("~/Documents/Luftqualitaet/Daten/Stuttgart/Halbstd-Werte-Stuttgart-Mitte-SZ_1987-2020.xlsx",
                                                   skip = 8,sheet =28)%>% dplyr::select(c(1,2,3,7,17))


Halbstd_Werte_Stuttgart_Mitte_SZ_2014<-Halbstd_Werte_Stuttgart_Mitte_SZ_2014%>% 
  mutate(station = as_factor("xxx"),name= as_factor("SZ"),datetime =  Datum)%>% 
  dplyr::select(station,name,datetime,
                NO2 =`NO2 (µg/m³)`,
                Temp=`Temp. (°C)...3`,
                WG= `WG (m/s)...7`)
SZ_2014<-Halbstd_Werte_Stuttgart_Mitte_SZ_2014

summary(SZ_2014)
#2015
Halbstd_Werte_Stuttgart_Mitte_SZ_2015 <- read_xlsx("~/Documents/Luftqualitaet/Daten/Stuttgart/Halbstd-Werte-Stuttgart-Mitte-SZ_1987-2020.xlsx",
                                                   skip = 8,sheet =29)%>% dplyr::select(c(1,2,3,7,17))


Halbstd_Werte_Stuttgart_Mitte_SZ_2015<-Halbstd_Werte_Stuttgart_Mitte_SZ_2015%>% 
  mutate(station = as_factor("xxx"),name= as_factor("SZ"),datetime =  Datum)%>% 
  dplyr::select(station,name,datetime,
                NO2 =`NO2 (µg/m³)`,
                Temp=`Temp. (°C)...3`,
                WG= `WG (m/s)...7`)
SZ_2015<-Halbstd_Werte_Stuttgart_Mitte_SZ_2015

summary(SZ_2015)
sum(is.na(SZ_2015$Temp))#253

#2016
Halbstd_Werte_Stuttgart_Mitte_SZ_2016 <- read_xlsx("~/Documents/Luftqualitaet/Daten/Stuttgart/Halbstd-Werte-Stuttgart-Mitte-SZ_1987-2020.xlsx",
                                                   skip = 8,sheet =30)%>% dplyr::select(c(1,2,3,7,17))


Halbstd_Werte_Stuttgart_Mitte_SZ_2016<-Halbstd_Werte_Stuttgart_Mitte_SZ_2016%>% 
  mutate(station = as_factor("xxx"),name= as_factor("SZ"),datetime =  Datum)%>% 
  dplyr::select(station,name,datetime,
                NO2 =`NO2 (µg/m³)`,
                Temp=`Temp. (°C)...3`,
                WG= `WG (m/s)...7`)
SZ_2016<-Halbstd_Werte_Stuttgart_Mitte_SZ_2016

summary(SZ_2016)
NROW(SZ_2016)# 17572
sum(is.na(SZ_2016$Temp))#36
sum(is.na(SZ_2016$NO2))#4096
sum(is.na(SZ_2016$WG))#35

# 2017
SZ_path_2017<- file.path(BW_path,BW_files[112])
SZ_path_2017 == "~/Documents/Luftqualitaet/Daten/BW/SZ-Temp_WG_NO2_2017.xlsx" 
SZ_2017 <- tibble()
for (i in 1:12){
  df<- read_xlsx(SZ_path_2017,
                 trim_ws = TRUE,sheet =i,col_types = c("date","numeric","numeric","numeric")) #%>% mutate(Temp=as.numeric(Temp),
  #          WG=as.numeric(WG),
  #         NO2= as.numeric(NO2) )
  
  SZ_2017<- bind_rows(SZ_2017,df)
}
SZ_2017<-SZ_2017 %>% 
  mutate(station= as_factor("xxx"),name=as_factor("SZ"))%>%
  dplyr::select(station,name,datetime,NO2,Temp,WG)
head(SZ_2017)
names(SZ_2017)#[1] "datetime" "Temp"     "WG"       "NO2" 
NROW(SZ_2017) #17520
summary(SZ_2017)
# 2018
SZ_2018 <- tibble()
for (i in 1:12){
  df<-read_xlsx(file.path(BW_path,"S-Mitte-SZ-Halbstd-Werte_2018.xlsx"),skip = 6,sheet =i,
                col_types = c("date",rep("numeric",19)))
  df<- dplyr::select(df,c(1,3,7,17))
  names(df)<-c("datetime","Temp","WG","NO2")
  df<- df%>% mutate(Temp= as.numeric(Temp),
                    WG= as.numeric(WG),
                    NO2 =as.numeric(NO2))
  SZ_2018 <- bind_rows(SZ_2018,df)
}
SZ_2018 <-SZ_2018 %>% mutate(station = as_factor("xxx"),name = as_factor("SZ"))%>%
  dplyr::select(station,name,datetime,NO2,Temp,WG)
summary(SZ_2018)
NROW(SZ_2018)#[1] 17568
sum(is.na(SZ_2018$NO2))#85
sum(is.na(SZ_2018$Temp))#91
sum(is.na(SZ_2018$WG))#WG
SZ_2019 <- tibble()
for (i in 1:12){
  df<-read_xlsx(file.path(BW_path,"S-Mitte-SZ-Halbstd-Werte_2019.xlsx"),skip = 6,sheet =i,
                col_types = c("date",rep("numeric",19)))
  df<- dplyr::select(df,c(1,3,7,17))
  names(df)<-c("datetime","Temp","WG","NO2")
  df<- df%>% mutate(Temp= as.numeric(Temp),
                    WG= as.numeric(WG),
                    NO2 =as.numeric(NO2))
  SZ_2019 <- bind_rows(SZ_2019,df)
}
SZ_2019 <-SZ_2019 %>% mutate(station = as_factor("xxx"),name = as_factor("SZ"))%>%
  dplyr::select(station,name,datetime,NO2,Temp,WG)
summary(SZ_2019)
NROW(SZ_2019)#[1] 17568
sum(is.na(SZ_2019$Temp))#8910
sum(is.na(SZ_2019$NO2))#8946
sum(is.na(SZ_2019$WG))#8913
#2020
SZ_2020 <- tibble() # 20 parameter
for (i in 1:12) {
  df<-read_xlsx(file.path(BW_path,"S-Mitte-SZ-Halbstd-Werte_2020.xlsx"),skip = 6,sheet =i,
                col_types = c("date",rep("numeric",19)))
  df<-dplyr::select(df,c(1,3,7,17))
  str(df)
  names(df)<-c("datetime","Temp","WG","NO2")
  SZ_2020 <- bind_rows(SZ_2020,df)
}
SZ_2020 <-SZ_2020 %>% mutate(station = as_factor("xxx"),name = as_factor("SZ"))%>%
  dplyr::select(station,name,datetime,NO2,Temp,WG)
summary(SZ_2020)

summary(SZ_2020)
NROW(SZ_2020)#[1] 17616
sum(is.na(SZ_2020$Temp))#74
sum(is.na(SZ_2020$NO2))#1427
sum(is.na(SZ_2020$WG))#74
# ======= Bind Years=====



SZ_17_20%>%ggplot(aes(x=datetime,y= NO2))+geom_smooth(col= "red")+
  ggtitle("NO2-Immissionen
Schwabenzentrum")+
  labs(x="", y="NO2 [μg/m3]")
#2019 Daten des AfU (nicht Schwabenzentrum), 
#!!!!!!!!!!keine NO2 Messungen!!!!
SZ_2019_AfU <- tibble()         
for (i in 1:12){
  df<-read_xlsx(file.path(BW_path,"S-Mitte_AfU_Halbstd.-Werte_2019 2.xlsx"),skip = 4,sheet =i)
  df<- df%>%dplyr::select(c(1,3,15))
  names(df)<-c("datetime","Temp","WG")
  df<- df%>% mutate(Temp= as.numeric(Temp),
                    WG= as.numeric(WG))
  SZ_2019_AfU <- bind_rows(SZ_2019_AfU,df)
  
}
SZ_2019_AfU<-SZ_2019_AfU%>%
  mutate(station=as_factor("xxx"),name=as_factor("SZ"))%>% 
  dplyr::select(station,name,datetime,Temp,WG)
summary(SZ_2019_AfU)
head(SZ_2019_AfU,2)
NROW(SZ_2019_AfU)#17580
sum(is.na(SZ_2019_AfU$WG))#880
sum(is.na(SZ_2019_AfU$Temp))
SZ_2019_AfU %>% ggplot(aes(x=datetime))+
  geom_smooth(aes(y=WG),method = "gam",col = "black")+
  geom_smooth(aes(y= Temp),col = "blue")+
  ggtitle("Temp,WG AfU Stuttgart
          2019")

#2020 Daten des AfU (nicht Schwabenzentrum)
SZ_2020_AfU <- tibble()         
for (i in 1:12){
  df<-read_excel(file.path(BW_path,"S-Mitte_AfU_Temp_WG_NO2_2020.xlsx"),
                skip = 6,
                sheet =i,
                col_types = c("date","numeric","numeric","numeric","numeric"),
                col_names = TRUE)
  names(df)<-c("datetime","Zeit","Temp","WG","NO2")

 
  SZ_2020_AfU <- bind_rows(SZ_2020_AfU,df)
  
}
SZ_2020_AfU<-SZ_2020_AfU%>%
  mutate(station=as_factor("xxx"),name=as_factor("SZ"))%>% 
  dplyr::select(station,name,datetime,Temp,WG,NO2)
summary(SZ_2020_AfU)
NROW(SZ_2020_AfU)#17616


#================
SZ_17_20 <- bind_rows(SZ_2017,SZ_2018,SZ_2019,SZ_2020_AfU)
summary(SZ_17_20)
SZ_17_20%>% filter(datetime<ymd("2021-01-01"))%>%
                     ggplot(aes(x= datetime,y= NO2))+
  geom_smooth(col= "red")+
  geom_smooth(method= "lm",linetype = 3, col= "red")+
  ggtitle("NO2-Immissionen
          Schwabenzentrum AfU Gaisburgstr.4",
          subtitle = "Mittelwerte & linearer Trend")+
  labs(x="",y= "NO2 [μg/m3]")
# Daten sichern
save(SZ_17_20,file = file.path(BW_path,"SZ_17_20.RData"))
SZ_00_20 <-bind_rows(SZ_2000,SZ_2001,SZ_2002,SZ_2003,SZ_2004,SZ_2005,SZ_2006,
                     SZ_2007,SZ_2008,SZ_2009,SZ_2010,SZ_2011,SZ_2012,SZ_2013,
                     SZ_2014,SZ_2015,SZ_2016,SZ_17_20)
SZ_00_20 %>% head(2)
summary(SZ_00_20)
save(SZ_00_20,file= file.path(BW_path,"SZ_00_20.RData"))
# Zufügen BW_list_tbl
BW_list_tbl$Stg_SZ_afu <-SZ_17_20
save(BW_list_tbl,file = file.path(BW_path,"BW_list_tbl.RData"))
#===================
# Overview
SZ_NO2_00_20_plt<-SZ_00_20%>% ggplot(aes(x= datetime,y= NO2))+
  geom_smooth(method="gam",formula= y~s(x, k = 20),col= "red")+
  geom_smooth(method= "lm",linetype = 3, col= "red")+
  ggtitle("NO2-Immissionen 
  Schwabenzentrum & AfU",
subtitle = "Mittelwerte & linearer Trend")
ggplot(SZ_00_20,aes(x =datetime,y=Temp))+
  geom_smooth(method="gam",
              formula= y~s(x, k = 20),col= "blue")+
  geom_smooth(method= "lm",linetype = 3, col= "blue")+
  ggtitle("Temperaturen Schwb.ztr.& AfU
          2000 bis 2020",
  subtitle = "Mittelwerte & linearer Trend")+
  labs(x="",y= "Temp [°C]")
SZ_00_20%>% ggplot(aes(x= datetime,y= WG))+
  geom_smooth(method="gam",formula= y~s(x, k = 20),col= "green")+
  geom_smooth(method= "lm",linetype = 3, col= "green")+
  ggtitle("Windgeschw. Schwb.ztr.& AfU",
  subtitle = "Mittelwerte & linearer Trend")+
  labs(x="",y= "WG [m/s]")
# AfU 2020
SZ_2020_AfU %>% ggplot(aes(x=datetime))+
  geom_smooth(aes(y=WG),method = "gam",col = "black")+
  geom_smooth(aes(y= Temp),method = "gam",col = "blue")+
  geom_smooth(aes(y= NO2),method = "gam",col = "red")+
  ggtitle("AfU Stuttgart 2020",subtitle="Temp(blue),WG(black),NO2(red)")+
  labs(x="",y = "NO2,WG,Temp")

 