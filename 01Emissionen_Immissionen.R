library(tidyverse)
library(lubridate)
library(readxl)
save.path <- paste0(getwd(),"/Abbldg/")
list_names <- c("Odw.no2" ,   "Sws.no2" ,   "Alb.no2"  ,
                "Brn.no2"  ,  "Rt.no2" ,
                "Can.no2"  ,  "Lbg.no2" ,   "Lbg.Fr.no2" ,"Nck.no2")

my_stations.year <- vector("list", length= length(list_names))
names(my_stations.year) <- list_names
# Kalenderjahre als Faktor
for  (nm in list_names) {
  my_stations$data[[nm]]$fullyear <- my_stations$data[[nm]]$datetime %>%
    floor_date("year") %>%
    str_extract("^.{4}") %>% as.numeric()
}
# Jahreswerte als Mittelwerte
for (nm in list_names) {
  my_stations.year[[nm]] <- my_stations$data[[nm]] %>%
    group_by(fullyear) %>%
    summarise(NO2.year.mean = mean(NO2), residual.year.mean = mean(residuals),n())
}
nm
my_stations$data[[nm]] %>% head()
my_stations.year[[nm]] %>% head()
reg_year.plot <- vector ( "list", length = length (list_names))
names(reg_year.plot) <- list_names
Nim <- length(list_names) # 9
for (nm in list_names) {
  reg_year.plot[[nm]]<- ggplot(my_stations.year[[nm]], aes (x = fullyear , y = NO2.year.mean) ) +
    geom_point()+
    geom_smooth(col = "red", se = FALSE)+
    geom_smooth(method = "lm", col = "blue")+
    labs ( x = "Kalenderjahr", y = "NO2 Jahresmittel")+
    ggtitle(paste( nm, "Jahresmittel" ),
            subtitle = "Trend und Regressionsgerade")
}
# Emissionen zufügen
# Daten aus Internet sind unter Daten/BRD/NO2_Emissionen_BRD.xls abgelegt
emissions <-  read_excel("~/Documents/Luftqualitaet/Daten/BW_dat/NO2_Emissionen_BRD.xls")
names(emissions)
emissions%>% head()
emissions.2000 <- emissions %>% filter(Jahr >= 2000)
emissions.2000 <-emissions.2000 %>% select(Jahr,BWVerkehr) %>%
  mutate (proz.Verkehr.no2 = BWVerkehr/BWVerkehr[1]*100)
# Emissionen NO2 in Proz von Jahr 2000
emission.proz.plot <- ggplot(emissions.2000, aes(Jahr,emissions.2000))+
                 geom_smooth(aes(x = Jahr, y = emissions.2000$proz.Verkehr.no2),col = "red")+
                 geom_smooth(method = "lm",aes(x = Jahr, y = emissions.2000$proz.Verkehr.no2),col = "black" ,linetype = 4 )+
                 labs( y = "NO2 % von 2000")+
                ggtitle ("  NO2 Verkehrsemissionen BW ",
                subtitle = " in % von 2000")
emissions_immissions <- vector("list",length = length(list_names))
names(emissions_immissions) <- list_names
for (nm in list_names) {
emissions_immissions[[nm]] <-left_join(my_stations.year[[nm]],emissions.2000,
          by = c("fullyear" = "Jahr") ) %>%
  mutate(immi.NO2.proz = NO2.year.mean/NO2.year.mean[1]*100)%>%
  dplyr::select(Jahr = "fullyear",immi.proz = "immi.NO2.proz", emi.proz = "proz.Verkehr.no2")
}
emissions_immissions[[1]] %>% head()
emi_immi.plot <- vector("list",length = Nim)
names(emi_immi.plot) <- list_names
for (nm in list_names){
  emi_immi.plot[[nm]] <- ggplot(emissions_immissions[[nm]], aes( Jahr,immi.proz ))+
    geom_point(color = "red", size = 2)+
    geom_smooth(mapping = aes(x = Jahr,y = immi.proz),color = "red")+
    geom_smooth(aes(x= Jahr, y = emi.proz),color = "black")+
    geom_smooth(method = "lm",aes(x= Jahr, y = emi.proz),color = "black",linetype = 4)+
    ggtitle(paste(nm,"Immissionen & Emissionen"),
            subtitle = "Proz v. 2000")+
              labs ( y = "NO2 % v. 2000")
            }
emi_immi.plot %>% walk(print)
# # Regression Jahreswerte
nm #[1] "Nck.no2"
my_stations.year[[nm]] # fullyear NO2.year.mean residual.year.mean `n()`
                   # 1     2004         106.             -13.1      8471
immission_stations <- vector("list",Nim)
names(immission_stations) <- list_names
for (nm in list_names){
  immission_stations[[nm]] <- my_stations.year[[nm]] %>%
    filter (`n()` >  2000) %>%
    dplyr::select(Jahr = "fullyear", NO2 = "NO2.year.mean")
  immission_stations[[nm]]$Name <- nm
  }
# alle NO2 Werte in enem tbl zusammenführen
IMMISSIONS_tbl <- immission_stations[[1]]
for (i in 2: Nim) {
IMMISSIONS_tbl <- IMMISSIONS_tbl %>% bind_rows(immission_stations[[i]])
}
summary(IMMISSIONS_tbl) # 148 Zeilen der 9 Stationen
ggplot(IMMISSIONS_tbl,aes(x = Jahr, y = NO2, color = Name))+
  geom_point()
# Emissionen zufügen
  ggplot(IMMISSIONS_tbl,aes(x = Jahr, y = NO2, color = Name))+
  geom_point()+
  geom_point (data = emissions.2000,
              aes(x = Jahr, y = proz.Verkehr.no2),color = "black")+
    ggtitle(" NO2 Jahresmittel Verkehrsemissionen/Immissionen",
            subtitle = "Emissionen Proz von 2000 (schwarz)
2 verkehrsnah, 4 städt. HIntergrund, 3 ländl. Hintergrund")+
    labs ( y = " NO2 μg/m3 bzw % von 2000")
# Import Emissionsdaten Stgt_Emissionen_NO2_StrVerkehr_95_18
  emi_path <- "~/documents/Luftqualitaet/Daten/Stuttgart/Stgt_Emissionen_StrVerkehr/"
  Stgt_Emissionen <- read.csv2(file.path(emi_path,
  "Stgt_Emissionen_NO2_StrVerkehr_95_18.csv"))
  ggplot(Stgt_Emissionen,aes(x = Jahr, y = Gesamt))+
    geom_point()+
    geom_point(aes(x = Jahr, y = Diesel.Pkw+Otto.Pkw),col = "red")
Stgt_Emissionen_Klasse <- Stgt_Emissionen[,-1] %>%
  pivot_longer(cols = -Jahr, names_to = "Fzg_typ", values_to = "Tonnen")
ggplot(Stgt_Emissionen_Klasse, aes (x = Jahr))+
  geom_smooth(method= "lm",aes(y = Tonnen,col = Fzg_typ))+
  geom_point(aes(y = Tonnen,col = Fzg_typ))


