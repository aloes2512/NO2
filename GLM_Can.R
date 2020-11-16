#Generalized linear Regression BOOK "Generalized.....
library(tidyverse)
library(readxl)
library(lubridate)
library(broom)
load("~/documents/Luftqualitaet/Daten/BW/BW.RData") # BW.all_data
summary(BW.all_data)
# ========================================================
# Can Daten in tbl zusammenfassen
BW.all_data$Stg.Can_data %>% summary()
Can_tbl <- inner_join(BW.all_data[["Stg.Can_data"]][["Stg.Can.no2"]],BW.all_data$Stg.Can_data$Stg.Can.no)
Can_tbl <- inner_join(Can_tbl,BW.all_data$Stg.Can_data$Stg.Can.temp)
Can_data_tbl <- Can_tbl %>% mutate(Heizb = ifelse((20-Temp) >0, 20-Temp,0))
Can_data_tbl%>% filter (Temp < 15) %>% NROW(.) # 114020
prozHzg <-NROW(Can_data_tbl%>% filter(Temp<15))/Can_data_tbl%>%  NROW(.)
WW_grdz <- 20 -mean(Can_data_tbl$Temp,na.rm=TRUE)*prozHzg
Can_data_tbl <- Can_tbl %>% mutate(Heizb = ifelse((20-Temp) >0, 20-Temp+WW_grdz,WW_grdz))
Can_data_tbl %>% summary()
# Lineares Regressionsmodel
Can_lmodel_15_19 <- Can_data_tbl%>% filter (datetime >= ymd("2015-01-01"))%>%
  lm(formula = ( NO2 ~ 1+ datetime+ NO+Heizb),na.action =na.exclude )
Can_lin_parms <-tidy(Can_lmodel_15_19)
names(Can_lmodel_15_19) #[1] "coefficients"  "residuals"     "effects"       "rank"          "fitted.values" "assign"
# [7] "qr"            "df.residual"   "xlevels"       "call"          "terms"         "model"
Can_data_tbl %>% filter(datetime >= ymd("2015-01-01")) %>%
  ggplot(aes(x =datetime,y=NO2))+
  geom_line(mapping =aes(y = predict(Can_lmodel_15_19)),alpha = 0.1 )+
  geom_smooth(mapping =aes(y = predict(Can_lmodel_15_19)),col ="red")+
  ggtitle("NO2 - Immissionen
   (lineares Modell) geglättete Darstellung ",
          subtitle = "Bad Cannstatt")+
  labs( x ="", y = "NO2[μg/m3]")
