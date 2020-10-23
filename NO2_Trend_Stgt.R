# Model für Stuttgart
library(tidyverse)
library(lubridate)
library(modelr)
load("~/documents/Luftqualitaet/Analysen/Stuttgart/STGT.RData")
head(STGT_NO2)
summary(STGT_NO2)
# Daten auf volle Kalenderjahre begrenzen
STGT_NO2_y_1 <- STGT_NO2 %>% filter (Station == 76361&
                                     datetime >= ymd("2004-01-01")&
                                    datetime <= ymd("2019-12-31"))
STGT_NO2_y_2<- STGT_NO2 %>% filter (Station == 9999136&
                                      datetime >= ymd("2016-01-01")&
                                      datetime <= ymd("2019-12-31"))
STGT_NO2_y_3 <- STGT_NO2 %>% filter (Station == 76362 &
                                       datetime >= ymd("2016-01-01")&
                                       datetime <= ymd("2019-12-31"))
STGT_NO2_y_4 <- STGT_NO2 %>% filter (Station == 55006 &
                                       datetime >= ymd("2006-01-01")&
                                       datetime <= ymd("2019-12-31"))
STGT_NO2_y_5 <- STGT_NO2 %>% filter (Station == 4449 &
                                       datetime <= ymd("2010-12-31")&
                                       datetime >= ymd("2000-01-01"))
STGT_NO2_y_6 <- STGT_NO2 %>% filter (Station == 4452 &
                                       datetime <= ymd("2019-12-31")&
                                       datetime >= ymd("2000-01-01"))
STGT_NO2_y_7 <- STGT_NO2 %>% filter (Station == 9999137 &
                                       datetime <= ymd("2019-12-31")&
                                       datetime >= ymd("2017-01-01"))

STGT_NO2_y <- bind_rows(STGT_NO2_y_1,STGT_NO2_y_2,STGT_NO2_y_3,
                        STGT_NO2_y_4,STGT_NO2_y_5,STGT_NO2_y_6,
                        STGT_NO2_y_7)
STGT_NO2_y$Messstelle <- STGT_NO2_y$Messstelle%>% as_factor()
STGT_NO2_y <- STGT_NO2_y %>% mutate( year = format(datetime,"%Y") %>% as.numeric())
STGT_NO2_y <- STGT_NO2_y %>% group_by(Station,Messstelle,year) %>%
  summarise( NO2 = mean(NO2,na.rm = TRUE))
save(STGT_NO2_y,file = "STGT_NO2_y.RData")
STGT_NO2_y_fig <- STGT_NO2_y %>% ggplot(aes(x = year, y = NO2,group = Messstelle))+
  geom_point(aes (col = Messstelle))+
  geom_line(col = "black")+
  geom_smooth(method = "lm",aes(col= Messstelle), se = FALSE)
ggsave(STGT_NO2_y_fig , filename = "Abbildungen/STGT_NO2.png")
# ===========================
by_Messstelle_y <- STGT_NO2_y %>%
  group_by(Station,Messstelle) %>% nest()
by_Messstelle_y$data %>% summary()
# Mittelwerte pro Kalenderjahr
by_Messstelle_y$data %>% head()

by_Messstelle_y$Messstelle <- by_Messstelle_y$Messstelle %>% as_factor()
NO2_Jahre_mittel_Stgt <-by_Messstelle_y %>% unnest(data)%>% ggplot(aes(x = year, y = NO2))+
  geom_point(aes(shape = Messstelle))+
  geom_line(aes(col= Messstelle))+
  ggtitle (" NO2 Jahresmittelwerte
           Stuttgart ")+
  labs ( x="", y = "NO2 [μg/m3]")
ggsave(NO2_Jahre_mittel_Stgt,filename = "Abbildungen/NO2_Jahre_mittel.png")
station_y_model <- function(df) {
  lm(NO2 ~ year, data = df)
}
by_Messstelle_y <- by_Messstelle_y %>%
  mutate(model = purrr::map(data,station_y_model))
by_Messstelle_y <-by_Messstelle_y %>%
  mutate (resid = map2(data,model,add_residuals))
by_Messstelle_y %>% head()
#turn back to tibble
resids <- by_Messstelle_y %>% unnest(resid)
no2.models <- by_Messstelle_y %>% unnest(model)
ggplot(resids,aes(x = year, y = resid))+
  #geom_line(aes(col = Messstelle))+
  geom_point(aes(shape = Messstelle))+
  geom_smooth(aes(col = Messstelle),se = FALSE)
# ======= Stundenwerte (datetime) =========
by_Messstelle <- STGT_NO2 %>%
  group_by(Station,Messstelle) %>% nest()
station_model <- function(df) {
  lm(NO2~datetime,data = df)
}

by_Messstelle <- by_Messstelle %>%
  mutate(model = purrr::map(data,station_model))
by_Messstelle %>% head()
by_Messstelle <-by_Messstelle %>%
  mutate (resid = map2(data,model,add_residuals))
  # turn back to tibble
resids0 <- unnest(by_Messstelle,resid)
NO2_Abweichungen_Stgt <- resids0 %>% ggplot(aes(x = datetime, y = resid))+
  geom_smooth(aes( col = Messstelle),se = FALSE)+
  ggtitle("NO2 Abweichungen von Regressionsgeraden
          Stuttgart",subtitle = "Basis 1-h Werte")

ggsave(NO2_Abweichungen_Stgt,filename = "Abbildungen/NO2_Abweichungen.png")
head(resids0)
