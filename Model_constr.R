# construct model
NO2_stations_grouped <- readRDS("NO2_stations_grouped.rds")
NO2_stations_grouped %>% unnest(data) %>% filter(name =="Sws")
levels(NO2_stations_grouped$name)
H.period.means <- function (grp_df,nm){
  df <- grp_df %>% filter(name == nm) %>% unnest(data)%>%
    group_by(Stunde)%>%
    summarise(NO2_h = mean(NO2,na.rm = TRUE))%>%
    mutate(name = nm,varH = NO2_h- mean(NO2_h))%>%
    arrange(Stunde)
  return(df)
}
WT.period.means <-function (grp_df,nm){
  df <- grp_df %>% filter(name == nm) %>% unnest(data)%>%
    group_by(WTag)%>%
    summarise(NO2_h = mean(NO2,na.rm = TRUE))%>%
    mutate(name = nm,varH = NO2_h- mean(NO2_h))%>%
    arrange(WTag)
  return(df)
}
NO2_Sws_Stunde <- H.period.means(NO2_stations_grouped,"Sws")
NO2_Sws_WTag <- WT.period.means(NO2_stations_grouped,"Sws")

indx <- seq(from =ymd_h("2000-01-01 00"),to =ymd_h("2000-01-31 23"), 
         by = "1 hour")
y_h <- rep_along(x,NO2_Sws_Stunde$varH)
woch_tag <- function(ti){
  w_tag = ti %>% format("%w")%>% as_factor()
}
w_tag <- woch_tag(indx)
w_tag[621] # 3
length(w_tag) #744
head(w_tag)
NO2_Sws_WTag$varH #[1] -1.081 -0.486  0.352  0.520  0.482  0.407 -0.194
wtag_wert <-NO2_Sws_WTag$varH 
y_w <- wtag_wert[w_tag]
y_w.h <- y_w+ y_h
monats.tbl <- tibble(indx , # Datumsindex
                     y_w ,  # Mittelwerte pro Wochentag
                     y_h ,  # Mittelwerte pro Tagesstunde
                     y_w.h)

ggplot(monats.tbl,aes(x=indx ))+
  geom_line(aes(y = y_w))+
  geom_point(aes (y = y_w.h),size = 0.05,col = "red")+
  labs( x ="", y = "NO2[μg/m3] ")+
  ggtitle("NO2 Mittlere Variation",
          subtitle = " Tagesgang(h-Werte)
  Wochengang(Wochentageswerte)")
deparse("NO2_Sws_WTag")%>% 
  str_replace("^.{5}","") %>% str_replace(".{6}$","")
