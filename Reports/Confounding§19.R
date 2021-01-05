# Confounding Tutorial Introduction to Data Science
#Rafael A. Irizarry
library(Lahman)
library(tidyverse)
# 3 Beziehungen zwischen unterschielichen Parametrn und R_per_game
Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>% 
  ggplot(aes(HR_per_game, R_per_game)) + geom_point(alpha = 0.5)
Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>% 
  ggplot(aes(SB_per_game, R_per_game)) + geom_point(alpha = 0.5)
Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  ggplot(aes(BB_per_game, R_per_game)) + geom_point(alpha = 0.5)
# Normalform transformieren
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(z_HR = round((HR - mean(HR))/sd(HR)),R_per_game = R/G) %>% 
  filter(z_HR %in% -2:3) %>%
  ggplot() + 
  stat_qq(aes(sample=R_per_game)) + 
  facet_wrap(~z_HR)
