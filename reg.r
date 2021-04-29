library(tidyverse)
library(lubridate)

games <- read.csv("games.csv")

game_rank <- read.csv("testOfWinPCTs.csv")


games <- games %>% filter(SEASON == 2017 | SEASON == 2018) %>% 
  mutate(time = ymd(GAME_DATE_EST))



games <- games %>% 
  add_column(WIN_PCT_home = game_rank$winPCT_home, 
             WIN_PCT_away = game_rank$winPCT_away)

head(test$home_rank)
head(test$away_rank)
