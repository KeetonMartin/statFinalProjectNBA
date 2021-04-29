library(tidyverse)
library(lubridate)

games <- read.csv("games.csv")

game_rank <- read.csv("testOfWinPCTs.csv")


games <- games %>% filter(SEASON == 2017 | SEASON == 2018) %>% 
  mutate(time = ymd(GAME_DATE_EST))



games <- games %>% 
  add_column(WIN_PCT_home = game_rank$winPCT_home, 
             WIN_PCT_away = game_rank$winPCT_away)

games$PTS_diff = games$PTS_home-games$PTS_away

model1 = lm(games$PTS_diff ~ games$FG_PCT_home); summary(model1)

model3 = lm(games$PTS_diff ~ 
              games$FG_PCT_home+
              games$FG_PCT_away+
              games$AST_home+
              games$AST_away+
              games$REB_home+
              games$REB_away
            ); summary(model3)

plot(model3)

shapiro.test(resid(model3))

model4 = lm(games$PTS_diff ~ 
              games$FG_PCT_home+
              games$FG_PCT_away+
              games$AST_home+
              games$AST_away+
              games$REB_home+
              games$REB_away+
              games$WIN_PCT_home+
              games$WIN_PCT_away
          ); summary(model4)

FullModel = lm(games$PTS_diff ~ 
                 games$FG_PCT_home+
                 games$FG_PCT_away+
                 games$AST_home+
                 games$AST_away+
                 games$REB_home+
                 games$REB_away+
                 games$FG3_PCT_home+
                 games$FG3_PCT_away+
                 games$FT_PCT_home+
                 games$FT_PCT_away+
                 games$WIN_PCT_home+
                 games$WIN_PCT_away
); summary(FullModel)

summary(model3)
summary(model4)
summary(FullModel)

plot(FullModel)

shapiro.test(resid(model4))
shapiro.test(resid(FullModel))

cor(games$WIN_PCT_home, games$FG3_PCT_home)
cor(games$WIN_PCT_home, games$FT_PCT_home)

cor(games$FG3_PCT_away, games$FG_PCT_away)

anova(model3, FullModel)
anova(model4, FullModel)

#New column
games$WIN_PCT_diff = games$WIN_PCT_home - games$WIN_PCT_away
summary(games$WIN_PCT_diff)
hist(games$WIN_PCT_diff)

summary(FullModel)

FullModel2 = lm(games$PTS_diff ~ 
                              games$FG_PCT_home+
                              games$FG_PCT_away+
                              games$AST_home+
                              games$AST_away+
                              games$REB_home+
                              games$REB_away+
                              games$FG3_PCT_home+
                              games$FG3_PCT_away+
                              games$FT_PCT_home+
                              games$FT_PCT_away+
                              games$WIN_PCT_diff
                        ); summary(FullModel2)

shapiro.test(resid(FullModel2))
plot(FullModel2)

#Sticking with FullModel

#transforming anything?
hist(games$PTS_diff)

plot(FullModel)

min(games$PTS_diff)

library(MASS)
library(car)

powerTransform(             cbind(games$FG_PCT_home,
                                  games$FG_PCT_away,
                                  games$AST_home,
                                  games$AST_away,
                                  games$REB_home,
                                  games$REB_away,
                                  games$FG3_PCT_home,
                                  games$FG3_PCT_away,
                                  games$FT_PCT_home,
                                  games$FT_PCT_away) ~1)

#We will try transforming these variables next time / soon 

hist(games$REB_home)

hist(games$FT_PCT_away^2)
