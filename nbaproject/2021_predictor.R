library(tidyverse)
library(ggplot2)
library(cowplot)
library("readxl")
library(caret)

nba_data <- read_excel("nbadata.xlsx")

nba_data <- nba_data %>%
  mutate(madeSecondRound = case_when(PlayoffWins >= 4 ~ 1,
                                     TRUE ~ 0 ))
nba_data <- nba_data %>%
  mutate(madeConfFinals = case_when(PlayoffWins >= 8 ~ 1,
                                    TRUE ~ 0))
nba_data <- nba_data %>%
  mutate(madeFinals = case_when(PlayoffWins >= 12 ~ 1,
                                TRUE ~ 0))
nba_data <- nba_data %>%
  mutate(champion = case_when(PlayoffWins >= 15 ~ 1,
                              TRUE ~ 0))

predicting <- filter(nba_data, Season != "2022-23")
predicting <- filter(predicting, Season != "2021-22")
predicting <- filter(predicting, Season != "1999-2000")
predicting <- filter(predicting, Season != "2000-01")
predicting <- filter(predicting, Season != "2001-02")
predicting <- filter(predicting, Season != "2002-03")
predicting <- filter(predicting, Season != "2003-04")
predicting <- filter(predicting, Season != "2004-05")
predicting <- filter(predicting, Season != "2005-06")
predicting <- filter(predicting, Season != "2006-07")
predicting <- filter(predicting, Season != "2007-08")
predicting <- filter(predicting, Season != "2008-09")
predicting <- filter(predicting, Season != "2009-10")
predicting <- filter(predicting, Season != "2010-11")


ctrl <- trainControl(method = "cv", number = 5)

model2 <- train(MadePlayoffs ~ AdjustedNet, data = predicting, method = "glm", trControl = ctrl)
print(model2)
model <- train(madeSecondRound ~ AdjustedNet, data = predicting, method = "glm", trControl = ctrl)
print(model)
model5 <- train(madeConfFinals ~ AdjustedNet, data = predicting, method = "glm", trControl = ctrl)
print(model5)
model3 <- train(madeFinals ~ AdjustedNet, data = predicting, method = "glm", trControl = ctrl)
print(model3)
model4 <- train(champion ~ AdjustedNet, data = predicting, method = "glm", trControl = ctrl)
print(model4)

predictionChampion <- glm(champion ~ AdjustedNet, data = predicting, family = "binomial")
summary(predictionChampion)
champOdds2021 <- function(adjustedNet) {
  return(1/(1 + exp(-(-5.8213+(65.5985*adjustedNet)))))
}

predictionFinals <- glm(madeFinals ~ AdjustedNet, data = predicting, family = "binomial")
summary(predictionFinals)
finalsOdds2021 <- function(adjustedNet) {
  return(1/(1 + exp(-(-4.0532+(48.4436*adjustedNet)))))
}
predictionConfFinals <- glm(madeConfFinals ~ AdjustedNet, data = predicting, family = "binomial")
summary(predictionConfFinals)
confFinalsOdds2021 <- function(adjustedNet) {
  return(1/(1 + exp(-(-3.3315+(55.1309*adjustedNet)))))
}
predictionSecondRound <-glm(madeSecondRound ~ AdjustedNet, data = predicting, family = "binomial")
summary(predictionSecondRound)
secondRoundOdds2021 <- function(adjustedNet) {
  return(1/(1 + exp(-(-2.3970+(73.8834*adjustedNet)))))
}

predicting <- predicting %>%
  mutate(championOdds = ifelse(Season == "2020-21", champOdds2021(AdjustedNet), 0))
predicting <- predicting %>%
  mutate(finalOdds = ifelse(Season == "2020-21", finalsOdds2021(AdjustedNet), 0))
predicting <- predicting %>%
  mutate(confFinalOdds = ifelse(Season == "2020-21", confFinalsOdds2021(AdjustedNet), 0))
predicting <- predicting %>%
  mutate(secondRoundOdds = ifelse(Season == "2020-21", secondRoundOdds2021(AdjustedNet), 0))

predicting$championOdds <- paste(formatC(predicting$championOdds * 100, format = "f", digits = 2), "%",sep="")
predicting$finalOdds <- paste(formatC(predicting$finalOdds * 100, format = "f", digits = 2), "%",sep="")
predicting$CARMELO2017 <- paste(formatC(predicting$CARMELO2017 * 100, format = "f", digits = 2), "%",sep="")
predicting$confFinalOdds <- paste(formatC(predicting$confFinalOdds * 100, format = "f", digits = 2), "%",sep="")
predicting$secondRoundOdds <- paste(formatC(predicting$secondRoundOdds * 100, format = "f", digits = 2), "%",sep="")

