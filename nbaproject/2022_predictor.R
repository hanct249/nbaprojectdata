library(tidyverse)
library(ggplot2)
library(cowplot)
library("readxl")
library(caret)
library(openxlsx)

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
predicting <- filter(predicting, Season != "2011-12")

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
champOdds2022 <- function(adjustedNet) {
  return(1/(1 + exp(-(-5.7781+(65.8175*adjustedNet)))))
}

predictionFinals <- glm(madeFinals ~ AdjustedNet, data = predicting, family = "binomial")
summary(predictionFinals)
finalsOdds2022 <- function(adjustedNet) {
  return(1/(1 + exp(-(-4.0897+(49.8140*adjustedNet)))))
}
predictionConfFinals <- glm(madeConfFinals ~ AdjustedNet, data = predicting, family = "binomial")
summary(predictionConfFinals)
confFinalsOdds2022 <- function(adjustedNet) {
  return(1/(1 + exp(-(-3.3203+(55.4530*adjustedNet)))))
}
predictionSecondRound <-glm(madeSecondRound ~ AdjustedNet, data = predicting, family = "binomial")
summary(predictionSecondRound)
secondRoundOdds2022 <- function(adjustedNet) {
  return(1/(1 + exp(-(-2.5526+(80.2396*adjustedNet)))))
}

predicting <- predicting %>%
  mutate(championOdds = ifelse(Season == "2021-22", champOdds2022(AdjustedNet), 0))
championOddsTotal <- sum(predicting$championOdds[predicting$Season == "2021-22"])
predicting <- predicting %>%
  mutate(adjustedChampionOdds = ifelse(Season == "2021-22", championOdds/championOddsTotal,0))

predicting <- predicting %>%
  mutate(finalOdds = ifelse(Season == "2021-22", finalsOdds2022(AdjustedNet), 0))
finalOddsTotal <- sum(predicting$finalOdds[predicting$Season == "2021-22"])
predicting <- predicting %>%
  mutate(adjFinalOdds = ifelse(Season == "2021-22", finalOdds/finalOddsTotal,0))

predicting <- predicting %>%
  mutate(confFinalOdds = ifelse(Season == "2021-22", confFinalsOdds2022(AdjustedNet), 0))
predicting <- predicting %>%
  mutate(secondRoundOdds = ifelse(Season == "2021-22", secondRoundOdds2022(AdjustedNet), 0))

predicting$championOdds <- paste(formatC(predicting$championOdds * 100, format = "f", digits = 2), "%",sep="")
predicting$adjustedChampionOdds <- paste(formatC(predicting$adjustedChampionOdds * 100, format = "f", digits = 2), "%",sep="")

predicting$finalOdds <- paste(formatC(predicting$finalOdds * 100, format = "f", digits = 2), "%",sep="")
predicting$adjFinalOdds <- paste(formatC(predicting$adjFinalOdds * 100, format = "f", digits = 2), "%",sep="")

predicting$CARMELO_c <- paste(formatC(predicting$CARMELO_c * 100, format = "f", digits = 2), "%",sep="")
predicting$confFinalOdds <- paste(formatC(predicting$confFinalOdds * 100, format = "f", digits = 2), "%",sep="")
predicting$secondRoundOdds <- paste(formatC(predicting$secondRoundOdds * 100, format = "f", digits = 2), "%",sep="")

write.xlsx(predicting, "output2.xlsx")

