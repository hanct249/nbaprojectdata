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
predicting <- filter(predicting, Season != "2020-21")
predicting <- filter(predicting, Season != "2019-20")
predicting <- filter(predicting, Season != "2018-19")
predicting <- filter(predicting, Season != "1999-2000")
predicting <- filter(predicting, Season != "2000-01")
predicting <- filter(predicting, Season != "2001-02")

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



predictionPlayoffs <- glm(MadePlayoffs ~ AdjustedNet, data = predicting, family = "binomial")
summary(predictionPlayoffs)
predictionFinals <- glm(madeFinals ~ AdjustedNet, data = predicting, family = "binomial")
summary(predictionFinals)
predictionChampion <- glm(champion ~ AdjustedNet, data = predicting, family = "binomial")
summary(predictionChampion)
predictionConfFinals <- glm(madeConfFinals ~ AdjustedNet, data = predicting, family = "binomial")
summary(predictionConfFinals)
predictionSecRound <- glm(madeSecondRound ~ AdjustedNet, data = predicting, family = "binomial")
summary(predictionSecRound)

adjustedNet <- 0

secondRoundOdds <- function(adjustedNet) {
  return(1/(1 + exp(-(-2.1784+(63.7836*adjustedNet)))))
}

confFinalsOdds <- function(adjustedNet) {
  return(1/(1 + exp(-(-3.7599+(63.6872*adjustedNet)))))
}

champOdds <- function(adjustedNet) {
  return(1/(1 + exp(-(-5.5226+(61.1535*adjustedNet)))))
}
finalsOdds <- function(adjustedNet) {
  return(1/(1 + exp(-(-4.2858+(51.7826*adjustedNet)))))
}
playoffodds <- function(adjustedNet) {
  return(1/(1 + exp(-(.3989+(112.1349*adjustedNet)))))
}

Hawks2023<- champOdds(0.0008174818)
Hawks2023
Celtics2023 <- champOdds(0.0586757689)
Celtics2023
Nets2023 <- champOdds(0.0095844898)
Nets2023
Hornets2023 <- champOdds(-0.0552847806)
Hornets2023
Bulls2023 <- champOdds(0.0113387213)
Bulls2023
Cavs2023 <- champOdds(0.0490334896)
Cavs2023
Mavs2023 <- champOdds(-0.0018126871)
Mavs2023
Nuggets2023 <- champOdds(0.0288697658)
Nuggets2023
Pistons2023 <- champOdds(-0.0693120240)
Pistons2023
Warriors2023 <- champOdds(0.0148441617)
Warriors2023
Rockets2023 <- champOdds(-0.0710656408)
Rockets2023
Pacers2023 <- champOdds(-0.0289879065)
Pacers2023
Clippers2023 <- champOdds(0.0034482144)
Clippers2023
Lakers2023 <- champOdds(0.0060782297)
Lakers2023
Grizzlies2023 <- champOdds(0.0350074244)
Grizzlies2023
Heat2023 <- champOdds(-0.0044408069)
Heat2023
Bucks2023 <- champOdds(0.0297477013)
Bucks2023
Twolves2023 <- champOdds(0.0016952636)
Twolves2023
Pelicans2023 <- champOdds(0.0157214824)
Pelicans2023
Knicks2023 <- champOdds(0.0244863899)
Knicks2023
Thunder2023 <- champOdds(0.0087080400)
Thunder2023
Magic2023 <- champOdds(-0.0210967336)
Magic2023
Sixers2023 <- champOdds(0.0376362102)
Sixers2023
Suns2023 <- champOdds(0.0192277426)
Suns2023
Blazers2023 <- champOdds(-0.0351245918)
Blazers2023
Kings2023 <- champOdds(0.0227322608)
Kings2023
Spurs2023 <- champOdds(-0.0868450153)
Spurs2023
Raptors2023 <- champOdds(0.0130911085)
Raptors2023
Jazz2023 <- champOdds(-0.0061956532)
Jazz2023
Wizards2023 <- champOdds(-0.0105781069)
Wizards2023

Hawks2022<- champOdds(0.0150518185)
Hawks2022
Celtics2022 <- champOdds(0.0662405011)
Celtics2022
Nets2022 <- champOdds(0.0078719899)
Nets2022
Hornets2022 <- champOdds(0.0042792942)
Hornets2022
Bulls2022 <- champOdds(-0.0046989220)
Bulls2022
Cavs2022 <- champOdds(0.0186519315)
Cavs2022
Mavs2022 <- champOdds(0.0303230006)
Mavs2022
Nuggets2022 <- champOdds(0.0204427164)
Nuggets2022
Pistons2022 <- champOdds(-0.0693409289)
Pistons2022
Warriors2022 <- champOdds(0.0491815009)
Warriors2022
Rockets2022 <- champOdds(-0.0747334957)
Rockets2022
Pacers2022 <- champOdds(-0.0325350822)
Pacers2022
Clippers2022 <- champOdds(-0.0002030457)
Clippers2022
Lakers2022 <- champOdds(-0.0253476510)
Lakers2022
Grizzlies2022 <- champOdds(0.0482794330)
Grizzlies2022
Heat2022 <- champOdds(0.0410979355)
Heat2022
Bucks2022 <- champOdds(0.0285236858)
Bucks2022
Twolves2022 <- champOdds(0.0249326590)
Twolves2022
Pelicans2022 <- champOdds(-0.0073901061)
Pelicans2022
Knicks2022 <- champOdds(-0.0046933591)
Knicks2022
Thunder2022 <- champOdds(-0.0711335682)
Thunder2022
Magic2022 <- champOdds(-0.0738277191)
Magic2022
Sixers2022 <- champOdds(0.0249341424)
Sixers2022
Suns2022 <- champOdds(0.0662393885)
Suns2022
Blazers2022 <- champOdds(-0.0810179318)
Blazers2022
Kings2022 <- champOdds(-0.0468986334)
Kings2022
Spurs2022 <- champOdds(0.0015884810)
Spurs2022
Raptors2022 <- champOdds(0.0195478802)
Raptors2022
Jazz2022 <- champOdds(0.0554598178)
Jazz2022
Wizards2022 <- champOdds(-0.0307359529)
Wizards2022

Hawks2021<- champOdds(0.0195405952)
Hawks2021
Celtics2021 <- champOdds(0.0114846452)
Celtics2021
Nets2021 <- champOdds(0.0374422521)
Nets2021
Hornets2021 <- champOdds(-0.0171604582)
Hornets2021
Bulls2021 <- champOdds(-0.0099985693)
Bulls2021
Cavs2021 <- champOdds(-0.0744521637)
Cavs2021
Mavs2021 <- champOdds(0.0204354737)
Mavs2021
Nuggets2021 <- champOdds(0.0428153378)
Nuggets2021
Pistons2021 <- champOdds(-0.0413298068)
Pistons2021
Warriors2021 <- champOdds(0.0096976131)
Warriors2021
Rockets2021 <- champOdds(-0.0663970312)
Rockets2021
Pacers2021 <- champOdds(-0.0001524536)
Pacers2021
Clippers2021 <- champOdds(0.0544535265)
Clippers2021
Lakers2021 <- champOdds(0.0267090237)
Lakers2021
Grizzlies2021 <- champOdds(0.0105912654)
Grizzlies2021
Heat2021 <- champOdds(-0.0010459697)
Heat2021
Bucks2021 <- champOdds(0.0517679374)
Bucks2021
Twolves2021 <- champOdds(-0.0467038462)
Twolves2021
Pelicans2021 <- champOdds(-0.0028398139)
Pelicans2021
Knicks2021 <- champOdds(0.0213367555)
Knicks2021
Thunder2021 <- champOdds(-0.0950404997)
Thunder2021
Magic2021 <- champOdds(-0.0834042184)
Magic2021
Sixers2021 <- champOdds(0.0490875254)
Sixers2021
Suns2021 <- champOdds(0.0526634971)
Suns2021
Blazers2021 <- champOdds(0.0159556316)
Blazers2021
Kings2021 <- champOdds(-0.0341744574)
Kings2021
Spurs2021 <- champOdds(-0.0135798543)
Spurs2021
Raptors2021 <- champOdds(-0.0037331937)
Raptors2021
Jazz2021 <- champOdds(0.0804171281)
Jazz2021
Wizards2021 <- champOdds(-0.0144754140)
Wizards2021

Hawks2020<- champOdds(-0.0656778372)
Hawks2020
Celtics2020 <- champOdds(0.0587760135)
Celtics2020
Nets2020 <- champOdds(-0.0030138364)
Nets2020
Hornets2020 <- champOdds(-0.0611541317)
Hornets2020
Bulls2020 <- champOdds(-0.0266580632)
Bulls2020
Cavs2020 <- champOdds(-0.0702183815)
Cavs2020
Mavs2020 <- champOdds(0.0442942622)
Mavs2020
Nuggets2020 <- champOdds(0.0215522508)
Nuggets2020
Pistons2020 <- champOdds(-0.0302472793)
Pistons2020
Warriors2020 <- champOdds(-0.0766082595)
Warriors2020
Rockets2020 <- champOdds(0.0260899886)
Rockets2020
Pacers2020 <- champOdds(0.0196930944)
Pacers2020
Clippers2020 <- champOdds(0.0596908579)
Clippers2020
Lakers2020 <- champOdds(0.0524057806)
Lakers2020
Grizzlies2020 <- champOdds(-0.0075529774)
Grizzlies2020
Heat2020 <- champOdds(0.0251737410)
Heat2020
Bucks2020 <- champOdds(0.0869060588)
Bucks2020
Twolves2020 <- champOdds(-0.0348032590)
Twolves2020
Pelicans2020 <- champOdds(-0.0102512040)
Pelicans2020
Knicks2020 <- champOdds(-0.0575228189)
Knicks2020
Thunder2020 <- champOdds(0.0197015138)
Thunder2020
Magic2020 <- champOdds(-0.0102876878)
Magic2020
Sixers2020 <- champOdds(0.0224334177)
Sixers2020
Suns2020 <- champOdds(0.0061009294)
Suns2020
Blazers2020 <- champOdds(-0.0083976606)
Blazers2020
Kings2020 <- champOdds(-0.0157122055)
Kings2020
Spurs2020 <- champOdds(-0.0066030525)
Spurs2020
Raptors2020 <- champOdds(0.0569322926)
Raptors2020
Jazz2020 <- champOdds(0.0242645096)
Jazz2020
Wizards2020 <- champOdds(-0.0393059162)
Wizards2020

Hawks2019<- champOdds(-5.094507e-02)
Hawks2019
Celtics2019<- champOdds(3.927954e-02)
Celtics2019
Nets2019 <- champOdds(-8.208761e-04)
Nets2019
Hornets2019 <- champOdds(-1.175531e-02)
Hornets2019
Bulls2019 <- champOdds(-7.555341e-02)
Bulls2019
Cavs2019 <- champOdds(-9.104407e-02)
Cavs2019
Mavs2019 <- champOdds(-1.266830e-02)
Mavs2019
Nuggets2019 <- champOdds(3.654622e-02)
Nuggets2019
Pistons2019 <- champOdds(-2.643947e-03)
Pistons2019
Warriors2019 <- champOdds(5.842050e-02)
Warriors2019
Rockets2019 <- champOdds(4.383917e-02)
Rockets2019
Pacers2019 <- champOdds(3.107601e-02)
Pacers2019
Clippers2019 <- champOdds(9.205806e-03)
Clippers2019
Lakers2019 <- champOdds(-1.358071e-02)
Lakers2019
Grizzlies2019 <- champOdds(-2.178416e-02)
Grizzlies2019
Heat2019 <- champOdds(-3.556687e-03)
Heat2019
Bucks2019 <- champOdds(7.846846e-02)
Bucks2019
Twolves2019 <- champOdds(-1.448938e-02)
Twolves2019
Pelicans2019 <- champOdds(-1.175531e-02)
Pelicans2019
Knicks2019 <- champOdds(-8.102179e-02)
Knicks2019
Thunder2019 <- champOdds(3.016502e-02)
Thunder2019
Magic2019 <- champOdds(5.557837e-03)
Magic2019
Sixers2019 <- champOdds(2.378722e-02)
Sixers2019
Suns2019 <- champOdds(-8.102071e-02)
Suns2019
Blazers2019 <- champOdds(3.837021e-02)
Blazers2019
Kings2019 <- champOdds(-1.084490e-02)
Kings2019
Spurs2019 <- champOdds(1.467444e-02)
Spurs2019
Raptors2019 <- champOdds(5.295054e-02)
Raptors2019
Jazz2019 <- champOdds(4.565800e-02)
Jazz2019
Wizards2019 <- champOdds(-2.451432e-02)
Wizards2019

Hawks2022<- finalsOdds(0.0150518185)
Hawks2022
Celtics2022 <- finalsOdds(0.0662405011)
Celtics2022
Nets2022 <- finalsOdds(0.0078719899)
Nets2022
Hornets2022 <- finalsOdds(0.0042792942)
Hornets2022
Bulls2022 <- finalsOdds(-0.0046989220)
Bulls2022
Cavs2022 <- finalsOdds(0.0186519315)
Cavs2022
Mavs2022 <- finalsOdds(0.0303230006)
Mavs2022
Nuggets2022 <- finalsOdds(0.0204427164)
Nuggets2022
Pistons2022 <- finalsOdds(-0.0693409289)
Pistons2022
Warriors2022 <- finalsOdds(0.0491815009)
Warriors2022
Rockets2022 <- finalsOdds(-0.0747334957)
Rockets2022
Pacers2022 <- finalsOdds(-0.0325350822)
Pacers2022
Clippers2022 <- finalsOdds(-0.0002030457)
Clippers2022
Lakers2022 <- finalsOdds(-0.0253476510)
Lakers2022
Grizzlies2022 <- finalsOdds(0.0482794330)
Grizzlies2022
Heat2022 <- finalsOdds(0.0410979355)
Heat2022
Bucks2022 <- finalsOdds(0.0285236858)
Bucks2022
Twolves2022 <- finalsOdds(0.0249326590)
Twolves2022
Pelicans2022 <- finalsOdds(-0.0073901061)
Pelicans2022
Knicks2022 <- finalsOdds(-0.0046933591)
Knicks2022
Thunder2022 <- finalsOdds(-0.0711335682)
Thunder2022
Magic2022 <- finalsOdds(-0.0738277191)
Magic2022
Sixers2022 <- finalsOdds(0.0249341424)
Sixers2022
Suns2022 <- finalsOdds(0.0662393885)
Suns2022
Blazers2022 <- finalsOdds(-0.0810179318)
Blazers2022
Kings2022 <- finalsOdds(-0.0468986334)
Kings2022
Spurs2022 <- finalsOdds(0.0015884810)
Spurs2022
Raptors2022 <- finalsOdds(0.0195478802)
Raptors2022
Jazz2022 <- finalsOdds(0.0554598178)
Jazz2022
Wizards2022 <- finalsOdds(-0.0307359529)
Wizards2022

Hawks2021<- finalsOdds(0.0195405952)
Hawks2021
Celtics2021 <- finalsOdds(0.0114846452)
Celtics2021
Nets2021 <- finalsOdds(0.0374422521)
Nets2021
Hornets2021 <- finalsOdds(-0.0171604582)
Hornets2021
Bulls2021 <- finalsOdds(-0.0099985693)
Bulls2021
Cavs2021 <- finalsOdds(-0.0744521637)
Cavs2021
Mavs2021 <- finalsOdds(0.0204354737)
Mavs2021
Nuggets2021 <- finalsOdds(0.0428153378)
Nuggets2021
Pistons2021 <- finalsOdds(-0.0413298068)
Pistons2021
Warriors2021 <- finalsOdds(0.0096976131)
Warriors2021
Rockets2021 <- finalsOdds(-0.0663970312)
Rockets2021
Pacers2021 <- finalsOdds(-0.0001524536)
Pacers2021
Clippers2021 <- finalsOdds(0.0544535265)
Clippers2021
Lakers2021 <- finalsOdds(0.0267090237)
Lakers2021
Grizzlies2021 <- finalsOdds(0.0105912654)
Grizzlies2021
Heat2021 <- finalsOdds(-0.0010459697)
Heat2021
Bucks2021 <- finalsOdds(0.0517679374)
Bucks2021
Twolves2021 <- finalsOdds(-0.0467038462)
Twolves2021
Pelicans2021 <- finalsOdds(-0.0028398139)
Pelicans2021
Knicks2021 <- finalsOdds(0.0213367555)
Knicks2021
Thunder2021 <- finalsOdds(-0.0950404997)
Thunder2021
Magic2021 <- finalsOdds(-0.0834042184)
Magic2021
Sixers2021 <- finalsOdds(0.0490875254)
Sixers2021
Suns2021 <- finalsOdds(0.0526634971)
Suns2021
Blazers2021 <- finalsOdds(0.0159556316)
Blazers2021
Kings2021 <- finalsOdds(-0.0341744574)
Kings2021
Spurs2021 <- finalsOdds(-0.0135798543)
Spurs2021
Raptors2021 <- finalsOdds(-0.0037331937)
Raptors2021
Jazz2021 <- finalsOdds(0.0804171281)
Jazz2021
Wizards2021 <- finalsOdds(-0.0144754140)
Wizards2021

Hawks2020<- finalsOdds(-0.0656778372)
Hawks2020
Celtics2020 <- finalsOdds(0.0587760135)
Celtics2020
Nets2020 <- finalsOdds(-0.0030138364)
Nets2020
Hornets2020 <- finalsOdds(-0.0611541317)
Hornets2020
Bulls2020 <- finalsOdds(-0.0266580632)
Bulls2020
Cavs2020 <- finalsOdds(-0.0702183815)
Cavs2020
Mavs2020 <- finalsOdds(0.0442942622)
Mavs2020
Nuggets2020 <- finalsOdds(0.0215522508)
Nuggets2020
Pistons2020 <- finalsOdds(-0.0302472793)
Pistons2020
Warriors2020 <- finalsOdds(-0.0766082595)
Warriors2020
Rockets2020 <- finalsOdds(0.0260899886)
Rockets2020
Pacers2020 <- finalsOdds(0.0196930944)
Pacers2020
Clippers2020 <- finalsOdds(0.0596908579)
Clippers2020
Lakers2020 <- finalsOdds(0.0524057806)
Lakers2020
Grizzlies2020 <- finalsOdds(-0.0075529774)
Grizzlies2020
Heat2020 <- finalsOdds(0.0251737410)
Heat2020
Bucks2020 <- finalsOdds(0.0869060588)
Bucks2020
Twolves2020 <- finalsOdds(-0.0348032590)
Twolves2020
Pelicans2020 <- finalsOdds(-0.0102512040)
Pelicans2020
Knicks2020 <- finalsOdds(-0.0575228189)
Knicks2020
Thunder2020 <- finalsOdds(0.0197015138)
Thunder2020
Magic2020 <- finalsOdds(-0.0102876878)
Magic2020
Sixers2020 <- finalsOdds(0.0224334177)
Sixers2020
Suns2020 <- finalsOdds(0.0061009294)
Suns2020
Blazers2020 <- finalsOdds(-0.0083976606)
Blazers2020
Kings2020 <- finalsOdds(-0.0157122055)
Kings2020
Spurs2020 <- finalsOdds(-0.0066030525)
Spurs2020
Raptors2020 <- finalsOdds(0.0569322926)
Raptors2020
Jazz2020 <- finalsOdds(0.0242645096)
Jazz2020
Wizards2020 <- finalsOdds(-0.0393059162)
Wizards2020

Hawks2019<- finalsOdds(-5.094507e-02)
Hawks2019
Celtics2019<- finalsOdds(3.927954e-02)
Celtics2019
Nets2019 <- finalsOdds(-8.208761e-04)
Nets2019
Hornets2019 <- finalsOdds(-1.175531e-02)
Hornets2019
Bulls2019 <- finalsOdds(-7.555341e-02)
Bulls2019
Cavs2019 <- finalsOdds(-9.104407e-02)
Cavs2019
Mavs2019 <- finalsOdds(-1.266830e-02)
Mavs2019
Nuggets2019 <- finalsOdds(3.654622e-02)
Nuggets2019
Pistons2019 <- finalsOdds(-2.643947e-03)
Pistons2019
Warriors2019 <- finalsOdds(5.842050e-02)
Warriors2019
Rockets2019 <- finalsOdds(4.383917e-02)
Rockets2019
Pacers2019 <- finalsOdds(3.107601e-02)
Pacers2019
Clippers2019 <- finalsOdds(9.205806e-03)
Clippers2019
Lakers2019 <- finalsOdds(-1.358071e-02)
Lakers2019
Grizzlies2019 <- finalsOdds(-2.178416e-02)
Grizzlies2019
Heat2019 <- finalsOdds(-3.556687e-03)
Heat2019
Bucks2019 <- finalsOdds(7.846846e-02)
Bucks2019
Twolves2019 <- finalsOdds(-1.448938e-02)
Twolves2019
Pelicans2019 <- finalsOdds(-1.175531e-02)
Pelicans2019
Knicks2019 <- finalsOdds(-8.102179e-02)
Knicks2019
Thunder2019 <- finalsOdds(3.016502e-02)
Thunder2019
Magic2019 <- finalsOdds(5.557837e-03)
Magic2019
Sixers2019 <- finalsOdds(2.378722e-02)
Sixers2019
Suns2019 <- finalsOdds(-8.102071e-02)
Suns2019
Blazers2019 <- finalsOdds(3.837021e-02)
Blazers2019
Kings2019 <- finalsOdds(-1.084490e-02)
Kings2019
Spurs2019 <- finalsOdds(1.467444e-02)
Spurs2019
Raptors2019 <- finalsOdds(5.295054e-02)
Raptors2019
Jazz2019 <- finalsOdds(4.565800e-02)
Jazz2019
Wizards2019 <- finalsOdds(-2.451432e-02)
Wizards2019

Hawks2022<- confFinalsOdds(0.0150518185)
Hawks2022
Celtics2022 <- confFinalsOdds(0.0662405011)
Celtics2022
Nets2022 <- confFinalsOdds(0.0078719899)
Nets2022
Hornets2022 <- confFinalsOdds(0.0042792942)
Hornets2022
Bulls2022 <- confFinalsOdds(-0.0046989220)
Bulls2022
Cavs2022 <- confFinalsOdds(0.0186519315)
Cavs2022
Mavs2022 <- confFinalsOdds(0.0303230006)
Mavs2022
Nuggets2022 <- confFinalsOdds(0.0204427164)
Nuggets2022
Pistons2022 <- confFinalsOdds(-0.0693409289)
Pistons2022
Warriors2022 <- confFinalsOdds(0.0491815009)
Warriors2022
Rockets2022 <- confFinalsOdds(-0.0747334957)
Rockets2022
Pacers2022 <- confFinalsOdds(-0.0325350822)
Pacers2022
Clippers2022 <- confFinalsOdds(-0.0002030457)
Clippers2022
Lakers2022 <- confFinalsOdds(-0.0253476510)
Lakers2022
Grizzlies2022 <- confFinalsOdds(0.0482794330)
Grizzlies2022
Heat2022 <- confFinalsOdds(0.0410979355)
Heat2022
Bucks2022 <- confFinalsOdds(0.0285236858)
Bucks2022
Twolves2022 <- confFinalsOdds(0.0249326590)
Twolves2022
Pelicans2022 <- confFinalsOdds(-0.0073901061)
Pelicans2022
Knicks2022 <- confFinalsOdds(-0.0046933591)
Knicks2022
Thunder2022 <- confFinalsOdds(-0.0711335682)
Thunder2022
Magic2022 <- confFinalsOdds(-0.0738277191)
Magic2022
Sixers2022 <- confFinalsOdds(0.0249341424)
Sixers2022
Suns2022 <- confFinalsOdds(0.0662393885)
Suns2022
Blazers2022 <- confFinalsOdds(-0.0810179318)
Blazers2022
Kings2022 <- confFinalsOdds(-0.0468986334)
Kings2022
Spurs2022 <- confFinalsOdds(0.0015884810)
Spurs2022
Raptors2022 <- confFinalsOdds(0.0195478802)
Raptors2022
Jazz2022 <- confFinalsOdds(0.0554598178)
Jazz2022
Wizards2022 <- confFinalsOdds(-0.0307359529)
Wizards2022

Hawks2021<- confFinalsOdds(0.0195405952)
Hawks2021
Celtics2021 <- confFinalsOdds(0.0114846452)
Celtics2021
Nets2021 <- confFinalsOdds(0.0374422521)
Nets2021
Hornets2021 <- confFinalsOdds(-0.0171604582)
Hornets2021
Bulls2021 <- confFinalsOdds(-0.0099985693)
Bulls2021
Cavs2021 <- confFinalsOdds(-0.0744521637)
Cavs2021
Mavs2021 <- confFinalsOdds(0.0204354737)
Mavs2021
Nuggets2021 <- confFinalsOdds(0.0428153378)
Nuggets2021
Pistons2021 <- confFinalsOdds(-0.0413298068)
Pistons2021
Warriors2021 <- confFinalsOdds(0.0096976131)
Warriors2021
Rockets2021 <- confFinalsOdds(-0.0663970312)
Rockets2021
Pacers2021 <- confFinalsOdds(-0.0001524536)
Pacers2021
Clippers2021 <- confFinalsOdds(0.0544535265)
Clippers2021
Lakers2021 <- confFinalsOdds(0.0267090237)
Lakers2021
Grizzlies2021 <- confFinalsOdds(0.0105912654)
Grizzlies2021
Heat2021 <- confFinalsOdds(-0.0010459697)
Heat2021
Bucks2021 <- confFinalsOdds(0.0517679374)
Bucks2021
Twolves2021 <- confFinalsOdds(-0.0467038462)
Twolves2021
Pelicans2021 <- confFinalsOdds(-0.0028398139)
Pelicans2021
Knicks2021 <- confFinalsOdds(0.0213367555)
Knicks2021
Thunder2021 <- confFinalsOdds(-0.0950404997)
Thunder2021
Magic2021 <- confFinalsOdds(-0.0834042184)
Magic2021
Sixers2021 <- confFinalsOdds(0.0490875254)
Sixers2021
Suns2021 <- confFinalsOdds(0.0526634971)
Suns2021
Blazers2021 <- confFinalsOdds(0.0159556316)
Blazers2021
Kings2021 <- confFinalsOdds(-0.0341744574)
Kings2021
Spurs2021 <- confFinalsOdds(-0.0135798543)
Spurs2021
Raptors2021 <- confFinalsOdds(-0.0037331937)
Raptors2021
Jazz2021 <- confFinalsOdds(0.0804171281)
Jazz2021
Wizards2021 <- confFinalsOdds(-0.0144754140)
Wizards2021

Hawks2020<- confFinalsOdds(-0.0656778372)
Hawks2020
Celtics2020 <- confFinalsOdds(0.0587760135)
Celtics2020
Nets2020 <- confFinalsOdds(-0.0030138364)
Nets2020
Hornets2020 <- confFinalsOdds(-0.0611541317)
Hornets2020
Bulls2020 <- confFinalsOdds(-0.0266580632)
Bulls2020
Cavs2020 <- confFinalsOdds(-0.0702183815)
Cavs2020
Mavs2020 <- confFinalsOdds(0.0442942622)
Mavs2020
Nuggets2020 <- confFinalsOdds(0.0215522508)
Nuggets2020
Pistons2020 <- confFinalsOdds(-0.0302472793)
Pistons2020
Warriors2020 <- confFinalsOdds(-0.0766082595)
Warriors2020
Rockets2020 <- confFinalsOdds(0.0260899886)
Rockets2020
Pacers2020 <- confFinalsOdds(0.0196930944)
Pacers2020
Clippers2020 <- confFinalsOdds(0.0596908579)
Clippers2020
Lakers2020 <- confFinalsOdds(0.0524057806)
Lakers2020
Grizzlies2020 <- confFinalsOdds(-0.0075529774)
Grizzlies2020
Heat2020 <- confFinalsOdds(0.0251737410)
Heat2020
Bucks2020 <- confFinalsOdds(0.0869060588)
Bucks2020
Twolves2020 <- confFinalsOdds(-0.0348032590)
Twolves2020
Pelicans2020 <- confFinalsOdds(-0.0102512040)
Pelicans2020
Knicks2020 <- confFinalsOdds(-0.0575228189)
Knicks2020
Thunder2020 <- confFinalsOdds(0.0197015138)
Thunder2020
Magic2020 <- confFinalsOdds(-0.0102876878)
Magic2020
Sixers2020 <- confFinalsOdds(0.0224334177)
Sixers2020
Suns2020 <- confFinalsOdds(0.0061009294)
Suns2020
Blazers2020 <- confFinalsOdds(-0.0083976606)
Blazers2020
Kings2020 <- confFinalsOdds(-0.0157122055)
Kings2020
Spurs2020 <- confFinalsOdds(-0.0066030525)
Spurs2020
Raptors2020 <- confFinalsOdds(0.0569322926)
Raptors2020
Jazz2020 <- confFinalsOdds(0.0242645096)
Jazz2020
Wizards2020 <- confFinalsOdds(-0.0393059162)
Wizards2020

Hawks2019<- confFinalsOdds(-5.094507e-02)
Hawks2019
Celtics2019<- confFinalsOdds(3.927954e-02)
Celtics2019
Nets2019 <- confFinalsOdds(-8.208761e-04)
Nets2019
Hornets2019 <- confFinalsOdds(-1.175531e-02)
Hornets2019
Bulls2019 <- confFinalsOdds(-7.555341e-02)
Bulls2019
Cavs2019 <- confFinalsOdds(-9.104407e-02)
Cavs2019
Mavs2019 <- confFinalsOdds(-1.266830e-02)
Mavs2019
Nuggets2019 <- confFinalsOdds(3.654622e-02)
Nuggets2019
Pistons2019 <- confFinalsOdds(-2.643947e-03)
Pistons2019
Warriors2019 <- confFinalsOdds(5.842050e-02)
Warriors2019
Rockets2019 <- confFinalsOdds(4.383917e-02)
Rockets2019
Pacers2019 <- confFinalsOdds(3.107601e-02)
Pacers2019
Clippers2019 <- confFinalsOdds(9.205806e-03)
Clippers2019
Lakers2019 <- confFinalsOdds(-1.358071e-02)
Lakers2019
Grizzlies2019 <- confFinalsOdds(-2.178416e-02)
Grizzlies2019
Heat2019 <- confFinalsOdds(-3.556687e-03)
Heat2019
Bucks2019 <- confFinalsOdds(7.846846e-02)
Bucks2019
Twolves2019 <- confFinalsOdds(-1.448938e-02)
Twolves2019
Pelicans2019 <- confFinalsOdds(-1.175531e-02)
Pelicans2019
Knicks2019 <- confFinalsOdds(-8.102179e-02)
Knicks2019
Thunder2019 <- confFinalsOdds(3.016502e-02)
Thunder2019
Magic2019 <- confFinalsOdds(5.557837e-03)
Magic2019
Sixers2019 <- confFinalsOdds(2.378722e-02)
Sixers2019
Suns2019 <- confFinalsOdds(-8.102071e-02)
Suns2019
Blazers2019 <- confFinalsOdds(3.837021e-02)
Blazers2019
Kings2019 <- confFinalsOdds(-1.084490e-02)
Kings2019
Spurs2019 <- confFinalsOdds(1.467444e-02)
Spurs2019
Raptors2019 <- confFinalsOdds(5.295054e-02)
Raptors2019
Jazz2019 <- confFinalsOdds(4.565800e-02)
Jazz2019
Wizards2019 <- confFinalsOdds(-2.451432e-02)
Wizards2019

Hawks2022<- secondRoundOdds(0.0150518185)
Hawks2022
Celtics2022 <- secondRoundOdds(0.0662405011)
Celtics2022
Nets2022 <- secondRoundOdds(0.0078719899)
Nets2022
Hornets2022 <- secondRoundOdds(0.0042792942)
Hornets2022
Bulls2022 <- secondRoundOdds(-0.0046989220)
Bulls2022
Cavs2022 <- secondRoundOdds(0.0186519315)
Cavs2022
Mavs2022 <- secondRoundOdds(0.0303230006)
Mavs2022
Nuggets2022 <- secondRoundOdds(0.0204427164)
Nuggets2022
Pistons2022 <- secondRoundOdds(-0.0693409289)
Pistons2022
Warriors2022 <- secondRoundOdds(0.0491815009)
Warriors2022
Rockets2022 <- secondRoundOdds(-0.0747334957)
Rockets2022
Pacers2022 <- secondRoundOdds(-0.0325350822)
Pacers2022
Clippers2022 <- secondRoundOdds(-0.0002030457)
Clippers2022
Lakers2022 <- secondRoundOdds(-0.0253476510)
Lakers2022
Grizzlies2022 <- secondRoundOdds(0.0482794330)
Grizzlies2022
Heat2022 <- secondRoundOdds(0.0410979355)
Heat2022
Bucks2022 <- secondRoundOdds(0.0285236858)
Bucks2022
Twolves2022 <- secondRoundOdds(0.0249326590)
Twolves2022
Pelicans2022 <- secondRoundOdds(-0.0073901061)
Pelicans2022
Knicks2022 <- secondRoundOdds(-0.0046933591)
Knicks2022
Thunder2022 <- secondRoundOdds(-0.0711335682)
Thunder2022
Magic2022 <- secondRoundOdds(-0.0738277191)
Magic2022
Sixers2022 <- secondRoundOdds(0.0249341424)
Sixers2022
Suns2022 <- secondRoundOdds(0.0662393885)
Suns2022
Blazers2022 <- secondRoundOdds(-0.0810179318)
Blazers2022
Kings2022 <- secondRoundOdds(-0.0468986334)
Kings2022
Spurs2022 <- secondRoundOdds(0.0015884810)
Spurs2022
Raptors2022 <- secondRoundOdds(0.0195478802)
Raptors2022
Jazz2022 <- secondRoundOdds(0.0554598178)
Jazz2022
Wizards2022 <- secondRoundOdds(-0.0307359529)
Wizards2022

Hawks2021<- secondRoundOdds(0.0195405952)
Hawks2021
Celtics2021 <- secondRoundOdds(0.0114846452)
Celtics2021
Nets2021 <- secondRoundOdds(0.0374422521)
Nets2021
Hornets2021 <- secondRoundOdds(-0.0171604582)
Hornets2021
Bulls2021 <- secondRoundOdds(-0.0099985693)
Bulls2021
Cavs2021 <- secondRoundOdds(-0.0744521637)
Cavs2021
Mavs2021 <- secondRoundOdds(0.0204354737)
Mavs2021
Nuggets2021 <- secondRoundOdds(0.0428153378)
Nuggets2021
Pistons2021 <- secondRoundOdds(-0.0413298068)
Pistons2021
Warriors2021 <- secondRoundOdds(0.0096976131)
Warriors2021
Rockets2021 <- secondRoundOdds(-0.0663970312)
Rockets2021
Pacers2021 <- secondRoundOdds(-0.0001524536)
Pacers2021
Clippers2021 <- secondRoundOdds(0.0544535265)
Clippers2021
Lakers2021 <- secondRoundOdds(0.0267090237)
Lakers2021
Grizzlies2021 <- secondRoundOdds(0.0105912654)
Grizzlies2021
Heat2021 <- secondRoundOdds(-0.0010459697)
Heat2021
Bucks2021 <- secondRoundOdds(0.0517679374)
Bucks2021
Twolves2021 <- secondRoundOdds(-0.0467038462)
Twolves2021
Pelicans2021 <- secondRoundOdds(-0.0028398139)
Pelicans2021
Knicks2021 <- secondRoundOdds(0.0213367555)
Knicks2021
Thunder2021 <- secondRoundOdds(-0.0950404997)
Thunder2021
Magic2021 <- secondRoundOdds(-0.0834042184)
Magic2021
Sixers2021 <- secondRoundOdds(0.0490875254)
Sixers2021
Suns2021 <- secondRoundOdds(0.0526634971)
Suns2021
Blazers2021 <- secondRoundOdds(0.0159556316)
Blazers2021
Kings2021 <- secondRoundOdds(-0.0341744574)
Kings2021
Spurs2021 <- secondRoundOdds(-0.0135798543)
Spurs2021
Raptors2021 <- secondRoundOdds(-0.0037331937)
Raptors2021
Jazz2021 <- secondRoundOdds(0.0804171281)
Jazz2021
Wizards2021 <- secondRoundOdds(-0.0144754140)
Wizards2021

Hawks2020<- secondRoundOdds(-0.0656778372)
Hawks2020
Celtics2020 <- secondRoundOdds(0.0587760135)
Celtics2020
Nets2020 <- secondRoundOdds(-0.0030138364)
Nets2020
Hornets2020 <- secondRoundOdds(-0.0611541317)
Hornets2020
Bulls2020 <- secondRoundOdds(-0.0266580632)
Bulls2020
Cavs2020 <- secondRoundOdds(-0.0702183815)
Cavs2020
Mavs2020 <- secondRoundOdds(0.0442942622)
Mavs2020
Nuggets2020 <- secondRoundOdds(0.0215522508)
Nuggets2020
Pistons2020 <- secondRoundOdds(-0.0302472793)
Pistons2020
Warriors2020 <- secondRoundOdds(-0.0766082595)
Warriors2020
Rockets2020 <- secondRoundOdds(0.0260899886)
Rockets2020
Pacers2020 <- secondRoundOdds(0.0196930944)
Pacers2020
Clippers2020 <- secondRoundOdds(0.0596908579)
Clippers2020
Lakers2020 <- secondRoundOdds(0.0524057806)
Lakers2020
Grizzlies2020 <- secondRoundOdds(-0.0075529774)
Grizzlies2020
Heat2020 <- secondRoundOdds(0.0251737410)
Heat2020
Bucks2020 <- secondRoundOdds(0.0869060588)
Bucks2020
Twolves2020 <- secondRoundOdds(-0.0348032590)
Twolves2020
Pelicans2020 <- secondRoundOdds(-0.0102512040)
Pelicans2020
Knicks2020 <- secondRoundOdds(-0.0575228189)
Knicks2020
Thunder2020 <- secondRoundOdds(0.0197015138)
Thunder2020
Magic2020 <- secondRoundOdds(-0.0102876878)
Magic2020
Sixers2020 <- secondRoundOdds(0.0224334177)
Sixers2020
Suns2020 <- secondRoundOdds(0.0061009294)
Suns2020
Blazers2020 <- secondRoundOdds(-0.0083976606)
Blazers2020
Kings2020 <- secondRoundOdds(-0.0157122055)
Kings2020
Spurs2020 <- secondRoundOdds(-0.0066030525)
Spurs2020
Raptors2020 <- secondRoundOdds(0.0569322926)
Raptors2020
Jazz2020 <- secondRoundOdds(0.0242645096)
Jazz2020
Wizards2020 <- secondRoundOdds(-0.0393059162)
Wizards2020

Hawks2019<- secondRoundOdds(-5.094507e-02)
Hawks2019
Celtics2019<- secondRoundOdds(3.927954e-02)
Celtics2019
Nets2019 <- secondRoundOdds(-8.208761e-04)
Nets2019
Hornets2019 <- secondRoundOdds(-1.175531e-02)
Hornets2019
Bulls2019 <- secondRoundOdds(-7.555341e-02)
Bulls2019
Cavs2019 <- secondRoundOdds(-9.104407e-02)
Cavs2019
Mavs2019 <- secondRoundOdds(-1.266830e-02)
Mavs2019
Nuggets2019 <- secondRoundOdds(3.654622e-02)
Nuggets2019
Pistons2019 <- secondRoundOdds(-2.643947e-03)
Pistons2019
Warriors2019 <- secondRoundOdds(5.842050e-02)
Warriors2019
Rockets2019 <- secondRoundOdds(4.383917e-02)
Rockets2019
Pacers2019 <- secondRoundOdds(3.107601e-02)
Pacers2019
Clippers2019 <- secondRoundOdds(9.205806e-03)
Clippers2019
Lakers2019 <- secondRoundOdds(-1.358071e-02)
Lakers2019
Grizzlies2019 <- secondRoundOdds(-2.178416e-02)
Grizzlies2019
Heat2019 <- secondRoundOdds(-3.556687e-03)
Heat2019
Bucks2019 <- secondRoundOdds(7.846846e-02)
Bucks2019
Twolves2019 <- secondRoundOdds(-1.448938e-02)
Twolves2019
Pelicans2019 <- secondRoundOdds(-1.175531e-02)
Pelicans2019
Knicks2019 <- secondRoundOdds(-8.102179e-02)
Knicks2019
Thunder2019 <- secondRoundOdds(3.016502e-02)
Thunder2019
Magic2019 <- secondRoundOdds(5.557837e-03)
Magic2019
Sixers2019 <- secondRoundOdds(2.378722e-02)
Sixers2019
Suns2019 <- secondRoundOdds(-8.102071e-02)
Suns2019
Blazers2019 <- secondRoundOdds(3.837021e-02)
Blazers2019
Kings2019 <- secondRoundOdds(-1.084490e-02)
Kings2019
Spurs2019 <- secondRoundOdds(1.467444e-02)
Spurs2019
Raptors2019 <- secondRoundOdds(5.295054e-02)
Raptors2019
Jazz2019 <- secondRoundOdds(4.565800e-02)
Jazz2019
Wizards2019 <- secondRoundOdds(-2.451432e-02)
Wizards2019

logisticPlayoffs <- glm(MadePlayoffs ~ AdjustedNet, data = nba_data, family = "binomial")
logisticSecond <- glm(madeSecondRound ~ AdjustedNet, data = nba_data, family = "binomial")
logisticConfFinals <- glm(madeConfFinals ~ AdjustedNet, data = nba_data, family = "binomial")
logisticFinals <- glm(madeFinals ~ AdjustedNet, data = nba_data, family = "binomial")
logisticChampions <- glm(champion ~ AdjustedNet, data = nba_data, family = "binomial")

#2021-22 Season Data

logistic2022Playoffs <- glm(MadePlayoffs ~ AdjustedNet, data = nba_data_2022, family = "binomial")
logistic2022Second <- glm(madeSecondRound ~ AdjustedNet, data = nba_data_2022, family = "binomial")
logistic2022ConfFinals <- glm(madeConfFinals ~ AdjustedNet, data = nba_data_2022, family = "binomial")
logistic2022Finals <- glm(madeFinals ~ AdjustedNet, data = nba_data_2022, family = "binomial")
logistic2022Champ <- glm(champion ~ AdjustedNet, data = nba_data_2022, family = "binomial")
predicted_data_2022po <- data.frame(prob.of.playoffs=logistic2022Playoffs$fitted.values,mp=nba_data_2022$MadePlayoffs)
predicted_data_2022po <- predicted_data_2022po[
  order(predicted_data_2022po$prob.of.playoffs,decreasing=FALSE),]
predicted_data_2022po$rank <- 1:nrow(predicted_data_2022po)
ggplot(data=predicted_data_2022po,aes(x=rank,y=prob.of.playoffs)) +
  geom_point(aes(color=mp),alpha=1, shape=4, stroke=1) +
  xlab("index") +
  ylab("Predicted probability of making playoffs")
predicted_data_second_2022 <- data.frame(prob.of.second=logistic2022Second$fitted.values,ms=nba_data_2022$madeSecondRound)
predicted_data_second_2022 <- predicted_data_second_2022[
  order(predicted_data_second_2022$prob.of.second,decreasing=FALSE),]
predicted_data_second_2022$rank <- 1:nrow(predicted_data_second_2022)
ggplot(data=predicted_data_second_2022,aes(x=rank,y=prob.of.second)) +
  geom_point(aes(color=ms),alpha=1, shape=4, stroke=1) +
  xlab("index") +
  ylab("Predicted probability of making second round")
predicted_data_conf_2022 <- data.frame(prob.of.conf=logistic2022ConfFinals$fitted.values,mc=nba_data_2022$madeConfFinals)
predicted_data_conf_2022 <- predicted_data_conf_2022[
  order(predicted_data_conf_2022$prob.of.conf,decreasing=FALSE),]
predicted_data_conf_2022$rank <- 1:nrow(predicted_data_conf_2022)
ggplot(data=predicted_data_conf_2022,aes(x=rank,y=prob.of.conf)) +
  geom_point(aes(color=mc),alpha=1, shape=4, stroke=1) +
  xlab("index") +
  ylab("Predicted probability of making conference finals")
predicted_data_finals_2022 <- data.frame(prob.of.finals=logistic2022Finals$fitted.values,mf=nba_data_2022$madeFinals)
predicted_data_finals_2022 <- predicted_data_finals_2022[
  order(predicted_data_finals_2022$prob.of.finals,decreasing=FALSE),]
predicted_data_finals_2022$rank <- 1:nrow(predicted_data_finals_2022)
ggplot(data=predicted_data_finals_2022,aes(x=rank,y=prob.of.finals)) +
  geom_point(aes(color=mf),alpha=1, shape=4, stroke=1) +
  xlab("index") +
  ylab("Predicted probability of making finals")
predicted_data_champ_2022 <- data.frame(prob.of.champ=logistic2022Champ$fitted.values,ch=nba_data_2022$champion)
predicted_data_champ_2022 <- predicted_data_champ_2022[
  order(predicted_data_champ_2022$prob.of.champ,decreasing=FALSE),]
predicted_data_champ_2022$rank <- 1:nrow(predicted_data_champ_2022)
ggplot(data=predicted_data_champ_2022,aes(x=rank,y=prob.of.champ)) +
  geom_point(aes(color=ch),alpha=1, shape=4, stroke=1) +
  xlab("index") +
  ylab("Predicted probability of being champion")

#2020-21 Season Data

logistic2021Playoffs <- glm(MadePlayoffs ~ AdjustedNet, data = nba_data_2021, family = "binomial")
logistic2021Second <- glm(madeSecondRound ~ AdjustedNet, data = nba_data_2021, family = "binomial")
logistic2021ConfFinals <- glm(madeConfFinals ~ AdjustedNet, data = nba_data_2021, family = "binomial")
logistic2021Finals <- glm(madeFinals ~ AdjustedNet, data = nba_data_2021, family = "binomial")
logistic2021Champ <- glm(champion ~ AdjustedNet, data = nba_data_2021, family = "binomial")
predicted_data_2021po <- data.frame(prob.of.playoffs=logistic2021Playoffs$fitted.values,mp=nba_data_2021$MadePlayoffs)
predicted_data_2021po <- predicted_data_2021po[
  order(predicted_data_2021po$prob.of.playoffs,decreasing=FALSE),]
predicted_data_2021po$rank <- 1:nrow(predicted_data_2021po)
ggplot(data=predicted_data_2021po,aes(x=rank,y=prob.of.playoffs)) +
  geom_point(aes(color=mp),alpha=1, shape=4, stroke=1) +
  xlab("index") +
  ylab("Predicted probability of making playoffs")
predicted_data_second_2021 <- data.frame(prob.of.second=logistic2021Second$fitted.values,ms=nba_data_2021$madeSecondRound)
predicted_data_second_2021 <- predicted_data_second_2021[
  order(predicted_data_second_2021$prob.of.second,decreasing=FALSE),]
predicted_data_second_2021$rank <- 1:nrow(predicted_data_second_2021)
ggplot(data=predicted_data_second_2021,aes(x=rank,y=prob.of.second)) +
  geom_point(aes(color=ms),alpha=1, shape=4, stroke=1) +
  xlab("index") +
  ylab("Predicted probability of making second round")
predicted_data_conf_2021 <- data.frame(prob.of.conf=logistic2021ConfFinals$fitted.values,mc=nba_data_2021$madeConfFinals)
predicted_data_conf_2021 <- predicted_data_conf_2021[
  order(predicted_data_conf_2021$prob.of.conf,decreasing=FALSE),]
predicted_data_conf_2021$rank <- 1:nrow(predicted_data_conf_2021)
ggplot(data=predicted_data_conf_2021,aes(x=rank,y=prob.of.conf)) +
  geom_point(aes(color=mc),alpha=1, shape=4, stroke=1) +
  xlab("index") +
  ylab("Predicted probability of making conference finals")
predicted_data_finals_2021 <- data.frame(prob.of.finals=logistic2021Finals$fitted.values,mf=nba_data_2021$madeFinals)
predicted_data_finals_2021 <- predicted_data_finals_2021[
  order(predicted_data_finals_2021$prob.of.finals,decreasing=FALSE),]
predicted_data_finals_2021$rank <- 1:nrow(predicted_data_finals_2021)
ggplot(data=predicted_data_finals_2021,aes(x=rank,y=prob.of.finals)) +
  geom_point(aes(color=mf),alpha=1, shape=4, stroke=1) +
  xlab("index") +
  ylab("Predicted probability of making finals")
predicted_data_champ_2021 <- data.frame(prob.of.champ=logistic2021Champ$fitted.values,ch=nba_data_2021$champion)
predicted_data_champ_2021 <- predicted_data_champ_2021[
  order(predicted_data_champ_2021$prob.of.champ,decreasing=FALSE),]
predicted_data_champ_2021$rank <- 1:nrow(predicted_data_champ_2021)
ggplot(data=predicted_data_champ_2021,aes(x=rank,y=prob.of.champ)) +
  geom_point(aes(color=ch),alpha=1, shape=4, stroke=1) +
  xlab("index") +
  ylab("Predicted probability of making finals")

#2019-20 Season Data

logistic2020Playoffs <- glm(MadePlayoffs ~ AdjustedNet, data = nba_data_2020, family = "binomial")
logistic2020Second <- glm(madeSecondRound ~ AdjustedNet, data = nba_data_2020, family = "binomial")
logistic2020ConfFinals <- glm(madeConfFinals ~ AdjustedNet, data = nba_data_2020, family = "binomial")
logistic2020Finals <- glm(madeFinals ~ AdjustedNet, data = nba_data_2020, family = "binomial")
logistic2020Champ <- glm(champion ~ AdjustedNet, data = nba_data_2020, family = "binomial")
predicted_data_2020po <- data.frame(prob.of.playoffs=logistic2020Playoffs$fitted.values,mp=nba_data_2020$MadePlayoffs)
predicted_data_2020po <- predicted_data_2020po[
  order(predicted_data_2020po$prob.of.playoffs,decreasing=FALSE),]
predicted_data_2020po$rank <- 1:nrow(predicted_data_2020po)
ggplot(data=predicted_data_2020po,aes(x=rank,y=prob.of.playoffs)) +
  geom_point(aes(color=mp),alpha=1, shape=4, stroke=1) +
  xlab("index") +
  ylab("Predicted probability of making playoffs")
predicted_data_second_2020 <- data.frame(prob.of.second=logistic2020Second$fitted.values,ms=nba_data_2020$madeSecondRound)
predicted_data_second_2020 <- predicted_data_second_2020[
  order(predicted_data_second_2020$prob.of.second,decreasing=FALSE),]
predicted_data_second_2020$rank <- 1:nrow(predicted_data_second_2020)
ggplot(data=predicted_data_second_2020,aes(x=rank,y=prob.of.second)) +
  geom_point(aes(color=ms),alpha=1, shape=4, stroke=1) +
  xlab("index") +
  ylab("Predicted probability of making second round")
predicted_data_conf_2020 <- data.frame(prob.of.conf=logistic2020ConfFinals$fitted.values,mc=nba_data_2020$madeConfFinals)
predicted_data_conf_2020 <- predicted_data_conf_2020[
  order(predicted_data_conf_2020$prob.of.conf,decreasing=FALSE),]
predicted_data_conf_2020$rank <- 1:nrow(predicted_data_conf_2020)
ggplot(data=predicted_data_conf_2020,aes(x=rank,y=prob.of.conf)) +
  geom_point(aes(color=mc),alpha=1, shape=4, stroke=1) +
  xlab("index") +
  ylab("Predicted probability of making conference finals")
predicted_data_finals_2020 <- data.frame(prob.of.finals=logistic2020Finals$fitted.values,mf=nba_data_2020$madeFinals)
predicted_data_finals_2020 <- predicted_data_finals_2020[
  order(predicted_data_finals_2020$prob.of.finals,decreasing=FALSE),]
predicted_data_finals_2020$rank <- 1:nrow(predicted_data_finals_2020)
ggplot(data=predicted_data_finals_2020,aes(x=rank,y=prob.of.finals)) +
  geom_point(aes(color=mf),alpha=1, shape=4, stroke=1) +
  xlab("index") +
  ylab("Predicted probability of making finals")
predicted_data_champ_2020 <- data.frame(prob.of.champ=logistic2020Champ$fitted.values,ch=nba_data_2020$champion)
predicted_data_champ_2020 <- predicted_data_champ_2020[
  order(predicted_data_champ_2020$prob.of.champ,decreasing=FALSE),]
predicted_data_champ_2020$rank <- 1:nrow(predicted_data_champ_2020)
ggplot(data=predicted_data_champ_2020,aes(x=rank,y=prob.of.champ)) +
  geom_point(aes(color=ch),alpha=1, shape=4, stroke=1) +
  xlab("index") +
  ylab("Predicted probability of making finals")

predicted_data_playoffs <- data.frame(prob.of.playoffs=logisticPlayoffs$fitted.values,mp=nba_data$MadePlayoffs)
predicted_data_playoffs <- predicted_data_playoffs[
  order(predicted_data_playoffs$prob.of.playoffs,decreasing=FALSE),]
predicted_data_playoffs$rank <- 1:nrow(predicted_data_playoffs)

predicted_data_second <- data.frame(prob.of.second=logisticSecond$fitted.values,ms=nba_data$madeSecondRound)
predicted_data_second <- predicted_data_second[
  order(predicted_data_second$prob.of.second,decreasing=FALSE),]
predicted_data_second$rank <- 1:nrow(predicted_data_second)

predicted_data_conf <- data.frame(prob.of.conf=logisticConfFinals$fitted.values,mc=nba_data$madeConfFinals)
predicted_data_conf <- predicted_data_conf[
  order(predicted_data_conf$prob.of.conf,decreasing=FALSE),]
predicted_data_conf$rank <- 1:nrow(predicted_data_conf)

predicted_data_finals <- data.frame(prob.of.finals=logisticFinals$fitted.values,mf=nba_data$madeFinals)
predicted_data_finals <- predicted_data_finals[
  order(predicted_data_finals$prob.of.finals,decreasing=FALSE),]
predicted_data_finals$rank <- 1:nrow(predicted_data_finals)

predicted_data_champ <- data.frame(prob.of.champ=logisticChampions$fitted.values,ch=nba_data$champion)
predicted_data_champ <- predicted_data_champ[
  order(predicted_data_champ$prob.of.champ,decreasing=FALSE),]
predicted_data_champ$rank <- 1:nrow(predicted_data_champ)

ggplot(data=predicted_data_playoffs,aes(x=rank,y=prob.of.playoffs)) +
  geom_point(aes(color=mp),alpha=1, shape=4, stroke=1) +
  xlab("index") +
  ylab("Predicted probability of making playoffs")

ggplot(data=predicted_data_second,aes(x=rank,y=prob.of.second)) +
  geom_point(aes(color=ms),alpha=1, shape=4, stroke=1) +
  xlab("index") +
  ylab("Predicted probability of making second round")

ggplot(data=predicted_data_conf,aes(x=rank,y=prob.of.conf)) +
  geom_point(aes(color=mc),alpha=1, shape=4, stroke=1) +
  xlab("index") +
  ylab("Predicted probability of making conference finals")

ggplot(data=predicted_data_finals,aes(x=rank,y=prob.of.finals)) +
  geom_point(aes(color=mf),alpha=1, shape=4, stroke=1) +
  xlab("index") +
  ylab("Predicted probability of making finals")

ggplot(data=predicted_data_champ,aes(x=rank,y=prob.of.champ)) +
  geom_point(aes(color=ch),alpha=1, shape=4, stroke=1) +
  xlab("index") +
  ylab("Predicted probability of being champion")
