#############
## Calculates the predictions and accuracy metrics for:
##    - Logistic regression to classify binary game outcome (won/lost)
##    - Linear regression to predict score differential
#############

require(tidyverse)
require(car)
require(boot)
require(ROCR)


outcome.type <- "Binary Outcome"  # "Score Differential" or "Binary Outcome"


## Week up to which the rankings are used, to do predictions for the future weeks.
## If you pick "MAX", only training error will be calculated.
## If you pick a number of weeks (e.g. 9), only test error will be outputted.

week <- "MAX"   # Pick "MAX for all weeks, or 9 for 9 weeks



## Whether to IMPUTE STATS that were ALWAYS UNAVAILABLE for NON-MAJOR TEAMS, 
## e.g. Sacks, Tackles, etc etc (mostly PURE DEFENSIVE PLAYS)
IMPUTE.NONMAJOR.NA <- TRUE



years <- 2009:2019
GLM_type <- "Gaussian"


Data <- read.csv(paste0("GROUP_LASSO/LASSO_Estimates/IMPUTED_EFFICIENCY_TOT_AVG_",
                        GLM_type, "_BIC_", "forward", "_", "2018", ".csv"))



main.stats <- c("Off_w_ST.Minus.Def_w_ST.Points.Margin")



for (stat in main.stats){
  
  print(stat)
  
  train.RSE <- test.RMSE <- matrix(0, nrow=length(years), ncol=4)
  train.AUC <- test.AUC <- matrix(0, nrow=length(years), ncol=4)
  train.prob.err <- train.acc <- test.prob.err <- test.acc <- matrix(0, nrow=length(years), ncol=4)
  
  
  for (year in years){
    
    all.condensed.games <- read.csv(file=paste0("Game_Logs/", year, "/All_DISTINCT_Condensed_Game_Logs.csv"))
    if (!include.non.majors) all.condensed.games <- all.condensed.games %>% filter(Team != "Non-Major" & Opponent != "Non-Major")
    all.offense.game.logs <- read.csv(file=paste0("Game_Logs/", year, 
                                                  "/",
                                                  ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                                  "FINAL_FULL_STATS_Offense&SpecialTeams.csv"))
    all.defense.game.logs <- read.csv(file=paste0("Game_Logs/", year, 
                                                  "/",
                                                  ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                                  "FINAL_FULL_STATS_Defense&SpecialTeams.csv"))
    
    
    latest.date <- function(week, year){
      data <- read.csv(paste("Game_Logs/", year, "/All_DISTINCT_Condensed_Game_Logs_NO_NEUTRAL_with_WEEKS.csv", sep=""))
      if (year <= 2017) return(max(as.Date(subset(data, Wk == week)$Date, "%b %d, %Y")))
      if (year > 2017) return(max(as.Date(subset(data, Wk == week)$Date, "%m/%d/%y")))
      
    }
    
    latest.week <- function(year){
      data <- read.csv(paste("Game_Logs/", year, "/All_DISTINCT_Condensed_Game_Logs_NO_NEUTRAL_with_WEEKS.csv", sep=""))
      return(max(data$Wk))
    }
    
    
    if (week == "MAX"){ 
      max.week <- latest.week(year)
    } else {
      max.week <- week
    }
    
    max.date <- latest.date(max.week, year)
    
    
    all.offense.game.logs.classic <- all.offense.game.logs
    all.offense.game.logs.adj.opp <- all.offense.game.logs
    all.offense.game.logs.adj.opp.and.ballside.eff <- all.offense.game.logs
    all.offense.game.logs.adj.opp.and.ballside.noneff <- all.offense.game.logs
    
    
    all.defense.game.logs.classic <- all.defense.game.logs
    all.defense.game.logs.adj.opp <- all.defense.game.logs
    all.defense.game.logs.adj.opp.and.ballside.eff <- all.defense.game.logs
    all.defense.game.logs.adj.opp.and.ballside.noneff <- all.defense.game.logs
    
    
    
    side <- "Offense"
    offensive.worth.classic <- read.csv(paste0("GROUP_LASSO/Rankings_NEW/", year, "/", side, "/Up_to_Week=", week, "/Classic/", 
                                               ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                               ifelse(efficiency, "EFFICIENCY_", ""),
                                               stat, ".csv"))
    offensive.worth.adj.opp <- read.csv(paste0("GROUP_LASSO/Rankings_NEW/", year, "/", side, "/Up_to_Week=", week, "/Gaussian_Adj_Opp/", 
                                               ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                               ifelse(efficiency, "EFFICIENCY_", ""),
                                               stat, ".csv"))
    offensive.worth.adj.opp.and.ballside.eff <- read.csv(paste0("GROUP_LASSO/Rankings_NEW/", year,"/", side, "/Up_to_Week=", week, "/", GLM_type,"_Adj_Opp_and_Other_BallSide", 
                                                                "/", 
                                                                ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                                                "EFFICIENCY_",
                                                                stat, ".csv"))
    offensive.worth.adj.opp.and.ballside.noneff <- read.csv(paste0("GROUP_LASSO/Rankings_NEW/", year,"/", side, "/Up_to_Week=", week, "/", GLM_type,"_Adj_Opp_and_Other_BallSide", 
                                                                   "/", 
                                                                   ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                                                   stat, ".csv"))
    
    side <- "Defense"
    defensive.worth.classic <- read.csv(paste0("GROUP_LASSO/Rankings_NEW/", year, "/", side, "/Up_to_Week=", week, "/Classic/", 
                                               ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                               ifelse(efficiency, "EFFICIENCY_", ""),
                                               stat, ".csv"))
    defensive.worth.adj.opp <- read.csv(paste0("GROUP_LASSO/Rankings_NEW/", year, "/", side, "/Up_to_Week=", week,  "/Gaussian_Adj_Opp/", 
                                               ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                               ifelse(efficiency, "EFFICIENCY_", ""),
                                               stat, ".csv"))
    defensive.worth.adj.opp.and.ballside.eff <- read.csv(paste0("GROUP_LASSO/Rankings_NEW/", year, "/", side, "/Up_to_Week=", week, "/", GLM_type,"_Adj_Opp_and_Other_BallSide", 
                                                                "/", 
                                                                ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                                                "EFFICIENCY_",
                                                                stat, ".csv"))
    defensive.worth.adj.opp.and.ballside.noneff <- read.csv(paste0("GROUP_LASSO/Rankings_NEW/", year, "/", side, "/Up_to_Week=", week, "/", GLM_type,"_Adj_Opp_and_Other_BallSide", 
                                                                   "/", 
                                                                   ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                                                   stat, ".csv"))
    

    
    ## 1. For classic averages:
    all.offense.game.logs.classic <- all.condensed.games %>% 
      left_join(all.offense.game.logs.classic %>% dplyr::select(Team, Opponent, Date, Off.Points.Team), by=c("Team", "Opponent", "Date")) %>% 
      left_join(all.defense.game.logs.classic %>% dplyr::select(Team, Opponent, Date, Def.Points.Team), by=c("Team", "Opponent", "Date")) %>% 
      mutate(Score.Diff = Off.Points.Team - Def.Points.Team,
             Binary.Outcome = ifelse(Score.Diff > 0, "Won", "Lost")) %>%
      left_join(offensive.worth.classic %>% dplyr::select(Team, Value), by="Team") %>%
      left_join(defensive.worth.classic %>% dplyr::select(Team, Value), by="Team") %>%
      left_join(offensive.worth.classic %>% dplyr::select(Team, Value), by=c("Opponent"="Team")) %>%
      left_join(defensive.worth.classic %>% dplyr::select(Team, Value), by=c("Opponent"="Team")) %>%
      rename(Homefield = X)
    
    response.stat <- outcome.type
    
    all.offense.game.logs.classic$Homefield <- factor(all.offense.game.logs.classic$Homefield, levels=c("","N","@"))
    all.offense.game.logs.classic$Homefield <- as.numeric(all.offense.game.logs.classic$Homefield)-1
    
    dim(all.offense.game.logs.classic)
    colnames(all.offense.game.logs.classic)[ncol(all.offense.game.logs.classic)-c(3:0)] <- paste0(stat, c(".TeamOffense",".TeamDefense",".OppOffense",".OppDefense"))
    
    ## 2. For opponent adjustments:
    all.offense.game.logs.adj.opp <- all.condensed.games %>% 
      left_join(all.offense.game.logs.adj.opp %>% dplyr::select(Team, Opponent, Date, Off.Points.Team), by=c("Team", "Opponent", "Date")) %>% 
      left_join(all.defense.game.logs.adj.opp %>% dplyr::select(Team, Opponent, Date, Def.Points.Team), by=c("Team", "Opponent", "Date")) %>% 
      mutate(Score.Diff = Off.Points.Team - Def.Points.Team,
             Binary.Outcome = ifelse(Score.Diff > 0, "Won", "Lost")) %>%
      left_join(offensive.worth.adj.opp %>% dplyr::select(Team, Value), by="Team") %>%
      left_join(defensive.worth.adj.opp %>% dplyr::select(Team, Value), by="Team") %>%
      left_join(offensive.worth.adj.opp %>% dplyr::select(Team, Value), by=c("Opponent"="Team")) %>%
      left_join(defensive.worth.adj.opp %>% dplyr::select(Team, Value), by=c("Opponent"="Team")) %>%
      rename(Homefield = X)
    
    all.offense.game.logs.adj.opp$Homefield <- factor(all.offense.game.logs.adj.opp$Homefield, levels=c("","N","@"))
    all.offense.game.logs.adj.opp$Homefield <- as.numeric(all.offense.game.logs.adj.opp$Homefield)-1
    
    colnames(all.offense.game.logs.adj.opp)[ncol(all.offense.game.logs.adj.opp)-c(3:0)] <- paste0(stat, c(".TeamOffense",".TeamDefense",".OppOffense",".OppDefense"))
    
    ## 3. For opponent adjustments & other ballside, EFFICIENCY stats:
    all.offense.game.logs.adj.opp.and.ballside.eff <- all.condensed.games %>% 
      left_join(all.offense.game.logs.adj.opp %>% dplyr::select(Team, Opponent, Date, Off.Points.Team), by=c("Team", "Opponent", "Date")) %>% 
      left_join(all.defense.game.logs.adj.opp %>% dplyr::select(Team, Opponent, Date, Def.Points.Team), by=c("Team", "Opponent", "Date")) %>% 
      mutate(Score.Diff = Off.Points.Team - Def.Points.Team,
             Binary.Outcome = ifelse(Score.Diff > 0, "Won", "Lost")) %>%
      left_join(offensive.worth.adj.opp.and.ballside.eff %>% dplyr::select(Team, Value), by="Team") %>%
      left_join(defensive.worth.adj.opp.and.ballside.eff %>% dplyr::select(Team, Value), by="Team") %>%
      left_join(offensive.worth.adj.opp.and.ballside.eff %>% dplyr::select(Team, Value), by=c("Opponent"="Team")) %>%
      left_join(defensive.worth.adj.opp.and.ballside.eff %>% dplyr::select(Team, Value), by=c("Opponent"="Team")) %>%
      rename(Homefield = X)
    
    all.offense.game.logs.adj.opp.and.ballside.eff$Homefield <- factor(all.offense.game.logs.adj.opp.and.ballside.eff$Homefield, levels=c("","N","@"))
    all.offense.game.logs.adj.opp.and.ballside.eff$Homefield <- as.numeric(all.offense.game.logs.adj.opp.and.ballside.eff$Homefield)-1
    
    colnames(all.offense.game.logs.adj.opp.and.ballside.eff)[ncol(all.offense.game.logs.adj.opp.and.ballside.eff)-c(3:0)] <- paste0(stat, c(".TeamOffense",".TeamDefense",".OppOffense",".OppDefense"))
    
    
    ## 4. For opponent adjustments & other ballside, NON-EFFICIENCY stats:
    all.offense.game.logs.adj.opp.and.ballside.noneff <- all.condensed.games %>% 
      left_join(all.offense.game.logs.adj.opp %>% dplyr::select(Team, Opponent, Date, Off.Points.Team), by=c("Team", "Opponent", "Date")) %>% 
      left_join(all.defense.game.logs.adj.opp %>% dplyr::select(Team, Opponent, Date, Def.Points.Team), by=c("Team", "Opponent", "Date")) %>% 
      mutate(Score.Diff = Off.Points.Team - Def.Points.Team,
             Binary.Outcome = ifelse(Score.Diff > 0, "Won", "Lost")) %>%
      left_join(offensive.worth.adj.opp.and.ballside.noneff %>% dplyr::select(Team, Value), by="Team") %>%
      left_join(defensive.worth.adj.opp.and.ballside.noneff %>% dplyr::select(Team, Value), by="Team") %>%
      left_join(offensive.worth.adj.opp.and.ballside.noneff %>% dplyr::select(Team, Value), by=c("Opponent"="Team")) %>%
      left_join(defensive.worth.adj.opp.and.ballside.noneff %>% dplyr::select(Team, Value), by=c("Opponent"="Team")) %>%
      rename(Homefield = X)
    
    all.offense.game.logs.adj.opp.and.ballside.noneff$Homefield <- factor(all.offense.game.logs.adj.opp.and.ballside.noneff$Homefield, levels=c("","N","@"))
    all.offense.game.logs.adj.opp.and.ballside.noneff$Homefield <- as.numeric(all.offense.game.logs.adj.opp.and.ballside.noneff$Homefield)-1
    
    colnames(all.offense.game.logs.adj.opp.and.ballside.noneff)[ncol(all.offense.game.logs.adj.opp.and.ballside.noneff)-c(3:0)] <- paste0(stat, c(".TeamOffense",".TeamDefense",".OppOffense",".OppDefense"))
    
    

    if (outcome.type == "Score Differential") na.ind <- which(is.na(all.offense.game.logs.classic[,"Score.Diff"]))
    if (outcome.type == "Binary Outcome") na.ind <- which(is.na(all.offense.game.logs.classic[,"Binary.Outcome"]))
    
    if (length(na.ind)>0){
      all.offense.game.logs.classic <- all.offense.game.logs.classic[-na.ind, ]
      all.offense.game.logs.adj.opp <- all.offense.game.logs.adj.opp[-na.ind, ]
      all.offense.game.logs.adj.opp.and.ballside.eff <- all.offense.game.logs.adj.opp.and.ballside.eff[-na.ind, ]
      all.offense.game.logs.adj.opp.and.ballside.noneff <- all.offense.game.logs.adj.opp.and.ballside.noneff[-na.ind, ]
    }
    
    
    all.offense.game.logs.classic.train <- subset(all.offense.game.logs.classic, as.Date(Date) <= max.date)
    all.offense.game.logs.classic.test <- subset(all.offense.game.logs.classic, as.Date(Date) > max.date)
    
    all.offense.game.logs.adj.opp.train <- subset(all.offense.game.logs.adj.opp, as.Date(Date) <= max.date)
    all.offense.game.logs.adj.opp.test <- subset(all.offense.game.logs.adj.opp, as.Date(Date) > max.date)
    
    all.offense.game.logs.adj.opp.and.ballside.eff.train <- subset(all.offense.game.logs.adj.opp.and.ballside.eff, as.Date(Date) <= max.date)
    all.offense.game.logs.adj.opp.and.ballside.eff.test <- subset(all.offense.game.logs.adj.opp.and.ballside.eff, as.Date(Date) > max.date)
    
    all.offense.game.logs.adj.opp.and.ballside.noneff.train <- subset(all.offense.game.logs.adj.opp.and.ballside.noneff, as.Date(Date) <= max.date)
    all.offense.game.logs.adj.opp.and.ballside.noneff.test <- subset(all.offense.game.logs.adj.opp.and.ballside.noneff, as.Date(Date) > max.date)
    
    needed.colnames <- colnames(all.offense.game.logs.classic.train)[ncol(all.offense.game.logs.classic.train) - c((4*length(response.stat)-1):0)]

    
    
    if (outcome.type == "Score Differential"){
      
      lm.classic.obj <- lm(Score.Diff  ~ .,
                           data=all.offense.game.logs.classic.train[,c("Score.Diff", needed.colnames, "Homefield")])
      lm.classic.obj
      train.RSE[which(year == years),1] <- summary(lm.classic.obj)$sigma
      test.RMSE[which(year == years),1] <-  sqrt(mean((all.offense.game.logs.classic.test$Score.Diff -
                                                         predict(lm.classic.obj,
                                                                 newdata=all.offense.game.logs.classic.test[,c(needed.colnames, "Homefield")]))^2))
      
      lm.adj.opp.obj <- lm(Score.Diff ~ .,
                           data=all.offense.game.logs.adj.opp.train[,c("Score.Diff", needed.colnames, "Homefield")])
      
      
      
      lm.adj.opp.obj
      train.RSE[which(year == years),2] <- summary(lm.adj.opp.obj)$sigma
      test.RMSE[which(year == years),2] <-  sqrt(mean((all.offense.game.logs.adj.opp.test$Score.Diff -
                                                         predict(lm.adj.opp.obj,
                                                                 newdata=all.offense.game.logs.adj.opp.test[,c(needed.colnames, "Homefield")]))^2))
      
      
      
      lm.adj.opp.and.ballside.eff.obj <- lm(Score.Diff ~ .,
                                            data=all.offense.game.logs.adj.opp.and.ballside.eff.train[,c("Score.Diff", needed.colnames, "Homefield")])
      lm.adj.opp.and.ballside.eff.obj
      train.RSE[which(year == years),3] <- summary(lm.adj.opp.and.ballside.eff.obj)$sigma
      
      test.RMSE[which(year == years),3] <-  sqrt(mean((all.offense.game.logs.adj.opp.and.ballside.eff.test$Score.Diff -
                                                         predict(lm.adj.opp.and.ballside.eff.obj,
                                                                 newdata=all.offense.game.logs.adj.opp.and.ballside.eff.test[,c(needed.colnames, "Homefield")]))^2))
      
      
      
      lm.adj.opp.and.ballside.noneff.obj <- lm(Score.Diff ~ .,
                                               data=all.offense.game.logs.adj.opp.and.ballside.noneff.train[,c("Score.Diff", needed.colnames, "Homefield")])
      lm.adj.opp.and.ballside.noneff.obj
      train.RSE[which(year == years),4] <- summary(lm.adj.opp.and.ballside.noneff.obj)$sigma
      
      test.RMSE[which(year == years),4] <-  sqrt(mean((all.offense.game.logs.adj.opp.and.ballside.noneff.test$Score.Diff -
                                                         predict(lm.adj.opp.and.ballside.noneff.obj,
                                                                 newdata=all.offense.game.logs.adj.opp.and.ballside.noneff.test[,c(needed.colnames, "Homefield")]))^2))
      
      
    }
    
    
    
    
    if (outcome.type == "Binary Outcome"){
      
      glm.classic.obj <- glm(as.factor(Binary.Outcome)  ~ .,
                             data=all.offense.game.logs.classic.train[,c("Binary.Outcome", needed.colnames, "Homefield")],
                             family="binomial")

      glm.classic.obj
      train.prob.err[which(year == years),1] <- mean(abs(resid(glm.classic.obj, type = "response")))
      train.acc[which(year == years),1] <- mean(ifelse(predict(glm.classic.obj, type="response") >=0.50, "Won", "Lost") == all.offense.game.logs.classic.train$Binary.Outcome)
      
      
      pred.obj <- prediction(ifelse(predict(glm.classic.obj, type="response") >=0.50, 1, 0), 
                             ifelse(all.offense.game.logs.classic.train$Binary.Outcome == "Won", 1, 0))
      train.AUC[which(year == years),1] <- performance(pred.obj, "auc")@y.values[[1]]
      
      
      
      if (week != "MAX"){
        test.pred <- predict(glm.classic.obj, 
                             newdata=all.offense.game.logs.classic.test[,c(needed.colnames, "Homefield")],
                             type="response")
        test.prob.err[which(year == years),1] <- mean(abs(test.pred - ifelse(all.offense.game.logs.classic.test$Binary.Outcome == "Won", 1, 0)))
        test.acc[which(year == years),1] <- mean(ifelse(test.pred >=0.50, "Won", "Lost") == 
                                                   all.offense.game.logs.classic.test$Binary.Outcome)
        
        pred.obj <- prediction(ifelse(test.pred >=0.50, 1, 0), 
                               ifelse(all.offense.game.logs.classic.test$Binary.Outcome == "Won", 1, 0))
        test.AUC[which(year == years),1] <- performance(pred.obj, "auc")@y.values[[1]]
      }
      
      
      glm.adj.opp.obj <- glm(as.factor(Binary.Outcome)  ~ .,
                             data=all.offense.game.logs.adj.opp.train[,c("Binary.Outcome", needed.colnames, "Homefield")],
                             family="binomial")
      
      
      
      glm.adj.opp.obj
      train.prob.err[which(year == years),2] <- mean(abs(resid(glm.adj.opp.obj, type = "response")))
      train.acc[which(year == years),2] <- mean(ifelse(predict(glm.adj.opp.obj, type="response") >=0.50, "Won", "Lost") == all.offense.game.logs.classic.train$Binary.Outcome)
      
      pred.obj <- prediction(ifelse(predict(glm.adj.opp.obj, type="response") >=0.50, 1, 0), 
                             ifelse(all.offense.game.logs.classic.train$Binary.Outcome == "Won", 1, 0))
      train.AUC[which(year == years),2] <- performance(pred.obj, "auc")@y.values[[1]]
      
      
      if (week != "MAX"){
        test.pred <- predict(glm.adj.opp.obj, 
                             newdata=all.offense.game.logs.adj.opp.test[,c(needed.colnames, "Homefield")],
                             type="response")
        test.prob.err[which(year == years),2] <- mean(abs(test.pred - ifelse(all.offense.game.logs.adj.opp.test$Binary.Outcome == "Won", 1, 0)))
        test.acc[which(year == years),2] <- mean(ifelse(test.pred >=0.50, "Won", "Lost") == 
                                                   all.offense.game.logs.adj.opp.test$Binary.Outcome)
        
        pred.obj <- prediction(ifelse(test.pred >=0.50, 1, 0), 
                               ifelse(all.offense.game.logs.classic.test$Binary.Outcome == "Won", 1, 0))
        test.AUC[which(year == years),2] <- performance(pred.obj, "auc")@y.values[[1]]
      }
      
      
      glm.adj.opp.and.ballside.eff.obj <- glm(as.factor(Binary.Outcome) ~ .,
                                              data=all.offense.game.logs.adj.opp.and.ballside.eff.train[,c("Binary.Outcome", needed.colnames, "Homefield")],
                                              family="binomial")
      glm.adj.opp.and.ballside.eff.obj
      train.prob.err[which(year == years),3] <- mean(abs(resid(glm.adj.opp.and.ballside.eff.obj, type = "response")))
      train.acc[which(year == years),3] <- mean(ifelse(predict(glm.adj.opp.and.ballside.eff.obj, type="response") >=0.50, "Won", "Lost") == all.offense.game.logs.classic.train$Binary.Outcome)
      
      pred.obj <- prediction(ifelse(predict(glm.adj.opp.and.ballside.eff.obj, type="response") >=0.50, 1, 0), 
                             ifelse(all.offense.game.logs.classic.train$Binary.Outcome == "Won", 1, 0))
      train.AUC[which(year == years),3] <- performance(pred.obj, "auc")@y.values[[1]]
      
      if (week != "MAX"){
        test.pred <- predict(glm.adj.opp.and.ballside.eff.obj, 
                             newdata=all.offense.game.logs.adj.opp.and.ballside.eff.test[,c(needed.colnames, "Homefield")],
                             type="response")
        test.prob.err[which(year == years),3] <- mean(abs(test.pred - ifelse(all.offense.game.logs.adj.opp.and.ballside.eff.test$Binary.Outcome == "Won", 1, 0)))
        
        test.acc[which(year == years),3] <- mean(ifelse(test.pred>=0.50, "Won", "Lost") == 
                                                   all.offense.game.logs.adj.opp.and.ballside.eff.test$Binary.Outcome)
        
        pred.obj <- prediction(ifelse(test.pred >=0.50, 1, 0), 
                               ifelse(all.offense.game.logs.classic.test$Binary.Outcome == "Won", 1, 0))
        test.AUC[which(year == years),3] <- performance(pred.obj, "auc")@y.values[[1]]
      }
      
      
      
      glm.adj.opp.and.ballside.noneff.obj <- glm(as.factor(Binary.Outcome) ~ .,
                                                 data=all.offense.game.logs.adj.opp.and.ballside.noneff.train[,c("Binary.Outcome", needed.colnames, "Homefield")],
                                                 family="binomial")

      glm.adj.opp.and.ballside.noneff.obj
      train.prob.err[which(year == years),4] <- mean(abs(resid(glm.adj.opp.and.ballside.noneff.obj, type = "response")))
      train.acc[which(year == years),4] <- mean(ifelse(predict(glm.adj.opp.and.ballside.noneff.obj, type="response") >=0.50, "Won", "Lost") == all.offense.game.logs.classic.train$Binary.Outcome)
      
      pred.obj <- prediction(ifelse(predict(glm.adj.opp.and.ballside.noneff.obj, type="response") >=0.50, 1, 0), 
                             ifelse(all.offense.game.logs.classic.train$Binary.Outcome == "Won", 1, 0))
      train.AUC[which(year == years),4] <- performance(pred.obj, "auc")@y.values[[1]]
      
      if (week != "MAX"){
        test.pred <- predict(glm.adj.opp.and.ballside.noneff.obj, 
                             newdata=all.offense.game.logs.adj.opp.and.ballside.noneff.test[,c(needed.colnames, "Homefield")],
                             type="response")
        test.prob.err[which(year == years),4] <- mean(abs(test.pred - ifelse(all.offense.game.logs.adj.opp.and.ballside.noneff.test$Binary.Outcome == "Won", 1, 0)))
        
        test.acc[which(year == years),4] <- mean(ifelse(test.pred>=0.50, "Won", "Lost") == 
                                                   all.offense.game.logs.adj.opp.and.ballside.noneff.test$Binary.Outcome)
        
        pred.obj <- prediction(ifelse(test.pred >=0.50, 1, 0), 
                               ifelse(all.offense.game.logs.classic.test$Binary.Outcome == "Won", 1, 0))
        test.AUC[which(year == years),4] <- performance(pred.obj, "auc")@y.values[[1]]
      }
      
    }
    
  }
  
  
  
  if (outcome.type == "Score Differential"){
    
    if (week == "MAX"){
      print("Training RSE:")
      print(matrix(c(round(apply(train.RSE, 2, mean), 3), round(apply(train.RSE, 2, sd), 3)), nrow=2, byrow=T))
    }
    
    if (week != "MAX"){
      print("Testing RMSE:")
      print(matrix(c(round(apply(test.RMSE, 2, mean), 3), round(apply(test.RMSE, 2, sd), 3)), nrow=2, byrow=T))
    }
    
  }
  
  
  
  if (outcome.type == "Binary Outcome"){
    
    if (week == "MAX"){
      
      print("Training AUROC")
      print(matrix(c(round(apply(train.AUC, 2, mean), 3), round(apply(train.AUC, 2, sd), 3)), nrow=2, byrow=T))
      
      print("Training Accuracy")
      print(matrix(c(round(apply(train.acc, 2, mean), 3), round(apply(train.acc, 2, sd), 3)), nrow=2, byrow=T))
      
    } else {
      
      print("Test AUROC")
      print(matrix(c(round(apply(test.AUC, 2, mean), 3), round(apply(test.AUC, 2, sd), 3)), nrow=2, byrow=T))
      
      print("Test Accuracy")
      print(matrix(c(round(apply(test.acc, 2, mean), 3), round(apply(test.acc, 2, sd), 3)), nrow=2, byrow=T))
      
    }
  }
  
  
}
