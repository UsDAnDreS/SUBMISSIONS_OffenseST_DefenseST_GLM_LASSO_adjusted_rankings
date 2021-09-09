#########
## Variable selection via Group LASSO
## using Bayesian Information Criterion (BIC)
#########


dir.create("GROUP_LASSO/LASSO_Estimates")


require(tidyverse)
require(glmnet)
require(stringr)
require(plotrix)
require(glmnetUtils)
require(splines)
require(caret)
require(grpreg)


## DF for SPLINES
spline.df <- 4


years <- 2009:2019
GLM_type <- "Gaussian"

## Whether to IMPUTE STATS that were ALWAYS UNAVAILABLE for NON-MAJOR TEAMS, 
## e.g. Sacks, Tackles, etc etc (mostly PURE DEFENSIVE PLAYS)
IMPUTE.NONMAJOR.NA <- TRUE



## Do we do:
##    - ALL THE STATS (IN THEIR ORIGINAL STATE), e.g. TOTAL YARDS, TOTAL PUNTS, TOTAL POINTS PER GAME, along with RUSH.AVG, PASS.AVG etc etc
##    - Or ONLY "EFFICIENCY" VERSIONS, such as 
##            * "PER-POSSESSION" (for "POSSESSION-ENDING" STATS such as POINTS, TDs, TOs etc)
##            * "PER-PLAY" (for "MORE-THAN-ONCE-IN-A-POSSESSION" STATS such as PASS-YDS, RUSH-YDS, TOT YDS, SACKS, TACKLES, 1ST DOWNS, PASSES DEFENDED, QB HURR etc etc)
efficiency <- T



## To avoid PERFECT/NEAR-PERFECT MULTICOLLINEARITY, 
## ALWAYS PICK THE "SUB-CATEGORIES" (e.g. instead of Tot.Yds, do Rush.Yds + Pass.Yds)
## If the GRAND category is "all that it takes", it WILL SHOW IN THE COEFFICIENTS 
## (e.g. BOTH POSITIVE or BOTH NEGATIVE)
individual <- T


tot_avg <- T          # Should we use the overall yards-per-play (TRUE), or break it up into Pass & Rush (FALSE)?
Off_w_ST.Yds.Avg <- F   # SHOULD WE USE "OFF_W_ST.Yds.Avg"? Or just "Tot.Avg" BY THE OFFENSE?

if (tot_avg) {efficiency <- T; individual <- F; }

if (efficiency & !tot_avg) {individual <- T; Off_w_ST.Yds.Avg <- F}

if (!efficiency){ tot_avg <- F; Off_w_ST.Yds.Avg <- F}




all.criters <- c("BIC")



Data <- read.csv(paste0("Game_Logs/2017/",
                        ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                        ifelse(efficiency, "EFFICIENCY_", "FINAL_"),
                        "FULL_STATS_Offense&SpecialTeams.csv"))
colnames(Data)





## The main stats to ADJUST (used as RESPONSE):

if (GLM_type == "Gaussian"){
  main.stats <- c("Off_w_ST.Pts", 
                  "Off_w_ST.Yds",
                  "Off_w_ST.TD",
                  "Off_w_ST.Minus.Def_w_ST.Points.Margin",
                  "Off_w_ST.Minus.Def_w_ST.Yds.Margin",
                  "Off_w_ST.Minus.Def_w_ST.TD.Margin"
  )
  
}



if (!efficiency){
  load(file= paste0(ifelse(individual, "INDIVIDUAL_", "EVERYTHING_"), "SELECT_COL_NAMES.RData"))
} else {
  load(file=paste0("EFFICIENCY_", ifelse(individual, "INDIVIDUAL_", ifelse(Off_w_ST.Yds.Avg, "OFF_W_ST_TOT_AVG_", "TOT_AVG_")), "SELECT_COL_NAMES.RData"))
}




for (criter in all.criters){
  
  stats.y <- select.col.names
  
  stats.y <- paste0(stats.y, ".y")  # Adding the ".y" for modeling purposes
  
  
  for (year in years){
    print(paste0("TOT_AVG: ", tot_avg))
    print(paste(IMPUTE.NONMAJOR.NA,
                efficiency, individual,
                criter))
    print(year)
    
    Data <- read.csv(paste0("Game_Logs/",year,"/",
                            ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                            ifelse(efficiency, "EFFICIENCY_", "FINAL_"),
                            "FULL_STATS_Offense&SpecialTeams.csv"))
    
    Data.other <- read.csv(paste0("Game_Logs/",year,"/",
                                  ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                  ifelse(efficiency, "EFFICIENCY_", "FINAL_"),
                                  "FULL_STATS_Defense&SpecialTeams.csv"))
    
    
    Data$X <- Data.other$X <- NULL
    Data$Game.ID <- Data.other$Game.ID <- NULL
    
    
    full.df <- Data %>% full_join(Data.other, by=c("Team", "Opponent", "Date", "Homefield"))
    
    ####################
    ####################
    ## GLM ADJUSTMENT ##
    ####################
    ####################
    
    # Making the contrasts for sum alpha_i =0,  sum beta_j = 0.
    options(contrasts = rep("contr.sum", 2))
    n.teams <- length(unique(Data$Team))
    contr.sum(n.teams)
    
    
    
    
    ## Setting "Homefield" to be 0-0 if "N", 1-0 if "Home" (or " "), 0-1 if "Away" (or "@")
    full.df$Team <- factor(full.df$Team)
    full.df$Opponent <- factor(full.df$Opponent)
    full.df$Homefield <- factor(full.df$Homefield, levels=c("","N","@"))
    
    contrasts(full.df$Homefield) <- "contr.treatment"
    
    contrasts(full.df$Team)
    contrasts(full.df$Opponent)
    contrasts(full.df$Homefield)
    
    
    ## Making Homefield a NUMERIC VARIABLE to be modeled with SINGLE PARAMETER:
    ##  0 - Home, 1- Neutral, 2 - Away
    full.df$Homefield012 <- as.numeric(full.df$Homefield)-1
    
    
    ## Object containing LASSO estimates
    final.lasso.df <- NULL
    final.cv.err.df <- NULL
    
    
    ###########
    ## Gaussian GLM
    ###########
    
    if (!efficiency) main.stats <- main.stats[1]
    
    
    for (stat in main.stats){
      
      print(stat)
      
      lm.obj.hfield <- lm(full.df[,paste(stat,".x",sep="")] ~ .,
                          data=data.frame(full.df[,c("Team","Opponent", "Homefield012")],
                                          full.df[,stats.y]))
      lm.obj.hfield
      
      
      ## Getting indices of NA-containing rows (hence those dropped from the linear model)
      na.ind <- which(apply(data.frame(full.df[,paste(stat,".x",sep="")],
                                       full.df[,c("Team","Opponent", "Homefield012")], 
                                       full.df[,stats.y]), 
                            1,
                            function(x) mean(is.na(x)) > 0))
      
      
      x <- model.matrix(lm.obj.hfield)[,-1]   # Disposing of intercept column (it's automatically included with "intercept=TRUE" when fitting LASSO)
      print(paste0("Rows w/ NAs: " ,dim(full.df)[1]))
      print(paste0("Rows w/out NAs: ", dim(x)[1]))

      if (length(na.ind) == 0) y <- full.df[, paste(stat,".x",sep="")]
      if (length(na.ind) >0) y <- full.df[-na.ind, paste(stat,".x",sep="")]
      
      
      ## For reproducibility
      set.seed(1)
      
      ## Guaranteeing folds are the same each time
      flds <- createFolds(y, k = 10, list = FALSE, returnTrain = FALSE)
      
      
      
      if (length(na.ind) == 0){
        gam.obj <- lm(as.formula(paste0(paste(stat,".x",sep=""), "~ Team + Opponent + Homefield012 +", paste0(paste("ns(",stats.y, paste0(", df=", spline.df, ")"), sep="", collapse="+")), 
                                        collapse="")),
                      data=data.frame(full.df[,c("Team","Opponent", "Homefield012")], 
                                      full.df[,c(stats.y, paste(stat,".x",sep=""))]))
        
      } else {
        gam.obj <- lm(as.formula(paste0(paste(stat,".x",sep=""), "~ Team + Opponent + Homefield012 +", paste0(paste("ns(",stats.y, paste0(", df=", spline.df, ")"), sep="", collapse="+")), 
                                        collapse="")),
                      data=data.frame(full.df[-na.ind,c("Team","Opponent", "Homefield012")], 
                                      full.df[-na.ind,c(stats.y, paste(stat,".x",sep=""))]))
        
      }
      
      
      x <- model.matrix(gam.obj)[,-1]
      
      ## Defining parameter groups for group penalty, 
      ## specifically grouping spline parameters for the same complementary football feature together
      group.ind <- numeric(ncol(x))
      group.ind[(ncol(x) - spline.df*length(stats.y) + 1):ncol(x)] <- rep((1 + 1:length(stats.y)), rep(spline.df, length(stats.y)))
      group.ind[1:(ncol(x) - spline.df*length(stats.y))] <- 1
      group.ind
      
      
      ## Applying Group LASSO
      ## (along with CV to keep track of CV errors for later comparison of tot_avg=TRUE vs FALSE options)
      cv.lasso.obj <- cv.grpreg(x,y, 
                                group=group.ind-1,
                                penalty="grLasso",
                                family=tolower(GLM_type))
      

      
      ## Setting up AIC/BIC criteria calculations
      df <- cv.lasso.obj$fit$df
      n.obs <- length(y)
      
      if (GLM_type == "Gaussian"){
        tLL <- n.obs*log(cv.lasso.obj$fit$loss)
      } else {
        tLL <- cv.lasso.obj$glmnet.fit$nulldev - deviance(cv.lasso.obj$glmnet.fit)
      }
      
      
      
      if (criter == "AIC") crit.vals <- tLL + 2*df
      if (criter == "BIC") crit.vals <- tLL + log(n.obs)*df 
      if (criter == "AICc") crit.vals <- tLL + 2*df+2*df*(df+1)/(n.obs-df-1)
      
      
      coef.est <- numeric(length(stats.y)*spline.df)
      coef.est <- cv.lasso.obj$fit$beta[(nrow(cv.lasso.obj$fit$beta) - spline.df*length(stats.y) + 1):nrow(cv.lasso.obj$fit$beta), which.min(crit.vals)]

      cv.err <- cv.lasso.obj$cve[which.min(crit.vals)]
      
      
      
      if (length(final.lasso.df) > 0){
        final.lasso.df <- rbind(final.lasso.df, 
                                data.frame(t(c(Stat=stat, 
                                               coef.est))))
        if (efficiency){
          final.cv.err.df <- rbind(final.cv.err.df,
                                   data.frame(t(c(Stat=stat,
                                                  cv.err))))
        }
      }
      if (length(final.lasso.df) == 0){
        final.lasso.df <- data.frame(t(c(Stat=stat, 
                                         coef.est)))
        
        if (efficiency){
          final.cv.err.df <- data.frame(t(c(Stat=stat,
                                            cv.err)))
        }
      }
      
      }

    
    write.csv(final.lasso.df,
              file=paste0(getwd(),"/GROUP_LASSO/LASSO_Estimates/", 
                          ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                          ifelse(efficiency, "EFFICIENCY_", ""),
                          ifelse(tot_avg, ifelse(Off_w_ST.Yds.Avg, "OFF_W_ST_TOT_AVG_", "TOT_AVG_"), ""), 
                          ifelse(individual, "INDIVIDUAL_", ""),
                          GLM_type,"_", criter, "_", year, 
                          ".csv"))
    
    
    if (efficiency){
      write.csv(final.cv.err.df,
                file=paste0(getwd(),"/GROUP_LASSO/LASSO_Estimates/", 
                            ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                            ifelse(efficiency, "EFFICIENCY_", ""),
                            ifelse(tot_avg, ifelse(Off_w_ST.Yds.Avg, "OFF_W_ST_TOT_AVG_", "TOT_AVG_"), ""),
                            ifelse(individual, "INDIVIDUAL_", ""),
                            "CV_MSE_",
                            GLM_type,"_", criter, "_", year,
                            ".csv"))
    }
    
    
  }
  
}

