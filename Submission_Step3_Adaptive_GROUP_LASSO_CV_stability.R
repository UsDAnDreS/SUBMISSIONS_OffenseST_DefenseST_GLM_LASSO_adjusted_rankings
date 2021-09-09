#########
## Variable selection via Adaptive Group LASSO
## (with gamma selected at previous step)
## using 20-fold Cross-Validation (with sparse lambda, AKA "lambda.1se", and a classic minimum-CV lambda, AKA "lambda.min")
## Running it 5 times (to alleviate randomness effect of CV procedure),
## keeping track of all estimates, to later evaluate their stability.
#########



require(tidyverse)
require(glmnet)
require(stringr)
require(plotrix)
require(caret)
require(splines)
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






## nfolds for CV
nfolds <- 20

## Number of CV runs for stability check on the same data
nruns <- 5


Data <- read.csv(paste0("Game_Logs/2017/",
                        ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                        ifelse(efficiency, "EFFICIENCY_", "FINAL_"),
                        "FULL_STATS_Offense&SpecialTeams.csv"))
colnames(Data)




if (GLM_type == "Gaussian"){
  main.stats <- c("Off_w_ST.Pts", 
                  "Off_w_ST.Yds",
                  "Off_w_ST.TD",
                  "Off_w_ST.Minus.Def_w_ST.Points.Margin",
                  "Off_w_ST.Minus.Def_w_ST.Yds.Margin",
                  "Off_w_ST.Minus.Def_w_ST.TD.Margin"
  )
  
}



stats.y <- select.col.names

stats.y <- paste0(stats.y, ".y")  # Adding the ".y" for modeling purposes


if (!efficiency) main.stats <- main.stats[1]



for (year in years){
  
  print(year)
  print(paste(IMPUTE.NONMAJOR.NA, efficiency, individual
  ))
  
  
  
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
  final.lasso.df.lambda.min <- final.lasso.df.lambda.1se <- NULL
  final.cv.err.df.lambda.min <- final.cv.err.df.lambda.1se <- NULL
  
  
  
  
  ###########
  ## Gaussian GLM
  ###########
  
  ## Obtaining the selected gamma values from Step 2
  all.gammas <-  read.csv(file=paste0(getwd(),"/GROUP_LASSO/Adaptive_LASSO/",
                                      "Gammas_",
                                      ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                      ifelse(efficiency, "EFFICIENCY_", ""),
                                      ifelse(tot_avg, ifelse(Off_w_ST.Yds.Avg, "OFF_W_ST_TOT_AVG_", "TOT_AVG_"), ""), 
                                      ifelse(individual, "INDIVIDUAL_", ""),
                                      GLM_type,"_", "BIC", "_", year, 
                                      ".csv"))
  
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
    
    stability.row.lambda.min <- stability.row.lambda.1se <- NULL
    all.coef.est.lambda.min <- all.coef.est.lambda.1se <- NULL
    all.cv.err.lambda.min <- all.cv.err.lambda.1se <- NULL
    
    for (i in 1:nruns){
      print(i)
      ## Guaranteeing folds are the same each time
      flds <- createFolds(y, k = nfolds, list = FALSE, returnTrain = FALSE)
      
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
      
      
      ## First stage of Adaptive Group LASSO: estimate the weights
      weights <- apply(matrix(gam.obj$coefficients[(ncol(x) - spline.df*length(stats.y) + 2):(ncol(x)+1)], nrow=spline.df),
                       2,
                       function(x) 1/sqrt(sum(x^2, na.rm=T)))
      
      
      ## Second stage of Adaptive Group LASSO: apply estimated weights to group penalties
      ## (along with CV to keep track of CV errors for later comparison of tot_avg=TRUE vs FALSE options)
      cv.lasso.obj <- cv.grpreg(x,y, 
                                group=group.ind-1,
                                foldid=flds,
                                penalty="grLasso",
                                family=tolower(GLM_type),
                                group.multiplier = weights^all.gammas[which(main.stats == stat), 2])
      
      
      plot(cv.lasso.obj$cve)
      
      lambda.min.ind <- which.min(cv.lasso.obj$cve)
      lambda.1se.ind <- which(cv.lasso.obj$cve <= cv.lasso.obj$cve[lambda.min.ind] + cv.lasso.obj$cvse[lambda.min.ind])[1]
      
      coef.est.lambda.min <- cv.lasso.obj$fit$beta[, lambda.min.ind]
      coef.est.lambda.1se <- cv.lasso.obj$fit$beta[, lambda.1se.ind]
      cv.err.lambda.min <- cv.lasso.obj$cve[lambda.min.ind]
      cv.err.lambda.1se <- cv.lasso.obj$cve[lambda.1se.ind]
      
      
      all.coef.est.lambda.min <- rbind(all.coef.est.lambda.min, 
                                       coef.est.lambda.min[(length(coef.est.lambda.min) - spline.df*length(stats.y) + 1):length(coef.est.lambda.min)])
      all.coef.est.lambda.1se <- rbind(all.coef.est.lambda.1se,
                                       coef.est.lambda.1se[(length(coef.est.lambda.1se) - spline.df*length(stats.y) + 1):length(coef.est.lambda.1se)])
      
      all.cv.err.lambda.min <- c(all.cv.err.lambda.min, cv.err.lambda.min)
      all.cv.err.lambda.1se <- c(all.cv.err.lambda.1se, cv.err.lambda.1se)
      
      if (is.null(stability.row.lambda.min)){
        stability.row.lambda.min <- ifelse(coef.est.lambda.min[(length(coef.est.lambda.min) - length(stats.y) + 1):length(coef.est.lambda.min)] !=0, 1, 0)
        stability.row.lambda.1se <- ifelse(coef.est.lambda.1se[(length(coef.est.lambda.1se) - length(stats.y) + 1):length(coef.est.lambda.1se)] !=0, 1, 0)
      } else {
        stability.row.lambda.min <- stability.row.lambda.min + ifelse(coef.est.lambda.min[(length(coef.est.lambda.min) - length(stats.y) + 1):length(coef.est.lambda.min)] !=0, 1, 0)
        stability.row.lambda.1se <- stability.row.lambda.1se + ifelse(coef.est.lambda.1se[(length(coef.est.lambda.1se) - length(stats.y) + 1):length(coef.est.lambda.1se)] !=0, 1, 0)
      }
    }
    
    rownames(all.coef.est.lambda.min) <- rownames(all.coef.est.lambda.1se) <- rep(stat, nrow(all.coef.est.lambda.1se))
    
    stability.row.lambda.min <- stability.row.lambda.min/nruns
    stability.row.lambda.1se <- stability.row.lambda.1se/nruns
    
    
    
    
    
    
    write.csv(all.coef.est.lambda.min,
              file=paste0(getwd(),"/GROUP_LASSO/Adaptive_LASSO/",
                          ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                          ifelse(efficiency, "EFFICIENCY_", ""),
                          ifelse(tot_avg, ifelse(Off_w_ST.Yds.Avg, "OFF_W_ST_TOT_AVG_", "TOT_AVG_"), ""),
                          ifelse(individual, "INDIVIDUAL_", ""),
                          "Stab_Estimates_", stat,"_", nruns,"-runs_",GLM_type,"_lambda.min_",nfolds, "-folds_",year, 
                          ".csv"))
    
    write.csv(all.coef.est.lambda.1se,
              file=paste0(getwd(),"/GROUP_LASSO/Adaptive_LASSO/",
                          ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                          ifelse(efficiency, "EFFICIENCY_", ""),
                          ifelse(tot_avg, ifelse(Off_w_ST.Yds.Avg, "OFF_W_ST_TOT_AVG_", "TOT_AVG_"), ""),
                          ifelse(individual, "INDIVIDUAL_", ""),
                          "Stab_Estimates_", stat,"_", nruns,"-runs_",GLM_type,"_lambda.1se_",nfolds, "-folds_",year, 
                          ".csv"))
    
    
    
    
    if (length(final.cv.err.df.lambda.min) > 0){
      
      final.cv.err.df.lambda.min <- rbind(final.cv.err.df.lambda.min,
                                          data.frame(t(c(Stat=stat,
                                                         all.cv.err.lambda.min))))
      
      final.cv.err.df.lambda.1se <- rbind(final.cv.err.df.lambda.1se,
                                          data.frame(t(c(Stat=stat,
                                                         all.cv.err.lambda.1se))))
      
    }
    if (length(final.cv.err.df.lambda.min) == 0){
      
      final.cv.err.df.lambda.min <- data.frame(t(c(Stat=stat,
                                                   all.cv.err.lambda.min)))
      
      final.cv.err.df.lambda.1se <- data.frame(t(c(Stat=stat,
                                                   all.cv.err.lambda.1se)))
    }
    
    
    
    }
  

  write.csv(final.cv.err.df.lambda.min,
            file=paste0(getwd(),"/GROUP_LASSO/Adaptive_LASSO/",
                        ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                        ifelse(efficiency, "EFFICIENCY_", ""),
                        ifelse(tot_avg, ifelse(Off_w_ST.Yds.Avg, "OFF_W_ST_TOT_AVG_", "TOT_AVG_"), ""),
                        ifelse(individual, "INDIVIDUAL_", ""),
                        "CV_MSE_",nruns,"-runs_",GLM_type,"_lambda.min_",nfolds, "-folds_",year, 
                        ".csv"))
  
  write.csv(final.cv.err.df.lambda.1se,
            file=paste0(getwd(),"/GROUP_LASSO/Adaptive_LASSO/",
                        ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                        ifelse(efficiency, "EFFICIENCY_", ""),
                        ifelse(tot_avg, ifelse(Off_w_ST.Yds.Avg, "OFF_W_ST_TOT_AVG_", "TOT_AVG_"), ""),
                        ifelse(individual, "INDIVIDUAL_", ""),
                        "CV_MSE_",nruns,"-runs_",GLM_type,"_lambda.1se_",nfolds, "-folds_",year, 
                        ".csv"))
  
}


