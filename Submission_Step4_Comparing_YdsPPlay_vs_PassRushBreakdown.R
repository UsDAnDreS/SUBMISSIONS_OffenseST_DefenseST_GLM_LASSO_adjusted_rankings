###########
## Comparing use of:
##  (tot_avg=TRUE)  - a single yards-per-play category, that doesn't distinguish between passes and rushes,
##      as opposed to
##  (tot_avg=FALSE) -  passing yards per attempt and rushing yards per attempt as separate features.
##
## Comparable across points & TD response statistical categories, 
##  but tot_avg=TRUE is much better in yards.
###########


require(tidyverse)
require(plotrix)

# T or F
tot_avg <- F          # Should we use the overall yards-per-play (TRUE), or break it up into Pass & Rush (FALSE)?



years <- 2009:2019
GLM_type <- "Gaussian"


## Whether to IMPUTE STATS that were ALWAYS UNAVAILABLE for NON-MAJOR TEAMS, 
## e.g. Sacks, Tackles, etc etc (mostly PURE DEFENSIVE PLAYS)
IMPUTE.NONMAJOR.NA <- TRUE


## Do we look at EFFICIENCY STATS (per-play, per-possession)?
efficiency <- T


## To avoid PERFECT/NEAR-PERFECT MULTICOLLINEARITY, 
## ALWAYS PICK THE "SUB-CATEGORIES" (e.g. instead of Tot.Yds, do Rush.Yds + Pass.Yds)
## If the GRAND category is "all that it takes", it WILL SHOW IN THE COEFFICIENTS 
## (e.g. BOTH POSITIVE or BOTH NEGATIVE)
individual <- T


Off_w_ST.Yds.Avg <- F   # SHOULD WE USE "OFF_W_ST.Yds.Avg"? Or just "Tot.Avg" BY THE OFFENSE?

if (tot_avg) {efficiency <- T; individual <- F; }

if (efficiency & !tot_avg) {individual <- T; Off_w_ST.Yds.Avg <- F}

if (!efficiency){ tot_avg <- F; Off_w_ST.Yds.Avg <- F}





nruns <- 5; nfolds <- 20
Data <- read.csv(paste0("GROUP_LASSO/LASSO_Estimates/", 
                        ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                        ifelse(efficiency, "EFFICIENCY_", ""),
                        ifelse(tot_avg, "TOT_AVG_", ""),
                        ifelse(individual, "INDIVIDUAL_", ""),
                        "Stability_", nruns, "-runs_", GLM_type, "_", "lambda.1se", "_", nfolds, "-folds", "_", "2018", ".csv"))

Data$Stat






all.lasso.lambda.types <- c("lambda.1se", "BIC")




######
### GROUP LASSO
######

lasso.consist.df <- list(NULL, NULL)
names(lasso.consist.df) <- all.lasso.lambda.types


for (year in years){
  
  for (criter in all.lasso.lambda.types){
    
    
    if (criter != "BIC"){
      
      select.mat <- as.matrix(read.csv(paste0("GROUP_LASSO/LASSO_Estimates/", 
                                              ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                              ifelse(efficiency, "EFFICIENCY_", ""),
                                              ifelse(tot_avg, "TOT_AVG_", ""),
                                              ifelse(individual, "INDIVIDUAL_", ""),
                                              "CV_MSE_",nruns,"-runs_",GLM_type,"_", criter, "_",nfolds, "-folds_",year, 
                                              ".csv")) %>% dplyr::select(-X, -Stat))
      rownames(select.mat) <- Data$Stat
    }
    
    if (criter == "BIC"){
      select.mat <- as.matrix(read.csv(paste0("GROUP_LASSO/LASSO_Estimates/", 
                                              ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                              ifelse(efficiency, "EFFICIENCY_", ""),
                                              ifelse(tot_avg, "TOT_AVG_", ""),
                                              ifelse(individual, "INDIVIDUAL_", ""),
                                              "CV_MSE_",
                                              GLM_type,"_BIC_", year, 
                                              ".csv")) %>% dplyr::select(-X, -Stat))
      rownames(select.mat) <- Data$Stat
    }
    
    
    if (is.null(lasso.consist.df[[criter]])){
      lasso.consist.df[[criter]] <- select.mat
      
    } else {
      lasso.consist.df[[criter]] <- cbind(lasso.consist.df[[criter]], select.mat)
    }
  }
  
}




######
### Adaptive GROUP LASSO
######

adaptivelasso.consist.df <- list(NULL, NULL)
names(adaptivelasso.consist.df) <- all.lasso.lambda.types


for (year in years){
  
  for (criter in all.lasso.lambda.types){
    
    
    if (criter != "BIC"){
      
      select.mat <- as.matrix(read.csv(paste0(getwd(),"/GROUP_LASSO/Adaptive_LASSO/", 
                                              ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                              ifelse(efficiency, "EFFICIENCY_", ""),
                                              ifelse(tot_avg, ifelse(Off_w_ST.Yds.Avg, "OFF_W_ST_TOT_AVG_", "TOT_AVG_"), ""),
                                              ifelse(individual, "INDIVIDUAL_", ""),
                                              "CV_MSE_",nruns,"-runs_",GLM_type,"_", criter, "_",nfolds, "-folds_",year, 
                                              ".csv")) %>% dplyr::select(-X, -Stat))
      rownames(select.mat) <- Data$Stat
      
      
      
      
    }
    
    if (criter == "BIC"){
      select.mat <- as.matrix(read.csv(paste0(getwd(),"/GROUP_LASSO/Adaptive_LASSO/", 
                                              ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                              ifelse(efficiency, "EFFICIENCY_", ""),
                                              ifelse(tot_avg, ifelse(Off_w_ST.Yds.Avg, "OFF_W_ST_TOT_AVG_", "TOT_AVG_"), ""),
                                              ifelse(individual, "INDIVIDUAL_", ""),
                                              "CV_MSE_",
                                              GLM_type,"_", criter, "_", year, 
                                              ".csv")) %>% dplyr::select(-X, -Stat))
      rownames(select.mat) <- Data$Stat
    }
    
    
    if (is.null(adaptivelasso.consist.df[[criter]])){
      adaptivelasso.consist.df[[criter]] <- select.mat
      
    } else {
      adaptivelasso.consist.df[[criter]] <- cbind(adaptivelasso.consist.df[[criter]], select.mat)
    }
  }
  
}






######
### NON-CONVEX GROUP PENALTY: SCAD
######

SCAD.consist.df <- list(NULL, NULL)
names(SCAD.consist.df) <- all.lasso.lambda.types


for (year in years){
  
  for (criter in all.lasso.lambda.types){
    
    
    if (criter != "BIC"){
      
      select.mat <- as.matrix(read.csv(paste0(getwd(),"/GROUP_LASSO/NonConvex/", 
                                              "SCAD", "_",
                                              ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                              ifelse(efficiency, "EFFICIENCY_", ""),
                                              ifelse(tot_avg, ifelse(Off_w_ST.Yds.Avg, "OFF_W_ST_TOT_AVG_", "TOT_AVG_"), ""),
                                              ifelse(individual, "INDIVIDUAL_", ""),
                                              "CV_MSE_",nruns,"-runs_",GLM_type,"_", criter, "_",nfolds, "-folds_",year, 
                                              ".csv")) %>% dplyr::select(-X, -Stat))
      rownames(select.mat) <- Data$Stat
      
      
      
      
    }
    
    if (criter == "BIC"){
      select.mat <- as.matrix(read.csv(paste0(getwd(),"/GROUP_LASSO/NonConvex/", 
                                              "SCAD", "_",
                                              ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                              ifelse(efficiency, "EFFICIENCY_", ""),
                                              ifelse(tot_avg, ifelse(Off_w_ST.Yds.Avg, "OFF_W_ST_TOT_AVG_", "TOT_AVG_"), ""),
                                              ifelse(individual, "INDIVIDUAL_", ""),
                                              "CV_MSE_",
                                              GLM_type,"_", criter, "_", year, 
                                              ".csv")) %>% dplyr::select(-X, -Stat))
      rownames(select.mat) <- Data$Stat
    }
    
    
    if (is.null(SCAD.consist.df[[criter]])){
      SCAD.consist.df[[criter]] <- select.mat
      
    } else {
      SCAD.consist.df[[criter]] <- cbind(SCAD.consist.df[[criter]], select.mat)
    }
  }
  
}





######
### NON-CONVEX GROUP PENALTY: MCP
######

MCP.consist.df <- list(NULL, NULL)
names(MCP.consist.df) <- all.lasso.lambda.types


for (year in years){
  
  for (criter in all.lasso.lambda.types){
    
    
    if (criter != "BIC"){
      
      select.mat <- as.matrix(read.csv(paste0(getwd(),"/GROUP_LASSO/NonConvex/", 
                                              "MCP", "_",
                                              ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                              ifelse(efficiency, "EFFICIENCY_", ""),
                                              ifelse(tot_avg, ifelse(Off_w_ST.Yds.Avg, "OFF_W_ST_TOT_AVG_", "TOT_AVG_"), ""),
                                              ifelse(individual, "INDIVIDUAL_", ""),
                                              "CV_MSE_",nruns,"-runs_",GLM_type,"_", criter, "_",nfolds, "-folds_",year, 
                                              ".csv")) %>% dplyr::select(-X, -Stat))
      rownames(select.mat) <- Data$Stat
      
      
      
      
    }
    
    if (criter == "BIC"){
      select.mat <- as.matrix(read.csv(paste0(getwd(),"/GROUP_LASSO/NonConvex/", 
                                              "MCP", "_",
                                              ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                              ifelse(efficiency, "EFFICIENCY_", ""),
                                              ifelse(tot_avg, ifelse(Off_w_ST.Yds.Avg, "OFF_W_ST_TOT_AVG_", "TOT_AVG_"), ""),
                                              ifelse(individual, "INDIVIDUAL_", ""),
                                              "CV_MSE_",
                                              GLM_type,"_", criter, "_", year, 
                                              ".csv")) %>% dplyr::select(-X, -Stat))
      rownames(select.mat) <- Data$Stat
    }
    
    
    if (is.null(MCP.consist.df[[criter]])){
      MCP.consist.df[[criter]] <- select.mat
      
    } else {
      MCP.consist.df[[criter]] <- cbind(MCP.consist.df[[criter]], select.mat)
    }
  }
  
}





####
# Making the final consistency AVERAGE CV MSE per METHOD matrix 
####



final.consist.df <- NULL

for (lambda.type in all.lasso.lambda.types){
  final.consist.df <- cbind(final.consist.df, apply(lasso.consist.df[[lambda.type]], 1, mean))
  final.consist.df <- cbind(final.consist.df, apply(elasticnet.consist.df[[lambda.type]], 1, mean))
  final.consist.df <- cbind(final.consist.df, apply(adaptivelasso.consist.df[[lambda.type]], 1, mean))
  final.consist.df <- cbind(final.consist.df, apply(SCAD.consist.df[[lambda.type]], 1, mean))
  final.consist.df <- cbind(final.consist.df, apply(MCP.consist.df[[lambda.type]], 1, mean))
  
}


for (criter in all.regsubsets.crit){
  final.consist.df <- cbind(final.consist.df, apply(regsubsets.consist.df[[criter]], 1, mean))
}


colnames(final.consist.df) <- c(rep(all.lasso.lambda.types, 4))



rownames(final.consist.df) <- gsub(".Margin", "", gsub("_w_ST", "", Data$Stat))


print(round(t(final.consist.df),2))

print(round(apply(t(final.consist.df), 2, mean), 2))
print(round(apply(t(final.consist.df), 2, sd), 2))






################
### TOT_AVG = TRUE
################

#            Off.Pts  Off.Yds Off.TD Off.Minus.Def.Points Off.Minus.Def.Yds Off.Minus.Def.TD
# lambda.1se  128.75 11296.19   2.77               137.49          11709.60             2.98
# BIC         129.80 11373.43   2.79               138.90          11797.89             3.01
# lambda.1se  130.38 11317.70   2.80               139.66          11765.09             3.02
# BIC         130.22 11318.41   2.80               139.63          11779.65             3.02
# lambda.1se  129.44 11380.42   2.76               137.23          11799.18             2.99
# BIC         134.40 11329.14   2.76               137.10          11698.34             2.98
# lambda.1se  129.03 11222.36   2.76               137.21          11682.20             2.98
# BIC         128.72 11215.00   2.76               136.78          11659.05             2.98


################
### TOT_AVG = FALSE
################

#            Off.Pts  Off.Yds Off.TD Off.Minus.Def.Points Off.Minus.Def.Yds Off.Minus.Def.TD
# lambda.1se  128.58 11472.65   2.77               137.26          11897.92             2.98
# BIC         129.57 11557.22   2.79               138.72          12008.65             3.01
# lambda.1se  130.07 11562.84   2.80               139.25          12019.77             3.02
# BIC         129.83 11562.57   2.80               139.25          12017.03             3.01
# lambda.1se  131.09 11686.69   2.77               137.05          12359.50             2.98
# BIC         131.32 11849.15   2.77               136.94          13969.85             2.98
# lambda.1se  130.84 11773.35   2.77               137.05          12322.59             2.98
# BIC         128.72 11679.30   2.76               136.56          12030.78             2.98



##### MEAN SUMMARIES for EACH STAT:

### TOT_AVG = TRUE

# > print(round(apply(t(final.consist.df[-7,]), 2, mean), 2))
# Off.Pts              Off.Yds               Off.TD Off.Minus.Def.Points    Off.Minus.Def.Yds     Off.Minus.Def.TD 
#  130.09             11306.58                 2.78               138.00             11736.37                 3.00 
# > print(round(apply(t(final.consist.df[-7,]), 2, sd), 2))
# Off.Pts              Off.Yds               Off.TD Off.Minus.Def.Points    Off.Minus.Def.Yds     Off.Minus.Def.TD 
#    1.85                61.28                 0.02                 1.20                55.42                 0.02 


### TOT_AVG = FALSE

# > print(round(apply(t(final.consist.df[-7,]), 2, mean), 2))
# Off.Pts              Off.Yds               Off.TD Off.Minus.Def.Points    Off.Minus.Def.Yds     Off.Minus.Def.TD 
#  130.00             11642.97                 2.78               137.76             12328.26                 2.99 
# > print(round(apply(t(final.consist.df[-7,]), 2, sd), 2))
# Off.Pts              Off.Yds               Off.TD Off.Minus.Def.Points    Off.Minus.Def.Yds     Off.Minus.Def.TD 
#    1.04               126.48                 0.02                 1.12               682.82                 0.02



### We see how:
##      - For POINTS and TD-RELATED STATS, the performances are NEARLY IDENTICAL (especially accounting for STANDARD DEVIATIONS of CV MSE values ACROSS YEARS)
##      - For YARDS, the TOT_AVG is a CLEARLY BETTER CHOICE, OUTPERFORMING SEVERAL STANDARD DEVIATIONS
##
## HENCE, go with TOT_AVG !!!
