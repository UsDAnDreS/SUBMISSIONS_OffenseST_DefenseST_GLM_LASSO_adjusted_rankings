#########
## Creating matrices that reflect selection consistency of each feature
## for Non-Convex Group Penalty (SCAD or MCP), via BIC or CV selection.
#########


require(tidyverse)
require(plotrix)

penalty <- "SCAD"   # "SCAD" or "MCP"

## lambda.min? or lambda.1se? or BIC?
lambda.type <- "BIC"

## Proportion out of 5 CV runs that indicates consistent selection of a feature.
lambda.thresh <- 0.8





# spline df
spline.df <- 4

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


tot_avg <- T          # Should we use the overall yards-per-play (TRUE), or break it up into Pass & Rush (FALSE)?
Off_w_ST.Yds.Avg <- F   # SHOULD WE USE "OFF_W_ST.Yds.Avg"? Or just "Tot.Avg" BY THE OFFENSE?

if (tot_avg) {efficiency <- T; individual <- F; }

if (efficiency & !tot_avg) {individual <- T; Off_w_ST.Yds.Avg <- F}

if (!efficiency){ tot_avg <- F; Off_w_ST.Yds.Avg <- F}






## number of folds in CV
nfolds <- 20

## number of runs to check CV stability
nruns <- 5


Data <- read.csv(paste0("GROUP_LASSO/NonConvex/", 
                        penalty, "_",
                        ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                        ifelse(efficiency, "EFFICIENCY_", ""),
                        ifelse(tot_avg, ifelse(Off_w_ST.Yds.Avg, "OFF_W_ST_TOT_AVG_", "TOT_AVG_"), ""),                   
                        ifelse(individual, "INDIVIDUAL_", ""),
                        "Stability_", nruns, "-runs_", GLM_type, "_", lambda.type, "_", nfolds, "-folds", "_", "2018", ".csv"))

Data$Stat


all.purities <- all.consist <- NULL


for (stat in Data$Stat){
  stat.df <- NULL
  for (year in years){
    print(year)
    
    if (lambda.type %in% c("lambda.min", "lambda.1se")){
      
      
      interm.df <- read.csv(paste0("GROUP_LASSO/NonConvex/", 
                                   penalty, "_",
                                   ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                   ifelse(efficiency, "EFFICIENCY_", ""),
                                   ifelse(tot_avg, ifelse(Off_w_ST.Yds.Avg, "OFF_W_ST_TOT_AVG_", "TOT_AVG_"), ""),
                                   ifelse(individual, "INDIVIDUAL_", ""),
                                   "Stab_Estimates_",stat,"_", nruns, "-runs_", GLM_type, "_", lambda.type, "_", nfolds, "-folds", "_", year, ".csv"))
      
      interm.df <- interm.df[, c(1, 2+spline.df*(0:(ncol(Data) - 3)))]
      colnames(interm.df)[-1] <- colnames(Data)[-c(1,2)]
      

      # As a measure of stability, calculate the proportion of times it was non-zero.
      
      interm.df[,-1] <- abs(interm.df[,-1])
      
      
      if (length(stat.df) != 0) stat.df <- rbind(stat.df, 
                                                 data.frame(t(apply(sign(interm.df[,-1]), 2, 
                                                                    function(x){
                                                                      z <- table(x);
                                                                      if (names(which.max(z))[1] == "0"){
                                                                        ifelse(length(z) == 1, 
                                                                               0, 
                                                                               as.numeric(names(which.max(z[names(z) != "0"]))[1])*mean(x == as.numeric(names(which.max(z[names(z) != "0"]))[1]))) 
                                                                      } else {
                                                                        as.numeric(names(which.max(z))[1])*mean(x == as.numeric(names(which.max(z))[1]))
                                                                      }}))))
      
      if (length(stat.df) == 0) stat.df <- data.frame(t(apply(sign(interm.df[,-1]), 2, function(x){
        z <- table(x);
        if (names(which.max(z))[1] == "0"){
          ifelse(length(z) == 1, 
                 0, 
                 as.numeric(names(which.max(z[names(z) != "0"]))[1])*mean(x == as.numeric(names(which.max(z[names(z) != "0"]))[1]))) 
        } else {
          as.numeric(names(which.max(z))[1])*mean(x == as.numeric(names(which.max(z))[1]))
        }})))
      
    }
    
    if (lambda.type %in% c("BIC")){
      interm.df <- read.csv(paste0("GROUP_LASSO/NonConvex/", 
                                   penalty, "_",
                                   ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                   ifelse(efficiency, "EFFICIENCY_", ""),
                                   ifelse(tot_avg, ifelse(Off_w_ST.Yds.Avg, "OFF_W_ST_TOT_AVG_", "TOT_AVG_"), ""),                    
                                   ifelse(individual, "INDIVIDUAL_", ""),
                                   GLM_type, "_", lambda.type, "_", year, ".csv"))
      
      interm.df$X <- NULL
      
      interm.df <- interm.df[, c(1, 2+spline.df*(0:(ncol(Data) - 3)))]
      colnames(interm.df)[-1] <- colnames(Data)[-c(1,2)]
      
      interm.df[,-1] <- abs(interm.df[,-1])
      
      if (length(stat.df) != 0) stat.df <- rbind(stat.df, interm.df %>% filter(Stat == stat) %>% dplyr::select(-Stat))
      if (length(stat.df) == 0) stat.df <- interm.df %>% filter(Stat == stat) %>% dplyr::select(-Stat)
    }
    
  }

  
  if (lambda.type %in% c("lambda.min", "lambda.1se")){
    
    stat.df <- apply(stat.df, 2, function(x) ifelse(abs(x) >= lambda.thresh, sign(x), 0))
    
    
    color2D.matplot(stat.df,
                    cs1=c(0,1),cs2=c(1,0),cs3=c(1,0),
                    show.legend=T,
                    show.values=2,
                    xlab='',
                    ylab='',
                    axes=F,
                    main=paste0(stat, " STABILITY"))
    par(las=2)
    axis(1,at=c(1:ncol(stat.df))-0.5,labels=colnames(stat.df))
    par(las=1)
    axis(2,at=c(nrow(stat.df):1)-0.5,labels=years)
    
  }
  
  
  
  if (lambda.type %in% c("BIC")){
    
    color2D.matplot(
      sign(stat.df),
      cs1=c(0,1),cs2=c(1,0),cs3=c(1,0),
      show.legend=T,
      show.values=2,
      xlab='',
      ylab='',
      axes=F,
      main=paste0(stat, " STABILITY"))
    par(las=2)
    axis(1,at=c(1:ncol(stat.df))-0.5,labels=colnames(stat.df))
    par(las=1)
    axis(2,at=c(nrow(stat.df):1)-0.5,labels=years)
    
    
  }
  
  
  
  ## The PROPORTION (ACROSS THE YEARS) of MAX SAME-SIGN SELECTION for each PREDICTOR
  all.consist <- rbind(all.consist,
                       data.frame(round(t(apply(sign(stat.df), 2, function(x){
                         z <- table(x);
                         if (names(which.max(z))[1] == "0"){
                           ifelse(length(z) == 1, 
                                  0, 
                                  sign(as.numeric(names(which.max(z[names(z) != "0"]))[1]))*mean(x == as.numeric(names(which.max(z[names(z) != "0"]))[1])))
                         } else {
                           sign(as.numeric(names(which.max(z))[1]))*mean(x == as.numeric(names(which.max(z))[1]))
                         }})), 2)))
  
  
  
  ## Mean "purity" scores
  all.purities <- c(all.purities, 
                    sum(apply(sign(stat.df), 2, 
                              function(x){ 
                                all.props <- table(x)/length(x);
                                return(-sum(all.props*log(all.props)))
                              })))
  
}


print(mean(all.purities))

rownames(all.consist) <- gsub(".Margin", "", gsub("_w_ST", "", Data$Stat))
print(t(all.consist))



write.csv(all.consist,
          paste0("GROUP_LASSO/NonConvex/", 
                 penalty, "_",
                 "STABILITY_ACROSS_YEARS_",
                 ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                 ifelse(efficiency, "EFFICIENCY_", ""),
                 ifelse(tot_avg, ifelse(Off_w_ST.Yds.Avg, "OFF_W_ST_TOT_AVG_", "TOT_AVG_"), ""),
                 ifelse(individual, "INDIVIDUAL_", ""),
                 GLM_type, "_", lambda.type, ".csv"))

Data$Stat
