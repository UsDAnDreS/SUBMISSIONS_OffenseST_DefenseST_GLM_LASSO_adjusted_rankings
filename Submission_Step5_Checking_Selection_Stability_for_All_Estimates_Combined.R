########
## Doing an equally weighted combination of:
##  - Group LASSO,
##  - Adaptive Group LASSO, 
##  - SCAD Group penalty,
##  - MCP Group penalty 
##
## Each method gets a weight of 1/4,
## while each tuning selection method (BIC or lambda.1se) within it gets 1/2.
#########


require(tidyverse)
require(plotrix)



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




nruns <- 5; nfolds <- 20
Data <- read.csv(paste0("GROUP_LASSO/LASSO_Estimates/",
                        ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                        ifelse(efficiency, "EFFICIENCY_", ""),
                        ifelse(tot_avg, ifelse(Off_w_ST.Yds.Avg, "OFF_W_ST_TOT_AVG_", "TOT_AVG_"), ""),                             
                        ifelse(individual, "INDIVIDUAL_", ""),
                        "Stability_", nruns, "-runs_", GLM_type, "_", "lambda.1se", "_", nfolds, "-folds", "_", "2018", ".csv"))

Data$Stat



all.lasso.lambda.types <- c("lambda.1se", "BIC")



## Group LASSO

lasso.consist.df <- lasso.rank.df <-  NULL

for (lambda.type in all.lasso.lambda.types){
  
  select.mat <- as.matrix(read.csv(paste0("GROUP_LASSO/LASSO_Estimates/", 
                                          "STABILITY_ACROSS_YEARS_",
                                          ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                          ifelse(efficiency, "EFFICIENCY_", ""),
                                          ifelse(tot_avg, ifelse(Off_w_ST.Yds.Avg, "OFF_W_ST_TOT_AVG_", "TOT_AVG_"), ""),                             
                                          ifelse(individual, "INDIVIDUAL_", ""),
                                          GLM_type, "_", lambda.type, ".csv")) %>% dplyr::select(-X))
  
  rank.mat <- t(apply(-select.mat, 1, function(x) rank(x#, ties.method = "min"
  )))
  
  if (!efficiency){
    select.mat <- select.mat[1,]
    rank.mat <- rank.mat[1,]
  }
  
  if (is.null(lasso.consist.df)){
    lasso.consist.df <- select.mat
    lasso.rank.df <- rank.mat
    
  } else {
    lasso.consist.df <- lasso.consist.df + select.mat
    lasso.rank.df <- lasso.rank.df + rank.mat
  }
}






## ADAPTIVE LASSO

adaptivelasso.consist.df <- adaptivelasso.rank.df <-  NULL

for (lambda.type in all.lasso.lambda.types){
  
  select.mat <- as.matrix(read.csv(paste0("GROUP_LASSO/Adaptive_LASSO/", 
                                          "STABILITY_ACROSS_YEARS_",
                                          ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                          ifelse(efficiency, "EFFICIENCY_", ""),
                                          ifelse(tot_avg, ifelse(Off_w_ST.Yds.Avg, "OFF_W_ST_TOT_AVG_", "TOT_AVG_"), ""),                             
                                          ifelse(individual, "INDIVIDUAL_", ""),
                                          GLM_type, "_", lambda.type, ".csv")) %>% dplyr::select(-X))
  
  rank.mat <- t(apply(-select.mat, 1, function(x) rank(x#, ties.method = "min"
  )))
  
  if (is.null(adaptivelasso.consist.df)){
    adaptivelasso.consist.df <- select.mat
    adaptivelasso.rank.df <- rank.mat
    
  } else {
    adaptivelasso.consist.df <- adaptivelasso.consist.df + select.mat
    adaptivelasso.rank.df <- adaptivelasso.rank.df + rank.mat
  }
}






## NON-CONVEX: SCAD

nonconvex.SCAD.consist.df <- nonconvex.SCAD.rank.df <-  NULL

for (lambda.type in all.lasso.lambda.types){
  
  select.mat <- as.matrix(read.csv(paste0("GROUP_LASSO/NonConvex/", 
                                          "SCAD_",
                                          "STABILITY_ACROSS_YEARS_",
                                          ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                          ifelse(efficiency, "EFFICIENCY_", ""),
                                          ifelse(tot_avg, ifelse(Off_w_ST.Yds.Avg, "OFF_W_ST_TOT_AVG_", "TOT_AVG_"), ""),                             
                                          ifelse(individual, "INDIVIDUAL_", ""),
                                          GLM_type, "_", lambda.type, ".csv")) %>% dplyr::select(-X))
  
  rank.mat <- t(apply(-select.mat, 1, function(x) rank(x#, ties.method = "min"
  )))
  
  if (is.null(nonconvex.SCAD.consist.df)){
    nonconvex.SCAD.consist.df <- select.mat
    nonconvex.SCAD.rank.df <- rank.mat
    
  } else {
    nonconvex.SCAD.consist.df <- nonconvex.SCAD.consist.df + select.mat
    nonconvex.SCAD.rank.df <- nonconvex.SCAD.rank.df + rank.mat
  }
}







## NON-CONVEX: MCP

nonconvex.MCP.consist.df <- nonconvex.MCP.rank.df <-  NULL

for (lambda.type in all.lasso.lambda.types){
  
  select.mat <- as.matrix(read.csv(paste0("GROUP_LASSO/NonConvex/", 
                                          "MCP_",
                                          "STABILITY_ACROSS_YEARS_",
                                          ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                          ifelse(efficiency, "EFFICIENCY_", ""),
                                          ifelse(tot_avg, ifelse(Off_w_ST.Yds.Avg, "OFF_W_ST_TOT_AVG_", "TOT_AVG_"), ""),                             
                                          ifelse(individual, "INDIVIDUAL_", ""),
                                          GLM_type, "_", lambda.type, ".csv")) %>% dplyr::select(-X))
  
  rank.mat <- t(apply(-select.mat, 1, function(x) rank(x#, ties.method = "min"
  )))
  
  if (is.null(nonconvex.MCP.consist.df)){
    nonconvex.MCP.consist.df <- select.mat
    nonconvex.MCP.rank.df <- rank.mat
    
  } else {
    nonconvex.MCP.consist.df <- nonconvex.MCP.consist.df + select.mat
    nonconvex.MCP.rank.df <- nonconvex.MCP.rank.df + rank.mat
  }
}






####
# Making the final consistency matrix, GIVING EACH METHOD - LASSO, STEPWISE, REGSUBSETS - EQUAL WEIGHT
####

if (efficiency){
  final.consist.df <- (1/4)* (lasso.consist.df/length(all.lasso.lambda.types) + 
                                adaptivelasso.consist.df/length(all.lasso.lambda.types) +
                                nonconvex.SCAD.consist.df/length(all.lasso.lambda.types) +
                                nonconvex.MCP.consist.df/length(all.lasso.lambda.types))
  
  
  final.rank.df <- (1/4)* (lasso.rank.df/length(all.lasso.lambda.types) + 
                             adaptivelasso.rank.df/length(all.lasso.lambda.types) +
                             nonconvex.SCAD.rank.df/length(all.lasso.lambda.types) +
                             nonconvex.MCP.rank.df/length(all.lasso.lambda.types))
} else {
  final.consist.df <- (1/4)* (lasso.consist.df/length(all.lasso.lambda.types) +
                                adaptivelasso.consist.df/length(all.lasso.lambda.types) +
                                nonconvex.SCAD.consist.df/length(all.lasso.lambda.types) +
                                nonconvex.MCP.consist.df/length(all.lasso.lambda.types)
  )
  
  
  final.rank.df <- (1/4)* (lasso.rank.df/length(all.lasso.lambda.types) + 
                             adaptivelasso.rank.df/length(all.lasso.lambda.types) +
                             nonconvex.SCAD.rank.df/length(all.lasso.lambda.types) +
                             nonconvex.MCP.rank.df/length(all.lasso.lambda.types)
  )
}



if (efficiency){
  rownames(final.consist.df) <- gsub(".Margin", "", gsub("_w_ST", "", Data$Stat))
  rownames(final.rank.df) <- gsub(".Margin", "", gsub("_w_ST", "", Data$Stat))
}



## Consistency matrix
print(round(t(final.consist.df),2))



## Heatmap (from the paper) 
mat.for.plot <- t(final.consist.df[c(1,4,3,6,2,5),])
mat.for.plot


rownames(mat.for.plot) <- str_remove(rownames(mat.for.plot), "\\.y")
rownames(mat.for.plot) <- str_replace(rownames(mat.for.plot), "\\.\\.", "\\.")
rownames(mat.for.plot) <- str_replace(rownames(mat.for.plot), "Conversion\\.\\.", "Conversion.Pct")
rownames(mat.for.plot) <- str_replace(rownames(mat.for.plot), "X1", "1")
rownames(mat.for.plot) <- str_replace(rownames(mat.for.plot), "X3", "3")
rownames(mat.for.plot) <- str_replace(rownames(mat.for.plot), "X4", "4")
rownames(mat.for.plot) <- str_replace(rownames(mat.for.plot), "Off\\.Pct", "Off.Pass.Comp.Pct")
rownames(mat.for.plot) <- str_replace(rownames(mat.for.plot), "Tackles\\.Sk", "Sacks")
rownames(mat.for.plot) <- str_replace(rownames(mat.for.plot), "Int\\.PD", "Pass.Defend")
rownames(mat.for.plot) <- str_replace(rownames(mat.for.plot), "Fumbles\\.FF", "Forced.Fumb")
rownames(mat.for.plot) <- str_replace(rownames(mat.for.plot), "Tot\\.TO\\.NonScoring", "NonScoring.TO")
rownames(mat.for.plot) <- str_replace(rownames(mat.for.plot), "Pts", "Points")
rownames(mat.for.plot) <- str_replace(rownames(mat.for.plot), "Off\\.Tot\\.Avg", "Off.Yds.Avg")
rownames(mat.for.plot) <-  str_replace(rownames(mat.for.plot), "\\.Avg", ".PPlay.Avg")
rownames(mat.for.plot) <-  str_replace(rownames(mat.for.plot), "\\_w\\_", "_")

colnames(mat.for.plot) <-  str_replace(colnames(mat.for.plot), "Pts", "Points")
colnames(mat.for.plot) <-  str_replace(colnames(mat.for.plot), "\\.Minus\\.Def", "")
colnames(mat.for.plot)[c(2,4,6)] <- paste0(colnames(mat.for.plot)[c(2,4,6)], ".Margin")

mat.for.plot

mat.for.plot <- mat.for.plot[c(nrow(mat.for.plot), 1:(nrow(mat.for.plot)-1)),]
mat.for.plot


# width = 430, height = 550 
## (GOTTA CROP AFTERWARDS THOUGH)


par(mar = c(5.5, 13.5, 4.1, 2.1))

color2D.matplot(mat.for.plot,
                cs1=c(0,1),cs2=c(1,0),cs3=c(1,0),
                show.legend=F,
                show.values=2,
                xlab='',
                ylab='',
                axes=F)
par(las=2)
staxlab(1,at=c(1:ncol(mat.for.plot))-0.25,labels=colnames(mat.for.plot),
        srt=45)
par(las=1)
axis(2,at=c(nrow(mat.for.plot):1)-0.5,labels=rownames(mat.for.plot))


par(mar = c(5.1, 4.1, 4.1, 2.1))
