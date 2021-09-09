###########
## Calculating Intra-Class Correlation (ICC) for observations from the same games
## in the models with
##    - All complementary statistics (efficiency=F)
##    - Efficiency-only complementary statistics (efficiency=T)
##
## Virtually all of them end up <0.10 in magnitude, indicating weak within-game dependence.
###########


require(tidyverse)
require(splines)


efficiency <- T

if (efficiency){
  load(file="EFFICIENCY_TOT_AVG_SELECT_COL_NAMES.RData")
} else {
  load(file="INDIVIDUAL_SELECT_COL_NAMES.RData")
}


## DF for SPLINES
spline.df <- 4

IMPUTE.NONMAJOR.NA <- T
GLM_type <- "Gaussian"

years <- 2009:2019
options(dplyr.summarise.inform = FALSE)


if (efficiency){
main.stats <- c("Off_w_ST.Pts", 
                "Off_w_ST.Yds",
                "Off_w_ST.TD",
                "Off_w_ST.Minus.Def_w_ST.Points.Margin",
                "Off_w_ST.Minus.Def_w_ST.Yds.Margin",
                "Off_w_ST.Minus.Def_w_ST.TD.Margin")
} else {
  main.stats <- c("Off_w_ST.Pts", 
                  "Off_w_ST.Minus.Def_w_ST.Points.Margin")
}
  


side <- "Offense"


for (stat in main.stats){
  
  print(stat)

  stats.y <- paste0(select.col.names, ".y")
  
  
  for (year in years){
    # print(year)
    
    Data <- read.csv(paste0("Game_Logs/",year,"/", 
                            ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                            ifelse(efficiency, "EFFICIENCY_", "FINAL_"),
                            "FULL_STATS_Offense&SpecialTeams.csv"))
    
    Data.other <- read.csv(paste0("Game_Logs/",year,"/", 
                                  ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                  ifelse(efficiency, "EFFICIENCY_", "FINAL_"),
                                  "FULL_STATS_Defense&SpecialTeams.csv"))
    
    Data.stuff <- Data %>% full_join(Data.other, by=c("Team", "Opponent", "Homefield", "Date", "Game.ID"))
    
    Data.stuff$Homefield <- factor(Data.stuff$Homefield, levels=c("","N","@"))
    Data.stuff$Homefield <- as.numeric(Data.stuff$Homefield)-1
    
    
    ## 
    # options(contrasts = rep("contr.sum", 2))
    options(contrasts = rep("contr.treatment", 2))
    
    
    ## Getting indices of NA-containing rows (hence those dropped from the linear model)
    na.ind <- which(apply(data.frame(Data.stuff[,paste(stat,".x",sep="")],
                                     Data.stuff[,c("Team","Opponent", "Homefield")], 
                                     Data.stuff[, stats.y]), 
                          1,
                          function(x) mean(is.na(x)) > 0))
    na.game.ids <- unique(Data.stuff[na.ind,]$Game.ID)
    
    if (length(na.ind) > 0){
      Data.stuff <- Data.stuff %>% filter(!Game.ID %in% na.game.ids)
    }
    
    
    ###########
    ## In a PTS.X ~ TEAM + OPPONENT + HOMEFIELD + PTS.Y + EVERYTHING ELSE,  REGRESSION,
    ##    - Hardly any ERROR correlation FOR THE SAME GAME in either case (~ 0.05 to 0.10)
    ###########
    
    lm.obj <- lm(as.formula(paste0(paste(stat,".x",sep=""), "~ Team + Opponent + Homefield +", paste0(paste("ns(",stats.y, paste0(", df=", spline.df, ")"), sep="", collapse="+")), 
                                   collapse="")),
                 data=data.frame(Data.stuff[,c("Team","Opponent", "Homefield")], 
                                 Data.stuff[,c(stats.y, paste(stat,".x",sep=""))]))
    
    
    
    icc.df <- matrix(NA, nrow=length(c(unique(Data.stuff$Game.ID), na.game.ids)), ncol=2)
    shuffle.ind <- sample(nrow(Data.stuff)) 
    shuffled.df <- Data.stuff[shuffle.ind,]
    res.vec <- resid(lm.obj)[shuffle.ind]
    for (j in 1:nrow(shuffled.df)){
      if (is.na(icc.df[shuffled.df[j, "Game.ID"], 1])){
        icc.df[shuffled.df[j, "Game.ID"], 1] <- res.vec[j]
      } else {
        icc.df[shuffled.df[j, "Game.ID"], 2] <- res.vec[j]
      }
    }
    
    icc.df <- na.omit(icc.df)
    
    # Implementing unbiased ICC formula
    
    s2 <- (1/(2*length(unique(Data.stuff$Game.ID)) - 1))*(sum((icc.df[,1] - mean(icc.df))^2) + sum((icc.df[,2] - mean(icc.df))^2))
    icc.coef <- 1/((length(unique(Data.stuff$Game.ID)) - 1)*s2)*sum((icc.df[,1] - mean(icc.df))*(icc.df[,2] - mean(icc.df)))
    print(icc.coef)
    
    
  }
}