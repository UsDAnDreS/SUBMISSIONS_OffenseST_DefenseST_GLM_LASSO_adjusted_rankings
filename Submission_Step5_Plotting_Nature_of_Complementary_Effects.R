########
## Reproducing the figure from the paper,
## showing the nature of consistent complementary football effects for:
##    * "Off_w_ST.Pts"
##    * "Off_w_ST.Yds"
########


library(tidyverse)
library(plotrix)
library(splines)



## DF for SPLINES
spline.df <- 4

# Which statistic are we adjusting (our response)?
## TAILORED TOWARDS ONLY  "Off_w_ST.Pts" or "Off_w_ST.Yds" - so PLEASE PICK ONE OF THESE
stat <- "Off_w_ST.Pts"


## Do we look at EFFICIENCY STATS (per-play, per-possession)?
efficiency <- T



## Whether to IMPUTE STATS that were ALWAYS UNAVAILABLE for NON-MAJOR TEAMS, 
## e.g. Sacks, Tackles, etc etc (mostly PURE DEFENSIVE PLAYS)
IMPUTE.NONMAJOR.NA <- TRUE




years <- 2009:2019
GLM_type <- "Gaussian"




## To avoid PERFECT/NEAR-PERFECT MULTICOLLINEARITY, 
## ALWAYS PICK THE "SUB-CATEGORIES" (e.g. instead of Tot.Yds, do Rush.Yds + Pass.Yds)
## If the GRAND category is "all that it takes", it WILL SHOW IN THE COEFFICIENTS 
## (e.g. BOTH POSITIVE or BOTH NEGATIVE)
individual <- F

## To avoid PERFECT/NEAR-PERFECT MULTICOLLINEARITY, 
## ALWAYS PICK THE "SUB-CATEGORIES" (e.g. instead of Tot.Yds, do Rush.Yds + Pass.Yds)
## If the GRAND category is "all that it takes", it WILL SHOW IN THE COEFFICIENTS 
## (e.g. BOTH POSITIVE or BOTH NEGATIVE)
if (efficiency) {tot_avg <- T; individual <- F}
if (!efficiency) {tot_avg <- F}





## Defining grids of values for complementary features
## over which we'd like the effect to be plotted.
our.grids <- cbind(Off_w_ST.Tot.TO.NonScoring.PossPct.y = seq(0, 0.5, length.out=100),
                   Off.Tot.Avg.y = seq(0, 12, length.out=100),
                   Off_w_ST.Punt.Sfty.PossPct.y = seq(0, 0.9, length.out=100),
                   Off.Rush.Att.y = seq(0, 60, length.out=100),
                   Off.Incmp.y = seq(0, 35, length.out=100),
                   Off.ST.Return.Yds.y = seq(0, 300, length.out=100),
                   Off.ST.Return.Ret.y = seq(0, 15, length.out=100),
                   Off_w_ST.Tot.TO.NonScoring.y = seq(0, 8, length.out=100))



if (efficiency){
    complement.stats.list <- list(Off_w_ST.Pts = c("Off_w_ST.Tot.TO.NonScoring.PossPct"),
                                  Off_w_ST.Yds = c("Off.Tot.Avg"),  
                                  Off_w_ST.TD = c("Off_w_ST.Tot.TO.NonScoring.PossPct"),
                                  Off_w_ST.Minus.Def_w_ST.Points.Margin = c("Off_w_ST.Tot.TO.NonScoring.PossPct"),
                                  Off_w_ST.Minus.Def_w_ST.Yds.Margin = c("Off.Tot.Avg"), 
                                  Off_w_ST.Minus.Def_w_ST.TD.Margin = c("Off_w_ST.Tot.TO.NonScoring.PossPct"),
                                  Off_w_ST.TD.TO.Diff = c("Off_w_ST.Tot.TO.NonScoring.PossPct"))
    
} else {
  
  
  complement.stats.list <- list(Off_w_ST.Pts = c("Off.Rush.Att", "Off.Incmp", "Off.ST.Return.Yds", "Off.ST.Return.Ret",
                                                 "Off_w_ST.Tot.TO.NonScoring"
  ),
  Off_w_ST.Minus.Def_w_ST.Points.Margin = c("Off.Rush.Att", "Off.Incmp", "Off.ST.Return.Yds", "Off.ST.Return.Ret",
                                            "Off_w_ST.Tot.TO.NonScoring")
  )
  
}


## Adding the ".y" post-fix
picked.pred <- paste0(complement.stats.list[[stat]], ".y")




side <- "Offense"

for (comp.stat.ind in 1:length(picked.pred)){
  
  all.preds.interm <- NULL
  
  for (year in years){
    print(year)
    
    Data <- read.csv(paste0("Game_Logs/",year,"/", ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""), ifelse(efficiency, "EFFICIENCY_", "FINAL_"), "FULL_STATS_", side,"&SpecialTeams.csv"))
    Data.other <- read.csv(paste0("Game_Logs/",year,"/", ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""), ifelse(efficiency, "EFFICIENCY_", "FINAL_"), "FULL_STATS_", c("Offense", "Defense")[c("Offense", "Defense")!=side],"&SpecialTeams.csv"))
    Data$X <- Data.other$X <- NULL
    
    
    Data.stuff <- Data %>% 
      left_join(Data.other, by=c("Team", "Opponent", "Date", "Homefield"))
    
    
    if (length(picked.pred) == 1){
      ggplot(mapping=aes(Data.stuff[,picked.pred], Data.stuff[,paste0(stat, ".x")])) +
        geom_point() +
        geom_smooth(method="lm")
    }
    
    
    
    
    Data.stuff$Homefield <- factor(Data.stuff$Homefield, levels=c("","N","@"))
    Data.stuff$Homefield <- as.numeric(Data.stuff$Homefield)-1
    
    
    ## 
    # options(contrasts = rep("contr.sum", 2))
    options(contrasts = rep("contr.treatment", 2))
    
    ## Adjusting for OPPONENT + your DEFENSE'S STATS
    
    Data.stuff$Off.Tot.Yds.x <- Data.stuff$Off_w_ST.Yds.x - Data.stuff$Off.ST.Return.Yds.x
    
    
    
    ## Getting indices of NA-containing rows (hence those dropped from the linear model)
    na.ind <- which(apply(data.frame(Data.stuff[,paste(stat,".x",sep="")],
                                     Data.stuff[,c("Team","Opponent", "Homefield")], 
                                     Data.stuff[,picked.pred]), 
                          1,
                          function(x) mean(is.na(x)) > 0))
    
    if (length(na.ind)) Data.stuff <- Data.stuff[-na.ind, ]
    
    gam.obj <- lm(as.formula(paste0(paste(stat,".x",sep=""), "~ Team + Opponent + Homefield +", paste0(paste("ns(",picked.pred, paste0(", df=", spline.df, ")"), sep="", collapse="+")), 
                                    collapse="")),
                  data=data.frame(Data.stuff[,c("Team","Opponent", "Homefield")], 
                                  Data.stuff[,c(picked.pred, paste(stat,".x",sep=""))]))
    
    if (length(picked.pred) == 1){
      gam.null.obj <- lm(as.formula(paste0(paste(stat,".x",sep=""), "~ Team + Opponent + Homefield", 
                                           collapse="")),
                         data=data.frame(Data.stuff[,c("Team","Opponent", "Homefield")], 
                                         Data.stuff[,c(picked.pred, paste(stat,".x",sep=""))]))
      
      anova.obj <- anova(gam.obj, gam.null.obj)
      print(picked.pred)
      # Printing statistical significance of complementary effect
      print(paste0("p-value: ", anova.obj$`Pr(>F)`[2]))      
    } else {
      
      for (pred in picked.pred){
        gam.null.obj <- lm(as.formula(paste0(paste(stat,".x",sep=""), "~ Team + Opponent + Homefield +", paste0(paste("ns(",picked.pred[picked.pred != pred], paste0(", df=", spline.df, ")"), sep="", collapse="+")), 
                                             collapse="")),
                           data=data.frame(Data.stuff[,c("Team","Opponent", "Homefield")], 
                                           Data.stuff[,c(picked.pred, paste(stat,".x",sep=""))]))
        
        anova.obj <- anova(gam.obj, gam.null.obj)
        print(pred)
        
        # Printing statistical significance of complementary effect
        print(paste0("p-value: ", anova.obj$`Pr(>F)`[2]))
      }
      
    }
    
    
    
    all.grids <- as.matrix(our.grids[, c(picked.pred)])
    
    
    if (length(picked.pred) == 1){
      
      full.newdata <- all.grids
      colnames(full.newdata) <- c(picked.pred)
      
      all.preds.interm <- cbind(all.preds.interm, predict(gam.obj, newdata = data.frame(Team="Army", Opponent="Navy", Homefield=1,
                                                                                        full.newdata)))
      
      if (year == 2009){
        plot(all.grids[, 1],
             predict(gam.obj, newdata = data.frame(Team="Army", Opponent="Navy", Homefield=1,
                                                   full.newdata)),
             type="l",
             main = paste0(picked.pred),
             ylab=stat,
             col=year,
             ylim=c(ifelse(stat == "Off_w_ST.Yds", 250, 0),
                    ifelse(stat == "Off_w_ST.Yds", 700, 50)))
      } else {
        lines(all.grids[, 1],
              predict(gam.obj, newdata = data.frame(Team="Army", Opponent="Navy", Homefield=1,
                                                    full.newdata)),
              type="l",
              main = paste0(picked.pred),
              xlab=picked.pred,
              ylab=stat,
              col=year)
      }
    } else {
      full.newdata <- cbind(
        all.grids[, comp.stat.ind],
        apply(as.matrix(Data.stuff[, c(picked.pred[-comp.stat.ind])]), 
              2,
              function(x) rep(mean(x), 100)))
      
      colnames(full.newdata) <- c(picked.pred[comp.stat.ind], picked.pred[-comp.stat.ind])
      
      
      all.preds.interm <- cbind(all.preds.interm, predict(gam.obj, newdata = data.frame(Team="Army", Opponent="Navy", Homefield=1,
                                                                                        full.newdata)))
      
      
      if (year == 2009){
        
        plot(all.grids[, comp.stat.ind],
             predict(gam.obj, newdata = data.frame(Team="Army", Opponent="Navy", Homefield=1,
                                                   full.newdata)),
             type="l",
             col=year,
             main = paste0(picked.pred[comp.stat.ind], ", Year = ", year),
             xlab=picked.pred[comp.stat.ind],
             ylab=stat,
             ylim=c(ifelse(stat == "Off_w_ST.Yds", 250, 0),
                    ifelse(stat == "Off_w_ST.Yds", 700, 50)))
      } else {
        lines(all.grids[, comp.stat.ind],
              predict(gam.obj, newdata = data.frame(Team="Army", Opponent="Navy", Homefield=1,
                                                    full.newdata)),
              type="l",
              col=year,
              main = paste0(picked.pred[comp.stat.ind], ", Year = ", year),
              xlab=picked.pred[comp.stat.ind],
              ylab="Off_w_ST.Yds")
      }
      
    }
    
  }  

}