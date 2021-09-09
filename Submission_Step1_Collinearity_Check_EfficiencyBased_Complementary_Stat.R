###########
## For EFFICIENCY-BASED STATISTICS (per-play or per-possession),
##  the process of dropping the complementary football statistics that:
##    - Have sub-categories impactful to the complementary side (e.g. pass attempts to completions and incompletions), to avoid perfect multicollinearity
##    - Have really strong pair-wise correlation with another more ubiquitous statistics (e.g. Off_w_ST.TD heavily correlates with a more all-encompassing and ubiquitous Off_w_ST.Pts)
##    - Are efficiency-based, but have low denominator (e.g. FG %, XP %, Punting.Avg, Off.ST.Return.Avg Off.X4thDown..Conversion %, RedZone conversion %,  Def_w_ST.Yds.Avg)
###########

require(car)
require(tidyverse)
require(plotrix)
require(MASS)
require(leaps)
require(glmnet)



years <- c(2009:2019)


side <- "Offense"   # "Offense" or "Defense"

###########
### FIRST, dealing with Offensive plays
############


##
Off.drop.cols <- c(
  # For PERFECT/NEAR-PERFECT MULTI-COLLINEARITY
  "Off.Rush.Avg", "Off.Pass.Avg",
  
  # UNNEEDED BREAKDOWN CONSIDERATIONS:
  "Off.XPM", "Off.XPF", "Off.FGM", "Off.FGF",   # Non-efficiency stats
  
  "Off.XP.", "Off.FG.",                         # Divided over SMALL DENOMINATOR
  "Off.X4thDown..Conversion..",                 # SMALL DENOMINATOR
  "Off.RedZone..TD..",                          # SMALL DENOMINATOR
  
  "Off.Pass.Avg.PerCmp",                        # Too "made-up" of a stat
  "Off.X1stDowns.PerPos",
  "Off.FGA.PossPct", "Off.FGM.PossPct", "Off.FGF.PossPct",   # FG Possession % is not REALLY AN EFFICIENCY METRIC, NEITHER OUTCOME (FG or NO FG) is CLEARLY INDICATIVE of a GOOD or BAD THING
  "Off.XPF.PossPct",
  
  # HIGH PAIR-WISE CORRELATIONS:
  "Off.Tot.TD", "Off.Tot.TD.PerPos",
  "Off.Points",
  "Off.Minus.Def.TD.Margin", "Off.Minus.Def.TD.Margin.PerPos",
  "Off.Minus.Def.Points.Margin",
  "Off.Minus.Def.Yds.Margin.Avg",
  "Off.Minus.Def.Points.Margin.PerPos",
  "Off.XPM.PerPos",
  "Off.Scoring.PossPct", "Off.NonScoring.PossPct",
  "Off.TD.PossPct", "Off.XPM.PossPct"
)


for (year in years){
  print(year)
  Data <- read.csv(paste0("Game_Logs/",year,"/", "IMPUTED_", "EFFICIENCY_FULL_STATS_", side,"&SpecialTeams.csv"))
  col.inds <- which(str_detect(colnames(Data), "Off") & !str_detect(colnames(Data), ".ST"))
  colnames(Data)[col.inds]
  
  select.col.names <- colnames(Data)[col.inds][!colnames(Data)[col.inds] %in% paste0(Off.drop.cols)]
  
  abs.cor <- abs(cor(Data[, select.col.names], use="complete"))
  color2D.matplot(ifelse(abs.cor >= 0.70, abs.cor, 0),
                  cs1=c(0,1),cs2=c(1,0),cs3=c(1,0),
                  show.legend=T,
                  show.values=2,
                  xlab='',
                  ylab='',
                  axes=F,
                  main=year)
  par(las=2)
  axis(1,at=c(1:ncol(abs.cor))-0.5,labels=colnames(abs.cor))
  par(las=1)
  axis(2,at=c(ncol(abs.cor):1)-0.5,labels=colnames(abs.cor))
}


print(select.col.names)



###########
### SECOND, dealing with DEFENSIVE PLAYS
############



Def.drop.cols <-c(
  # UNNEEDED BREAKDOWN CONSIDERATIONS:
  "Def.Sfty", "Def.Sfty.PossPct",
  "Def.KicksPunts.Blocked",  # Way too exotic of an event, and really would need the PUNTS BLOCKED extracted from it to be the MOST INFORMATIVE (o/wise FG BLOCKED are among FGF...)
  #For HIGH PAIRWISE CORRELATIONS 
  "Def.QB.Hurries", "Def.QB.Hurries.PerPos",  "Def.QB.Hurries.Avg",  # "Def.QB.Hurries.To.Pass.Att.Ratio",
  "Def.Tackles.Solo.Avg", "Def.Tackles.Ast.Avg", 
  "Def.Tackles.Loss", "Def.Tackles.Loss.PerPos",
  "Def.Tackles.Sk", "Def.Tackles.Sk.PerPos",  "Def.Tackles.Sk.Avg", # "Def.Tackles.To.Pass.Att.Ratio",
  "Def.Fumbles.FF", "Def.Fumbles.FF.PerPos",
  "Def.Tot.TD",  "Def.Tot.TD.PossPct",
  "Def.Tot.Pts", "Def.Tot.Pts.PerPos",  # HEAVILY correlated with TDs, but there are some SAFETIES in there, results of which are VERY DIFFERENT from a TD (you get 2pts + POSSESSION)
  "Def.Int.PD.PerPos",
  "Def.Tot.TO"
)





for (year in years){
  print(year)
  Data <- read.csv(paste0("Game_Logs/",year,"/", "IMPUTED_", "EFFICIENCY_FULL_STATS_", side,"&SpecialTeams.csv"))
  col.inds <- which(str_detect(colnames(Data), "^Def.") & !str_detect(colnames(Data), ".ST"))
  colnames(Data)[col.inds]
  
  select.col.names <- colnames(Data)[col.inds][!colnames(Data)[col.inds] %in% paste0(Def.drop.cols)]
  
  abs.cor <- abs(cor(Data[, select.col.names], use="complete"))
  color2D.matplot(ifelse(abs.cor >= 0.70, abs.cor, 0),
                  cs1=c(0,1),cs2=c(1,0),cs3=c(1,0),
                  show.legend=T,
                  show.values=2,
                  xlab='',
                  ylab='',
                  axes=F,
                  main=year)
  par(las=2)
  axis(1,at=c(1:ncol(abs.cor))-0.5,labels=colnames(abs.cor))
  par(las=1)
  axis(2,at=c(ncol(abs.cor):1)-0.5,labels=colnames(abs.cor))
}


print(select.col.names)











###########
### THIRD, dealing with SPECIAL TEAMS (& ST+OFFENSE, ST+DEFENSE STATS)
############


ST.drop.cols <-  c(
  #For NON-CORRELATIONAL CONSIDERATIONS
  "Off.ST.Return.TD", "Off.ST.Return.Points", "Off.ST.Return.Yds",
  "Off.ST.Return.Yds.PerPos",  # Not a "possession-ender"
  "Off_w_ST.TD.TO.Diff",  # For use as a RESPONSE ONLY !!!
  "Off.ST.Return.TD.PossPct",   # SIMPLY LEAVING IT TO "PTS.PERPOS", otherwise AGAIN A RISK OF REVERSE CAUSALITY
  
  
  # LOW DENOMINATOR:
  "Off.ST.Return.TD.PerRet", 
  "Off.ST.Return.Yds.PerRet",   #ALSO, AVG might be distinguishing 0-return teams vs AT LEAST 1 RETURN => TOO MUCH....
  "Off.ST.Punting.Avg",      
  
  #For HIGH PAIRWISE CORRELATIONAL/UNNEEDED BREAKDOWN CONSIDERATIONS
  "Off_w_ST.Tot.TO.NonScoring",
  "Off.ST.Return.Points.PerRet","Off.ST.Return.Points.PerPos",
  "Off.ST.Punting.Punts", 
  "Off_w_ST.Fum", "Off_w_ST.Int",
  "Off_w_ST.Fum.PerPos", "Off_w_ST.Int.PerPos",
  "Off_w_ST.Tot.TO",
  "Off_w_ST.Tot.TO.PossPct", # "Off_w_ST.Tot.TO.NonScoring.PossPct",  
  "Off_w_ST.Pts", "Off_w_ST.Yds",
  "Off_w_ST.TD", "Off_w_ST.TD.PerPos",
  "Def_w_ST.Pts",  "Def_w_ST.Pts.PerPos",  # Heavily corr with "Def_w_ST.TD.PerPos"
  "Def_w_ST.TD", 
  "Off_w_ST.Minus.Def_w_ST.Points.Margin",
  "Off_w_ST.Minus.Def_w_ST.TD.Margin",
  "Off_w_ST.Minus.Def_w_ST.Yds.Margin",
  "Off_w_ST.Minus.Def_w_ST.TD.Margin.PerPos",
  "Off_w_ST.Minus.Def_w_ST.Yds.Margin.Avg",
  "Off_w_ST.Minus.Def_w_ST.Points.Margin.PerPos",
  "Off_w_ST.Scoring.PossPct",
  "Off.ST.Punting.Punts.PerPos",
  "Off_w_ST.Punt.PossPct",    # REPLACING WITH SFTY+PUNTS
  "Off_w_ST.TD.PossPct", "Off_w_ST.Empty.PossPct",  # Correlated with Pts Per Pos
  # "Def_w_ST.TD.PossPct", # STRONLY (0.92+) CORRELATED with Negative.Possession %,
  # but I pick ""Def_w_ST.TD.PossPct", as it RETURNS THE BALL TO OPPOSING OFFENSE
  # (unlike Sfty) => Hence a BIG DIFFERENCE IN THE "DIRECT AFTERMATH" ASPECT
  "Off_w_ST.Negative.PossPct", # STRONGLY CORRELATED with a MORE INFORMATIVE "Def_w_ST.TD.PossPct"
  "Off_w_ST.Plays.PerPos"  # NOT REALLY AN "EFFICIENCY" METRIC... all depends on "WHAT KIND OF PLAYS" we're talking..
)



for (year in years){
  print(year)
  Data <- read.csv(paste0("Game_Logs/",year,"/", "IMPUTED_", "EFFICIENCY_FULL_STATS_", side,"&SpecialTeams.csv"))
  col.inds <- which(str_detect(colnames(Data), ".ST."))
  colnames(Data)[col.inds]
  
  
  select.col.names <- colnames(Data)[col.inds][!colnames(Data)[col.inds] %in% ST.drop.cols]
  
  abs.cor <- abs(cor(Data[, select.col.names], use="complete"))
  color2D.matplot(ifelse(abs.cor >= 0.70, abs.cor, 0),
                  cs1=c(0,1),cs2=c(1,0),cs3=c(1,0),
                  show.legend=T,
                  show.values=2,
                  xlab='',
                  ylab='',
                  axes=F,
                  main=year)
  par(las=2)
  axis(1,at=c(1:ncol(abs.cor))-0.5,labels=colnames(abs.cor))
  par(las=1)
  axis(2,at=c(ncol(abs.cor):1)-0.5,labels=colnames(abs.cor))
}


print(select.col.names)







###########
### FOURTH, bringing in ALL THE STATS 
############



ALL.drop.cols <-  c(paste0(Off.drop.cols), 
                    paste0(Def.drop.cols), 
                    ST.drop.cols,
                    
                    "Off.Tot.Avg", 
                    "Off.Points.PerPos",
                    "Def.Tot.TD.PossPct",
                    
                    "Off_w_ST.FG.PossPct",
                    "Off.Negative.PossPct", # HEAVY correlation with "Off_w_ST.Negative.PossPct"
                    "Off_w_ST.FGM.PossPct", "Off_w_ST.FGF.PossPct"  # Should be just "Off.FGM/FGF.PossPct" (IF ANYTHING)
)


for (year in years){
  print(year)
  Data <- read.csv(paste0("Game_Logs/",year,"/", "IMPUTED_", "EFFICIENCY_FULL_STATS_", side,"&SpecialTeams.csv"))
  our.col.names <- colnames(Data)[(which(colnames(Data) == "Game.ID")+1):ncol(Data)]
  our.col.names <- our.col.names[our.col.names != "Homefield"]
  
  
  select.col.names <- our.col.names[!(our.col.names %in% ALL.drop.cols)]
  select.col.names
  
  
  abs.cor <- abs(cor(Data[, select.col.names], use="complete"))
  color2D.matplot(ifelse(abs.cor >= 0.70, abs.cor, 0),
                  cs1=c(0,1),cs2=c(1,0),cs3=c(1,0),
                  show.legend=T,
                  show.values=2,
                  xlab='',
                  ylab='',
                  axes=F,
                  main=year)
  par(las=2)
  axis(1,at=c(1:ncol(abs.cor))-0.5,labels=colnames(abs.cor))
  par(las=1)
  axis(2,at=c(ncol(abs.cor):1)-0.5,labels=colnames(abs.cor))
  
  # print(summary(Data$Def.Int.TD))
  #print(summary(Data$Def_w_ST.Tot.Fumbles.TD))
}


print(select.col.names)

## CHECKING FOR ALIASED/NEAR-PERFECTLY MULTI-COLLINEAR PREDICTORS
vif(lm(Data$Def.Tot.TD ~ .,
       Data[, select.col.names]))



select.col.names <- unique(c(select.col.names, "Off.Tot.Avg"))
select.col.names <- select.col.names[select.col.names != "Off_w_ST.Yds.Avg"]

  
print(select.col.names)




save(select.col.names, file=paste0("EFFICIENCY_", "TOT_AVG_", "SELECT_COL_NAMES.RData"))



