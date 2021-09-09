###########
## The process of dropping the complementary football statistics that:
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


side <- "Offense"   # "Offense" or "Defense", but it doesn't truly matter in the end.



###########
### FIRST, dealing with Offensive plays
############


Off.drop.cols <- c(# PERFECT/NEAR-PERFECT MULTI-COLLINEARITY CONSIDERATIONS:
  "Plays",             
  "Rush.TD",  "Pass.TD",
  "Pass.Att",  # Cmp + Incmp
  "RedZone..Attempts", "X3rdDown..Attempts", "X4thDown..Attempts", 
  "RedZone..Scores", # RedZone FG + RedZone TD
  "FGA", "Tot.Yds", "Tot.Avg",
  
  # UNNEEDED BREAKDOWN CONSIDERATIONS:
  "Pass", "Rush", "Pen",    # Doesn't really matter to complementary side how 1st down is obtained
  "Fum", "Int",             # Same here
  "Pass.Pts", "Rush.Pts", "Kicking.Pts", # All included in points, doesn't matter to complementary side
  "X.2PM", "X.2PM.Pass", "X.2PM.Rush",
  
  "TD.FR.by.Off",   # Really rare occurrence, already included into offensive TD
  "Points.Team",    # Includes points scored by defense as well, can't be used here
  
  # LOW-DENOMINATOR:
  "XP.", "FG.", "X4thDown..Conversion..",
  "RedZone..Score..", "RedZone..FG..", "RedZone..TD..",
  
  # HIGH PAIR-WISE CORRELATIONS:
  "Tot.TD",                   # heavy corr with Points
  "XPM",                      # heavily correlated with Points
  "XPA",   # CORRELATED with TDs/Points
  "Minus.Def.TD.Margin",      # heavy corr with Points
  "Minus.Def.Points.Margin",   # heavy corr with Points
  "Minus.Def.Yds.Margin"      # heavy corr with Yds
)



for (year in years){
  print(year)
  Data <- read.csv(paste0("Game_Logs/",year,"/", "IMPUTED_", "FINAL_FULL_STATS_", side,"&SpecialTeams.csv"))
  col.inds <- which(str_detect(colnames(Data), "Off") & !str_detect(colnames(Data), ".ST"))
  colnames(Data)[col.inds]
  
  select.col.names <- colnames(Data)[col.inds][!colnames(Data)[col.inds] %in% paste0("Off.", Off.drop.cols)]
  
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


## CHECKING FOR ALIASED/NEAR-PERFECTLY MULTI-COLLINEAR PREDICTORS
vif(lm(Data$Def.Tot.TD ~ .,
       Data[, select.col.names]))

print(select.col.names)





###########
### SECOND, dealing with DEFENSIVE PLAYS
############



Def.drop.cols <- c(
  # UNNEEDED BREAKDOWN CONSIDERATIONS:
  "FGR.TD", "Int.Int",
  "Int.Yds", "Fumbles.Yds",
  "Int.TD", "Fumbles.TD",
  "Fumbles.FR",
  "Tackles.Ast", "Tackles.Solo",
  "Points.Team",
  
  
  #For HIGH PAIRWISE-CORRELATIONAL PURPOSES
  "Tot.Pts","Int.Avg"
)




for (year in years){
  print(year)
  Data <- read.csv(paste0("Game_Logs/",year, "/", "IMPUTED_", "FINAL_FULL_STATS_", side,"&SpecialTeams.csv"))
  col.inds <- which(str_detect(colnames(Data), "^Def.") & !str_detect(colnames(Data), ".ST"))
  colnames(Data)[col.inds]
  
  select.col.names <- colnames(Data)[col.inds][!colnames(Data)[col.inds] %in% paste0("Def.", Def.drop.cols)]
  
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


## CHECKING FOR ALIASED/NEAR-PERFECTLY MULTI-COLLINEAR PREDICTORS
vif(lm(Data$Off.Cmp ~ .,
       Data[, select.col.names]))


print(select.col.names)

















###########
### THIRD, dealing with SPECIAL TEAMS (& ST+OFFENSE, ST+DEFENSE STATS)
############


ST.drop.cols <-  c(# For PERFECT/NEAR-PERFECT MULTI-COLLINEARITY CONSIDERATIONS:
  "Off_w_ST.Plays",  # Pass Att + Rush Att + Ret
  "Off_w_ST.Yds", # Pass Yds + Rush Yds + Ret Yds
  "Off_w_ST.Yds.Avg",  # Pass Avg + Rush Avg + Ret Avg
  
  # UNNEEDED BREAKDOWN CONSIDERATIONS:
  "Off.ST.KickRet.Ret", "Off.ST.PuntRet.Ret",
  "Off.ST.KickRet.Yds", "Off.ST.PuntRet.Yds",
  "Off.ST.KickRet.Avg", "Off.ST.PuntRet.Avg",
  "Off.ST.KickRet.TD","Off.ST.PuntRet.TD",
  
  "Off.ST.Kickoff.No", "Off.ST.Kickoff.Yds", "Off.ST.Kickoff.Avg",      # The Kickoff stats were mostly brought in for purposes of creating the return probability decision tree graphic  
  "Off.ST.Kickoff.Touchbacks",   "Off.ST.Kickoff.Touchbacks..",
  "Off.ST.Kickoff.OutOfBounds",  "Off.ST.Kickoff.Onside",  
  
  "Off.ST.Tot.TO",
  "Def.ST.Fumbles.TD", "Def_w_ST.Tot.Fumbles.TD", # No difference from "Receiving.TD", which is: TD's when the OPPONENT IS RETURNING THE KICK/PUNT
  "Def.ST.Receiving.TD", "Def.ST.Tot.TO", "Def.ST.Receiving.Pts",
  "Off_w_ST.Int", "Off_w_ST.Fum",
  "Off_w_ST.TD.TO.Diff",
  
  
  #For HIGH PAIR-WISE CORRELATIONAL CONSIDERATIONS
  "Off.ST.Punting.Yds",
  "Off.ST.Return.Points",
  "Def_w_ST.Pts",
  "Off_w_ST.Tot.TO", 
  "Off_w_ST.TD",
  "Off_w_ST.Minus.Def_w_ST.TD.Margin",
  "Off_w_ST.Minus.Def_w_ST.Points.Margin",
  "Off_w_ST.Minus.Def_w_ST.Yds.Margin",
  
  # LOW-DENOMINATOR:
  "Off.ST.Return.Avg",
  "Off.ST.Punting.Avg"
  
)



for (year in years){
  print(year)
  Data <- read.csv(paste0("Game_Logs/",year, "/", "IMPUTED_", "FINAL_FULL_STATS_", side,"&SpecialTeams.csv"))
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


## CHECKING FOR ALIASED/NEAR-PERFECTLY MULTI-COLLINEAR PREDICTORS
vif(lm(Data$Off.Cmp ~ .,
       Data[, select.col.names]))

print(select.col.names)







###########
### FOURTH, bringing in ALL THE STATS 
############



ALL.drop.cols <- c(paste0("Off.", Off.drop.cols), 
                   paste0("Def.", Def.drop.cols), 
                   ST.drop.cols,

                   
                   # UNNEEDED BREAKDOWN CONSIDERATIONS:
                   "No.Possessions",  # Way too heavily dictated by the other side of the ball
                   "Off.Points.Team", "Def.Points.Team",
                   "Def.Tot.TO", "Def.ST.KicksPunts.Blocked",
                   "Off_w_ST.Fum", "Def_w_ST.Tot.Fumbles.TD",
                   # "Off.Tot.Pts", 
                   
                   # For CORRELATIONAL CONSIDERATIONS:
                   "Def.Tot.Pts","Def.Tot.Yds", "Def.Tot.TD",
                   "Off.Tot.TO" ,
                   "Off.Points",  
                   "Off.Tot.Yds", # 
                   "Off.Tot.Avg", #"Off_w_ST.Yds.Avg", #
                   "No.Scoring.Possessions",  # Heavy correlation with Off_w_ST.Pts
                   "No.Negative.Possessions",   # Heavy corr with Def_w_ST.TD
                   "No.Empty.Possessions"

                   
)




for (year in years){
  print(year)
  Data <- read.csv(paste0("Game_Logs/",year, "/", "IMPUTED_", "FINAL_FULL_STATS_", side,"&SpecialTeams.csv"))
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
  

}


## CHECKING FOR ALIASED/NEAR-PERFECTLY MULTI-COLLINEAR PREDICTORS
vif(lm(Data$Off.X4thDown..Conversion.. ~ .,
       Data[, select.col.names]))


print(select.col.names)



save(select.col.names, file=paste0("INDIVIDUAL_SELECT_COL_NAMES.RData"))
