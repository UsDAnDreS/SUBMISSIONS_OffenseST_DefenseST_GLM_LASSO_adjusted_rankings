#######################
## Produces the table with ranking shifts resulting from the adjustment for complementary football features
## within a single season.
#######################

require(tidyverse)
require(gridExtra)
require(plotrix)



#####
## For which week we get the rankings?(if all weeks - put in "MAX")
#####

week <- "MAX" # a number, or "MAX" 

## For which stat?
stat <- "Off_w_ST.Pts"

## Which season?
year <- 2012

## Offense or defense?
side <- "Offense"



## !!! ONLY WORKS FOR "EFFICIENCY = TRUE", GLM_type = "Gaussian" !!!
## Do we look at EFFICIENCY STATS (per-play, per-possession)?
efficiency <- T
GLM_type <- "Gaussian"





## Whether to IMPUTE STATS that were ALWAYS UNAVAILABLE for NON-MAJOR TEAMS, 
## e.g. Sacks, Tackles, etc etc (mostly PURE DEFENSIVE PLAYS)
IMPUTE.NONMAJOR.NA <- TRUE


## Do we look at EFFICIENCY STATS (per-play, per-possession)?
efficiency <- T



## GLM type (if it's between Gaussian and Poisson)
GLM_type <- "Gaussian"

Data <- read.csv(paste0("Stepwise_Estimates/", 
                        ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                        ifelse(efficiency, "EFFICIENCY_", ""),
                        "TOT_AVG_",
                        GLM_type, "_BIC_", "forward", "_", "2018",
                        ifelse(include.non.majors, "", "_EXCLUDES_NON_MAJORS"), ".csv"))
Data$Stat






################
######## PICKED COMPLEMENTARY STATS TO ADJUST FOR
################



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
                                            "Off_w_ST.Tot.TO.NonScoring"
  )
  )
  
  
}



picked.pred <- paste0(complement.stats.list[[stat]], ".y")








year <- 2012

side <- "Offense"

adj_opp.rank.table <- read.csv(paste0("GROUP_LASSO/Rankings_NEW/", year, "/", side, "/Up_to_Week=", week, "/Gaussian_Adj_Opp/", 
                                      ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                      ifelse(efficiency, "EFFICIENCY_", ""),
                                      stat, ".csv"))
adj_opp_and_ballside.rank.table <- read.csv(paste0("GROUP_LASSO/Rankings_NEW/", year, "/", side, "/Up_to_Week=", week, "/Gaussian_Adj_Opp_and_Other_BallSide",
                                                   "/",
                                                   ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                                   ifelse(efficiency, "EFFICIENCY_", ""),
                                                   stat, ".csv"))



## GLM Adjusted vs (Adjusted + Other-Side of the Ball Adjusted) 
opp_vs_ballside_shift <- data.frame(adj_opp.rank.table[,c("Team","Value","Rank")], 
                                    adj_opp_and_ballside.rank.table[,c("Value","Rank")],
                                    Rank.Diff = adj_opp.rank.table$Rank - adj_opp_and_ballside.rank.table$Rank,
                                    Value.Diff = ifelse(rep(side, nrow(adj_opp_and_ballside.rank.table)) == "Offense",
                                                        adj_opp_and_ballside.rank.table$Value - adj_opp.rank.table$Value,
                                                        adj_opp.rank.table$Value - adj_opp_and_ballside.rank.table$Value))

opp_vs_ballside_shift <-
  opp_vs_ballside_shift %>%
  mutate(Value=round(Value,2), Value.1=round(Value.1,2))


## Top-12 ranked
opp_vs_ballside_shift %>% arrange(Rank.1)  %>% head(12)

## Sorting BY RANK shift
opp_vs_ballside_shift %>% 
  arrange(desc(abs(Rank.Diff)))  %>% head(10)

## Sorting by VALUE shift
opp_vs_ballside_shift %>% 
  arrange(desc(abs(Value.Diff)))  %>% head(10)



if (year == 2012){
  top.off <- 12
  top.def <- 10
  if (side == "Offense") which.ind <- c(1:top.off, 87, 80)
  if (side == "Defense") which.ind <- c(1:top.def, 51, 64)
}



#########
## CHECKING WHY THESE DROPS/RISES HAPPEN for "team.of.interest"
#########

strongest.shifts <- F

if (strongest.shifts){
  our.table <- rbind(opp_vs_ballside_shift %>% 
                       arrange(desc(abs(Rank.Diff)))  %>% 
                       head(5),
                     opp_vs_ballside_shift %>% 
                       arrange(desc(abs(Value.Diff)))  %>% head(5))
  
} else {
  our.table <- opp_vs_ballside_shift %>% arrange(Rank.1) # %>% head(12)
}

team.of.interest <- our.table$Team
team.of.interest




all.offense.game.logs <- read.csv(file=paste0("Game_Logs/", year, "/", ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""), ifelse(efficiency, "EFFICIENCY_", "FINAL_"), "FULL_STATS_Offense&SpecialTeams.csv"))
all.defense.game.logs <- read.csv(file=paste0("Game_Logs/", year, "/", ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""), ifelse(efficiency, "EFFICIENCY_", "FINAL_"), "FULL_STATS_Defense&SpecialTeams.csv"))



if (side == "Offense"){
  our.mat <- data.frame(Team=all.defense.game.logs[,c("Team")], 
                        Value=all.defense.game.logs[, complement.stats.list[[stat]]])
} else {
  our.mat <- data.frame(Team=all.offense.game.logs[,c("Team")], 
                        Value=all.offense.game.logs[, complement.stats.list[[stat]]])
  
}

our.avg.mat <- our.mat %>% 
  group_by(Team) %>%
  summarise(Value = mean(Value, na.rm=T))


our.rank.mat <- data.frame(Team=our.avg.mat$Team, 
                           TO.Rank=rank(our.avg.mat[,-1]),
                           TO.Value=round(our.avg.mat$Value,2))
colnames(our.rank.mat)[c(2,3)] <- paste0(substr(side,1,3),".", colnames(our.rank.mat)[c(2,3)])





if (side == "Offense"){
  our.rank.mat <- data.frame(Team=our.avg.mat$Team,
                             TO.Value=format(round(our.avg.mat$Value,2), nsmall=2),
                             TO.Rank=rank(-our.avg.mat[,-1]))
  our.rank.mat <- our.rank.mat %>% rename(Def.TO.Rank = TO.Rank, Def.TO.Value = TO.Value)
  
  final.table <- our.table %>% mutate(PPG.Value.Shift = ifelse(Value.Diff > 0, 
                                                               paste0("($\\textcolor{darkgreen}{\\Uparrow}$ ", format(round(abs(Value.Diff),2), nsmall=2), ")"),
                                                               ifelse(Value.Diff <0,
                                                                      paste0("($\\textcolor{red}{\\Downarrow}$ ", format(round(abs(Value.Diff),2), nsmall=2), ")"),
                                                                      paste0("($\\textcolor{blue}{$", abs(Value.Diff), "}$)"))),
                                      PPG.Rank.Shift = ifelse(Rank.Diff > 0, 
                                                              paste0("($\\textcolor{darkgreen}{\\Uparrow}$ ", abs(Rank.Diff), ")"),
                                                              ifelse(Rank.Diff <0,
                                                                     paste0("($\\textcolor{red}{\\Downarrow}$ ", abs(Rank.Diff), ")"),
                                                                     paste0("($\\textcolor{blue}{", abs(Rank.Diff), "}$)")))) %>%
    dplyr::select(-Value, -Rank,  -Rank.Diff, -Value.Diff) %>%
    left_join(our.rank.mat)
  
  
} else {
  our.rank.mat <- data.frame(Team=our.avg.mat$Team,
                             TO.Value=format(round(our.avg.mat$Value,2), nsmall=2),
                             TO.Rank=rank(our.avg.mat[,-1]))
  our.rank.mat <- our.rank.mat %>% rename(Def.TO.Rank = TO.Rank, Def.TO.Value = TO.Value)
  
  final.table <- our.table %>% mutate(PPG.Value.Shift = ifelse(Value.Diff > 0, 
                                                               paste0("($\\textcolor{darkgreen}{\\Downarrow}$ ", format(round(abs(Value.Diff),2), nsmall=2), ")"),
                                                               ifelse(Value.Diff <0,
                                                                      paste0("($\\textcolor{red}{\\Uparrow}$ ", format(round(abs(Value.Diff),2), nsmall=2), ")"),
                                                                      paste0("($\\textcolor{blue}{$", abs(Value.Diff), "}$)"))),
                                      PPG.Rank.Shift = ifelse(Rank.Diff > 0, 
                                                              paste0("($\\textcolor{darkgreen}{\\Uparrow}$ ", abs(Rank.Diff), ")"),
                                                              ifelse(Rank.Diff <0,
                                                                     paste0("($\\textcolor{red}{\\Downarrow}$ ", abs(Rank.Diff), ")"),
                                                                     paste0("($\\textcolor{blue}{", abs(Rank.Diff), "}$)")))) %>%
    dplyr::select(-Value, -Rank,  -Rank.Diff, -Value.Diff) %>%
    left_join(our.rank.mat)
  
}


final.table$Value.1 <- format(round(final.table$Value.1, 2), nsmall=2)



for (j in 1){
  
  if (side == "Offense"){
    
    cat(paste0(c("Team", "Points Scored", "Non-Scoring Turnovers Forced"), collapse = " & "), "\\\\")
    cat(paste0(c("", "(Per Game)", "(Per Possession)"), collapse = " & "), "\\\\ \\hline ")
  }
  
  
  if (side == "Defense"){
    
    cat(paste0(c("Team", "Points Allowed", "Non-Scoring Turnovers Committed"), collapse = " & "), "\\\\")
    cat(paste0(c("", "(Per Game)", "(Per Possession)"), collapse = " & "), "\\\\ \\hline ")
  }
  
  cat(" \n \\begin{tabular}{r} \\\\", paste0(final.table$Team[which.ind[1:ifelse(side == "Offense", top.off, top.def)]], collapse = " \\\\ "), " \\\\ .... \\\\",
      paste0(final.table$Team[which.ind[-c(1:ifelse(side == "Offense", top.off, top.def))]], collapse = " \\\\ .... \\\\ "), " \\\\ .... \\\\",
      "\\end{tabular} &")
  
  
  ## The Offensive Pts Per Game shifts
  cat("\n \\begin{tabular}{rrrr}",
      "Value & (Shift) & Rank & (Shift)\\\\ \\hline ")
  
  final.string <- NULL
  for (i in which.ind[1:ifelse(side == "Offense", top.off, top.def)]){
    final.string <- paste0(final.string, paste0(final.table[i, c(2,4,3,5)],  collapse=" & "), " \\\\ ")
  }
  
  for (i in which.ind[-c(1:ifelse(side == "Offense", top.off, top.def))]){
    final.string <- paste0(final.string, "... & ... \\\\", paste0(final.table[i, c(2,4,3,5)],  collapse=" & "), " \\\\ ")
  }
  
  final.string <- paste0(final.string,  "... & ... \\\\", " \\end{tabular} & ")
  cat(final.string)
  
  ## The Defensive TDs ranks
  cat("\n \\begin{tabular}{rr} \\n",
      "Value & Rank \\\\ \\hline ")
  
  final.string <- NULL
  for (i in which.ind[1:ifelse(side == "Offense", top.off, top.def)]){
    final.string <- paste0(final.string, paste0(c(final.table[i, 6:7]), collapse =" & "), " \\\\ ")
  }
  
  for (i in which.ind[-c(1:ifelse(side == "Offense", top.off, top.def))]){
    final.string <- paste0(final.string, "... & ... \\\\", paste0(c(final.table[i, 6:7]), collapse =" & "), " \\\\ ")
  }
  
  
  final.string <- paste0(final.string, "... & ... \\\\", " \\end{tabular}")
  
  cat(final.string)
  
  
}
