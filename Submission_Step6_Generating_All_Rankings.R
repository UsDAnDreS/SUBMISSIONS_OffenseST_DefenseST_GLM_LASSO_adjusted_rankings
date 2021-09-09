#################################
## Generating the 
##      1. Classic AVERAGE rankings.
##      2. The pure "adjusted for opponent" rankings.
##      3. The "adjusted for opponent" PLUS for "other side of the ball performance" rankings.
#################################

require(tidyverse)

#####
## FOR WHICH WEEK SHOULD WE GET THE RANKINGS? (if all weeks - put in "MAX")
#####

years <- 2009:2019
week <- 9  # any number ~ 5-20, or "MAX" (to include ALL the weeks & games)


## Do we look at EFFICIENCY STATS (per-play, per-possession)? Or any complementary stat?
efficiency <- T





################
######## PICKED COMPLEMENTARY STATS TO ADJUST FOR
################



if (efficiency){
  
    complement.stats.list <- list(Off_w_ST.Pts = c("Off_w_ST.Tot.TO.NonScoring.PossPct"),
                                  Off_w_ST.Yds = c("Off.Tot.Avg"),
                                  Off_w_ST.TD = c("Off_w_ST.Tot.TO.NonScoring.PossPct"),
                                  Off_w_ST.Minus.Def_w_ST.Points.Margin = c("Off_w_ST.Tot.TO.NonScoring.PossPct"),
                                  Off_w_ST.Minus.Def_w_ST.Yds.Margin = c("Off.Tot.Avg"), 
                                  Off_w_ST.Minus.Def_w_ST.TD.Margin = c("Off_w_ST.Tot.TO.NonScoring.PossPct"))
  
} else {
  
  
  complement.stats.list <- list(Off_w_ST.Pts = c("Off.Rush.Att", "Off.Incmp", "Off.ST.Return.Yds", "Off.ST.Return.Ret",
                                                 "Off_w_ST.Tot.TO.NonScoring"),
  Off_w_ST.Minus.Def_w_ST.Points.Margin = c("Off.Rush.Att", "Off.Incmp", "Off.ST.Return.Yds", "Off.ST.Return.Ret",
                                            "Off_w_ST.Tot.TO.NonScoring")
  )
  
}


picked.pred <- paste0(complement.stats.list[[stat]], ".y")





dir.create("Rankings_NEW")
dir.create("Selected_Estimates")  # To keep the estimated coefficients of predictors





Data <- read.csv(paste0("GROUP_LASSO/LASSO_Estimates/", 
                        ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                        ifelse(efficiency, "EFFICIENCY_", ""),
                        "TOT_AVG_",
                        GLM_type, "_BIC_", "forward", "_", "2018", ifelse(before.2009, "_BEFORE_2009", ""), 
                        ifelse(include.non.majors, "", "_EXCLUDES_NON_MAJORS"), ".csv"))
Data$Stat



## Main stats to get rankings on

if (efficiency){
  main.stats <- Data$Stat
} else {
  main.stats <- c("Off_w_ST.Pts", "Off_w_ST.Minus.Def_w_ST.Points.Margin")
}



for (year in years){
  print(year)
  
  dir.create(paste0("GROUP_LASSO/Rankings_NEW/", year))
  dir.create(paste0("GROUP_LASSO/Rankings_NEW/", year,"/Offense"))
  dir.create(paste0("GROUP_LASSO/Rankings_NEW/", year,"/Defense"))
  dir.create(paste0("GROUP_LASSO/Rankings_NEW/", year,"/Offense/Up_to_Week=", week))
  dir.create(paste0("GROUP_LASSO/Rankings_NEW/", year,"/Defense/Up_to_Week=", week))
  
  dir.create(paste0("GROUP_LASSO/Rankings_NEW/", year,"/Offense/Up_to_Week=", week, "/Classic"))
  dir.create(paste0("GROUP_LASSO/Rankings_NEW/", year,"/Defense/Up_to_Week=", week, "/Classic"))
  dir.create(paste0("GROUP_LASSO/Rankings_NEW/", year,"/Offense/Up_to_Week=", week,"/", GLM_type,"_Adj_Opp"))
  dir.create(paste0("GROUP_LASSO/Rankings_NEW/", year,"/Defense/Up_to_Week=", week,"/", GLM_type,"_Adj_Opp"))
  dir.create(paste0("GROUP_LASSO/Rankings_NEW/", year,"/Offense/Up_to_Week=", week,"/", GLM_type,"_Adj_Opp_and_Other_BallSide")) # ifelse(efficiency, "_EFFICIENCY", "")))
  dir.create(paste0("GROUP_LASSO/Rankings_NEW/", year,"/Defense/Up_to_Week=", week,"/", GLM_type,"_Adj_Opp_and_Other_BallSide")) # ifelse(efficiency, "_EFFICIENCY", "")))
  
  dir.create(paste0("GROUP_LASSO/Selected_Estimates/", year))
  dir.create(paste0("GROUP_LASSO/Selected_Estimates/", year, "/", GLM_type,"_Adj_Opp_and_Other_BallSide")) # ifelse(efficiency, "_EFFICIENCY", "")))
  
  
  latest.date <- function(week, year){
    data <- read.csv(paste("Game_Logs/", year, "/All_DISTINCT_Condensed_Game_Logs_NO_NEUTRAL_with_WEEKS.csv", sep=""))
    if (year <= 2017) return(max(as.Date(subset(data, Wk == week)$Date, "%b %d, %Y")))
    if (year > 2017) return(max(as.Date(subset(data, Wk == week)$Date, "%m/%d/%y")))
    
  }
  
  latest.week <- function(year){
    data <- read.csv(paste("Game_Logs/", year, "/All_DISTINCT_Condensed_Game_Logs_NO_NEUTRAL_with_WEEKS.csv", sep=""))
    return(max(data$Wk))
  }
  
  
  if (week == "MAX"){ 
    max.week <- latest.week(year)
  } else {
    max.week <- week
  }
  
  max.date <- latest.date(max.week, year)
  
  
  
  full.df <- NULL
  
  Data_offense <- subset(read.csv(paste0("Game_Logs/", year, "/", 
                                         ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                         ifelse(efficiency, "EFFICIENCY_", "FINAL_"),
                                         "FULL_STATS_Offense&SpecialTeams.csv")), as.Date(Date) <= max.date)
  Data_defense <- subset(read.csv(paste0("Game_Logs/", year, "/", 
                                         ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                                         ifelse(efficiency, "EFFICIENCY_", "FINAL_"), 
                                         "FULL_STATS_Defense&SpecialTeams.csv")), as.Date(Date) <= max.date)
  
  
  Data_offense$X <- Data_defense$X <- NULL
  full.df <- full_join(Data_offense, Data_defense, 
                       by = c("Team", "Opponent", "Date", "Homefield"))
  
  ## Checking the # of games per each team
  print(table(table(full.df$Team)))
  
  
  ## Making sure factor level ORDERS correspond between Team & Opponent)
  full.df$Team <- factor(full.df$Team)
  full.df$Opponent <- factor(full.df$Opponent)
  full.df$Homefield <- factor(full.df$Homefield, levels=c("@","N",""))
  
  # Number of distinct colleges that year (+ non-major)
  n.teams <- nlevels(full.df$Team)
  
  
  
  
  
  ####################
  ####################
  ## GLM ADJUSTMENT ##
  ####################
  ####################
  
  
  ## Setting contrasts so that we have x_ij = mu + alpha_i + beta_j, 
  ## with mu being the "average team"
  contrasts(full.df$Team) <- contrasts(full.df$Opponent) <- contr.sum(n.teams)
  
  ## Making Homefield a NUMERIC VARIABLE to be modeled with SINGLE PARAMETER:
  ##  1 - Home, 0- Neutral, -1 - Away
  full.df$Homefield012 <- as.numeric(full.df$Homefield)-2
  
  
  
  
  ###########
  ## Gaussian GLM
  ###########
  
  if (GLM_type == "Gaussian"){
    
    for (stat in main.stats){
      print(stat)
      
      dec <- 1  
      
      na.ind <- which(apply(data.frame(full.df[,paste(stat,".x",sep="")],
                                       full.df[,c("Team","Opponent", "Homefield")],
                                       full.df[,paste0(complement.stats.list[[stat]], ".y")]),
                            1,
                            function(x) mean(is.na(x)) > 0))
      
      if (length(na.ind) > 0) full.df <- full.df[-na.ind, ]
      
      
      #########
      ## 1. Classic average rankings
      #########
      
      offensive.worth.vector <- tapply(full.df[, colnames(full.df) == paste0(stat, ".x")], full.df$Team, function(x) mean(x, na.rm=T))
      defensive.worth.vector <- tapply(full.df[, colnames(full.df) == paste0(stat, ".y")], full.df$Team, function(x) mean(x, na.rm=T))
      
      
      offensive.worth.df <- data.frame(Team=names(offensive.worth.vector), 
                                       Value=offensive.worth.vector,
                                       Rank=rank(-offensive.worth.vector*dec))
      defensive.worth.df <- data.frame(Team=names(defensive.worth.vector), 
                                       Value=defensive.worth.vector,
                                       Rank=rank(defensive.worth.vector*dec))
      rownames(offensive.worth.df) <- rownames(defensive.worth.df) <- NULL
      
      
      
      #########
      ### 2. The pure "adjusted for opponent" rankings.
      #########
      
      ### In case teams don't have that specific stat at all (e.g. Non-Majors)
      clean.df <- data.frame(Stat=full.df[,paste(stat,".x",sep="")],
                             full.df[,c("Team","Opponent", "Homefield012")])
      clean.df <- na.omit(clean.df)
      clean.df$Team <- factor(clean.df$Team)
      clean.df$Opponent <- factor(clean.df$Opponent)
      
      ## Setting contrasts so that we have x_ij = mu + alpha_i + beta_j, 
      ## with mu being the "average team"
      contrasts(clean.df$Team) <- contr.sum(nlevels(clean.df$Team))
      contrasts(clean.df$Opponent) <- contr.sum(nlevels(clean.df$Opponent))
      n.teams <- nlevels(clean.df$Team)
      n.opps <- nlevels(clean.df$Opponent)
      
      
      lm.obj.adj.opp <- lm(Stat ~ .,
                           data=clean.df)
      lm.obj.adj.opp
      
      
      
      offensive.worth.adj.opp <- c(coef(lm.obj.adj.opp)[1] + coef(lm.obj.adj.opp)[2:n.teams],
                                   coef(lm.obj.adj.opp)[1] - sum(coef(lm.obj.adj.opp)[2:n.teams])) # + coef(lm.obj.adj.opp)['Homefield012']
      defensive.worth.adj.opp <- c(coef(lm.obj.adj.opp)[1] + coef(lm.obj.adj.opp)[(n.teams+1):(n.teams+n.opps-1)],
                                   coef(lm.obj.adj.opp)[1] - sum(coef(lm.obj.adj.opp)[(n.teams+1):(n.teams+n.opps-1)])) # + coef(lm.obj.adj.opp)['Homefield012']
      
      
      
      offensive.worth.adj.opp.df <- data.frame(Team=levels(clean.df$Team), 
                                               Value=offensive.worth.adj.opp,
                                               Rank=rank(-offensive.worth.adj.opp*dec))
      
      offense.lacking.teams <- levels(full.df$Team)[! levels(full.df$Team) %in% levels(clean.df$Team)]
      
      ## If some teams were missed => create a "Value=NA, Rank=NA" row with their name.
      if (length(offense.lacking.teams) > 0){
        offensive.worth.adj.opp.df <- rbind(offensive.worth.adj.opp.df,
                                            data.frame(Team = offense.lacking.teams,
                                                       Value = NA,
                                                       Rank=NA))
      }
      
      
      defensive.worth.adj.opp.df <- data.frame(Team=levels(factor(clean.df$Opponent)), 
                                               Value=defensive.worth.adj.opp,
                                               Rank=rank(defensive.worth.adj.opp*dec))
      defense.lacking.teams <- levels(full.df$Team)[! levels(full.df$Team) %in% levels(clean.df$Opponent)]
      
      
      ## If some teams were missed => create a "Value=NA, Rank=NA" row with their name.
      if (length(defense.lacking.teams) > 0){
        defensive.worth.adj.opp.df <- rbind(defensive.worth.adj.opp.df,
                                            data.frame(Team = defense.lacking.teams,
                                                       Value = NA,
                                                       Rank=NA))
      }
      
      rownames(offensive.worth.adj.opp.df) <- rownames(defensive.worth.adj.opp.df) <- NULL
      
      
      ## Sorting data by the team name.
      offensive.worth.adj.opp.df <- offensive.worth.adj.opp.df[order(offensive.worth.adj.opp.df$Team),]
      defensive.worth.adj.opp.df <- defensive.worth.adj.opp.df[order(defensive.worth.adj.opp.df$Team),]
      
      
      
      
      #########
      ###  3. The "adjusted for opponent" PLUS for "other side of the ball performance" rankings.
      #########
      
      
      if (length(complement.stats.list[[stat]]) ==0){  
        offensive.worth.adj.opp.and.other.side.df <- offensive.worth.adj.opp.df
        defensive.worth.adj.opp.and.other.side.df <- defensive.worth.adj.opp.df
      } else {
        
        # Centering the "other side of the ball" predictors allows for easier "intercept interpretation":
        # now it projects onto AVERAGE values in those predictors (which makes MUCH MORE sense than 0)
        
        clean.df <- data.frame(Stat=full.df[,paste(stat,".x",sep="")],
                               full.df[,c("Team","Opponent", "Homefield012")], 
                               #scale(full.df[,stats.y[stat.logic.ind]], scale=F)
                               scale(full.df[,paste0(complement.stats.list[[stat]], ".y")], scale=F)
        )
        
        colnames(clean.df)[(ncol(clean.df)-length(complement.stats.list[[stat]])+1):ncol(clean.df)] <- complement.stats.list[[stat]]
        clean.df <- na.omit(clean.df)
        clean.df$Team <- factor(clean.df$Team)
        clean.df$Opponent <- factor(clean.df$Opponent)
        
        ## Setting contrasts so that we have x_ij = mu + alpha_i + beta_j, 
        ## with mu being the "average team"
        contrasts(clean.df$Team) <- contr.sum(nlevels(clean.df$Team))
        contrasts(clean.df$Opponent) <- contr.sum(nlevels(clean.df$Opponent))
        n.teams <- nlevels(clean.df$Team)
        n.opps <- nlevels(clean.df$Opponent)
        
        lm.obj.adj.opp.and.other.side <- 
          lm(as.formula(paste0("Stat ~ Team + Opponent + Homefield012 +", paste0(paste("ns(",complement.stats.list[[stat]], ",", paste0(spline.df, ")"), sep="", collapse="+")), 
                               collapse="")),
             data=clean.df)
        
        ## "Centering" the spline components, so that intercept still presents an interpretable quantity
        mmat <- model.matrix(lm.obj.adj.opp.and.other.side)
        mmat[, ncol(mmat)-((length(complement.stats.list[[stat]])*spline.df-1) :0)] <- scale(mmat[, ncol(mmat)-((length(complement.stats.list[[stat]])*spline.df-1) :0)],
                                                                                             scale=F)
        
        lm.obj.adj.opp.and.other.side <- lm(clean.df$Stat ~ mmat[,-1])
         
        
        
        offensive.worth.adj.opp.and.other.side <- c(coef(lm.obj.adj.opp.and.other.side)[1] + coef(lm.obj.adj.opp.and.other.side)[2:n.teams],
                                                    coef(lm.obj.adj.opp.and.other.side)[1] - sum(coef(lm.obj.adj.opp.and.other.side)[2:n.teams]))   # + coef(lm.obj.adj.opp.and.other.side)['Homefield012']
        defensive.worth.adj.opp.and.other.side <- c(coef(lm.obj.adj.opp.and.other.side)[1] + coef(lm.obj.adj.opp.and.other.side)[(n.teams+1):(n.teams+n.opps-1)],
                                                    coef(lm.obj.adj.opp.and.other.side)[1] - sum(coef(lm.obj.adj.opp.and.other.side)[(n.teams+1):(n.teams+n.opps-1)]))  # + coef(lm.obj.adj.opp.and.other.side)['Homefield012']
        
        
        # Offensive side won't have "Non-Major", as the rows 
        offensive.worth.adj.opp.and.other.side.df <- data.frame(Team=levels(clean.df$Team), 
                                                                Value=offensive.worth.adj.opp.and.other.side,
                                                                Rank=rank(-offensive.worth.adj.opp.and.other.side*dec))
        offense.lacking.teams <- levels(full.df$Team)[! levels(full.df$Team) %in% levels(clean.df$Team)]
        
        ## If some teams were missed => create a "Value=NA, Rank=NA" row with their name.
        if (length(offense.lacking.teams) > 0){
          offensive.worth.adj.opp.and.other.side.df <- rbind(offensive.worth.adj.opp.and.other.side.df,
                                                             data.frame(Team = offense.lacking.teams,
                                                                        Value = NA,
                                                                        Rank=NA))
        }
        
        defensive.worth.adj.opp.and.other.side.df <- data.frame(Team=levels(factor(clean.df$Opponent)), 
                                                                Value=defensive.worth.adj.opp.and.other.side,
                                                                Rank=rank(defensive.worth.adj.opp.and.other.side*dec))
        defense.lacking.teams <- levels(full.df$Team)[! levels(full.df$Team) %in% levels(clean.df$Opponent)]
        
        
        ## If some teams were missed => create a "Value=NA, Rank=NA" row with their name.
        if (length(defense.lacking.teams) > 0){
          defensive.worth.adj.opp.and.other.side.df <- rbind(defensive.worth.adj.opp.and.other.side.df,
                                                             data.frame(Team = defense.lacking.teams,
                                                                        Value = NA,
                                                                        Rank=NA))
        }
        
        rownames(offensive.worth.adj.opp.and.other.side.df) <- rownames(defensive.worth.adj.opp.and.other.side.df) <- NULL
        
        ## Sorting data by the team name.
        offensive.worth.adj.opp.and.other.side.df <- offensive.worth.adj.opp.and.other.side.df[order(offensive.worth.adj.opp.and.other.side.df$Team),]
        defensive.worth.adj.opp.and.other.side.df <- defensive.worth.adj.opp.and.other.side.df[order(defensive.worth.adj.opp.and.other.side.df$Team),]
        
        
      }
      
      
      
      write.csv(coef(lm.obj.adj.opp.and.other.side)[(which(names(coef(lm.obj.adj.opp.and.other.side)) == "mmat[, -1]Homefield012")+1):length(coef(lm.obj.adj.opp.and.other.side))],
                row.names=T,
                file=paste0("GROUP_LASSO/Selected_Estimates/", year, "/", 
                            GLM_type,"_Adj_Opp_and_Other_BallSide", 
                            "/", 
                            ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                            ifelse(efficiency, "EFFICIENCY_", ""),
                            stat,".csv"))
      
      
      write.csv(offensive.worth.df,
                row.names=F,
                file=paste0("GROUP_LASSO/Rankings_NEW/", year, "/Offense/Up_to_Week=", week, "/", 
                            "Classic/", 
                            ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                            ifelse(efficiency, "EFFICIENCY_", ""),
                            stat,".csv"))
      
      write.csv(defensive.worth.df,
                row.names=F,
                file=paste0("GROUP_LASSO/Rankings_NEW/", year, "/Defense/Up_to_Week=", week, "/", 
                            "Classic/", 
                            ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                            ifelse(efficiency, "EFFICIENCY_", ""),
                            stat,".csv"))
      
      
      write.csv(offensive.worth.adj.opp.df,
                row.names=F,
                file=paste0("GROUP_LASSO/Rankings_NEW/", year, "/Offense/Up_to_Week=", week, "/", 
                            GLM_type,"_Adj_Opp/", 
                            ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                            ifelse(efficiency, "EFFICIENCY_", ""),
                            stat,".csv"))
      
      write.csv(defensive.worth.adj.opp.df,
                row.names=F,
                file=paste0("GROUP_LASSO/Rankings_NEW/", year, "/Defense/Up_to_Week=", week, "/", 
                            GLM_type,"_Adj_Opp/", 
                            ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                            ifelse(efficiency, "EFFICIENCY_", ""),
                            stat,".csv"))
      
      
      
      write.csv(offensive.worth.adj.opp.and.other.side.df,
                row.names=F,
                file=paste0("GROUP_LASSO/Rankings_NEW/", year,"/Offense/Up_to_Week=", week, "/",
                            GLM_type,"_Adj_Opp_and_Other_BallSide", "/", 
                            ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                            ifelse(efficiency, "EFFICIENCY_", ""),
                            stat, ".csv"))
      
      write.csv(defensive.worth.adj.opp.and.other.side.df,
                row.names=F,
                file=paste0("GROUP_LASSO/Rankings_NEW/", year,"/Defense/Up_to_Week=", week, "/", 
                            GLM_type,"_Adj_Opp_and_Other_BallSide", "/", 
                            ifelse(IMPUTE.NONMAJOR.NA, "IMPUTED_", ""),
                            ifelse(efficiency, "EFFICIENCY_", ""),
                            stat, ".csv"))
      
      
    }    
  }
  
  
}







