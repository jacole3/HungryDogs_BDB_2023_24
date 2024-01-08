# Note: some miscellaneous cleansing code, including but not limited to checking for discrepancies in tricodes (e.g. JAX vs. JAC), was omitted here
# This is because certain lines of code didn't result in any changes to the data set

gc(reset = TRUE)
Sys.setenv('R_MAX_VSIZE'=64000000000)

library(vctrs)
library(readxl)
library(tidyselect)
library(naniar)
library(tidyr)
library(plyr)
library(statsr)
library(reactable)
library(plotly)
library(ggplot2)
library(survey)
library(rafalib)
library(modelr)
library(na.tools)
library(ggimage)
library(ggrepel)
library(remotes)
library(reshape2)
library(glue)
library(readr)
library(car)
library(caret)
library(rsample)      
library(ggthemes)
library(scales)
library(Metrics)
library(MLmetrics)
library(here)
library(gbm)
library(devtools)
library(nflfastR)
library(teamcolors)
library(slider)
library(Rfast)
library(gganimate)
library(viridis)
library(deldir)
library(ggvoronoi)
library(randomForest)
library(ranger)
library(xgboost)
library(tidymodels)
library(usemodels)
library(textrecipes)
library(vip)
library(pROC)
library(patchwork)
library(tictoc)
library(data.table)
library(nflreadr)
library(nflplotR)
library(reticulate)
library(nflverse)
library(tidyverse)
library(dplyr)
setwd("C:/Users/justi/OneDrive/Penn/BigDataBowl")

# Use this code to install ggvoronoi package if it doesn't work immediately
# url_geo <- "https://cran.r-project.org/src/contrib/Archive/rgeos/rgeos_0.6-4.tar.gz"
# pkgFile_geo <- "rgeos_0.6-4.tar.gz"
# download.file(url = url_geo, destfile = pkgFile_geo)
# 
# url_voronoi <- "https://cran.r-project.org/src/contrib/Archive/ggvoronoi/ggvoronoi_0.8.5.tar.gz"
# pkgFile_voronoi <- "ggvoronoi_0.8.5.tar.gz"
# download.file(url = url_voronoi, destfile = pkgFile_voronoi)
# 
# require(devtools)
# install_version("rgeos", version = "0.6.4", repos = "http://cran.us.r-project.org")
# install_version("ggvoronoi", version = "0.8.5", repos = "http://cran.us.r-project.org")
# 
# # # Install package
# install.packages(pkgs=pkgFile_geo, type="source", repos=NULL)
# 
# # Delete package tarball
# unlink(pkgFile)

options(digits = 4)
options(scipen = 999) 

games <- fread("games.csv")
players <- fread("players.csv")
plays <- fread("plays.csv")
tackles <- fread("tackles.csv")
tracking_week_1 <- fread("tracking_week_1.csv")
# tracking_week_2 <- fread("tracking_week_2.csv")
# tracking_week_3 <- fread("tracking_week_3.csv")
# tracking_week_4 <- fread("tracking_week_4.csv")
# tracking_week_5 <- fread("tracking_week_5.csv")
# tracking_week_6 <- fread("tracking_week_6.csv")
# tracking_week_7 <- fread("tracking_week_7.csv")
# tracking_week_8 <- fread("tracking_week_8.csv")
# tracking_week_9 <- fread("tracking_week_9.csv")

# For this file, we only include Week 1 so it would run faster on the average laptop
# In the Final_Model_And_Plots GitHub file, we combine data from this file, but for every week instead of just Week 1

tracking_combined <- rbind(tracking_week_1)
                           # , tracking_week_2, tracking_week_3,
                           # tracking_week_4, tracking_week_5, tracking_week_6,
                           # tracking_week_7, tracking_week_8, tracking_week_9)

# Before anything else, fix Robbie Chosen/Robby Anderson name error
# View(players %>% filter(nflId == 43808))
# View(tracking_combined %>% filter(nflId == 43808))
players <- players %>% mutate(displayName = 
                                ifelse(nflId == 43808, "Robby Anderson", displayName))

# These plays don't have any tracking data besides the football, get rid of them
# View(tracking_combined %>% filter(gameId == 2022101605, playId == 2970))
tracking_combined <- tracking_combined %>%
  filter(gameId != 2022101605 | playId != 2970)

# View(tracking_combined %>% filter(gameId == 2022102307, playId == 1505))
tracking_combined <- tracking_combined %>%
  filter(gameId != 2022102307 | playId != 1505)

# View(tracking_combined %>% filter(gameId == 2022091801, playId == 63))
tracking_combined <- tracking_combined %>%
  filter(gameId != 2022091801 | playId != 63)

# These plays have multiple "first_contact" events
# View(tracking_combined %>% filter(gameId == 2022100209, playId == 1581, event == "first_contact"))
# View(tracking_combined %>% filter(gameId == 2022103100, playId == 1689, event == "first_contact"))
# View(tracking_combined %>% filter(gameId == 2022103004, playId == 2106, event == "first_contact"))
FirstContact_Events <- tracking_combined %>% filter(event == "first_contact") %>%
  group_by(gameId, playId, nflId) %>%
  mutate(FirstContact_rank = rank(frameId, ties.method = "first")) %>%
  ungroup()
FirstContact_Events <- FirstContact_Events %>% 
  select(c("gameId", "playId", "nflId", "displayName", "frameId", "FirstContact_rank"))
MultiFirstContact_Plays <- FirstContact_Events %>% 
  group_by(gameId, playId) %>% 
  summarize(n = n(), Frames = n_distinct(frameId)) %>% arrange(desc(Frames))
tracking_combined <- tracking_combined %>%
  left_join(FirstContact_Events, by = c("gameId", "playId", "nflId", "displayName", "frameId"))

table(tracking_combined$event)
# For any "FirstContact_rank" bigger than 1, change the event name to NA
tracking_combined <- tracking_combined %>% mutate(event =
      ifelse(is.na(FirstContact_rank), event,
             ifelse(FirstContact_rank > 1 & event == "first_contact", NA, event)))

rm(FirstContact_Events, MultiFirstContact_Plays)
tracking_combined <- tracking_combined %>% select(-"FirstContact_rank")

# Note that o means orientation, and dir means direction, both scaled 0-360
# In both cases, 0 is facing the visitor sideline (i.e. where Y = 53.3)
# Thus, both 0 and 180 are parallel to the LOS when discussing o and dir
# Similarly, X = 120 at back of visitor end zone (X = 0 at back of home EZ)

# And playDirection refers to which way the offense is facing, NOT the actual play direction
# E.G. a rush outside the left tackle doesn't necessarily have playDirection == left

# To clear memory, now get rid of the individual weeks
rm(tracking_week_1)
   # , tracking_week_2, tracking_week_3,
   # tracking_week_4, tracking_week_5, tracking_week_6,
   # tracking_week_7, tracking_week_8, tracking_week_9)

# Code that will eventually help w/ gap classification and pre-snap alignment
# To start, standardize so that the highest "y" is always to offense's left
# I.e., make it so that it doesn't matter which end zone the offense is aiming at
tracking_combined <- tracking_combined %>%
  mutate(y = ifelse(playDirection == "right", y, (53.3 - y)))

# Likewise, adjust "o" and "dir" in same manner, make 0 always be offense's left
# So 90 is always toward the EZ offense is aiming at, 180 is to offense's right, etc.
tracking_combined <- tracking_combined %>%
  mutate(o = ifelse(playDirection == "right", o, 
                    ifelse((playDirection == "left" & o < 180), o + 180, o - 180)))
tracking_combined <- tracking_combined %>%
  mutate(dir = ifelse(playDirection == "right", dir, 
                      ifelse((playDirection == "left" & dir < 180), dir + 180, dir - 180)))

# Same adjustment for "x" - make high "x" always be where offense is aiming at
tracking_combined <- tracking_combined %>%
  mutate(x = ifelse(playDirection == "right", x, (120 - x)))

# Now create line of scrimmage for each play using ball data
# This link explains when the frames start: https://www.kaggle.com/competitions/nfl-big-data-bowl-2024/discussion/447639

# Recall that Frame 1 doesn't always mean the original LOS (but it does for designed runs)
Frame1_Ball_Location <- tracking_combined %>%
  filter(club == "football", frameId == 1) %>%
  select(gameId, playId, x, y) %>%
  rename(Ball_X_Frame1 = x, Ball_Y_Frame1 = y)

tracking_combined <- tracking_combined %>%
  left_join(Frame1_Ball_Location, by = c("playId", "gameId"))

tracking_combined <- tracking_combined %>%
  mutate(X_dist_FromBallFrame1 = x - Ball_X_Frame1, Y_distFromMOF = y - 26.65,
         Y_dist_FromBallFrame1 = y - Ball_Y_Frame1)

# Code for where the ball is at any given point, and each player's distance from it
ball_df <- tracking_combined %>% 
  filter(club == "football") %>% 
  select(gameId, playId, frameId, x, y) %>% 
  rename(ball_x = x,
         ball_y = y)

tracking_combined <- tracking_combined %>% 
  left_join(ball_df, by = c("playId", "gameId", "frameId"))

tracking_combined <- tracking_combined %>% 
  mutate(TotDistFromBall = sqrt((x - ball_x) ^ 2 + (y - ball_y) ^ 2),
         Y_DistFromBall = (y - ball_y), X_DistFromBall = (x - ball_x),
         Y_AbsDistFromBall = abs(y - ball_y), X_AbsDistFromBall = abs(x - ball_x))
rm(ball_df, Frame1_Ball_Location)

# Likewise, add the ball's distance from goal line and sideline
tracking_combined <- tracking_combined %>%
  mutate(Ball_DistFromGoalLine = 110 - ball_x,
         Ball_DistFromSideline = ifelse(ball_y >= 26.65, 53.3 - ball_y, ball_y))

# And mutate a binary variable for being near goal line or sideline
tracking_combined <- tracking_combined %>% 
  mutate(BallNearGoalLine = ifelse(Ball_DistFromGoalLine <= 3, 1, 0),
         BallNearSideline = ifelse(Ball_DistFromSideline <= 3, 1, 0))

nflverse_pbp <- nflfastR::load_pbp(2022)
nflverse_pbp <- nflverse_pbp %>% filter(week == 1)
# This gives descriptions of NFLVerse columns: View(field_descriptions)

# View(nflverse_pbp %>% filter(run_location == "middle" & !is.na(run_gap)))
# This is empty, meaning all "middle" runs have NA for run_gap - adjust this
nflverse_pbp <- nflverse_pbp %>% mutate(
  run_gap = ifelse(run_location == "middle", "center", run_gap))

# General data cleansing/checking for errors
sort(table(players$nflId)) # no duplicates
sort(table(players$displayName)) # no duplicates

table(plays$passResult)
# C means complete, R means scramble

# View(plays %>% filter(str_detect(playDescription, "(sack)")))
# This is empty, so no sacks are included in the data set

# View(plays %>% filter(!passResult %in% c("C", "R")))
# This is empty, therefore all plays that don't have "C" or "R" are designed runs
plays <- plays %>% mutate(passResult = 
                            ifelse(passResult %in% c("C", "R"), passResult, NA))

# Arrange plays so they are sorted chronologically
plays <- plays %>% arrange(gameId, playId)

# The absoluteYardlineNumber is not accurate, just use nflverse's yardline_100
# And yardlineNumber doesn't give full field picture, e.g. -35 and +35 both say 35
# View(plays %>% filter(possessionTeam == yardlineSide & yardlineNumber < 20))
# This shows us all plays when the offensive team is inside its own 20
# Sometimes absoluteYardlineNumber is under 30, sometimes it's more than 100
plays <- plays %>% select(-"absoluteYardlineNumber", -"yardlineNumber")

# Fix one column that is spelled wrong
plays <- plays %>% rename(visitorTeamWinProbabilityAdded = `visitorTeamWinProbilityAdded`)

# View(plays %>% filter(is.na(passProbability))) - these are all penalties
# View(plays %>% filter(is.na(passProbability) & is.na(penaltyYards))) - empty
# View(plays %>% filter(is.na(preSnapHomeTeamWinProbability))) - empty
# View(plays %>% filter(is.na(preSnapVisitorTeamWinProbability))) - empty
# View(plays %>% filter(is.na(homeTeamWinProbabilityAdded))) - empty
# View(plays %>% filter(is.na(visitorTeamWinProbabilityAdded))) - empty
# View(plays %>% filter(is.na(expectedPoints))) - empty

# View(plays %>% filter(is.na(expectedPointsAdded)))
# One play to fix here, a David Montgomery run on 10/09/22
# Since it's only one play, just find the right answer from nflverse
# View(nflverse_pbp %>% filter(old_game_id == 2022100904, qtr == 2) %>% select(1:35, 73, 74))
plays <- plays %>% mutate(expectedPointsAdded =
                            ifelse(is.na(expectedPointsAdded), -0.29447450, expectedPointsAdded))             

# Then, next obligation is to properly merge everything
# Must change the class of nflverse_pbp to make it compatible with the rest
class(nflverse_pbp) <- c("data.table", "data.frame")

# Also must coerce old_game_id in nflverse into a numeric variable
nflverse_pbp$old_game_id <- as.numeric(nflverse_pbp$old_game_id)

PlaysAndGames <- merge(x = games, y = plays, by = "gameId") 
PlaysAndGames_NFLVerse <- merge(x = PlaysAndGames, y = nflverse_pbp, 
                                by.x = c("gameId", "playId", "season", "week", "down"), 
                                by.y = c("old_game_id", "play_id",  "season", "week", "down")) 
rm(games, plays, nflverse_pbp, PlaysAndGames)

max(tackles$pff_missedTackle) # no player had more than 1 MT on a play

# Use all.y = TRUE, since we also want players who didn't make tackles
TrackingWithTackles <- merge(x = tackles, y = tracking_combined,
                             by = c("gameId", "playId", "nflId"), all.y = TRUE)

TrackingWithTackles_PlayerNames <- merge(x = TrackingWithTackles, y = players,
                                         by = c("nflId"))
# If we wanted to keep club == "football", we could use all.x = TRUE here
rm(players, tackles, tracking_combined, TrackingWithTackles)

# Check for name discrepancies again (all players, not just ball-carriers)
# View(TrackingWithTackles_PlayerNames %>% filter(displayName.x != displayName.y))
TrackingWithTackles_PlayerNames <- TrackingWithTackles_PlayerNames %>%
  select(-"displayName.x")
TrackingWithTackles_PlayerNames <- TrackingWithTackles_PlayerNames %>%
  rename(displayName = displayName.y)

max(PlaysAndGames_NFLVerse$interception, na.rm = TRUE) # there are no interceptions

# Make a new version of PlaysAndGames_NFLVerse with fewer columns
NFLVerse_Reduced <- PlaysAndGames_NFLVerse %>% 
  select(-c(7, 10:11, 16, 20, 31, 42:46, 48:49, 51, 55:57, 59, 61:62, 64:68, 70:75, 
            80:87, 89:94, 98:105, 108:117, 122:129, 132:133, 135:144, 149:177, 179:180,
            183, 185:187, 189:191, 196:200, 202, 204:244, 
            249:309, 311:315, 317:322, 325:331, 333:367, 370, 372:376,
            378:383, 385, 387:398, 400, 407:408))

rm(PlaysAndGames_NFLVerse)
MergedData <- merge(x = NFLVerse_Reduced, y = TrackingWithTackles_PlayerNames,
                    by = c("gameId", "playId"))
rm(TrackingWithTackles_PlayerNames, NFLVerse_Reduced)

# Arrange it by game ID, play ID, player ID, and frame ID
MergedData <- MergedData %>% arrange(gameId, playId, nflId, frameId)

# Now we create a variable for where the ball was at the time of the snap
# But this is only approximate, since NFLVerse doesn't round to decimals of yards
# For designed runs, we already have precise ball location at LOS (Frame1_Ball_Location above)
MergedData <- MergedData %>%
  mutate(X_LOS_Approx = 110 - yardline_100)

MergedData <- MergedData %>%
  mutate(X_distFromLOS_Approx = x - X_LOS_Approx)

# Add a WP success column
MergedData <- MergedData %>% 
  mutate(WPSuccess = ifelse(wpa > 0, 1, 0))
# Check for NAs: View(MergedData %>% filter(is.na(WPSuccess)))

# And do the same for defensive WPA (just negative offensive WPA)
MergedData <- MergedData %>% mutate(DefWPA = (-1) * wpa)

# Add a column for "Is Ball Carrier"
MergedData <- MergedData %>% mutate(IsBallCarrier = 
                                      ifelse(ballCarrierId == nflId, TRUE, FALSE))

# Diagnose if any plays don't have any "IsBallCarrier"
Frame1_DF <- MergedData %>% filter(frameId == 1)
BallCarriers_Snap <- Frame1_DF %>% 
  group_by(playId, gameId) %>%
  summarize(Players = n(), BallCarriers = sum(IsBallCarrier)) %>%
  arrange(desc(BallCarriers), desc(Players))
# Every play has exactly one ball-carrier
rm(BallCarriers_Snap, Frame1_DF)

# Use first_contact, ball_x, and X_LOS_Approx to create yards after contact
# Note that even some plays with tackles don't have an event == "first_contact"
# E.G. View(MergedData %>% filter(playId == 56)) -- that means tackle was first contact
BallLocation_FirstContact <- MergedData %>% filter(IsBallCarrier == TRUE, event == "first_contact") %>%
  select(gameId, playId, ballCarrierId, ball_x) %>%
  rename(ball_x_FirstContact = ball_x)
MergedData <- merge(x = MergedData, y = BallLocation_FirstContact,
                    by = c("gameId", "playId", "ballCarrierId"), all.x = TRUE)

MergedData <- MergedData %>% mutate(YdsBeforeContact = ball_x_FirstContact - X_LOS_Approx)
MergedData <- MergedData %>% mutate(YdsBeforeContact = 
                                      ifelse(is.na(YdsBeforeContact), prePenaltyPlayResult, YdsBeforeContact))
MergedData <- MergedData %>% mutate(YdsAfterContact = prePenaltyPlayResult - YdsBeforeContact)
rm(BallLocation_FirstContact)

# Find other ways to filter down data, e.g. excluding garbage time
MergedData <- MergedData %>%
  filter(wp >= 0.05 & wp <= 0.95)

# Turn weather into a numeric variable using str_extract
# The given "temp" variable is all NAs
MergedData <- MergedData %>% 
  mutate(Temperature = (str_extract(MergedData$weather, "\\b\\d+")))
class(MergedData$Temperature) <- "numeric"
MergedData <- MergedData %>% select(-"weather")

# Similarly, change height into inches, rather than feet-inches
convert_to_inches <- function(height) {
  parts <- strsplit(height, "-")[[1]]
  feet <- as.numeric(parts[1])
  inches <- as.numeric(parts[2])
  total_inches <- feet * 12 + inches
  return(total_inches)
}

MergedData <- MergedData %>% 
  mutate(height_inches = sapply(height, convert_to_inches))
MergedData <- MergedData %>% select(-"height")

# If needed, calculate player age by using birth date and game date
# MergedData <- MergedData %>% 
#  mutate(NumericBirthDate = as.Date(birthDate, origin = "1970-01-01"))
# MergedData <- MergedData %>% 
#   mutate(NumericGameDate = as.Date(time, origin = "1970-01-01"))
# MergedData <- MergedData %>% 
#   mutate(Age_Days = NumericGameDate - NumericBirthDate)
# MergedData <- MergedData %>% mutate(Age_Years = Age_Days / 365.25)
MergedData <- MergedData %>% select(-c("birthDate", "time")) # "Age_Days"
# class(MergedData$Age_Years) <- "numeric"

# Also code for each player's maximum speed
TopSeasonSpeeds <- MergedData %>% 
  group_by(nflId, displayName) %>%
  mutate(Indiv_SpeedRank = rank(-s, ties.method = "first")) %>%
  ungroup()
TopSeasonSpeeds <- TopSeasonSpeeds %>% select(nflId, displayName, s, Indiv_SpeedRank)  
TopSeasonSpeeds <- TopSeasonSpeeds %>% filter(Indiv_SpeedRank == 1)
TopSeasonSpeeds <- TopSeasonSpeeds %>% select(-"Indiv_SpeedRank")
TopSeasonSpeeds <- TopSeasonSpeeds %>% rename(Season_MaxSpeed = s)

MergedData <- merge(x = MergedData, y = TopSeasonSpeeds,
                    by = c("nflId", "displayName"))
rm(TopSeasonSpeeds)

table(MergedData$offenseFormation)
# Make simplified version for under center vs. not under center
MergedData <- MergedData %>% mutate(QBAlignment =
                                      ifelse(offenseFormation %in% c("EMPTY", "PISTOL", "SHOTGUN", "WILDCAT"), "Gun", "UC"))

# Make sure we have both offensive and defensive players
table(MergedData$position)

# Make broader "PosGroup" label from there
MergedData <- MergedData %>% mutate(PosGroup = ifelse(position %in% c("C", "G", "T"), "OL",
                                                      ifelse(position %in% c("CB", "DB", "FS", "SS"), "DB",
                                                             ifelse(position %in% c("DE", "DT", "NT"), "DL",
                                                                    ifelse(position %in% c("FB", "RB"), "RB",
                                                                           ifelse(position %in% c("ILB", "MLB", "OLB"), "LB",
                                                                                  ifelse(position == "QB", "QB",
                                                                                         ifelse(position == "TE", "TE", "WR"))))))))
table(MergedData$PosGroup)

MergedData <- MergedData %>%
  mutate(PlayerSideOfBall = ifelse(((club == homeTeamAbbr) &
                                      (possessionTeam == homeTeamAbbr)) |
                                     ((club == visitorTeamAbbr) &
                                        (possessionTeam == visitorTeamAbbr)),
                                   "offense",
                                   "defense"))

# Get rid of some columns not needed for our specific models (can add back later if desired)
MergedData <- MergedData %>% 
  select(-c("season", "gameDate", "quarter", "yardlineSide", "preSnapHomeScore",
            "preSnapVisitorScore", "preSnapHomeTeamWinProbability", "preSnapVisitorTeamWinProbability",
            "homeTeamWinProbabilityAdded", "visitorTeamWinProbabilityAdded", "expectedPoints",
            "quarter_seconds_remaining", "half_seconds_remaining", "game_seconds_remaining",
            "no_huddle", "posteam_score", "defteam_score", "score_differential",
            "cp", "cpoe", "qb_epa", "collegeName"))

# Add distance ranks for each side of ball
# E.G. who is closest to ball-carrier at any given point of the play
MergedData <- MergedData %>%
  group_by(gameId, playId, frameId, PlayerSideOfBall) %>%
  mutate(TotDistFromBall_Rank_BySideOfBall = rank(TotDistFromBall, ties.method = "first"),
         Y_AbsDistFromBall_Rank_BySide = rank(Y_AbsDistFromBall, ties.method = "first"),
         X_AbsDistFromBall_Rank_BySide = rank(X_AbsDistFromBall, ties.method = "first"),
         Y_NetDistFromBall_Rank_BySide = rank(Y_DistFromBall, ties.method = "first"),
         X_NetDistFromBall_Rank_BySide = rank(X_DistFromBall, ties.method = "first")) %>%
  ungroup()
# table(MergedData$TotDistFromBall_Rank_BySideOfBall); rank is never higher than 11

# And same idea for "overall" ranks (i.e., doesn't matter if player is on offense or defense)
MergedData <- MergedData %>%
  group_by(gameId, playId, frameId) %>%
  mutate(TotDistFromBall_Rank_OVR = rank(TotDistFromBall, ties.method = "first"),
         Y_AbsDistFromBall_Rank_OVR = rank(Y_AbsDistFromBall, ties.method = "first"),
         X_AbsDistFromBall_Rank_OVR = rank(X_AbsDistFromBall, ties.method = "first"),
         Y_NetDistFromBall_Rank_OVR = rank(Y_DistFromBall, ties.method = "first"),
         X_NetDistFromBall_Rank_OVR = rank(X_DistFromBall, ties.method = "first")) %>%
  ungroup()
# table(MergedData$TotDistFromBall_Rank_OVR); rank is never higher than 22

# Make a column for an individual having a tackle attempt
MergedData <- MergedData %>% mutate(IndivTackleAttempt =
                                      ifelse(!is.na(tackle) | !is.na(assist) | !is.na(forcedFumble) | !is.na(pff_missedTackle), 1, 
                                             ifelse(PlayerSideOfBall == "defense", 0, NA)))

# Add columns for "total tackles", as well as a binary column for whether one had a tackle
# Note that this is empty: View(MergedData %>% filter(forcedFumble > 0, tackle == 0, assist == 0))
# I.e., it's impossible to force a fumble and not be credited with tackle or assist
MergedData <- MergedData %>% mutate(IndivTotTackles = 
                                      ifelse(!is.na(tackle), tackle + 0.5*assist,
                                             ifelse(PlayerSideOfBall == "defense", 0, NA)))
MergedData <- MergedData %>% mutate(Indiv_MadeTackle = ifelse(IndivTotTackles > 0, TRUE, FALSE))
# Note that we can't add a column for "tackler name" b/c some plays have more than one
# I.e., this would result in multiple rows per frame, per play

# Create a variable for "did the entire play have a missed tackle at all?"
TackleAttempts_Merged <- MergedData %>% 
  filter(!is.na(tackle) | !is.na(assist) | !is.na(forcedFumble) | !is.na(pff_missedTackle))
# This group_by shows most missed tackles by any single player on a play
MissedTackles_Merged_ByPlayer <- TackleAttempts_Merged %>% 
  group_by(gameId, playId, nflId, displayName) %>%
  summarize(Frames = n(), MissedTackles = pff_missedTackle[1]) %>% arrange(desc(MissedTackles), playId)
# Now we sum the individual missed tackles, for teammates who each missed one on the same play
MissedTackles_Merged <- MissedTackles_Merged_ByPlayer %>%
  group_by(gameId, playId) %>%
  summarize(TeamPlayers_TackleAtt = n(), TeamMT_FullPlay = sum(MissedTackles)) %>% arrange(desc(TeamMT_FullPlay))
MergedData <- merge(x = MergedData, y = MissedTackles_Merged,
                    by = c("gameId", "playId"), all.x = TRUE)
MergedData <- MergedData %>% 
  replace_na(list(TeamPlayers_TackleAtt = 0, TeamMT_FullPlay = 0)) 
MergedData <- MergedData %>% arrange(gameId, playId, nflId, frameId)
rm(TackleAttempts_Merged, MissedTackles_Merged, MissedTackles_Merged_ByPlayer)

# Also make variable for "penalized tackle", first for individuals
MergedData <- MergedData %>% mutate(IndivTackle_Penalized =
                                      ifelse((tackle == 1 | assist == 1 | forcedFumble == 1) & (nflId == foulNFLId1 | nflId == foulNFLId2), 1, NA))
# View(MergedData %>% filter(tackle == 0 & assist == 0 & !is.na(IndivTackle_Penalized))); this is empty

# And now "penalized tackle" for teams
IndivTackles_Penalized <- MergedData %>% 
  filter(!is.na(IndivTackle_Penalized))
# This group_by shows most penalized tackles by any single player on a play
IndivTackles_Penalized_ByPlayer <- IndivTackles_Penalized %>% 
  group_by(gameId, playId, nflId, displayName) %>%
  summarize(Frames = n(), Tackle_Pen = IndivTackle_Penalized[1]) %>% arrange(desc(Tackle_Pen), playId)
# Now we sum them, if any teammates had one on the same play
PenalizedTackles_Merged <- IndivTackles_Penalized_ByPlayer %>%
  group_by(gameId, playId) %>%
  summarize(TeamDef_Tkl_Pen = sum(Tackle_Pen)) %>% arrange(desc(TeamDef_Tkl_Pen))
MergedData <- merge(x = MergedData, y = PenalizedTackles_Merged,
                    by = c("gameId", "playId"), all.x = TRUE)
MergedData <- MergedData %>% 
  replace_na(list(TeamDef_Tkl_Pen = 0)) 
MergedData <- MergedData %>% arrange(gameId, playId, nflId, frameId)
rm(IndivTackles_Penalized, IndivTackles_Penalized_ByPlayer, PenalizedTackles_Merged)

# And do the same thing for "clean tackles"
MergedData <- MergedData %>% mutate(IndivTotTackle_Clean =
                                      ifelse(tackle == 1 & (is.na(IndivTackle_Penalized)), 1, 
                                             ifelse(assist == 1 & (is.na(IndivTackle_Penalized)), 0.5, NA)))

MergedData <- MergedData %>% mutate(TeamDef_Tackle_Clean =
                                      ifelse((solo_tackle == 1 | assist_tackle == 1 | fumble_forced == 1) & TeamDef_Tkl_Pen == 0, 1, 0))

# These will be useful in the "aggregating frames into plays" section
# E.G., getting the average defensive WPA added across a single player's tackle attempts
MergedData <- MergedData %>% mutate(PlayEPA_TackleAttemptOnly =
                                      ifelse(IndivTackleAttempt == 1, expectedPointsAdded, NA))
MergedData <- MergedData %>% mutate(PlayEPA_TacklerOnly =
                                      ifelse(IndivTotTackles > 0, expectedPointsAdded, NA))
MergedData <- MergedData %>% mutate(PlayDefWPA_TackleAttemptOnly =
                                      ifelse(IndivTackleAttempt == 1, DefWPA, NA))
MergedData <- MergedData %>% mutate(PlayDefWPA_TacklerOnly =
                                      ifelse(IndivTotTackles > 0, DefWPA, NA))
MergedData <- MergedData %>% mutate(PlayEPASuccess_TackleAttemptOnly =
                                      ifelse(IndivTackleAttempt == 1, success, NA))
MergedData <- MergedData %>% mutate(PlayEPASuccess_TacklerOnly =
                                      ifelse(IndivTotTackles > 0, success, NA))
MergedData <- MergedData %>% mutate(PlayWPASuccess_TackleAttemptOnly =
                                      ifelse(IndivTackleAttempt == 1, WPSuccess, NA))
MergedData <- MergedData %>% mutate(PlayWPASuccess_TacklerOnly =
                                      ifelse(IndivTotTackles > 0, WPSuccess, NA))

# Add columns for YAC Over Expected, etc.
MergedData <- MergedData %>% mutate(YACOE = yards_after_catch - xyac_mean_yardage,
                                    YAC_EPA_OE = yac_epa - xyac_epa,
                                    YAC_Success_OE = ifelse(passResult == "C", (success - xyac_fd), NA),
                                    YAC_FD_OE = ifelse(passResult == "C", (first_down - xyac_fd), NA))

# Now we want to exclude "extra" frames, i.e. ones after the play ended
table(MergedData$event)
# Include fumbles b/c we want to limit to only defensive players making tackles
Frames_EndOfPlay <- MergedData %>%
  filter(event %in% c("out_of_bounds", "safety", "qb_sack", "qb_slide", "tackle", "touchdown", "fumble", "fumble_defense_recovered")) %>%
  select(gameId, playId, nflId, displayName, frameId) %>%
  rename(FrameNumber_EndOfPlay = frameId)

# Some plays have multiple of these events, e.g. View(DesignedRuns_Merged %>% filter(playId == 3449))
# Therefore, make it so that only the first relevant frame shows up
Frames_EndOfPlay <- Frames_EndOfPlay %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(Frame_Rank = rank(FrameNumber_EndOfPlay, ties.method = "first")) %>%
  ungroup() 
Frames_EndOfPlay <- Frames_EndOfPlay %>% filter(Frame_Rank == 1)
Frames_EndOfPlay <- Frames_EndOfPlay %>% select(-"Frame_Rank")

MergedData <- merge(x = MergedData, y = Frames_EndOfPlay, 
                    by = c("playId", "gameId", "nflId", "displayName"))
MergedData <- MergedData %>% arrange(gameId, playId, nflId, frameId)

MergedData <- MergedData %>% group_by(gameId, playId, nflId) %>% 
  mutate(Unnecessary_Late = ifelse(frameId > FrameNumber_EndOfPlay, TRUE, FALSE)) %>% 
  ungroup()

MergedData <- MergedData %>% filter(Unnecessary_Late == FALSE)
rm(Frames_EndOfPlay)
MergedData <- MergedData %>% mutate(FrameNumber_FiveBeforeEndOfPlay =
      ifelse(FrameNumber_EndOfPlay >= 6, FrameNumber_EndOfPlay - 5, 1))
MergedData <- MergedData %>% select(-"Unnecessary_Late")

# Use lead() to label when defense made a tackle five frames ahead
MergedData <- MergedData %>% mutate(TeamTackle_FiveFramesAhead =
                                      ifelse(lead(event, 5) %in% c("out_of_bounds", "qb_sack", "qb_slide", "tackle", "fumble", "safety") & playId == lead(playId, 5) & nflId == lead(nflId, 5), TRUE,
                                             ifelse(playId == lead(playId, 5) & nflId == lead(nflId, 5), FALSE, NA)))

# Repeat for individual tackles instead of team tackles
MergedData <- MergedData %>% mutate(IndivSoloTackle_FiveFramesAhead =
                                      ifelse(tackle == 1 & TeamTackle_FiveFramesAhead == 1, 1, 
                                             ifelse(PlayerSideOfBall == "defense", 0, NA)))

# Likewise for individual assists
MergedData <- MergedData %>% mutate(IndivAssist_FiveFramesAhead =
                                      ifelse(assist == 1 & TeamTackle_FiveFramesAhead == 1, 1, 
                                             ifelse(PlayerSideOfBall == "defense", 0, NA)))
MergedData <- MergedData %>%
  mutate(IndivTotTackles_FiveFramesAhead = IndivSoloTackle_FiveFramesAhead + 0.5*IndivAssist_FiveFramesAhead)

# Use lead() to create tackles "within the next five frames", both for teams and players
MergedData <- MergedData %>% mutate(TeamTackle_Within0.5Sec =
                                      ifelse((lead(event) %in% c("out_of_bounds", "qb_sack", "qb_slide", "tackle", "fumble", "safety") & playId == lead(playId) & nflId == lead(nflId)) |
                                               (lead(event, 2) %in% c("out_of_bounds", "qb_sack", "qb_slide", "tackle", "fumble", "safety") & playId == lead(playId, 2) & nflId == lead(nflId, 2)) |
                                               (lead(event, 3) %in% c("out_of_bounds", "qb_sack", "qb_slide", "tackle", "fumble", "safety") & playId == lead(playId, 3) & nflId == lead(nflId, 3)) | 
                                               (lead(event, 4) %in% c("out_of_bounds", "qb_sack", "qb_slide", "tackle", "fumble", "safety") & playId == lead(playId, 4) & nflId == lead(nflId, 4)) | 
                                               (lead(event, 5) %in% c("out_of_bounds", "qb_sack", "qb_slide", "tackle", "fumble", "safety") & playId == lead(playId, 5) & nflId == lead(nflId, 5)), TRUE,
                                             ifelse((playId == lead(playId) & nflId == lead(nflId)) &
                                                      (playId == lead(playId, 2) & nflId == lead(nflId, 2)) &
                                                      (playId == lead(playId, 3) & nflId == lead(nflId, 3)) &
                                                      (playId == lead(playId, 4) & nflId == lead(nflId, 4)) &
                                                      (playId == lead(playId, 5) & nflId == lead(nflId, 5)), FALSE, NA)))

# Now do the same for individual tackles and assists
MergedData <- MergedData %>% mutate(IndivSoloTackle_Within0.5Sec =
                                      ifelse(tackle == 1 & TeamTackle_Within0.5Sec == 1, 1, 
                                             ifelse(PlayerSideOfBall == "defense", 0, NA)))
MergedData <- MergedData %>% mutate(IndivAssist_Within0.5Sec =
                                      ifelse(assist == 1 & TeamTackle_Within0.5Sec == 1, 1, 
                                             ifelse(PlayerSideOfBall == "defense", 0, NA)))
MergedData <- MergedData %>%
  mutate(IndivTotTackles_Within0.5Sec = IndivSoloTackle_Within0.5Sec + 0.5*IndivAssist_Within0.5Sec)

# And, for both teams and players, label tackles in the current frame
MergedData <- MergedData %>% mutate(TeamTackle_CurrentFrame =
                                      ifelse(event %in% c("out_of_bounds", "qb_sack", "qb_slide", "tackle", "fumble", "safety"), 1, 0))
MergedData <- MergedData %>% mutate(IndivSoloTackle_CurrentFrame =
                                      ifelse(tackle == 1 & TeamTackle_CurrentFrame == 1, 1, 
                                             ifelse(PlayerSideOfBall == "defense", 0, NA)))
MergedData <- MergedData %>% mutate(IndivAssist_CurrentFrame =
                                      ifelse(assist == 1 & TeamTackle_CurrentFrame == 1, 1, 
                                             ifelse(PlayerSideOfBall == "defense", 0, NA)))
MergedData <- MergedData %>% 
  mutate(IndivTotTackles_CurrentFrame = IndivSoloTackle_CurrentFrame + 0.5*IndivAssist_CurrentFrame)

# Create a Player_Role variable (i.e. defender, ball-carrier, or other offensive player)
MergedData <- MergedData %>%
  mutate(Player_Role = case_when(
    nflId == ballCarrierId ~ "Ball Carrier",
    possessionTeam != club & displayName != "football" ~ "Defense",
    possessionTeam == club & ballCarrierId != nflId ~ "Offense",
    displayName == "football" ~ "Football")) 

# Now, add ball-carrier traits like weight, speed, etc., to all frames
# Writing a function for calculating distancea
calc_distance <- function(x, y, x_baseline = 0, y_baseline = 0) {
  sqrt((x-x_baseline)^2 + (y - y_baseline)^2)
}

## Each player's distance to the ball carrier
# ball_x won't be exact same as X_ball_carrier (e.g. arm extended with ball)
MergedData <- MergedData %>%
  group_by(gameId, playId, frameId) %>%
  mutate(X_ball_carrier = x[which(nflId == ballCarrierId)],
         Y_ball_carrier = y[which(nflId == ballCarrierId)],
         dist_to_ball_carrier = calc_distance(x, 
                                              y, 
                                              x_baseline = X_ball_carrier, 
                                              y_baseline = Y_ball_carrier)) %>%
  ungroup()

BallCarrier_Traits <- MergedData %>% 
  filter(IsBallCarrier > 0) %>% 
  select(gameId, playId, frameId, s, a, dis, o, dir, weight, height_inches, Season_MaxSpeed) %>% 
  rename(ball_carrier_speed = s, ball_carrier_acc = a, ball_carrier_dist_ran = dis,
         ball_carrier_orient = o, ball_carrier_direction = dir, ball_carrier_weight = weight,
         ball_carrier_height = height_inches, BC_Season_MaxSpeed = Season_MaxSpeed)
BallCarrier_Traits <- BallCarrier_Traits %>% 
  mutate(ball_carrier_momentum = ball_carrier_speed * ball_carrier_weight)

MergedData <- MergedData %>% 
  left_join(BallCarrier_Traits, by = c("playId", "gameId", "frameId"))

MergedData <- MergedData %>% 
  mutate(Rel_Speed_ToBC = s - ball_carrier_speed, Rel_Acc_ToBC = a - ball_carrier_acc,
         Rel_Weight_ToBC = weight - ball_carrier_weight, Rel_Height_ToBC = height_inches - ball_carrier_height,
         Rel_Orient_ToBC = o - ball_carrier_orient, Rel_Dir_ToBC = dir - ball_carrier_direction,
         Rel_SeasonMaxSpeed_ToBC = Season_MaxSpeed - BC_Season_MaxSpeed, Momentum = s * weight,
         Rel_Momentum_ToBC = Momentum - ball_carrier_momentum)
rm(BallCarrier_Traits)

# Now get cosine similarity for the direction and orientation
# This gives 1 if you're in same direction, -1 if opposite direction, 0 if perpendicular
MergedData <- MergedData %>% 
  mutate(CosSimilarity_Orient_ToBC = cos(Rel_Orient_ToBC*pi/180),
         CosSimilarity_Dir_ToBC = cos(Rel_Dir_ToBC*pi/180))
MergedData <- MergedData %>% select(-c("Rel_Orient_ToBC", "Rel_Dir_ToBC"))

# Relative velocity accounts for direction, relative speed does NOT
# E.G., if Players X and Y are moving 10 yds/sec in opposite direction, relative speed is 0
# But, relative velocity is 10 - (-1 * 10), or 20
MergedData <- MergedData %>%
  mutate(Rel_Velocity_ToBC = s - (ball_carrier_speed * CosSimilarity_Dir_ToBC))

# Now include the projections for each player's future distance (from distances_vectorized file)
frame_length <- 0.5

# Adding projections for each player's location going forward (span of next 0.5 seconds)
MergedData <- MergedData %>%
  mutate(X_proj_1 = x + (s*.1*cos((90-dir)*pi/180)),
         X_proj_2 = x + (s*.2*cos((90-dir)*pi/180)),
         X_proj_3 = x + (s*.3*cos((90-dir)*pi/180)),
         X_proj_4 = x + (s*.4*cos((90-dir)*pi/180)),
         X_proj_5 = x + (s*.5*cos((90-dir)*pi/180)),
         Y_proj_1 = y + (s*.1*sin((90-dir)*pi/180)),
         Y_proj_2 = y + (s*.2*sin((90-dir)*pi/180)),
         Y_proj_3 = y + (s*.3*sin((90-dir)*pi/180)),
         Y_proj_4 = y + (s*.4*sin((90-dir)*pi/180)),
         Y_proj_5 = y + (s*.5*sin((90-dir)*pi/180)))

# And, now, also add the ball-carrier's projected location within the next 0.5 seconds
BallCarrier_ProjDist <- MergedData %>% 
  filter(IsBallCarrier > 0) %>% 
  select(gameId, playId, frameId, s, a, o, dir, x, y) %>% 
  rename(ball_carrier_speed = s, ball_carrier_acc = a,
         ball_carrier_orient = o, ball_carrier_direction = dir,
         ball_carrier_x = x, ball_carrier_y = y)
BallCarrier_ProjDist <- BallCarrier_ProjDist %>% 
  mutate(ball_carrier_X_proj_1 = ball_carrier_x + (ball_carrier_speed*.1*cos((90-ball_carrier_direction)*pi/180)),
         ball_carrier_X_proj_2 = ball_carrier_x + (ball_carrier_speed*.2*cos((90-ball_carrier_direction)*pi/180)),
         ball_carrier_X_proj_3 = ball_carrier_x + (ball_carrier_speed*.3*cos((90-ball_carrier_direction)*pi/180)),
         ball_carrier_X_proj_4 = ball_carrier_x + (ball_carrier_speed*.4*cos((90-ball_carrier_direction)*pi/180)),
         ball_carrier_X_proj_5 = ball_carrier_x + (ball_carrier_speed*.5*cos((90-ball_carrier_direction)*pi/180)),
         ball_carrier_Y_proj_1 = ball_carrier_y + (ball_carrier_speed*.1*sin((90-ball_carrier_direction)*pi/180)),
         ball_carrier_Y_proj_2 = ball_carrier_y + (ball_carrier_speed*.2*sin((90-ball_carrier_direction)*pi/180)),
         ball_carrier_Y_proj_3 = ball_carrier_y + (ball_carrier_speed*.3*sin((90-ball_carrier_direction)*pi/180)),
         ball_carrier_Y_proj_4 = ball_carrier_y + (ball_carrier_speed*.4*sin((90-ball_carrier_direction)*pi/180)),
         ball_carrier_Y_proj_5 = ball_carrier_y + (ball_carrier_speed*.5*sin((90-ball_carrier_direction)*pi/180)))
BallCarrier_ProjDist <- BallCarrier_ProjDist %>% 
  select(c("playId", "gameId", "frameId", "ball_carrier_X_proj_1", 
           "ball_carrier_X_proj_2", "ball_carrier_X_proj_3", "ball_carrier_X_proj_4",
           "ball_carrier_X_proj_5", "ball_carrier_Y_proj_1", "ball_carrier_Y_proj_2",
           "ball_carrier_Y_proj_3", "ball_carrier_Y_proj_4", "ball_carrier_Y_proj_5"))
MergedData <- MergedData %>% 
  left_join(BallCarrier_ProjDist, by = c("playId", "gameId", "frameId"))

MergedData <- MergedData %>%
  group_by(gameId, playId, frameId) %>%
  mutate(ball_carrier_X_proj_1 = ball_carrier_X_proj_1,
         ball_carrier_X_proj_2 = ball_carrier_X_proj_2,
         ball_carrier_X_proj_3 = ball_carrier_X_proj_3,
         ball_carrier_X_proj_4 = ball_carrier_X_proj_4,
         ball_carrier_X_proj_5 = ball_carrier_X_proj_5,
         ball_carrier_Y_proj_1 = ball_carrier_Y_proj_1,
         ball_carrier_Y_proj_2 = ball_carrier_Y_proj_2,
         ball_carrier_Y_proj_3 = ball_carrier_Y_proj_3,
         ball_carrier_Y_proj_4 = ball_carrier_Y_proj_4,
         ball_carrier_Y_proj_5 = ball_carrier_Y_proj_5,
         proj_dist_to_ball_carrier_1 = calc_distance(X_proj_1, 
                                                   Y_proj_1, 
                                                   x_baseline = ball_carrier_X_proj_1, 
                                                   y_baseline = ball_carrier_Y_proj_1),
         proj_dist_to_ball_carrier_2 = calc_distance(X_proj_2, 
                                                     Y_proj_2, 
                                                     x_baseline = ball_carrier_X_proj_2, 
                                                     y_baseline = ball_carrier_Y_proj_2),
         proj_dist_to_ball_carrier_3 = calc_distance(X_proj_3, 
                                                     Y_proj_3, 
                                                     x_baseline = ball_carrier_X_proj_3, 
                                                     y_baseline = ball_carrier_Y_proj_3),
         proj_dist_to_ball_carrier_4 = calc_distance(X_proj_4, 
                                                     Y_proj_4, 
                                                     x_baseline = ball_carrier_X_proj_4, 
                                                     y_baseline = ball_carrier_Y_proj_4),
         proj_dist_to_ball_carrier_5 = calc_distance(X_proj_5, 
                                                     Y_proj_5, 
                                                     x_baseline = ball_carrier_X_proj_5, 
                                                     y_baseline = ball_carrier_Y_proj_5)) %>%
  ungroup()
rm(BallCarrier_ProjDist)

# Now mutate for the minimum projected distance to ball-carrier over the next 0.5 seconds
MergedData <- MergedData %>% mutate(min_proj_dist_to_ball_carrier =
    pmin(proj_dist_to_ball_carrier_1, proj_dist_to_ball_carrier_2, proj_dist_to_ball_carrier_3,
        proj_dist_to_ball_carrier_4, proj_dist_to_ball_carrier_5))
MergedData <- MergedData %>% select(c(-"proj_dist_to_ball_carrier_1", -"proj_dist_to_ball_carrier_2", 
        -"proj_dist_to_ball_carrier_3", -"proj_dist_to_ball_carrier_4", -"proj_dist_to_ball_carrier_5"))

# Arrange in this order so that all 11 players on one team show up before other team
MergedData <- MergedData %>% arrange(gameId, playId, frameId, club, nflId)

## Calculating distances to closest players on opposing teams
MergedData <- MergedData %>%
  group_by(gameId, playId, frameId) %>%
  mutate(min_dist_opp_player = map_dbl(.x=row_number(), ~min(calc_distance(x = x[which(club[.x]!=club & club!='football')],
                                                                y = y[which(club[.x]!=club & club!='football')],
                                                                x_baseline = x[.x],
                                                                y_baseline = y[.x]))),
         num_opp_players_same_dist = map_dbl(.x=row_number(), ~11 - length(unique(calc_distance(x = x[which(club[.x]!=club & club!='football')],
                                                                                    y = y[which(club[.x]!=club & club!='football')],
                                                                                    x_baseline = x[.x],
                                                                                    y_baseline = y[.x])))),
         num_opp_players_same_dist = ifelse(displayName == 'football', 0, num_opp_players_same_dist),
         min_dist_opp_index = map_dbl(.x=row_number(), ~which(calc_distance(x = x[which(club[.x]!=club & club!='football')],
                                                                      y = y[which(club[.x]!=club & club!='football')],
                                                                      x_baseline = x[.x],
                                                                      y_baseline = y[.x])== min(calc_distance(x = x[which(club[.x]!=club & club!='football')],
                                                                                                                  y = y[which(club[.x]!=club & club!='football')],
                                                                                                                  x_baseline = x[.x],
                                                                                                                  y_baseline = y[.x])))[1]),
         second_closest_dist_opp_player = map_dbl(.x=row_number(), ~Rfast::nth(calc_distance(x = x[which(club[.x]!=club & club!='football')],
                                                                                  y = y[which(club[.x]!=club & club!='football')],
                                                                                  x_baseline = x[.x],
                                                                                  y_baseline = y[.x]), 2, descending = F)),
         second_closest_opp_index = map_dbl(.x=row_number(), ~which(calc_distance(x = x[which(club[.x]!=club & club!='football')],
                                                                                y = y[which(club[.x]!=club & club!='football')],
                                                                                x_baseline = x[.x],
                                                                                y_baseline = y[.x])== Rfast::nth(calc_distance(x = x[which(club[.x]!=club & club!='football')],
                                                                                                                                   y = y[which(club[.x]!=club & club!='football')],
                                                                                                                                   x_baseline = x[.x],
                                                                                                                                   y_baseline = y[.x]),2,descending = F))[1]) # Where I account for duplicates
  ) %>%
  ungroup()

# Because we don't have "football", use 12 instead of 13 here
MergedData <- MergedData %>%
  group_by(gameId, playId, frameId) %>%
  mutate(
    closest_opp_player_name = case_when(
      row_number()<=11 ~ displayName[11+min_dist_opp_index],
      row_number()>=12 ~ displayName[min_dist_opp_index]
    ),
    closest_opp_player_nflID = case_when(
      row_number()<=11 ~ nflId[11+min_dist_opp_index],
      row_number()>=12 ~ nflId[min_dist_opp_index]
    ),
    second_closest_opp_player_name = case_when(
      row_number()<=11 ~ displayName[11+second_closest_opp_index],
      row_number()>=12 ~ displayName[second_closest_opp_index]
    ),
    second_closest_opp_player_nflID = case_when(
      row_number()<=11 ~ nflId[11+second_closest_opp_index],
      row_number()>=12 ~ nflId[second_closest_opp_index]
    )
  ) %>%
  ungroup()

## grabbing directions of first and second closest players on opposing teams:
MergedData <- MergedData %>%
  arrange(gameId, playId, frameId, club, nflId) %>%
  group_by(gameId, playId, frameId) %>%
  mutate(
    dir_of_closest_opp_player = case_when(
      row_number()<=11 ~ dir[11+min_dist_opp_index],
      row_number()>=12 ~ dir[min_dist_opp_index]
    ),
    dir_of_second_closest_opp_player = case_when(
      row_number()<=11 ~ dir[11+second_closest_opp_index],
      row_number()>=12 ~ dir[second_closest_opp_index]
    )
  ) %>%
  ungroup()

# Incorporate the "blocking code" which was created in Python
tracking_w1_blocked_info <- fread("TrackingWeek1_BlockedInfo.csv")
# View(tracking_w1_blocked_info[1:10,])

tracking_w1_blocked_info <- tracking_w1_blocked_info %>%
  rename(min_dist_opp_player = min_dist, num_opp_players_same_dist = num_same_dist,
         min_dist_opp_index = min_dist_pos, second_closest_dist_opp_player = second_closest_dist,
         second_closest_opp_index = second_closest_pos, closest_opp_player_name = closest_player_name,
         closest_opp_player_nflID = closest_player_nflID, second_closest_opp_player_name = second_closest_player_name,
         second_closest_opp_player_nflID = second_closest_player_nflID)

MergedData_blockers <- MergedData %>%
  left_join(tracking_w1_blocked_info %>%
              select(1:3, 5, 48:50), by = c("gameId", "playId", "nflId", "frameId"))
rm(MergedData, tracking_w1_blocked_info)
MergedData_blockers <- MergedData_blockers %>% arrange(gameId, playId, nflId, frameId)

# View(MergedData_blockers[1:100,])

dist <- 1
frames <- 5
MergedData_blockers <- MergedData_blockers %>%
  mutate(within_dist_ofBC = ifelse(PlayerSideOfBall == "offense", NA, 
                                   ifelse(dist_to_ball_carrier <= dist, 1, 0))) %>%
  group_by(gameId, playId, nflId) %>%
  mutate(within_dist_ofBC_frames_ahead = ifelse(PlayerSideOfBall != "defense", NA,
    ifelse((lead(within_dist_ofBC) == 1 & playId == lead(playId) & nflId == lead(nflId)) |
         (lead(within_dist_ofBC, 2) == 1 & playId == lead(playId, 2) & nflId == lead(nflId, 2)) |
         (lead(within_dist_ofBC, 3) == 1 & playId == lead(playId, 3) & nflId == lead(nflId, 3)) | 
         (lead(within_dist_ofBC, 4) == 1 & playId == lead(playId, 4) & nflId == lead(nflId, 4)) | 
         (lead(within_dist_ofBC, frames) == 1 & playId == lead(playId, frames) & nflId == lead(nflId, frames)), 1,
       ifelse((playId == lead(playId) & nflId == lead(nflId)) &
                (playId == lead(playId, 2) & nflId == lead(nflId, 2)) &
                (playId == lead(playId, 3) & nflId == lead(nflId, 3)) &
                (playId == lead(playId, 4) & nflId == lead(nflId, 4)) &
                (playId == lead(playId, frames) & nflId == lead(nflId, frames)), 0, NA))))

DesignedRuns_Merged <- MergedData_blockers %>% filter(pass == 0)
Scrambles_Merged <- MergedData_blockers %>% filter(passResult == "R")
# AllRushes_Merged <- MergedData_blockers %>% filter(pass == 0 | passResult == "R"); likely not necessary
Completions_Merged <- MergedData_blockers %>% filter(passResult == "C")

# Getting rid of pre-snap frames for designed runs
# We must wait until after running the "pre-snap alignment code" to get rid of frames before handoff
Frames_AtSnap <- DesignedRuns_Merged %>%
  filter(event %in% c("ball_snap", "snap_direct")) %>%
  select(gameId, playId, nflId, displayName, frameId) %>%
  rename(FrameNumber_AtSnap = frameId)

DesignedRuns_Merged <- merge(x = DesignedRuns_Merged, y = Frames_AtSnap, 
                             by = c("playId", "gameId", "nflId", "displayName"), all.x = TRUE)
DesignedRuns_Merged <- DesignedRuns_Merged %>% arrange(gameId, playId, nflId, frameId)

DesignedRuns_Merged <- DesignedRuns_Merged %>% group_by(gameId, playId, nflId) %>% 
  mutate(Unnecessary_Early = ifelse(frameId < FrameNumber_AtSnap, TRUE, FALSE)) %>% 
  ungroup()

DesignedRuns_Merged <- DesignedRuns_Merged %>% filter(Unnecessary_Early == FALSE)
rm(Frames_AtSnap)
DesignedRuns_Merged <- DesignedRuns_Merged %>% select(-c("Unnecessary_Early", "FrameNumber_AtSnap"))

# For dropbacks, get rid of frames where ball-carrier doesn't have ball yet
# For designed runs, this will go in pre-snap alignment code file, b/c we need to define pre-snap gaps first
Scrambles_StartOfRun <- Scrambles_Merged %>%
  filter(event %in% c("run", "lateral")) %>%
  select(gameId, playId, nflId, displayName, frameId) %>%
  rename(FrameNumber_AtStart = frameId)

# Account for plays that could have multiple of these events
Scrambles_StartOfRun <- Scrambles_StartOfRun %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(Frame_Rank = rank(FrameNumber_AtStart, ties.method = "first")) %>%
  ungroup() 
Scrambles_StartOfRun <- Scrambles_StartOfRun %>% filter(Frame_Rank == 1)
Scrambles_StartOfRun <- Scrambles_StartOfRun %>% select(-"Frame_Rank")

Scrambles_Merged <- merge(x = Scrambles_Merged, y = Scrambles_StartOfRun, 
                          by = c("playId", "gameId", "nflId", "displayName"))
Scrambles_Merged <- Scrambles_Merged %>% arrange(gameId, playId, nflId, frameId)

Scrambles_Merged <- Scrambles_Merged %>% group_by(gameId, playId, nflId) %>% 
  mutate(Unnecessary_Early = ifelse(frameId < FrameNumber_AtStart, TRUE, FALSE)) %>% 
  ungroup()

Scrambles_Merged <- Scrambles_Merged %>% filter(Unnecessary_Early == FALSE)
rm(Scrambles_StartOfRun)
Scrambles_Merged <- Scrambles_Merged %>% select(-c("Unnecessary_Early", "FrameNumber_AtStart"))

Completions_Arrival <- Completions_Merged %>%
  filter(event %in% c("pass_outcome_caught")) %>%
  select(gameId, playId, nflId, displayName, frameId) %>%
  rename(FrameNumber_AtStart = frameId)

Completions_Merged <- merge(x = Completions_Merged, y = Completions_Arrival, 
                            by = c("playId", "gameId", "nflId", "displayName"))
Completions_Merged <- Completions_Merged %>% arrange(gameId, playId, nflId, frameId)

Completions_Merged <- Completions_Merged %>% group_by(gameId, playId, nflId) %>% 
  mutate(Unnecessary_Early = ifelse(frameId < FrameNumber_AtStart, TRUE, FALSE)) %>% 
  ungroup()

Completions_Merged <- Completions_Merged %>% filter(Unnecessary_Early == FALSE)
rm(Completions_Arrival)
Completions_Merged <- Completions_Merged %>% select(-c("Unnecessary_Early", "FrameNumber_AtStart"))
# rm(AllRushes_Merged)

final_merged_data <- rbind(DesignedRuns_Merged, Scrambles_Merged, Completions_Merged)
rm(MergedData_blockers)
final_merged_data <- final_merged_data %>% arrange(gameId, playId, nflId, frameId)

# In final_merged_data, create code for "the first surge", i.e. first defender to get within a yard of BC
# First, must identify the frame when the defense as a whole first got a "surge"
final_merged_data <- final_merged_data %>%
  group_by(gameId, playId, frameId) %>% 
  mutate(TeamDefSurge_InFrame = sum(within_dist_ofBC, na.rm = TRUE)) %>%
  ungroup() 
TeamDef_InitialSurges <- final_merged_data %>% filter(TeamDefSurge_InFrame > 0) %>%
  group_by(gameId, playId, defensiveTeam) %>%
  mutate(TeamDefSurge_QuickestFrame_Rank = rank(frameId, ties.method = "first")) %>%
  ungroup()
TeamDef_InitialSurges <- TeamDef_InitialSurges %>% filter(TeamDefSurge_QuickestFrame_Rank == 1)
TeamDef_InitialSurges <- TeamDef_InitialSurges %>% 
  select(c("gameId", "playId", "frameId")) %>%
  rename(FirstDefSurge_Frame = frameId)

final_merged_data <- merge(x = final_merged_data, y = TeamDef_InitialSurges, 
                    by = c("gameId", "playId"), all.x = TRUE)
final_merged_data <- final_merged_data %>% arrange(gameId, playId, nflId, frameId)
rm(TeamDef_InitialSurges)

# Now, add a column for when an individual player had a "surge" in said frame
final_merged_data <- final_merged_data %>% mutate(IndivDefender_InitialSurge =
     ifelse(is.na(FirstDefSurge_Frame) | is.na(within_dist_ofBC), NA, 
            ifelse(frameId == FirstDefSurge_Frame & within_dist_ofBC > 0, 1, 0)))

# And now make it so that defender gets credit for initial surge on each frame of that play
final_merged_data <- final_merged_data %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(IndivDefender_InitialSurge_OnPlay = sum(IndivDefender_InitialSurge)) %>%
  ungroup() 
final_merged_data <- final_merged_data %>% select(-"IndivDefender_InitialSurge")

# Add code for the time length between the initial surge (if there is one) and end of play
final_merged_data <- final_merged_data %>% mutate(Surge_To_EndOfPlay_Frames =
      ifelse(is.na(FirstDefSurge_Frame), NA, FrameNumber_EndOfPlay - FirstDefSurge_Frame))

# Also account for first frame of each play not necessarily being 1 anymore
# I.e., now we start plays when the ball-carrier has the ball
FirstFrames_WithBC <- final_merged_data %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(Frame_Rank = rank(frameId, ties.method = "first")) %>%
  ungroup() 
FirstFrames_WithBC <- FirstFrames_WithBC %>% filter(Frame_Rank == 1)
FirstFrames_WithBC <- FirstFrames_WithBC %>% 
  select(c("gameId", "playId", "nflId", "displayName", "frameId")) %>%
  rename(FirstFrame_WithBC = frameId)

final_merged_data <- merge(x = final_merged_data, y = FirstFrames_WithBC, 
                               by = c("playId", "gameId", "nflId", "displayName"))
final_merged_data <- final_merged_data %>% arrange(gameId, playId, nflId, frameId)
rm(FirstFrames_WithBC)

# Make the modifications to standardize BlockedScore on 0 to 1 scale
FixingBlockingScore <- function(final_merged_data) {
  final_merged_dataTemp <- final_merged_data[final_merged_data$BlockedScore != 0 & final_merged_data$BlockedScore != 25 & final_merged_data$a >= 1, ]
  final_merged_dataTemp2 <- final_merged_data[final_merged_data$BlockedScore != 0 & final_merged_data$BlockedScore != 25 & final_merged_data$a < 1, ]
  
  final_merged_dataTemp2$BlockedScore <- final_merged_dataTemp2$BlockedScore * final_merged_dataTemp2$s
  final_merged_dataTemp$BlockedScore <- (final_merged_dataTemp$BlockedScore / final_merged_dataTemp$a) * final_merged_dataTemp$s
  
  final_merged_dataTempWhole <- rbind(final_merged_dataTemp, final_merged_dataTemp2)
  final_merged_data <- merge(final_merged_data, final_merged_dataTempWhole, by = names(final_merged_dataTempWhole), all.x = TRUE)
  
  final_merged_data$BlockedScore[final_merged_data$BlockedScore == 25] <- NA
  
  return(final_merged_data)
} 
final_merged_data <- FixingBlockingScore(final_merged_data)

final_merged_data <- final_merged_data %>% mutate(BlockedScore =
      ifelse(is.na(BlockedScore), NA, BlockedScore / max(final_merged_data$BlockedScore, na.rm = TRUE)))

# Ideas for predictor variables of our models
# Blocking scores
# Distance to ball carrier
# Projected future distance to ball carrier
# Maybe X_DistFromBall and Y_DistFromBall separately as predictors? Being 3 yds to left is different than 3 yds behind
# Likewise, maybe X_AbsDistFromBall, Y_AbsDistFromBall
# number of blockers, and min distance to opposing player(s)
# Perhaps the ranks of distance to ball-carrier (e.g., Player X is second-closest defender to the ball-carrier)
# CosSimilarity_Dir_ToBC (i.e. is defender going in same direction as ball-carrier)
# Cosine similarity for orientation could be useful as well (at least test it out)
# acceleration, and acceleration relative to the ball carrier
# speed of defender, and speed relative to ball carrier (relative velocity)
# yardline_100 (initial field position)
# Ball-carrier distance from sideline, or goal line ... prob don't need both field pos and distance from GL

# For tackling specifically, weight and momentum come into play

final_merged_data_sub <- final_merged_data %>%
  filter(PlayerSideOfBall == "defense" & nflId != ballCarrierId & !is.infinite(BlockedScore) & dist_to_ball_carrier <= 10) %>%
  select(gameId, playId, frameId, nflId, displayName, down, defendersInTheBox, yardline_100,
         goal_to_go, ydstogo, run_location, run_gap, tackle, assist, forcedFumble, pff_missedTackle,
         x, y, s, a, dis, o, dir, event, ball_x, ball_y, TotDistFromBall, X_DistFromBall, Y_DistFromBall,
         X_AbsDistFromBall, Y_AbsDistFromBall, Ball_DistFromGoalLine, weight, Ball_DistFromSideline,
         BallNearSideline, BallNearGoalLine, FrameNumber_EndOfPlay, FrameNumber_FiveBeforeEndOfPlay,
         Temperature, TotDistFromBall_Rank_BySideOfBall, Y_AbsDistFromBall_Rank_BySide,
         X_AbsDistFromBall_Rank_BySide, Y_NetDistFromBall_Rank_BySide, X_NetDistFromBall_Rank_BySide,
         TotDistFromBall_Rank_OVR, Y_AbsDistFromBall_Rank_OVR,
         X_AbsDistFromBall_Rank_OVR, Y_NetDistFromBall_Rank_OVR, X_NetDistFromBall_Rank_OVR,
         IndivTackleAttempt, IndivTotTackles, Indiv_MadeTackle, IndivTackle_Penalized,
         IndivTotTackle_Clean, IndivSoloTackle_FiveFramesAhead, IndivAssist_FiveFramesAhead,
         IndivTotTackles_FiveFramesAhead, IndivSoloTackle_Within0.5Sec, IndivAssist_Within0.5Sec,
         IndivTotTackles_Within0.5Sec, IndivSoloTackle_CurrentFrame, IndivAssist_CurrentFrame,
         IndivTotTackles_CurrentFrame, Player_Role, X_ball_carrier, Y_ball_carrier,
         dist_to_ball_carrier, ball_carrier_speed, ball_carrier_acc, ball_carrier_orient,
         ball_carrier_direction, ball_carrier_weight, BC_Season_MaxSpeed, ball_carrier_momentum,
         Rel_Speed_ToBC, Rel_Acc_ToBC, Rel_Weight_ToBC, Rel_SeasonMaxSpeed_ToBC, Rel_Momentum_ToBC,
         Rel_Velocity_ToBC, X_proj_5, Y_proj_5, ball_carrier_X_proj_5, ball_carrier_Y_proj_5,
         min_proj_dist_to_ball_carrier, min_dist_opp_player, second_closest_dist_opp_player,
         dir_of_closest_opp_player, dir_of_second_closest_opp_player,
         closest_opp_player_name, closest_opp_player_nflID, 
         second_closest_opp_player_name, second_closest_opp_player_nflID,
         BlockedScore, CosSimilarity_Dir_ToBC, CosSimilarity_Orient_ToBC,
         In_BallCarrier_Radius, NumberOfBlockers, within_dist_ofBC,
         within_dist_ofBC_frames_ahead, TeamDefSurge_InFrame,
         FirstDefSurge_Frame, IndivDefender_InitialSurge_OnPlay, FirstFrame_WithBC,
         Surge_To_EndOfPlay_Frames)

# Model to predict whether a defender gets within the one-yard radius in the next five frames
# We tested dozens of models, including logistic regression and random forest
# Other models we tested can be seen at the bottom of this file, along with the "RandomFor_vs_Logit" GitHub file
mod_logistic <- glm(within_dist_ofBC_frames_ahead ~ dist_to_ball_carrier*min_proj_dist_to_ball_carrier +
                      TotDistFromBall_Rank_OVR + NumberOfBlockers + min_proj_dist_to_ball_carrier*BlockedScore, 
                    data = final_merged_data_sub, family = 'binomial')
summary(mod_logistic)
final_merged_data_sub$pred_within_dist_ofBC_logistic <- predict(mod_logistic, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_logistic = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_logistic)

# Build logistic model for tackle probability too in case it's needed
tackle_mod_logistic <- glm(Indiv_MadeTackle ~ Rel_Velocity_ToBC + dist_to_ball_carrier*min_proj_dist_to_ball_carrier +
                     TotDistFromBall_Rank_OVR + NumberOfBlockers + ball_carrier_momentum, 
                   data = final_merged_data_sub, family = 'binomial')
summary(tackle_mod_logistic)
final_merged_data_sub$pred_tackle_logistic <- predict(tackle_mod_logistic, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_tackle_logistic = Indiv_MadeTackle - pred_tackle_logistic)

mckenzie_catch <- final_merged_data %>%
  filter(gameId==2022090800 & playId==617) %>%
  left_join(final_merged_data_sub %>% select(pred_within_dist_ofBC_1, gameId, playId, nflId, frameId), by = c("gameId", "playId", "nflId", 'frameId'))

singletary_run <- final_merged_data %>%
  filter(gameId==2022090800 & playId==101) %>%
  left_join(final_merged_data_sub %>% select(pred_within_dist_ofBC_1, gameId, playId, nflId, frameId), by = c("gameId", "playId", "nflId", 'frameId'))


plotly::ggplotly(
  singletary_run %>%
    filter(frameId >= unique(frameId[which(event=='handoff')]), frameId <= unique(frameId[which(event=='first_contact')])) %>%
    ggplot(aes(x = x, y = y, 
               text = paste0('Dir: ', dir, '\n',
                             'Player Name: ',displayName, '\n',
                             'Closest Opposing Player: ', closest_opp_player_name, '\n',
                             'Closest Opposing Player Dist: ', round(min_dist_opp_player, 3), '\n',
                             'Distance to Ball: ', round(dist_to_ball_carrier, 3), '\n',
                             'Second Closest Opposing Player: ',  second_closest_opp_player_name, '\n',
                             'Second Closest Opposing Player Dist: ', round(second_closest_dist_opp_player, 3), '\n',
                             'Blocked Score: ', round(BlockedScore, 3), '\n',
                             'Near Ball-Carrier Prediction: ', round(pred_within_dist_ofBC_1, 3)
               )
    )) +
    stat_voronoi(geom = "path") +
    geom_point(aes(color = Player_Role)) +
    # geom_segment(aes(x = x, y = y, xend = X_proj_5,
    #                  yend = Y_proj_5, color = Player_Role)) +
    scale_color_manual(values = c("Ball Carrier" = "black", 
                                  "Offense" = "red",
                                  "Defense" = "blue",
                                  "Football" = "brown")) +
    theme_bw() +
    labs(x = "X (High X = Where Offense Is Aiming)", y = "Y (High Y = Offense's Left)", 
         title = "Frame-By-Frame Diagram of D. Singletary Rush (From Handoff to First Contact)") +
    geom_hline(yintercept = 0, color = 'darkgreen', linetype = 'dashed') +
    geom_hline(yintercept = 53.3, color = 'darkgreen', linetype = 'dashed') +
    facet_wrap(~frameId) +
    theme(plot.title = element_text(size = 10, hjust = 0.5))
)

#############################################################################################
# Below are dozens of logistic regression models that were tested and ultimately not used

mod1 <- glm(within_dist_ofBC_frames_ahead ~ BlockedScore + CosSimilarity_Dir_ToBC + 
              Rel_Velocity_ToBC + dist_to_ball_carrier + NumberOfBlockers + 
              dist_to_ball_carrier*BlockedScore, 
              data = final_merged_data_sub, family = 'binomial')
summary(mod1)
final_merged_data_sub$pred_within_dist_ofBC_1 <- predict(mod1, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_1 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_1)
# Pretty strong first attempt, the interaction does have a significant p-value
  
# Try a model 2 without the interaction of BlockedScore*dist_to_ball_carrier
mod2 <- glm(within_dist_ofBC_frames_ahead ~ BlockedScore + CosSimilarity_Dir_ToBC + 
              Rel_Velocity_ToBC + dist_to_ball_carrier + NumberOfBlockers,
            data = final_merged_data_sub, family = 'binomial')
summary(mod2)
final_merged_data_sub$pred_within_dist_ofBC_2 <- predict(mod2, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_2 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_2)
# This is pretty comparable, BlockedScore still does have somewhat significant P-value 
# However, BlockedScore became less significant when we removed the interaction

# Try a model 3 that adds minimum projected future distance to ball-carrier
mod3 <- glm(within_dist_ofBC_frames_ahead ~ BlockedScore + CosSimilarity_Dir_ToBC + 
              Rel_Velocity_ToBC + dist_to_ball_carrier + NumberOfBlockers + 
              min_proj_dist_to_ball_carrier, 
            data = final_merged_data_sub, family = 'binomial')
summary(mod3)
final_merged_data_sub$pred_within_dist_ofBC_3 <- predict(mod3, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_3 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_3)
# min_proj_dist_to_ball_carrier has very low p-value, but it made BlockedScore p-value go way up

# Correlation matrix to detect collinearity between predictor variables
# For obvious reasons, dist_to_ball_carrier and min_proj_dist_to_ball_carrier are very correlated
final_merged_data_sub %>% 
  select(within_dist_ofBC_frames_ahead, BlockedScore, CosSimilarity_Dir_ToBC, 
           Rel_Velocity_ToBC, dist_to_ball_carrier, NumberOfBlockers,
           min_proj_dist_to_ball_carrier) %>%
  cor(use = "complete.obs") %>%
  round(2)
# Takeaway: min_proj_dist_to_ball_carrier probably is worth considering, even with actual distance too
# DON'T use BlockedScore and min_proj_dist_to_ball_carrier separately (but interaction is good)

# Try a model 4 that's very basic, excludes BlockedScore and CosSimilarity from model 3
mod4 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Velocity_ToBC + dist_to_ball_carrier +
              NumberOfBlockers + min_proj_dist_to_ball_carrier, 
            data = final_merged_data_sub, family = 'binomial')
summary(mod4)
final_merged_data_sub$pred_within_dist_ofBC_4 <- predict(mod4, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_4 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_4)
# All have extremely significant p-values
# Takeaway: simple might be best
# Especially b/c # of blockers and rel_velocity already account for some of CosSimilarity & BlockedScore

# Try a model 5 that takes model 4, replaces dist_to_ball_carrier with the X and Y versions
mod5 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Velocity_ToBC + X_DistFromBall + Y_DistFromBall +
              NumberOfBlockers + min_proj_dist_to_ball_carrier, 
            data = final_merged_data_sub, family = 'binomial')
summary(mod5)
final_merged_data_sub$pred_within_dist_ofBC_5 <- predict(mod5, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_5 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_5)
# Takeaway: this is slightly worse, don't use X/Y net distance from ball on their own

# Try a model 6 that takes model 5, uses dist_to_ball_carrier along with the X and Y versions
mod6 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Velocity_ToBC + dist_to_ball_carrier + 
              X_DistFromBall + Y_DistFromBall +
              NumberOfBlockers + min_proj_dist_to_ball_carrier, 
            data = final_merged_data_sub, family = 'binomial')
summary(mod6)
final_merged_data_sub$pred_within_dist_ofBC_6 <- predict(mod6, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_6 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_6)
# Takeaway: don't use, X and Y aren't as valuable on their own

# Try versions of models 5 and 6 with absolute distance from ball, rather than net distance
mod7 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Velocity_ToBC + X_AbsDistFromBall + Y_AbsDistFromBall +
              NumberOfBlockers + min_proj_dist_to_ball_carrier, 
            data = final_merged_data_sub, family = 'binomial')
summary(mod7)
final_merged_data_sub$pred_within_dist_ofBC_7 <- predict(mod7, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_7 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_7)
# Takeaway: this is very good, X_AbsDist and Y_AbsDist worth considering separately

mod8 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Velocity_ToBC + dist_to_ball_carrier + 
              X_AbsDistFromBall + Y_AbsDistFromBall +
              NumberOfBlockers + min_proj_dist_to_ball_carrier, 
            data = final_merged_data_sub, family = 'binomial')
summary(mod8)
final_merged_data_sub$pred_within_dist_ofBC_8 <- predict(mod8, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_8 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_8)
# Takeaway: this is not as good, X and Y don't matter as much with overall distance included

# Try a model 9 that takes out Rel_Velocity from model 4, adds Rel_Speed and CosSimilarity
mod9 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Speed_ToBC + dist_to_ball_carrier +
              NumberOfBlockers + min_proj_dist_to_ball_carrier + CosSimilarity_Dir_ToBC, 
            data = final_merged_data_sub, family = 'binomial')
summary(mod9)
final_merged_data_sub$pred_within_dist_ofBC_9 <- predict(mod9, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_9 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_9)
# All have significant p-values, pretty strong
# Takeaway: perhaps relative speed and CosSimilarity, separately, are at least as good as Rel_Velocity
# Certainly at least one of CosSimilarity and Rel_Velo, if not both, will be included

# Now try a model 10, which is model 4 plus CosSimilarity (equivalent to model 3 w/o BlockedScore)
# Model 10 is model 9 with Rel_Velocity added
mod10 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Velocity_ToBC + dist_to_ball_carrier +
              NumberOfBlockers + min_proj_dist_to_ball_carrier + CosSimilarity_Dir_ToBC, 
            data = final_merged_data_sub, family = 'binomial')
summary(mod10)
final_merged_data_sub$pred_within_dist_ofBC_10 <- predict(mod10, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_10 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_10)
# Takeaway: CosSimilarity has significant P-value, this is a strong option
# At first glance, seems best to either have Rel_Velo, or the combo of CosSimilarity and Rel_Speed

# Try model 11, which is model 3 plus BlockedScore (equivalent to model 4 w/o CosSimilarity)
mod11 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Velocity_ToBC + dist_to_ball_carrier +
               NumberOfBlockers + min_proj_dist_to_ball_carrier + BlockedScore, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod11)
final_merged_data_sub$pred_within_dist_ofBC_11 <- predict(mod11, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_11 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_11)
# BlockedScore has very high p-value here, don't use
# Takeaway: generally seems that BlockedScore doesn't have much value when min proj distance is included
# Unless the interaction between them is also included (see below)

# Try model 12, which is model 10 plus X and Y net distances
mod12 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Velocity_ToBC + dist_to_ball_carrier +
               X_DistFromBall + Y_DistFromBall +
               NumberOfBlockers + min_proj_dist_to_ball_carrier + CosSimilarity_Dir_ToBC, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod12)
final_merged_data_sub$pred_within_dist_ofBC_12 <- predict(mod12, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_12 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_12)
# Pretty good, but perhaps too many variables, though all are significant
# Takeaway: Can probably get by without X & Y distance if we have tot dist, proj dist, and CosSimilarity

# Try model 13 that's model 9 with min_dist_opp_player added
mod13 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Speed_ToBC + dist_to_ball_carrier +
              min_dist_opp_player + NumberOfBlockers + min_proj_dist_to_ball_carrier + CosSimilarity_Dir_ToBC, 
            data = final_merged_data_sub, family = 'binomial')
summary(mod13)
final_merged_data_sub$pred_within_dist_ofBC_13 <- predict(mod13, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_13 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_13)
# Takeaway: it does look like min_dist_opp_player has significant value
# Among the best so far

# Try model 14 that's model 9 with min_dist_opp_player replacing NumberOfBlockers
mod14 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Speed_ToBC + dist_to_ball_carrier +
               min_dist_opp_player + min_proj_dist_to_ball_carrier + CosSimilarity_Dir_ToBC, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod14)
final_merged_data_sub$pred_within_dist_ofBC_14 <- predict(mod14, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_14 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_14)
# Takeaway: also a pretty good one, Rel_Speed has some value, 13 probably slightly better

# Try model 15 that's model 9 with TotDistFromBall_Rank_BySideOfBall added
mod15 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Speed_ToBC + dist_to_ball_carrier +
              TotDistFromBall_Rank_BySideOfBall +
               NumberOfBlockers + min_proj_dist_to_ball_carrier + CosSimilarity_Dir_ToBC, 
            data = final_merged_data_sub, family = 'binomial')
summary(mod15)
final_merged_data_sub$pred_within_dist_ofBC_15 <- predict(mod15, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_15 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_15)
# Takeaway: the rank does appear to add very significant predictive value, this is strong

# Try model 16 which is model 15, but with overall rank instead (rather than by side of ball)
mod16 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Speed_ToBC + dist_to_ball_carrier +
               TotDistFromBall_Rank_OVR +
               NumberOfBlockers + min_proj_dist_to_ball_carrier + CosSimilarity_Dir_ToBC, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod16)
final_merged_data_sub$pred_within_dist_ofBC_16 <- predict(mod16, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_16 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_16)
# Takeaway: extremely similar to model 15, probably worth using one of those ranks (OVR gives more info)

# Try model 17 which is model 16, including CosSimilarity for orientation in addition to direction
mod17 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Speed_ToBC + dist_to_ball_carrier +
               TotDistFromBall_Rank_OVR + CosSimilarity_Orient_ToBC +
               NumberOfBlockers + min_proj_dist_to_ball_carrier + CosSimilarity_Dir_ToBC, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod17)
final_merged_data_sub$pred_within_dist_ofBC_17 <- predict(mod17, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_17 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_17)
# Takeaway: orientation is significant, but probably too many predictors here

# Try model 18 which is model 16, with CosSimilarity for orientation instead of the directional one
mod18 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Speed_ToBC + dist_to_ball_carrier +
               TotDistFromBall_Rank_OVR + CosSimilarity_Orient_ToBC +
               NumberOfBlockers + min_proj_dist_to_ball_carrier, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod18)
final_merged_data_sub$pred_within_dist_ofBC_18 <- predict(mod18, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_18 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_18)
# Takeaway: this is strong, indicates we should probably only include one (at most) of orientation and direction
# Would go with direction over orientation in that case

# Try model 19 which is model 16 with defender acceleration added
mod19 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Speed_ToBC + dist_to_ball_carrier +
               TotDistFromBall_Rank_OVR + a +
               NumberOfBlockers + min_proj_dist_to_ball_carrier + CosSimilarity_Dir_ToBC, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod19)
final_merged_data_sub$pred_within_dist_ofBC_19 <- predict(mod19, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_19 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_19)
# Takeaway: solid, but could do without Rel_Speed, as has often been the case

# Try model 20 which is model 19 with relative acceleration added
mod20 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Speed_ToBC + dist_to_ball_carrier +
               TotDistFromBall_Rank_OVR + a + Rel_Acc_ToBC +
               NumberOfBlockers + min_proj_dist_to_ball_carrier + CosSimilarity_Dir_ToBC, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod20)
final_merged_data_sub$pred_within_dist_ofBC_20 <- predict(mod20, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_20 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_20)
# Takeaway: Rel_Acc is not as valuable, don't use (at least in conjunction with defender acc)

# Try model 21 which is model 19 with relative acceleration in place of defender acceleration
mod21 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Speed_ToBC + dist_to_ball_carrier +
               TotDistFromBall_Rank_OVR + Rel_Acc_ToBC +
               NumberOfBlockers + min_proj_dist_to_ball_carrier + CosSimilarity_Dir_ToBC, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod21)
final_merged_data_sub$pred_within_dist_ofBC_21 <- predict(mod21, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_21 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_21)
# Takeaway: Rel_Acc does have more significance when it's on its own, still probably can avoid

# Try model 22 which is model 19 with defender speed added
mod22 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Speed_ToBC + dist_to_ball_carrier +
               TotDistFromBall_Rank_OVR + a + s +
               NumberOfBlockers + min_proj_dist_to_ball_carrier + CosSimilarity_Dir_ToBC, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod22)
final_merged_data_sub$pred_within_dist_ofBC_22 <- predict(mod22, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_22 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_22)
# Takeaway: speed has significance, this is a pretty strong model

# Try model 23 which is model 22 without defender acceleration (equivalent to model 16 with defender speed)
mod23 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Speed_ToBC + dist_to_ball_carrier +
               TotDistFromBall_Rank_OVR + s +
               NumberOfBlockers + min_proj_dist_to_ball_carrier + CosSimilarity_Dir_ToBC, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod23)
final_merged_data_sub$pred_within_dist_ofBC_23 <- predict(mod23, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_23 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_23)
# Takeaway: speed still has significance, this is a very strong model

# Try model 24 which is model 23 with field position added
mod24 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Speed_ToBC + dist_to_ball_carrier +
               TotDistFromBall_Rank_OVR + s + yardline_100 +
               NumberOfBlockers + min_proj_dist_to_ball_carrier + CosSimilarity_Dir_ToBC, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod24)
final_merged_data_sub$pred_within_dist_ofBC_24 <- predict(mod24, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_24 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_24)
# Takeaway: don't use, yardline_100 not significant

# Try model 25 which is model 23 with goal_to_go added
mod25 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Speed_ToBC + dist_to_ball_carrier +
               TotDistFromBall_Rank_OVR + s + goal_to_go +
               NumberOfBlockers + min_proj_dist_to_ball_carrier + CosSimilarity_Dir_ToBC, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod25)
final_merged_data_sub$pred_within_dist_ofBC_25 <- predict(mod25, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_25 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_25)
# Takeaway: don't use, goal_to_go not very significant

# Try model 26 which is model 23 with distance from sideline added
mod26 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Speed_ToBC + dist_to_ball_carrier +
               TotDistFromBall_Rank_OVR + s + Ball_DistFromSideline +
               NumberOfBlockers + min_proj_dist_to_ball_carrier + CosSimilarity_Dir_ToBC, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod26)
final_merged_data_sub$pred_within_dist_ofBC_26 <- predict(mod26, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_26 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_26)
# Takeaway: don't use, but try binary variable for "BallNearSideline"

# Try model 27 which is model 23 with distance from goal line added
mod27 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Speed_ToBC + dist_to_ball_carrier +
               TotDistFromBall_Rank_OVR + s + Ball_DistFromGoalLine +
               NumberOfBlockers + min_proj_dist_to_ball_carrier + CosSimilarity_Dir_ToBC, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod27)
final_merged_data_sub$pred_within_dist_ofBC_27 <- predict(mod27, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_27 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_27)
# Takeaway: don't use, but try binary variable for "BallNearSideline"

# Try model 28 which is model 23 with the binary version of distance from sideline added
mod28 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Speed_ToBC + dist_to_ball_carrier +
               TotDistFromBall_Rank_OVR + s + BallNearSideline +
               NumberOfBlockers + min_proj_dist_to_ball_carrier + CosSimilarity_Dir_ToBC, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod28)
final_merged_data_sub$pred_within_dist_ofBC_28 <- predict(mod28, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_28 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_28)
# Takeaway: ball near sideline has significance, probably can avoid though, lot of predictors

# Try model 29 which is model 23 with the binary version of distance from goal line added
mod29 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Speed_ToBC + dist_to_ball_carrier +
               TotDistFromBall_Rank_OVR + s + BallNearGoalLine +
               NumberOfBlockers + min_proj_dist_to_ball_carrier + CosSimilarity_Dir_ToBC, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod29)
final_merged_data_sub$pred_within_dist_ofBC_29 <- predict(mod29, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_29 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_29)
# Takeaway: similarly, pretty good but perhaps too many predictors

# Try model 30 which is model 23 without CosSimilarity (still keep proj distance)
mod30 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Speed_ToBC + dist_to_ball_carrier +
               TotDistFromBall_Rank_OVR + s +
               NumberOfBlockers + min_proj_dist_to_ball_carrier, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod30)
final_merged_data_sub$pred_within_dist_ofBC_30 <- predict(mod30, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_30 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_30)
# Takeaway: pretty good, but version with Rel_Velo instead of Rel_Speed (model 41) is better

# Try model 31 which is model 23, but with interaction of current and projected distance to BC
mod31 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Speed_ToBC + dist_to_ball_carrier*min_proj_dist_to_ball_carrier +
               TotDistFromBall_Rank_OVR + s +
               NumberOfBlockers + CosSimilarity_Dir_ToBC, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod31)
final_merged_data_sub$pred_within_dist_ofBC_31 <- predict(mod31, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_31 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_31)
# A lot of predictors, but very good, worth considering

# Try model 32, which is model 1 but with interaction with BlockedScore and min projected distance to BC
# Rather than interaction between BlockedScore and current distance to BC
mod32 <- glm(within_dist_ofBC_frames_ahead ~ BlockedScore + CosSimilarity_Dir_ToBC + 
              Rel_Velocity_ToBC + dist_to_ball_carrier + NumberOfBlockers + 
              min_proj_dist_to_ball_carrier*BlockedScore, 
            data = final_merged_data_sub, family = 'binomial')
summary(mod32)
final_merged_data_sub$pred_within_dist_ofBC_32 <- predict(mod1, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_32 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_32)
# Takeaway: that interaction is pretty significant, perhaps worthy of use

# Try model 33, which is model 32 without CosSimilarity
mod33 <- glm(within_dist_ofBC_frames_ahead ~ BlockedScore + 
               Rel_Velocity_ToBC + dist_to_ball_carrier + NumberOfBlockers + 
               min_proj_dist_to_ball_carrier*BlockedScore, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod33)
final_merged_data_sub$pred_within_dist_ofBC_33 <- predict(mod1, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_33 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_33)
# Takeaway: extremely comparable to model 32, prob slightly better since there are fewer predictors

# Try model 34 that's model 13 with Rel_Velo in place of Rel_Speed and CosSimilarity
mod34 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Velocity_ToBC + dist_to_ball_carrier +
               min_dist_opp_player + NumberOfBlockers + min_proj_dist_to_ball_carrier, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod34)
final_merged_data_sub$pred_within_dist_ofBC_34 <- predict(mod34, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_34 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_34)
# Takeaway: Really good, in the conversation for the best

# Try model 35 that's model 34 with CosSimilarity added
mod35 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Velocity_ToBC + dist_to_ball_carrier +
               min_dist_opp_player + NumberOfBlockers + min_proj_dist_to_ball_carrier + CosSimilarity_Dir_ToBC, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod35)
final_merged_data_sub$pred_within_dist_ofBC_35 <- predict(mod35, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_35 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_35)
# Takeaway: Also very good, but CosSimilarity isn't as significant as others, 34 is slightly better

# Try model 36 that's model 14 with Rel_Velo in place of Rel_Speed and CosSimilarity
mod36 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Velocity_ToBC + dist_to_ball_carrier +
               min_dist_opp_player + min_proj_dist_to_ball_carrier, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod36)
final_merged_data_sub$pred_within_dist_ofBC_36 <- predict(mod36, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_36 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_36)
# Takeaway: Extremely good, among the best

# Try model 37 that's model 36 with CosSimilarity added
mod37 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Velocity_ToBC + dist_to_ball_carrier +
               min_dist_opp_player + min_proj_dist_to_ball_carrier + CosSimilarity_Dir_ToBC, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod37)
final_merged_data_sub$pred_within_dist_ofBC_37 <- predict(mod37, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_37 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_37)
# Takeaway: Also very good, but CosSimilarity isn't as significant as others, 35 is slightly better

# Try model 38 that's model 16 with Rel_Velo in place of Rel_Speed and CosSimilarity
mod38 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Velocity_ToBC + dist_to_ball_carrier +
               TotDistFromBall_Rank_OVR +
               NumberOfBlockers + min_proj_dist_to_ball_carrier, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod38)
final_merged_data_sub$pred_within_dist_ofBC_38 <- predict(mod38, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_38 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_38)
# Takeaway: great significance from all variables, among the best

# Try model 39 that's model 38 with CosSimilarity added
mod39 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Velocity_ToBC + dist_to_ball_carrier +
               TotDistFromBall_Rank_OVR +
               NumberOfBlockers + min_proj_dist_to_ball_carrier + CosSimilarity_Dir_ToBC, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod39)
final_merged_data_sub$pred_within_dist_ofBC_39 <- predict(mod39, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_39 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_39)
# Takeaway: don't use, CosSimilarity not significant on its own here

# Try model 40 that's model 19 with Rel_Velo in place of Rel_Speed and CosSimilarity
mod40 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Velocity_ToBC + dist_to_ball_carrier +
               TotDistFromBall_Rank_OVR + a +
               NumberOfBlockers + min_proj_dist_to_ball_carrier, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod40)
final_merged_data_sub$pred_within_dist_ofBC_40 <- predict(mod40, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_40 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_40)
# Takeaway: pretty good, but acceleration is not as valuable as others

# Try model 41 that's model 23 with Rel_Velo in place of Rel_Speed and CosSimilarity
mod41 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Velocity_ToBC + dist_to_ball_carrier +
               TotDistFromBall_Rank_OVR + s +
               NumberOfBlockers + min_proj_dist_to_ball_carrier, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod41)
final_merged_data_sub$pred_within_dist_ofBC_41 <- predict(mod41, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_41 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_41)
# Takeaway: might be the best so far

# Try model 42 that's model 41 with CosSimilarity added
mod42 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Velocity_ToBC + dist_to_ball_carrier +
               TotDistFromBall_Rank_OVR + s +
               NumberOfBlockers + min_proj_dist_to_ball_carrier + CosSimilarity_Dir_ToBC, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod42)
final_merged_data_sub$pred_within_dist_ofBC_42 <- predict(mod42, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_42 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_42)
# Also very good, maybe too many predictors

# Try model 43 which is model 31, but without current defender speed
mod43 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Speed_ToBC + dist_to_ball_carrier*min_proj_dist_to_ball_carrier +
               TotDistFromBall_Rank_OVR + 
               NumberOfBlockers + CosSimilarity_Dir_ToBC, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod43)
final_merged_data_sub$pred_within_dist_ofBC_43 <- predict(mod43, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_43 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_43)
# Not as good, Rel_Speed isn't significant

# Try model 44, which is model 43, but with Rel_Velo in place of Rel_Speed and CosSimilarity
mod44 <- glm(within_dist_ofBC_frames_ahead ~ Rel_Velocity_ToBC + dist_to_ball_carrier*min_proj_dist_to_ball_carrier +
               TotDistFromBall_Rank_OVR + NumberOfBlockers, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod44)
final_merged_data_sub$pred_within_dist_ofBC_44 <- predict(mod44, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_44 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_44)
# Takeaway: interaction b/w current dist and projected dist is great, very strong model

# Try model 45, which is model 44 without Rel_Velo
mod45 <- glm(within_dist_ofBC_frames_ahead ~ dist_to_ball_carrier*min_proj_dist_to_ball_carrier +
               TotDistFromBall_Rank_OVR + NumberOfBlockers, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod45)
final_merged_data_sub$pred_within_dist_ofBC_45 <- predict(mod45, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_45 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_45)
# Takeaway: interaction b/w current dist and projected dist is great, very strong model

# Try model 46, which is model 45 with another interaction (BlockedScore and proj dist)
mod46 <- glm(within_dist_ofBC_frames_ahead ~ dist_to_ball_carrier*min_proj_dist_to_ball_carrier +
               TotDistFromBall_Rank_OVR + NumberOfBlockers + min_proj_dist_to_ball_carrier*BlockedScore, 
             data = final_merged_data_sub, family = 'binomial')
summary(mod46)
final_merged_data_sub$pred_within_dist_ofBC_46 <- predict(mod46, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_46 = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_46)

# See the RandomForvsLogit GitHub file for testing of ranger() model

# Use for loops to get SD, mean error, RMSE for our models
# Number of columns
n <- 46

# Initialize empty vectors to store results
mean_values <- numeric(n)
sd_values <- numeric(n)
rmse_values <- numeric(n)

# Loop over columns
for (i in 1:n) {
  # Get column name dynamically
  col_name <- paste("pred_near_BC_error_", i, sep = "")
  
  # Calculate mean, standard deviation, and RMSE
  mean_values[i] <- mean(final_merged_data_sub[[col_name]], na.rm = TRUE)
  sd_values[i] <- sd(final_merged_data_sub[[col_name]], na.rm = TRUE)
  rmse_values[i] <- sqrt(mean((final_merged_data_sub[[col_name]])^2, na.rm = TRUE))
}

# Create a data frame to store the results
ErrorsDF <- data.frame(
  pred_near_BC_error_column = paste("pred_near_BC_error_", 1:n, sep = ""),
  Mean_Error = mean_values,
  SD_Error = sd_values,
  RMSE_Error = rmse_values
)
ErrorsDF
