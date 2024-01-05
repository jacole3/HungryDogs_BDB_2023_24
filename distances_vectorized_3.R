setwd("C:/Users/justi/OneDrive/Penn/BigDataBowl")
#install.packages('Rfast')
library(Rfast)
library(tidyverse)
library(gganimate)
library(deldir)
library(ggvoronoi)

# Code I used to download ggvoronoi
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

# This GitHub file is split into two parts: one starts from scratch (i.e., if you've done no prior code), and one picks off where "Initial_DataCleansing_Code" left off
# Right here is the first part

## Frames for how long we want to project forward
frame_length <- 0.5

## Distance Formula
calc_distance <- function(x, y, x_baseline = 0, y_baseline = 0) {
  sqrt((x-x_baseline)^2 + (y - y_baseline)^2)
}

## Reading in all four files:
tracking_w1 <- read_csv("tracking_week_1.csv")
games<- read_csv("games.csv")
players <- read_csv("players.csv")
plays <- read_csv("plays.csv")

#merging tracking data to the play descriptions, indicating the play direction
tracking_w1 <- tracking_w1 %>%
  left_join(plays, by = c("gameId", "playId")) %>%
  left_join(games, by = "gameId") %>%
  mutate(ball_carrier_indicator = ifelse(ballCarrierId==nflId,1,0),
         toLeft = ifelse(playDirection=="left", 1, 0))

#standardizing directions:
tracking_w1 <- tracking_w1 %>%
  mutate(TeamOnOffense = ifelse(possessionTeam==homeTeamAbbr, "home", "away"),
         AwayOrHome = ifelse(club==homeTeamAbbr, "home", "away"),
         IsPlayerOnOffense = ifelse(possessionTeam==club, "offense", 
                        ifelse(club == "football", "football", "defense")),
         X_std = ifelse(toLeft==1, 120-x, x), ## Standardizes X
         Y_std = ifelse(toLeft==1, 53.3-y, y), ## Standardized Y
         Player_Role= case_when(
           nflId==ballCarrierId ~ "Ball Carrier",
           possessionTeam!=club & displayName!="football"~ "Defense",
           possessionTeam==club & ballCarrierId!=nflId ~ "Offense",
           displayName == "football" ~ "Football"
         )) 

#adjusting the orientations as well
tracking_w1 <- tracking_w1 %>%
  mutate(o2 = ifelse(playDirection == "right", o, 
                     ifelse((playDirection == "left" & o < 180), o + 180, o - 180)),
         dir2 = ifelse(playDirection == "right", dir, 
                       ifelse((playDirection == "left" & dir < 180), dir + 180, dir - 180)))

# This is a basic visualization of which "dir" values are most common, based on play direction
tracking_w1 %>%
  ggplot(aes(x = dir)) +
  geom_density(fill = 'dodgerblue') +
  facet_wrap(~playDirection)

# Here's same concept, but based on which side of ball player is on
tracking_w1 %>%
  ggplot(aes(x = dir)) +
  geom_density(fill = 'dodgerblue') +
  facet_wrap(~IsPlayerOnOffense)

# Adding projections for each player's location going forward (by 0.5 seconds)
# Note that we tested a kinematics-based approach (incorporating acceleration) in addition to the "speed*frame_length" approach
# Turned out the simpler approach (without acceleration) was more accurate in projecting future distances
tracking_w1 <- tracking_w1 %>%
  mutate(X_proj_1 = X_std + ((s*frame_length + 0.5*a*(frame_length)^2)*cos((90-dir)*pi/180)),
         Y_proj_1 = Y_std + ((s*frame_length + 0.5*a*(frame_length)^2)*sin((90-dir)*pi/180)),
         X_proj_2 = X_std + (s*frame_length*cos((90-dir)*pi/180)),
         Y_proj_2 = Y_std + (s*frame_length*sin((90-dir)*pi/180)))

# Figure out which approach is more accurate (physics-based, or just speed * time)
AccuracyTest <- tracking_w1 %>% mutate(X_Actual_0.5SecAhead = 
   ifelse(displayName == lead(displayName, 5), lead(X_std, 5), NA))
AccuracyTest <- AccuracyTest %>% mutate(Y_Actual_0.5SecAhead = 
   ifelse(displayName == lead(displayName, 5), lead(Y_std, 5), NA))

AccuracyTest <- AccuracyTest %>%
  mutate(X_proj_Error1 = X_proj_1 - X_Actual_0.5SecAhead,
         X_proj_Error2 = X_proj_2 - X_Actual_0.5SecAhead,
         Y_proj_Error1 = Y_proj_1 - Y_Actual_0.5SecAhead,
         Y_proj_Error2 = Y_proj_2 - Y_Actual_0.5SecAhead)

sd(AccuracyTest$X_proj_Error1, na.rm = TRUE)
sd(AccuracyTest$X_proj_Error2, na.rm = TRUE)
sd(AccuracyTest$Y_proj_Error1, na.rm = TRUE)
sd(AccuracyTest$Y_proj_Error2, na.rm = TRUE)

mean(AccuracyTest$X_proj_Error1, na.rm = TRUE)
mean(AccuracyTest$X_proj_Error2, na.rm = TRUE)
mean(AccuracyTest$Y_proj_Error1, na.rm = TRUE)
mean(AccuracyTest$Y_proj_Error2, na.rm = TRUE)

sqrt(mean((AccuracyTest$X_proj_Error1)^2, na.rm = TRUE))
sqrt(mean((AccuracyTest$X_proj_Error2)^2, na.rm = TRUE))
sqrt(mean((AccuracyTest$Y_proj_Error1)^2, na.rm = TRUE))
sqrt(mean((AccuracyTest$Y_proj_Error2)^2, na.rm = TRUE))
# Method 2 (non-physics approach) has significantly lower RMSE

# So, get rid of first version
tracking_w1 <- tracking_w1 %>% select(-c("X_proj_1", "Y_proj_1"))
tracking_w1 <- tracking_w1 %>% 
  rename(X_proj = X_proj_2, Y_proj = Y_proj_2)
rm(AccuracyTest)

# And, now, also add the ball-carrier's projected distance in 0.5 seconds
BallCarrier_ProjDist <- tracking_w1 %>% 
  filter(IsBallCarrier > 0) %>% 
  select(gameId, playId, frameId, s, a, o, dir, X_std, Y_std) %>% 
  rename(ball_carrier_speed = s, ball_carrier_acc = a,
         ball_carrier_orient = o, ball_carrier_direction = dir,
         ball_carrier_x = X_std, ball_carrier_y = Y_std)
BallCarrier_ProjDist <- BallCarrier_ProjDist %>% 
  mutate(ball_carrier_X_proj = ball_carrier_x + (ball_carrier_speed*frame_length*cos((90-ball_carrier_direction)*pi/180)),
         ball_carrier_Y_proj = ball_carrier_y + (ball_carrier_speed*frame_length*sin((90-ball_carrier_direction)*pi/180)))
BallCarrier_ProjDist <- BallCarrier_ProjDist %>% 
  select(c("playId", "gameId", "frameId", "ball_carrier_X_proj", "ball_carrier_Y_proj"))
tracking_w1 <- tracking_w1 %>% 
  left_join(BallCarrier_ProjDist, by = c("playId", "gameId", "frameId"))

tracking_w1 <- tracking_w1 %>%
  group_by(gameId, playId, frameId) %>%
  mutate(ball_carrier_X_proj = ball_carrier_X_proj,
         ball_carrier_Y_proj = ball_carrier_Y_proj,
         proj_dist_to_ball_carrier = calc_distance(X_proj, 
                                              Y_proj, 
                                              x_baseline = ball_carrier_X_proj, 
                                              y_baseline = ball_carrier_Y_proj)) %>%
  ungroup()

## Here's a sample play and the corresponding voronoi diagram
Zay_Jones_catch <- tracking_w1 %>%
  filter(gameId==2022091109 & playId==1915) %>%
  arrange(frameId)

playDescription <- unique(Zay_Jones_catch$playDescription)

plotly::ggplotly(
Zay_Jones_catch %>%
  filter(frameId>18) %>%
  ggplot(aes(x = X_std, y = Y_std, text = paste0('Dir: ', dir2, '\n',
                                                 'Player Name: ',displayName))) +
  stat_voronoi(geom="path") +
  geom_point(aes(color = Player_Role)) +
  geom_segment(aes(x = X_std, y = Y_std, xend = X_proj,
                   yend = Y_proj, color = Player_Role)) +
  scale_color_manual(values = c("Ball Carrier" = "black", 
                                "Offense" = "red",
                                "Defense" = "blue",
                                "Football" = "brown")) +
  labs(x = "X (High X = Where Offense Is Aiming)", y = "Y (High Y = Offense's Left)", 
       caption = paste0("Description: ", playDescription)) +
  geom_hline(yintercept = 0, color = 'darkgreen', linetype='dashed') +
  geom_hline(yintercept = 53.3, color = 'darkgreen', linetype='dashed') +
  facet_wrap(~frameId)
)

## Let's start getting the distances
## This is the week 1 data with a subset of the columns
tracking_sub <- tracking_w1 %>%
  select(gameId, playId, nflId, displayName,
         frameId, time, jerseyNumber, club,
         playDirection, x, y, s, a, dis,
         o, dir, event, passResult, passLength,
         ballCarrierId, ballCarrierDisplayName, playDescription,
         possessionTeam, defensiveTeam, toLeft, TeamOnOffense,
         AwayOrHome, IsPlayerOnOffense, Player_Role, 
         X_std, Y_std, o2, dir2, X_proj, Y_proj) %>%
  arrange(gameId, playId, frameId, club, nflId)
# Important to arrange in this order so that all 11 players on one team appear before the other team's players

## Each Player's Distance to the ball carrier
tracking_sub <- tracking_sub %>%
  group_by(gameId, playId, frameId) %>%
  mutate(X_ball_carrier = X_std[which(nflId==ballCarrierId)],
         Y_ball_carrier = Y_std[which(nflId==ballCarrierId)],
         dist_to_ball_carrier = calc_distance(X_std, 
                                              Y_std, 
                                              x_baseline = X_ball_carrier, 
                                              y_baseline = Y_ball_carrier)) %>%
  ungroup()


#### More Distances and two more sample plays:
McKenzie_catch <- tracking_sub %>%
  filter(gameId==2022090800 & playId==617)

Singletary_run <- tracking_sub %>%
  filter(gameId==2022090800 & playId==101)

## Another Voronoi Diagram
ggplotly(
  Singletary_run %>%
  filter(frameId>=unique(frameId[which(event=='handoff')]), frameId<=unique(frameId[which(event=='first_contact')])) %>%
  ggplot(aes(x = X_std, y = Y_std)) +
  stat_voronoi(geom="path") +
  geom_point(aes(color = Player_Role)) +
  geom_segment(aes(x = X_std, y = Y_std, xend = X_proj,
                   yend = Y_proj, color = Player_Role)) +
  scale_color_manual(values = c("Ball Carrier" = "black", 
                                "Offense" = "red",
                                "Defense" = "blue",
                                "Football" = "brown")) +
  labs(x = "X (High X = Where Offense Is Aiming)", y = "Y (High Y = Offense's Left)") +
  geom_hline(yintercept = 0, color = 'darkgreen', linetype='dashed') +
  geom_hline(yintercept = 53.3, color = 'darkgreen', linetype='dashed') +
  facet_wrap(~frameId)
)

## Most important chunk: Calculating distances to closest players on opposing teams
tracking_sub <- tracking_sub %>%
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

tracking_sub <- tracking_sub %>%
  group_by(gameId, playId, frameId) %>%
  mutate(
    closest_opp_player_name = case_when(
      row_number()<=11 ~ displayName[12+min_dist_opp_index],
      row_number()>=13 ~ displayName[min_dist_opp_index]
    ),
    closest_opp_player_nflID = case_when(
      row_number()<=11 ~ nflId[12+min_dist_opp_index],
      row_number()>=13 ~ nflId[min_dist_opp_index]
    ),
    second_closest_opp_player_name = case_when(
      row_number()<=11 ~ displayName[12+second_closest_opp_index],
      row_number()>=13 ~ displayName[second_closest_opp_index]
    ),
    second_closest_opp_player_nflID = case_when(
      row_number()<=11 ~ nflId[12+second_closest_opp_index],
      row_number()>=13 ~ nflId[second_closest_opp_index]
    )
  ) %>%
  ungroup()

## View a sample play:
View(tracking_sub %>%
       filter(gameId==2022090800 & playId==617))

McKenzie_catch <- tracking_sub %>%
  filter(gameId==2022090800 & playId==617)
plotly::ggplotly(
  McKenzie_catch %>%
    filter(frameId >= unique(frameId[which(event == 'pass_arrived')]), frameId <= unique(frameId[which(event == 'first_contact')])) %>%
    ggplot(aes(x = X_std, y = Y_std, 
               text = paste0('Dir: ', dir, '\n',
                             'Player Name: ',displayName, '\n',
                             'Closest Opposing Player: ', closest_opp_player_name, '\n',
                             'Closest Opposing Player Dist: ', round(min_dist_opp_player, 3), '\n',
                             'Distance to Ball: ', round(dist_to_ball_carrier, 3), '\n',
                             'Second Closest Opposing Player: ', second_closest_opp_player_name, '\n',
                             'Second Closest Opposing Player Dist: ', round(second_closest_dist_opp_player, 3)
               )
    )) +
    stat_voronoi(geom = "path") +
    geom_point(aes(color = Player_Role)) +
    geom_segment(aes(x = X_std, y = Y_std, xend = X_proj,
                     yend = Y_proj, color = Player_Role)) +
    scale_color_manual(values = c("Ball Carrier" = "black", 
                                  "Offense" = "red",
                                  "Defense" = "blue",
                                  "Football" = "brown")) +
    labs(x = "X (High X = Where Offense Is Aiming)", y = "Y (High Y = Offense's Left)", 
    caption = paste0("Description: ", playDescription)) +
    geom_hline(yintercept = 0, color = 'darkgreen', linetype='dashed') +
    geom_hline(yintercept = 53.3, color = 'darkgreen', linetype='dashed') +
    facet_wrap(~frameId)
)

##########################################################################################

# Now this is the second part of the file, i.e. the one that assumes you pick up where Initial_DataCleansing_Code left off
# This one will run much slower, though, since it includes the full data set, rather than just Week 1

# Frames for how long we want to project forward
frame_length <- 0.5

# Recall, when making MergedData, we standardized so highest "y" is always to offense's left
# I.e., make it so that it doesn't matter which end zone the offense is aiming at
# Likewise, adjust "o" and "dir" were adjusted so 0 is always to offense's left
# So 90 is always toward the EZ offense is aiming at, 180 is to offense's right, etc.
# Same adjustment for "x" - high "x" is always where offense is aiming at

# Adding projections for each player's location going forward (by 0.5 seconds)
# Test the kinematics-based approach vs. the simple approach solely based on current player speed
MergedData <- MergedData %>%
  mutate(X_proj_1 = x + ((s*frame_length + 0.5*a*(frame_length)^2)*cos((90-dir)*pi/180)),
         Y_proj_1 = y + ((s*frame_length + 0.5*a*(frame_length)^2)*sin((90-dir)*pi/180)),
         X_proj_2 = x + (s*frame_length*cos((90-dir)*pi/180)),
         Y_proj_2 = y + (s*frame_length*sin((90-dir)*pi/180)))

# Figure out which approach is more accurate (physics-based, or just speed * time)
AccuracyTest <- MergedData %>% mutate(X_Actual_0.5SecAhead = 
   ifelse(displayName == lead(displayName, 5), lead(x, 5), NA))
AccuracyTest <- AccuracyTest %>% mutate(Y_Actual_0.5SecAhead = 
   ifelse(displayName == lead(displayName, 5), lead(y, 5), NA))

AccuracyTest <- AccuracyTest %>%
  mutate(X_proj_Error1 = X_proj_1 - X_Actual_0.5SecAhead,
         X_proj_Error2 = X_proj_2 - X_Actual_0.5SecAhead,
         Y_proj_Error1 = Y_proj_1 - Y_Actual_0.5SecAhead,
         Y_proj_Error2 = Y_proj_2 - Y_Actual_0.5SecAhead)

sd(AccuracyTest$X_proj_Error1, na.rm = TRUE)
sd(AccuracyTest$X_proj_Error2, na.rm = TRUE)
sd(AccuracyTest$Y_proj_Error1, na.rm = TRUE)
sd(AccuracyTest$Y_proj_Error2, na.rm = TRUE)

mean(AccuracyTest$X_proj_Error1, na.rm = TRUE)
mean(AccuracyTest$X_proj_Error2, na.rm = TRUE)
mean(AccuracyTest$Y_proj_Error1, na.rm = TRUE)
mean(AccuracyTest$Y_proj_Error2, na.rm = TRUE)

sqrt(mean((AccuracyTest$X_proj_Error1)^2, na.rm = TRUE))
sqrt(mean((AccuracyTest$X_proj_Error2)^2, na.rm = TRUE))
sqrt(mean((AccuracyTest$Y_proj_Error1)^2, na.rm = TRUE))
sqrt(mean((AccuracyTest$Y_proj_Error2)^2, na.rm = TRUE))
# Method 2 (non-physics approach) has significantly lower RMSE

# So, get rid of first version
MergedData <- MergedData %>% select(-c("X_proj_1", "Y_proj_1"))
MergedData <- MergedData %>% 
  rename(X_proj = X_proj_2, Y_proj = Y_proj_2)
rm(AccuracyTest)

# And, now, also add the ball-carrier's projected distance in 0.5 seconds
BallCarrier_ProjDist <- MergedData %>% 
  filter(IsBallCarrier > 0) %>% 
  select(gameId, playId, frameId, s, a, o, dir, x, y) %>% 
  rename(ball_carrier_speed = s, ball_carrier_acc = a,
         ball_carrier_orient = o, ball_carrier_direction = dir,
         ball_carrier_x = x, ball_carrier_y = y)
BallCarrier_ProjDist <- BallCarrier_ProjDist %>% 
  mutate(ball_carrier_X_proj = ball_carrier_x + (ball_carrier_speed*frame_length*cos((90-ball_carrier_direction)*pi/180)),
         ball_carrier_Y_proj = ball_carrier_y + (ball_carrier_speed*frame_length*sin((90-ball_carrier_direction)*pi/180)))
BallCarrier_ProjDist <- BallCarrier_ProjDist %>% 
  select(c("playId", "gameId", "frameId", "ball_carrier_X_proj", "ball_carrier_Y_proj"))
MergedData <- MergedData %>% 
  left_join(BallCarrier_ProjDist, by = c("playId", "gameId", "frameId"))

MergedData <- MergedData %>%
  group_by(gameId, playId, frameId) %>%
  mutate(ball_carrier_X_proj = ball_carrier_X_proj,
         ball_carrier_Y_proj = ball_carrier_Y_proj,
         proj_dist_to_ball_carrier = calc_distance(X_proj, 
                                              Y_proj, 
                                              x_baseline = ball_carrier_X_proj, 
                                              y_baseline = ball_carrier_Y_proj)) %>%
  ungroup()

## Here's a sample play and the corresponding voronoi diagram
McKenzie_catch <- MergedData %>%
  filter(gameId == 2022090800 & playId == 617) %>%
  arrange(frameId)

playDescription <- unique(McKenzie_catch$playDescription)

ggplotly(
  McKenzie_catch %>%
    filter(frameId >= unique(frameId[which(event=='pass_arrived')])) %>%
    ggplot(aes(x = x, y = y, text = paste0('Dir: ', dir, '\n',
                                           'Player Name: ',displayName))) +
    # stat_voronoi(geom="path") +
    geom_point(aes(color = Player_Role)) +
    geom_segment(aes(x = x, y = y, xend = X_proj,
                     yend = Y_proj, color = Player_Role)) +
    scale_color_manual(values = c("Ball Carrier" = "black", 
                                  "Offense" = "red",
                                  "Defense" = "blue")) +
    theme_bw() + 
    labs(x = "X (High X = Where Offense Is Aiming)", y = "Y (High Y = Offense's Left)", 
         title = "Frame-By-Frame Diagram of J. Allen Pass to I. McKenzie (From Arrival of Pass)") +
    geom_hline(yintercept = 0, color = 'darkgreen', linetype = 'dashed') +
    geom_hline(yintercept = 53.3, color = 'darkgreen', linetype = 'dashed') +
    facet_wrap(~frameId) +
    theme(plot.title = element_text(size = 10, hjust = 0.5))
)

# Recall in MergedData, we already have TotDistFromBall
# Also Y_DistFromBall, X_DistFromBall, Y_AbsDistFromBall, X_AbsDistFromBall
# And the overall distance from ball-carrier

# Arrange like this so that all 11 players on one team show up before the other team
MergedData <- MergedData %>% arrange(gameId, playId, frameId, club, nflId)

# Another sample play to work with
Singletary_run <- MergedData %>%
  filter(gameId == 2022090800 & playId == 101)

## Another Voronoi diagram
ggplotly(
  Singletary_run %>%
  filter(frameId >= unique(frameId[which(event=='handoff')]), frameId <= unique(frameId[which(event=='first_contact')])) %>%
    ggplot(aes(x = x, y = y, text = paste0('Dir: ', dir, '\n',
                                           'Player Name: ',displayName))) +
  # stat_voronoi(geom = "path") +
  geom_point(aes(color = Player_Role)) +
  geom_segment(aes(x = x, y = y, xend = X_proj,
                   yend = Y_proj, color=Player_Role)) +
  scale_color_manual(values = c("Ball Carrier" = "black", 
                                "Offense" = "red",
                                "Defense" = "blue")) +
  theme_bw() +
  labs(x = "X (High X = Where Offense Is Aiming)", y = "Y (High Y = Offense's Left)", 
       title = "Frame-By-Frame Diagram of D. Singletary Rush (From Handoff to First Contact)") +
  geom_hline(yintercept = 0, color = 'darkgreen', linetype = 'dashed') +
  geom_hline(yintercept = 53.3, color = 'darkgreen', linetype = 'dashed') +
  facet_wrap(~frameId) +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
)

## Most important chunk: Calculating distances to closest players on opposing teams
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

# This section was adjusted so that it accounts for "football" no longer being there
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

## View a sample play: View(McKenzie_catch)
McKenzie_catch <- MergedData %>%
  filter(gameId == 2022090800 & playId == 617) %>%
  arrange(frameId)

plotly::ggplotly(
  McKenzie_catch %>%
    filter(frameId >= unique(frameId[which(event == 'pass_arrived')]), frameId <= unique(frameId[which(event == 'first_contact')])) %>%
    ggplot(aes(x = x, y = y, 
               text = paste0('Dir: ', dir, '\n',
                             'Player Name: ',displayName, '\n',
                             'Closest Opposing Player: ', closest_opp_player_name, '\n',
                             'Closest Opposing Player Dist: ', round(min_dist_opp_player, 3), '\n',
                             'Distance to Ball: ', round(dist_to_ball_carrier, 3), '\n',
                             'Second Closest Opposing Player: ', second_closest_opp_player_name, '\n',
                             'Second Closest Opposing Player Dist: ', round(second_closest_dist_opp_player, 3)
               )
    )) +
    # stat_voronoi(geom = "path") +
    geom_point(aes(color = Player_Role)) +
    geom_segment(aes(x = x, y = y, xend = X_proj,
                     yend = Y_proj, color = Player_Role)) +
    scale_color_manual(values = c("Ball Carrier" = "black", 
                                  "Offense" = "red",
                                  "Defense" = "blue")) +
    theme_bw() +
    labs(x = "X (High X = Where Offense Is Aiming)", y = "Y (High Y = Offense's Left)", 
    title = "Frame-By-Frame Diagram of J. Allen Pass to I. McKenzie (Pass Arrival to First Contact)") +
    geom_hline(yintercept = 0, color = 'darkgreen', linetype = 'dashed') +
    geom_hline(yintercept = 53.3, color = 'darkgreen', linetype = 'dashed') +
    facet_wrap(~frameId) +
    theme(plot.title = element_text(size = 9, hjust = 0.5))
)

