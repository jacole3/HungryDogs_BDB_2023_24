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
  mutate(ball_carrier_ind = ifelse(ballCarrierId==nflId,1,0),
         toLeft = ifelse(playDirection=="left", 1, 0))

#standardizing directions:
tracking_w1 <- tracking_w1 %>%
  mutate(TeamOnOffense = ifelse(possessionTeam==homeTeamAbbr, "home", "away"),
         AwayOrHome = ifelse(club==homeTeamAbbr, "home", "away"),
         IsPlayerOnOffense = ifelse(possessionTeam==club, "offense", "defense"),
         X_std = ifelse(toLeft==1, 120-x, x), ## Standardizes X
         Y_std = ifelse(toLeft==1, 160/3-y, y), ## Standardized Y
         Player_Role= case_when(
           nflId==ballCarrierId ~ "Ball Carrier",
           possessionTeam!=club & displayName!="football"~ "Defense",
           possessionTeam==club & ballCarrierId!=nflId ~ "Offense",
           displayName=="football" ~ "Football"
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
  geom_density(fill='dodgerblue') +
  facet_wrap(~playDirection)

# Here's same concept, but based on which side of ball player is on
tracking_w1 %>%
  ggplot(aes(x = dir)) +
  geom_density(fill='dodgerblue') +
  facet_wrap(~IsPlayerOnOffense)

#adding projection forward
tracking_w1 <- tracking_w1 %>%
  mutate(X_proj = X_std + (s*frame_length*cos((90-dir2)*pi/180)),
         Y_proj = Y_std + (s*frame_length*sin((90-dir2)*pi/180)))
# Note that we tested a kinematics-based approach (incorporating acceleration) in addition to the "speed*frame_length" approach
# Turned out the simpler approach (without acceleration) was more accurate in projecting future distances

## Here's a sample play and the corresponding voronoi diagram
Zay_Jones_catch <- tracking_w1 %>%
  filter(gameId==2022091109 & playId==1915) %>%
  arrange(frameId)

playDescription <- unique(Zay_Jones_catch$playDescription)

plotly::ggplotly(
Zay_Jones_catch %>%
  filter(frameId>18) %>%
  ggplot(aes(x = X_std, y = Y_std, text = paste0('Dir: ', dir2,
                                                 'Player Name: ',displayName))) +
  stat_voronoi(geom="path") +
  geom_point(aes(color = Player_Role)) +
  geom_segment(aes(x = X_std, y = Y_std, xend = X_proj,
                   yend = Y_proj, color=Player_Role)) +
  scale_color_manual(values = c("Ball Carrier" = "black", 
                                "Offense" = "red",
                                "Defense" = "blue",
                                "Football"="brown")) +
  labs(x = "X (High X = Where Offense Is Aiming)", y = "Y (High Y = Offense's Left)", 
       caption = paste0("Description: ", playDescription)) +
  geom_hline(yintercept = 0, color = 'darkgreen', linetype='dashed')+
  geom_hline(yintercept = 53.3, color = 'darkgreen', linetype='dashed')+
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
mckenzie_catch <- tracking_sub %>%
  filter(gameId==2022090800 & playId==617)

Singletary_run <- tracking_sub %>%
  filter(gameId==2022090800 & playId==101)

## Another Voronoi Diagram
Singletary_run %>%
  filter(frameId>=unique(frameId[which(event=='handoff')]), frameId<=unique(frameId[which(event=='first_contact')])) %>%
  ggplot(aes(x = X_std, y = Y_std)) +
  stat_voronoi(geom="path") +
  geom_point(aes(color = Player_Role)) +
  geom_segment(aes(x = X_std, y = Y_std, xend = X_proj,
                   yend = Y_proj, color=Player_Role)) +
  scale_color_manual(values = c("Ball Carrier" = "black", 
                                "Offense" = "red",
                                "Defense" = "blue",
                                "Football" = "brown")) +
  labs(x = "X (High X = Where Offense Is Aiming)", y = "Y (High Y = Offense's Left)") +
  geom_hline(yintercept = 53.3, color = 'darkgreen', linetype='dashed')+
  geom_hline(yintercept = 53.3, color = 'darkgreen', linetype='dashed')+
  facet_wrap(~frameId)


## Most important chunk: Calculating Distances to Closest Players on Opposing teams
tracking_sub <- tracking_sub %>%
  group_by(gameId, playId, frameId) %>%
  mutate(min_dist = map_dbl(.x=row_number(), ~min(calc_distance(x = X_std[which(club[.x]!=club & club!='football')],
                                                                y = Y_std[which(club[.x]!=club & club!='football')],
                                                                x_baseline = X_std[.x],
                                                                y_baseline = Y_std[.x]))),
         num_same_dist = map_dbl(.x=row_number(), ~11 - length(unique(calc_distance(x = X_std[which(club[.x]!=club & club!='football')],
                                                                     y = Y_std[which(club[.x]!=club & club!='football')],
                                                                     x_baseline = X_std[.x],
                                                                     y_baseline = Y_std[.x])))),
         num_same_dist=ifelse(displayName=='football', 0, num_same_dist),
         min_dist_pos = map_dbl(.x=row_number(), ~which(calc_distance(x = X_std[which(club[.x]!=club & club!='football')],
                                                                      y = Y_std[which(club[.x]!=club & club!='football')],
                                                                      x_baseline = X_std[.x],
                                                                      y_baseline = Y_std[.x])== min(calc_distance(x = X_std[which(club[.x]!=club & club!='football')],
                                                                                                                  y = Y_std[which(club[.x]!=club & club!='football')],
                                                                                                                  x_baseline = X_std[.x],
                                                                                                                  y_baseline = Y_std[.x])))[1]),
         second_closest_dist = map_dbl(.x=row_number(), ~Rfast::nth(calc_distance(x = X_std[which(club[.x]!=club & club!='football')],
                                                                             y = Y_std[which(club[.x]!=club & club!='football')],
                                                                             x_baseline = X_std[.x],
                                                                             y_baseline = Y_std[.x]), 2, descending = F)),
         second_closest_def_pos = map_dbl(.x=row_number(), ~which(calc_distance(x = X_std[which(club[.x]!=club & club!='football')],
                                                                                y = Y_std[which(club[.x]!=club & club!='football')],
                                                                                x_baseline = X_std[.x],
                                                                                y_baseline = Y_std[.x])== Rfast::nth(calc_distance(x = X_std[which(club[.x]!=club & club!='football')],
                                                                                                                                   y = Y_std[which(club[.x]!=club & club!='football')],
                                                                                                                                   x_baseline = X_std[.x],
                                                                                                                                   y_baseline = Y_std[.x]),2,descending = F))[1])#Where I account for duplicates
         
  ) %>%
  ungroup()


tracking_sub <- tracking_sub %>%
  group_by(gameId, playId, frameId) %>%
  mutate(
    closest_player_name = case_when(
      row_number()<=11 ~ displayName[12+min_dist_pos],
      row_number()>=13 ~ displayName[min_dist_pos]
    ),
    closest_player_nflID = case_when(
      row_number()<=11 ~ nflId[12+min_dist_pos],
      row_number()>=13 ~ nflId[min_dist_pos]
    ),
    second_closest_player_name = case_when(
      row_number()<=11 ~ displayName[12+second_closest_def_pos],
      row_number()>=13 ~ displayName[second_closest_def_pos]
    ),
    second_closest_player_nflID = case_when(
      row_number()<=11 ~ nflId[12+second_closest_def_pos],
      row_number()>=13 ~ nflId[second_closest_def_pos]
    )
  ) %>%
  ungroup()

## View a sample play:
View(tracking_sub %>%
       filter(gameId==2022090800 & playId==617))


mckenzie_catch <- tracking_sub %>%
  filter(gameId==2022090800 & playId==617)
plotly::ggplotly(
  mckenzie_catch %>%
    filter(frameId>=unique(frameId[which(event=='pass_arrived')]), frameId<=unique(frameId[which(event=='first_contact')])) %>%
    ggplot(aes(x = X_std, y = Y_std, 
               text = paste0('Dir: ', dir2, '\n',
                             'Player Name: ',displayName, '\n',
                             'Closest Opposing Player: ', closest_player_name, '\n',
                             'Closest Opposing Player Dist: ', round(min_dist,3), '\n',
                             'Distance to Ball: ', round(dist_to_ball_carrier,3), '\n',
                             'Second Closest Opposing Player', second_closest_player_name, '\n',
                             'Second Closest Opposing Player Dist: ', round(second_closest_dist,3)
               )
                             )) +
    stat_voronoi(geom="path") +
    geom_point(aes(color = Player_Role)) +
    geom_segment(aes(x = X_std, y = Y_std, xend = X_proj,
                     yend = Y_proj, color=Player_Role)) +
    scale_color_manual(values = c("Ball Carrier" = "black", 
                                  "Offense" = "red",
                                  "Defense" = "blue",
                                  "Football" = "brown")) +
    labs(x = "X (High X = Where Offense Is Aiming)", y = "Y (High Y = Offense's Left)", 
    caption = paste0("Description: ", playDescription)) +
    geom_hline(yintercept = 0, color = 'darkgreen', linetype='dashed')+
    geom_hline(yintercept = 53.3, color = 'darkgreen', linetype='dashed')+
    facet_wrap(~frameId)
)
