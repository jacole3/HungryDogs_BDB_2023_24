setwd("C:/Users/justi/OneDrive/Penn/BigDataBowl")
library(tidyverse)

# This GitHub file is split into two parts: one starts from scratch (i.e., if you've done no prior code), and one picks off where "Initial_DataCleansing_Code" and "distances_vectorized" left off
# Right here is the first part

tracking_w1 <- read_csv("tracking_week_1.csv")

## distance formula:
calc_distance <- function(x, y, x_baseline = 0, y_baseline = 0) {
  sqrt((x-x_baseline)^2 + (y - y_baseline)^2)
}

## arranging by each game, play, and frame within the play
## also am grabbing each unique combination of games, plays, and frames (to make looping easier)
tracking_w1 <- tracking_w1 %>%
  arrange(gameId, playId, frameId) %>%
  mutate(game_play_comb = paste0(gameId, "-", playId),
         game_play_frame_comb = paste0(gameId, "-", playId, "-", frameId))

## this turns each "game-play-frame" combination into a number
tracking_w1$number <- as.numeric(factor(tracking_w1$game_play_frame_comb))
length(unique(tracking_w1$game_play_frame_comb)) ## 61193 unique frames in this dataset
tracking_w1 <- tracking_w1 %>%
  arrange(game_play_frame_comb)


game_play_frame_comb<-c()                          
player <- c() # player of interest
closest_player <- c() # other players in frame
dist <- c()
t1 <- Sys.time()
for (i in 1:length(unique(tracking_w1$game_play_frame_comb))) {
  print(i)
  frame_i <- tracking_w1[which(tracking_w1$number==i), c("game_play_frame_comb","nflId", "club", "displayName", "frameId", "x", "y")] # removing game_play_frame_comb(go back if)
  #other_players_i <- rep(frame_i$nflId, 23)
  game_play_frame_comb_i <- rep(frame_i$game_play_frame_comb[which(frame_i$club!="football")],3) #not that simple
  game_play_frame_comb <- c(game_play_frame_comb, game_play_frame_comb_i)
  for (j in 1:23) { ##looping through each player in the frame
    if (frame_i$club[j]!="football") {
      club_i <- frame_i$club[j] #player's club
      player_i <- frame_i$nflId[j] #player
      x_coord <- frame_i$x[j] #player's x coord
      y_coord <- frame_i$y[j] #player's y coord
      player <- c(player, rep(player_i,3)) #player's name repeated three times
      
      #calculating distances and grabbing other stats
      dist_i <- calc_distance(x = x_coord, y = y_coord, x_baseline = frame_i$x, y_baseline = frame_i$y)#player's distance to everyone on the field
      min_dist_same_team <- min(dist_i[which(frame_i$club==club_i & dist_i>0)])
      min_dist_diff_team <- min(dist_i[which(frame_i$club!=club_i & frame_i$club!="football" & dist_i>0)])
      min_dist_football <- min(dist_i[which(frame_i$club=="football" & dist_i>0)])
      dist <- c(dist, min_dist_same_team, min_dist_diff_team, min_dist_football)
      closest_player_i <- frame_i$nflId[c(which(dist_i==min_dist_same_team),
                                          which(dist_i==min_dist_diff_team),
                                          which(dist_i==min_dist_football))]
      closest_player <- c(closest_player, closest_player_i)
    }
    else if (frame_i$club[j]=="football") {
      next
    }
  }
}
t2 <- Sys.time()
t2-t1


closest_player
## checking output:
test_output <- data.frame(cbind(game_play_frame_comb,player,closest_player, dist))
colnames(test_output) <- c("game_play_frame_comb", "nflid", "closest_player", "distance")
write.csv(test_output,"w1_dist.csv")

View(test_output[1:100,])

test_output <- test_output%>%
  group_by(game_play_frame_comb) %>%
  mutate(count=n()) %>%
  ungroup()

View(tracking_w1%>% filter(game_play_frame_comb=='2022090800-101-1'))
View(tracking_w1%>% filter(number==1))

## This was the original code that I used (feel free to test)
##looping through everything
# player <- c()
# names <- c()
# dist <- c()
# t1 <- Sys.time()
# for (plays in 1:length(unique(tracking_w1$game_play_comb))){
#   n_frames <- max(tracking_w1$frameId[which(tracking_w1$number==plays)])
#   play_i <- tracking_w1[which(tracking_w1$number==plays), c("nflId", "frameId", "x", "y")] ##the specific play
#   print(plays)
#   print(n_frames)
#   for (frame in 1:n_frames) {
#     player_ids <- play_i$nflId[which(play_i$frameId==frame)]
#     x_vect <- play_i$x[which(play_i$frameId==frame)] ##grabbing coords of each frame
#     y_vect <- play_i$y[which(play_i$frameId==frame)]
#     for (i in 1:23) {
#       #print(i)
#       player_i <- rep(player_ids[i], 23)
#       x_coord <- x_vect[i]
#       y_coord <- y_vect[i]
#       player <- c(player, player_i)
#       dist_i <- calc_distance(x = x_coord, y = y_coord, x_baseline = x_vect, y_baseline = y_vect)
#       dist <- c(dist, dist_i)
#     }
#   }
# }
# t2 <- Sys.time()
# t2-t1

tracking_w1 %>%
  filter(game_play_frame_comb%in% c("2022090800-101-1", "2022090800-101-2")) %>%
  ggplot(aes(x = x, y = y, color = club)) +
  geom_point()+
  facet_wrap(~game_play_frame_comb)

#############################################################
# Now this is the second part of the file, i.e. the one that assumes you pick up where Initial_DataCleansing_Code left off
# This one will run much slower, though, since it includes the full data set, rather than just Week 1

# Recall that calc_distance() formula was already established in distances_vectorized

# Also recall that TotDistFromBall, Y_DistFromBall, Y_AbsDistFromBall
# X_DistFromBall, and X_AbsDistFromBall were all established in the data cleansing code file

## Arrange by each game, play, and frame within the play
## Also grab each unique combination of games, plays, and frames (to make looping easier)
MergedData <- MergedData %>%
  arrange(gameId, playId, frameId) %>%
  mutate(game_play_comb = paste0(gameId, "-", playId),
         game_play_frame_comb = paste0(gameId, "-", playId, "-", frameId))

## this turns each "game-play-frame" combination into a number
MergedData$game_play_frame_number <- as.numeric(factor(MergedData$game_play_frame_comb))
length(unique(MergedData$game_play_frame_comb)) ## shows how many frames are in data set
MergedData <- MergedData %>%
  arrange(game_play_frame_comb)

game_play_frame_comb<-c()                          
player <- c() # player of interest
closest_player <- c() # other players in frame
dist <- c()
t1 <- Sys.time()
for (i in 1:length(unique(MergedData$game_play_frame_comb))) {
  print(i)
  frame_i <- MergedData[which(MergedData$game_play_frame_number==i), c("game_play_frame_comb","nflId", "club", "displayName", "frameId", "x", "y")] 
  game_play_frame_comb_i <- rep(frame_i$game_play_frame_comb[which(frame_i$club!="football")],3) # not that simple
  game_play_frame_comb <- c(game_play_frame_comb, game_play_frame_comb_i)
  for (j in 1:22) { ##looping through each player in the frame
    if (frame_i$club[j]!="football") {
      club_i <- frame_i$club[j] #player's club
      player_i <- frame_i$nflId[j] #player
      x_coord <- frame_i$x[j] #player's x coord
      y_coord <- frame_i$y[j] #player's y coord
      player <- c(player, rep(player_i,3)) #player's name repeated three times
      
      #calculating distances and grabbing other stats
      dist_i <- calc_distance(x = x_coord, y = y_coord, x_baseline = frame_i$x, y_baseline = frame_i$y)#player's distance to everyone on the field
      min_dist_same_team <- min(dist_i[which(frame_i$club==club_i & dist_i>0)])
      min_dist_diff_team <- min(dist_i[which(frame_i$club!=club_i & frame_i$club!="football" & dist_i>0)])
      dist <- c(dist, min_dist_same_team, min_dist_diff_team)
      closest_player_i <- frame_i$nflId[c(which(dist_i==min_dist_same_team),
                                          which(dist_i==min_dist_diff_team))]
      closest_player <- c(closest_player, closest_player_i)
    }
    else if (frame_i$club[j]=="football") {
      next
    }
  }
}
t2 <- Sys.time()
t2-t1

closest_player
## checking output:
test_output <- data.frame(cbind(game_play_frame_comb, player, closest_player, dist))
colnames(test_output) <- c("game_play_frame_comb", "nflid", "closest_player", "distance")
# If we need to export to CSV: write.csv(test_output,"Merged_distances.csv")
# View(test_output[1:100,])

test_output <- test_output %>%
  group_by(game_play_frame_comb) %>%
  mutate(count = n()) %>%
  ungroup()

# View(MergedData %>% filter(game_play_frame_comb == '2022090800-101-1'))
# View(MergedData %>% filter(game_play_frame_number == 1))

MergedData %>%
  filter(game_play_frame_comb%in% c("2022090800-101-1", "2022090800-101-2")) %>%
  ggplot(aes(x = x, y = y, color = club)) +
  geom_point()+
  facet_wrap(~game_play_frame_comb)
  
