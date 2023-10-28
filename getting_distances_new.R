setwd("C:/Users/justi/OneDrive/Penn/BigDataBowl")
library(tidyverse)
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


## testing loop on one play
test_play <- tracking_w1 %>%
  filter(gameId==2022090800 & playId==101)
length(unique(test_play$game_play_frame_comb)) ## 49 frames in this play

player <- c() # player of interest
other_players <- c() # other players in frame
names <- c()
dist <- c()
for (i in 1:length(unique(test_play$game_play_frame_comb))) {
  print(i)
  frame_i <- test_play[which(test_play$number==i), c("game_play_frame_comb","nflId", "displayName", "frameId", "x", "y")]
  #other_players_i <- rep(frame_i$nflId, 23)
  game_play_frame_comb <- rep(frame_i$game_play_frame_comb, 23)
  for (j in 1:23) { ##looping through each player in the frame
    player_i <- rep(frame_i$nflId[j], 23)
    x_coord <- frame_i$x[j]
    y_coord <- frame_i$y[j]
    player <- c(player, player_i)
    other_players <- c(other_players, other_players_i)
    dist_i <- calc_distance(x = x_coord, y = y_coord, x_baseline = frame_i$x, y_baseline = frame_i$y)
    dist <- c(dist, dist_i)
  }
}

## checking output:
test_output <- cbind(game_play_frame_comb,player, dist)
colnames(test_output) <- c("game_play_frame_comb", "nflid", "distance")
##from the test output, we can join to the original dataset on the game play frame comb and nflid



######################################################
##run on the full file:
player <- c() # player of interest
other_players <- c() # other players in frame
names <- c()
dist <- c()
t1 <- Sys.time()
for (i in 1:length(unique(tracking_w1$game_play_frame_comb))) {
  print(i)
  frame_i <- test_play[which(test_play$number==i), c("game_play_frame_comb","nflId", "displayName", "frameId", "x", "y")]
  #other_players_i <- rep(frame_i$nflId, 23)
  game_play_frame_comb <- rep(frame_i$game_play_frame_comb, 23)
  for (j in 1:23) { ##looping through each player in the frame
    player_i <- rep(frame_i$nflId[j], 23)
    x_coord <- frame_i$x[j]
    y_coord <- frame_i$y[j]
    player <- c(player, player_i)
    #other_players <- c(other_players, other_players_i)
    dist_i <- calc_distance(x = x_coord, y = y_coord, x_baseline = frame_i$x, y_baseline = frame_i$y)
    dist <- c(dist, dist_i)
  }
}
t2 <- Sys.time()
t2-t1

## checking output:
test_output <- cbind(game_play_frame_comb,player, dist)
colnames(test_output) <- c("game_play_frame_comb", "nflid", "distance")





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
