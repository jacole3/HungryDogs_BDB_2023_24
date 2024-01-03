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

######
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
         Y_DistFromBall = (y - ball_y), X_DistFromBall = (x = ball_x),
         Y_AbsDistFromBall = abs(y - ball_y), X_AbsDistFromBall = abs(x - ball_x))
rm(ball_df)
  
