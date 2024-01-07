setwd("C:/Users/justi/OneDrive/Penn/BigDataBowl")
#install.packages('Rfast')
library(Rfast)
library(tidyverse)
library(gganimate)
library(deldir)
library(ggvoronoi)
library(rpart)
library(rpart.plot)
library(tree)
library(randomForest)
library(ranger)

## Distance Formula
calc_distance <- function(x, y, x_baseline = 0, y_baseline = 0) {
  sqrt((x-x_baseline)^2 + (y - y_baseline)^2)
}

full_dat <- data.table::fread("full_data_modeling_w1.csv")


## selecting fairly large subset of our data:
modeling_data <- full_dat %>%
  select(gameId, playId, frameId, nflId, displayName, Player_Role, jerseyNumber, ballCarrierId, ballCarrierDisplayName,
         playDescription, possessionTeam, defensiveTeam, yardlineSide, passResult,
         passLength, playResult, offenseFormation, defendersInTheBox, expectedPointsAdded,
         yards_after_catch, assist, solo_tackle, club, x, y, s, a, dis, o, dir, event,
         dist_to_ball_carrier, ball_carrier_speed, ball_carrier_acc, ball_carrier_dist_ran,
         ball_carrier_orient, ball_carrier_direction, ball_carrier_momentum, Rel_Speed_ToBC,
         Rel_Acc_ToBC, CosSimilarity_Dir_ToBC, CosSimilarity_Orient_ToBC, Rel_Velocity_ToBC,
         X_proj, Y_proj, ball_carrier_X_proj, ball_carrier_Y_proj, min_dist_opp_player, min_dist_opp_index,
         min_dist_opp_player, closest_opp_player_nflID, closest_opp_player_name, second_closest_dist_opp_player, second_closest_opp_index, 
         second_closest_opp_player_name, second_closest_opp_player_nflID, In_BallCarrier_Radius,
         NumberOfBlockers, BlockedScore, within_dist_ofBC, within_dist_ofBC_frames_ahead)

rm(full_dat)

## grabbing directions of first and second closest players on opposing teams:
modeling_data <- modeling_data %>%
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

dist <- 1
frames <- 5
modeling_data <- modeling_data %>%
  mutate(within_dist_ofBC = ifelse(dist_to_ball_carrier <= dist, 1, 0)) %>%
  mutate(proj_distance = calc_distance(x = X_proj,
                                       y= Y_proj,
                                       x_baseline = ball_carrier_X_proj,
                                       y_baseline = ball_carrier_Y_proj)) %>%
  arrange(gameId, playId, frameId, nflId) %>%
  group_by(gameId, playId, nflId) %>%
  mutate(within_dist_ofBC_frames_ahead = ifelse(club == possessionTeam, NA,
    ifelse((lead(within_dist_ofBC) == 1 & playId == lead(playId) & nflId == lead(nflId)) |
         (lead(within_dist_ofBC, 2) == 1 & playId == lead(playId, 2) & nflId == lead(nflId, 2)) |
         (lead(within_dist_ofBC, 3) == 1 & playId == lead(playId, 3) & nflId == lead(nflId, 3)) | 
         (lead(within_dist_ofBC, 4) == 1 & playId == lead(playId, 4) & nflId == lead(nflId, 4)) | 
         (lead(within_dist_ofBC, frames) == 1 & playId == lead(playId, frames) & nflId == lead(nflId, frames)), 1,
       ifelse((playId == lead(playId) & nflId == lead(nflId)) &
                (playId == lead(playId, 2) & nflId == lead(nflId, 2)) &
                (playId == lead(playId, 3) & nflId == lead(nflId, 3)) &
                (playId == lead(playId, 4) & nflId == lead(nflId, 4)) &
                (playId == lead(playId, frames) & nflId == lead(nflId, frames)), 0, NA)))) %>%
  ungroup()


defenders_data <- modeling_data %>%
  filter(club!=possessionTeam) %>%
  filter(!is.infinite(BlockedScore))

mod1 <- glm(within_dist_ofBC_frames_ahead ~ CosSimilarity_Dir_ToBC + Rel_Velocity_ToBC + 
              dist_to_ball_carrier + proj_distance + NumberOfBlockers,
            data = defenders_data, family = 'binomial')


mod2 <- ranger(within_dist_ofBC_frames_ahead ~ CosSimilarity_Dir_ToBC + Rel_Velocity_ToBC + 
              dist_to_ball_carrier + proj_distance + NumberOfBlockers + BlockedScore,
            data = defenders_data, num.trees = 250)

# If that version of mod2 gives an error, run the following:
# mod2 <- ranger(sum(!is.na(within_dist_ofBC_frames_ahead)) ~ CosSimilarity_Dir_ToBC + Rel_Velocity_ToBC + 
#              dist_to_ball_carrier + proj_distance + NumberOfBlockers + BlockedScore,
#            data = defenders_data, num.trees = 250)

#rpart.plot(mod2)

defenders_data$pred <- predict(mod2,data =  defenders_data)$predictions

modeling_data <- modeling_data %>%
  left_join(defenders_data %>% select(pred, gameId, playId, nflId, frameId), by = c("gameId", "playId", "nflId", 'frameId'))

rm(defenders_data)

mckenzie_catch <- modeling_data %>%
  filter(gameId==2022090800 & playId==617)

singletary_run <- modeling_data %>%
  filter(gameId==2022090800 & playId==101)

##Good code to check:
View(singletary_run %>%
       filter(club==defensiveTeam) %>%
       arrange(displayName, frameId) %>%
       select(frameId, club, displayName, Player_Role,  dist_to_ball_carrier, within_dist_ofBC, within_dist_ofBC_frames_ahead, pred))

plotly::ggplotly(
  singletary_run %>%
    #filter(frameId >= unique(frameId[which(event=='handoff')]), frameId <= unique(frameId[which(event=='first_contact')])) %>%
    filter(frameId>=34, frameId<=40) %>%
    ggplot(aes(x = x, y = y, 
               text = paste0('Prediction: ', pred,
                 'Dir: ', dir, '\n',
                             'Player Name: ',displayName, '\n',
                             'Closest Opposing Player: ', closest_opp_player_name, '\n',
                             'Closest Opposing Player Dist: ', round(min_dist_opp_player, 3), '\n',
                             'Distance to Ball: ', round(dist_to_ball_carrier, 3), '\n',
                             'Second Closest Opposing Player: ',  second_closest_opp_player_name, '\n',
                             'Second Closest Opposing Player Dist: ', round(second_closest_dist_opp_player, 3), '\n',
                             'Blocked Score: ', round(BlockedScore, 3), '\n',
                             'Ball Carrier Direction: ', ball_carrier_direction, '\n', 
                             'Cos Similarity: ', round(CosSimilarity_Dir_ToBC, 3), '\n'
               )
    )) +
    #stat_voronoi(geom = "path") +
    geom_point(aes(color = Player_Role)) +
    geom_segment(aes(x = x, y = y, xend = X_proj,
                     yend = Y_proj, color = Player_Role)) +
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


##line charts
library(viridis)
singletary_run %>%
  filter(dist_to_ball_carrier<=10 & club==defensiveTeam) %>%
  mutate(time_in_secs = frameId*0.1) %>%
  ggplot( aes(x=time_in_secs, y=pred, group=displayName, color=displayName)) +
  geom_line() +
  geom_point() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Player's Probability of Being within 1 Yard of Ball Carrier \n
          Five Frames into the Future") +
  #theme_ipsum() +
  ylab("Probability") +
  transition_reveal(time_in_secs)




#############
games <- read.csv('games.csv')
players <- read.csv('players.csv')
plays <- read.csv('plays.csv')

#df_tracking <- fread('week1.csv')
#df_tracking <- df_tracking %>%
#  mutate(x = ifelse(playDirection == "left", 120-x, x),
#         y = ifelse(playDirection == "left", 160/3 - y, y))
#View(df_tracking[1,])









#Sample plays and corresponding visualizations
set.seed(1)

example_play <- modeling_data %>%
  sample_n(1)

example_game <- example_play$gameId
example_play <- example_play$playId
# games<- read_csv("games.csv")
# players <- read_csv("players.csv")
# plays <- read_csv("plays.csv")
# 
# #merging games data to play
# example_play <- inner_join(example_play,
#                            games,
#                            by = c("gameId" = "gameId")) #gets teams involved, using gameID as an index
# 
# #merging tracking data to play
# example_play <- inner_join(example_play,
#                            modeling_data,
#                            by = c("gameId" = "gameId",
#                                   "playId" = "playId"))

example_play <- modeling_data %>%
  filter(gameId==example_game & playId==example_play)
plot_title <- str_trim(gsub("\\s*\\([^\\)]+\\)","",as.character(example_play$playDescription[1])))



xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3

# Specific boundaries for a given play
ymin <- max(round(min(example_play$x, na.rm = TRUE) - 10, -1), 0)
ymax <- min(round(max(example_play$x, na.rm = TRUE) + 10, -1), 120)

#hash marks
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)

cols_fill <- c("#FB4F14", "#663300", "#A5ACAF")
cols_col <- c("#000000", "#663300", "#000000")


#plotting
ggplot() +
  
  #setting size and color parameters
  scale_size_manual(values = c(6, 6, 6), guide = FALSE) + 
  scale_shape_manual(values = c(21, 21, 21), guide = FALSE) +
  scale_fill_manual(values = c("red", 'blue', "black"), guide = FALSE) + 
  scale_colour_manual(values = c("red", 'blue', "pink"), guide = FALSE) +
  
  #adding hash marks
  annotate("text", x = df.hash$x[df.hash$x < 55/2], 
           y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
  annotate("text", x = df.hash$x[df.hash$x > 55/2], 
           y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
  
  #adding yard lines
  annotate("segment", x = xmin, 
           y = seq(max(10, ymin), min(ymax, 110), by = 5), 
           xend =  xmax, 
           yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
  
  #adding field yardline text
  annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 270, size = 4) + 
  annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
           angle = 90, size = 4) + 
  
  #adding field exterior
  annotate("segment", x = c(xmin, xmin, xmax, xmax), 
           y = c(ymin, ymax, ymax, ymin), 
           xend = c(xmin, xmax, xmax, xmin), 
           yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
  
  #adding players
  geom_point(data = example_play, aes(x = (xmax-y),
                                      y = x, 
                                      shape = club,
                                      fill = club,
                                      group = nflId,
                                      size = club,
                                      colour = Player_Role), 
             alpha = 0.7) +  
  
  #adding jersey numbers
  geom_text(data = example_play, aes(x = (xmax-y), y = x, label = jerseyNumber), colour = "white", 
            vjust = 0.36, size = 3.5) + 
  
  #applying plot limits
  ylim(ymin, ymax) + 
  coord_fixed() +
  
  #applying theme
  #theme_nothing() + 
  theme(plot.title = element_text()) +
  
  #titling plot with play description
  labs(title = plot_title) +
  
  #setting animation parameters
  transition_time(frameId)  +
  ease_aes('linear') + 
  NULL
