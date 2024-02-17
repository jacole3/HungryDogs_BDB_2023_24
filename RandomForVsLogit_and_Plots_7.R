setwd("C:/Users/justi/OneDrive/Penn/BigDataBowl")
#install.packages('Rfast')
library(Rfast)
library(tidyverse)
library(gganimate)
library(ggforce)
library(deldir)
library(ggvoronoi)
library(rpart)
library(rpart.plot)
library(tree)
library(randomForest)
library(ranger)
library(pROC)

## Distance Formula
calc_distance <- function(x, y, x_baseline = 0, y_baseline = 0) {
  sqrt((x-x_baseline)^2 + (y - y_baseline)^2)
}

# This is a Week 1 version of the full_merged_data DF created in the full_modeling_code GitHub file, in CSV form
# This could easily be replicated with any given week
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

## Had to fix the within distance into the future measure
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
    mutate(within_dist_ofBC = ifelse(dist_to_ball_carrier <= dist, 1, 0)) %>%
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
                (playId == lead(playId, frames) & nflId == lead(nflId, frames)), 0, NA)))) %>%
  ungroup()


## Further cleaning to the blocked score:
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

modeling_data <- FixingBlockingScore(final_merged_data = modeling_data)

modeling_data <- modeling_data %>% mutate(BlockedScore =
                                                    ifelse(is.na(BlockedScore), NA, BlockedScore / max(modeling_data$BlockedScore, na.rm = TRUE)))


#Projections Using Ryan's Method:
modeling_data <- modeling_data %>%
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
BallCarrier_ProjDist <- modeling_data %>% 
  filter(nflId==ballCarrierId) %>% 
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
modeling_data <- modeling_data %>% 
  left_join(BallCarrier_ProjDist, by = c("playId", "gameId", "frameId"))

modeling_data <- modeling_data %>%
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
modeling_data <- modeling_data %>% mutate(min_proj_dist_to_ball_carrier =
                                      pmin(proj_dist_to_ball_carrier_1, proj_dist_to_ball_carrier_2, proj_dist_to_ball_carrier_3,
                                           proj_dist_to_ball_carrier_4, proj_dist_to_ball_carrier_5))
modeling_data <- modeling_data %>% select(c(-"proj_dist_to_ball_carrier_1", -"proj_dist_to_ball_carrier_2", 
                                      -"proj_dist_to_ball_carrier_3", -"proj_dist_to_ball_carrier_4", -"proj_dist_to_ball_carrier_5"))

######################################
## Now, we have our cleaned data
## Let's do a train-test split
modeling_data <- modeling_data %>%
  mutate(game_play_combination = paste0(gameId, '-', playId))

set.seed(44)
unique_plays <- unique(modeling_data$game_play_combination)
train_indices <- sample(1:length(unique_plays), 0.8*length(unique_plays))
train_plays <- unique_plays[train_indices]
test_plays <- unique_plays[-train_indices]

## MODELING:
## first filtering to only inlude defenders and players within 10 yards of the ball carrier
defenders_data <- modeling_data %>%
  filter(club!=possessionTeam) %>%
  filter(!is.infinite(BlockedScore), dist_to_ball_carrier <= 10)

## Train/test data:
train_data <- defenders_data %>%
  filter(game_play_combination %in% train_plays)

test_data <- defenders_data %>%
  filter(game_play_combination %in% test_plays)

ranger_model <- ranger(within_dist_ofBC_frames_ahead ~ 
                 dist_to_ball_carrier + 
                 min_proj_dist_to_ball_carrier + 
                 NumberOfBlockers + BlockedScore,
            data = train_data, num.trees = 500, importance = "impurity")

pred.train <- predict(ranger_model, train_data, type = "response") 
pred.test <- predict(ranger_model,test_data, type = "response") 


train_error <- data.frame(true_outcome = train_data$within_dist_ofBC_frames_ahead, train_prob = pred.train$predictions) 
train_error <- train_error %>%
  mutate(prediction = ifelse(train_prob >= 0.5, 1, 0),
         match = ifelse(prediction==true_outcome, 1, 0))
mean(train_error$true_outcome != train_error$prediction) #training error

ranger_model$prediction.error #oob error


test_error <- data.frame(true_outcome = test_data$within_dist_ofBC_frames_ahead, test_preds = pred.test$predictions)
test_error <- test_error %>%
  mutate(prediction = ifelse(test_preds >= 0.5, 1, 0))
mean(test_error$prediction != test_error$true_outcome)

ranger.test.roc <- roc(test_data$within_dist_ofBC_frames_ahead, pred.test$predictions)
plot(1-ranger.test.roc$specificities, ranger.test.roc$sensitivities, col="red", pch=16,
                   xlab="False Positive", 
                   ylab="Sensitivity")

#rpart.plot(mod2)


defenders_data$pred <- predict(ranger_model,data = defenders_data)$predictions

## Sample Logistic Regression
logistic_mod <- glm(within_dist_ofBC_frames_ahead ~ dist_to_ball_carrier*min_proj_dist_to_ball_carrier +
                      TotDistFromBall_Rank_OVR + NumberOfBlockers + min_proj_dist_to_ball_carrier*BlockedScore, 
                    data = final_merged_data_sub, family = 'binomial')
summary(logistic_mod)

logistic_train_preds <- predict(logistic_mod, data = train_data, type ='response')
logistic_test_preds <- predict(logistic_mod, newdata = test_data, type ='response')

train_error_logistic <- data.frame(true_outcome = train_data$within_dist_ofBC_frames_ahead, train_prob = logistic_train_preds) 
train_error_logistic <- train_error_logistic %>%
  mutate(prediction = ifelse(train_prob >= 0.5, 1, 0),
         match = ifelse(prediction==true_outcome, 1, 0))
mean(train_error_logistic$true_outcome != train_error_logistic$prediction) #training error logistic

test_error_logistic <- data.frame(true_outcome = test_data$within_dist_ofBC_frames_ahead, test_prob = logistic_test_preds) 
test_error_logistic <- test_error_logistic %>%
  mutate(prediction = ifelse(test_prob >= 0.5, 1, 0),
         match = ifelse(prediction==true_outcome, 1, 0))
mean(test_error_logistic$true_outcome != test_error_logistic$prediction) #training error

defenders_data$pred_logistic <- predict(logistic_mod, newdata = defenders_data, type = 'response')

modeling_data <- modeling_data %>%
  left_join(defenders_data %>% select(pred, pred_logistic, gameId, playId, nflId, frameId), by = c("gameId", "playId", "nflId", 'frameId'))

#rm(defenders_data)

mckenzie_catch <- modeling_data %>%
  filter(gameId==2022090800 & playId==617)

singletary_run <- modeling_data %>%
  filter(gameId==2022090800 & playId==101)

##Good code to check:
View(singletary_run %>%
       filter(club==defensiveTeam) %>%
       arrange(displayName, frameId) %>%
       select(frameId, club, displayName, Player_Role, event,  dist_to_ball_carrier, within_dist_ofBC, within_dist_ofBC_frames_ahead, within_lead1:within_lead5, within_dist_ofBC_frames_ahead))

plotly::ggplotly(
  singletary_run %>%
    filter(frameId >= unique(frameId[which(event=='handoff')]), frameId <= unique(frameId[which(event=='first_contact')])) %>%
    # filter(frameId >= 38, frameId < 45) %>%
    ggplot(aes(x = x, y = y, 
               text = paste0('Ranger Prediction: ', pred,
                             'Logistic Prediction: ', pred_logistic, '\n', 
                             'Player Name: ', displayName, '\n',
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

plot(defenders_data$pred_logistic, defenders_data$pred)

#ranger:
MLmetrics::LogLoss(test_error$test_preds, test_error$true_outcome)
#logistic:
MLmetrics::LogLoss(test_error_logistic$test_prob, test_error_logistic$true_outcome)

#d <- AmesHousing::make_ames()
#
# nt <- seq(1, 501, 10)
# 
# oob_mse <- vector("numeric", length(nt))
# 
# for(i in 1:length(nt)){
#   rf <- ranger(within_dist_ofBC_frames_ahead ~ 
#                  dist_to_ball_carrier + 
#                  min_proj_dist_to_ball_carrier + 
#                  NumberOfBlockers + BlockedScore,
#                data = defenders_data, num.trees = nt[i], write.forest = FALSE)
#   oob_mse[i] <- rf$prediction.error
#   print(i)
# }
# 
# 
# plot(x = nt, y = oob_mse, col = "red", type = "l")
# 
# summary(mod2)
# v<-as.vector(mod2$variable.importance$Importance)
# w<-(as.vector((row.names(df))))
# DF<-cbind(w,v)
# DF<-as.data.frame(DF)
#  DF

##line charts
library(viridis)
singletary_run %>%
  filter(dist_to_ball_carrier<=10 & club==defensiveTeam) %>%
  mutate(time_in_secs = frameId*0.1) %>%
  ggplot(aes(x=time_in_secs, y = pred, group=displayName, color=displayName)) +
  geom_line() +
  geom_point() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Player's Probability of Being within 1 Yard of Ball Carrier \n
          Five Frames into the Future, via Random Forest Model") +
  #theme_ipsum() +
  ylab("Probability") +
  transition_reveal(time_in_secs)

singletary_run %>%
  filter(dist_to_ball_carrier<=10 & club==defensiveTeam) %>%
  mutate(time_in_secs = frameId*0.1) %>%
  ggplot(aes(x=time_in_secs, y = pred_logistic, group=displayName, color=displayName)) +
  geom_line() +
  geom_point() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Player's Probability of Being within 1 Yard of Ball Carrier \n
          Five Frames into the Future, via Logistic Regression Model") +
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
xmax <- 53.3
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

FixingBlockingScore <- function(DF) {
  DFTemp <- DF[DF$BlockedScore != 0 & DF$BlockedScore != 25 & DF$a >= 1, ]
  DFTemp2 <- DF[DF$BlockedScore != 0 & DF$BlockedScore != 25 & DF$a < 1, ]
  
  DFTemp2$BlockedScore <- DFTemp2$BlockedScore * DFTemp2$s
  DFTemp$BlockedScore <- (DFTemp$BlockedScore / DFTemp$a) * DFTemp$s
  
  DFTempWhole <- rbind(DFTemp, DFTemp2)
  DF <- merge(DF, DFTempWhole, by = names(DFTempWhole), all.x = TRUE)
  
  DF$BlockedScore[DF$BlockedScore == 25] <- NA
  
  return(DF)
}

modeling_data$blocking_Score_new <- FixingBlockingScore(modeling_data)

# 
# def FixingBlockingScore(DF):
#   DFTemp=DF.loc[(DF.BlockedScore!=0) & (DF.BlockedScore!=25)&(DF.a>=1)]
# DFTemp2=DF.loc[(DF.BlockedScore!=0) & (DF.BlockedScore!=25)&(DF.a<1)]
# DFTemp2.BlockedScore=DFTemp2.BlockedScore*DFTemp2.s
# DFTemp.BlockedScore=(DFTemp.BlockedScore/DFTemp.a)*DFTemp.s
# DFTempWhole=pd.concat([DFTemp,DFTemp2],ignore_index=True)
# DF=DF.merge(DFTempWhole, on=list(DFTempWhole.columns),how='left')
# DF.BlockedScore.replace(25,np.nan,inplace=True)
# return DF


# Code for the Aaron Jones/Jonathan Allen play
# This would require you to load in Week 7 data rather than just Week 1
handoff <- jones_run %>%
  filter(event=='handoff') %>%
  distinct(frameId) %>%
  pull()

first_contact <- jones_run %>%
  filter(event=='first_contact') %>%
  distinct(frameId) %>%
  pull()

tackle <- jones_run %>%
  filter(event=='tackle') %>%
  distinct(frameId) %>%
  pull()
 
##line charts
library(viridis)
jones_run %>%
  filter(dist_to_ball_carrier<=10 & club==defensiveTeam) %>%
  mutate(time_in_secs = frameId*0.1,
         name_num = paste0(displayName, ' - ', jerseyNumber)) %>%
  rename(`Player Name/Number` = name_num) %>%
  ggplot( aes(x=time_in_secs, y=pred_logistic, group=displayName, color=`Player Name/Number`)) +
  geom_line() +
  geom_point() +
  scale_color_brewer() +
  ggtitle("Probability of Being Within 1 Yard of Ball Carrier \n
          in the Next Half-Second") +
  #theme_ipsum() +
  ylab("Probability") +
  transition_reveal(time_in_secs) +
  theme_bw() +
  geom_vline(aes(xintercept = handoff*0.1), linetype='dashed', color = 'dodgerblue') +
  geom_vline(aes(xintercept = first_contact*0.1), linetype='dashed', color = 'green') +
  geom_vline(aes(xintercept = tackle*0.1), linetype='dashed', color = 'red') +
  geom_text(aes(x = 1, y = 0.85, label = 'Handoff'))+
  geom_text(aes(x = 2.25, y = 0.85, label = 'First Contact')) +
  geom_text(aes(x = 7.4, y = 0.85, label = 'Tackle')) +
  labs(x = 'Time in Seconds')

# Code for a Nick Chubb example
# This would require loading in Week 3 data rather than just Week 1
chubb_run <- modeling_data %>%
  filter(gameId==2022092200 & playId==84)

##Good code to check:
View(chubb_run %>%
       filter(club==defensiveTeam) %>%
       arrange(displayName, frameId) %>%
       select(frameId, club, displayName, Player_Role, event,  dist_to_ball_carrier, within_dist_ofBC, within_dist_ofBC_frames_ahead, within_lead1:within_lead5, within_dist_ofBC_frames_ahead2))

plotly::ggplotly(
  chubb_run %>%
    filter(frameId >= unique(frameId[which(event=='handoff')]), frameId <= unique(frameId[which(event=='first_contact')])) %>%
    # filter(frameId>=38, frameId<45) %>%
    ggplot(aes(x = x, y = y, 
               text = paste0('Ranger Prediction: ', pred,
                             'Logistic Prediction: ', pred_logistic, '\n', 
                             'Player Name: ', displayName, '\n',
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
    geom_segment(aes(x = x, y = y, xend = X_proj_5,
                     yend = Y_proj_5, color = Player_Role)) +
    scale_color_manual(values = c("Ball Carrier" = "black", 
                                  "Offense" = "red",
                                  "Defense" = "blue",
                                  "Football" = "brown")) +
    theme_bw() +
    labs(x = "X (High X = Where Offense Is Aiming)", y = "Y (High Y = Offense's Left)", 
         title = "Frame-By-Frame Diagram of N. Chubb Rush (From Handoff to First Contact)") +
    geom_hline(yintercept = 0, color = 'darkgreen', linetype = 'dashed') +
    geom_hline(yintercept = 53.3, color = 'darkgreen', linetype = 'dashed') +
    facet_wrap(~frameId) +
    theme(plot.title = element_text(size = 10, hjust = 0.5))
)

plot(defenders_data$pred_logistic, defenders_data$pred)

#ranger:
MLmetrics::LogLoss(test_error$test_preds, test_error$true_outcome)
#logistic:
MLmetrics::LogLoss(test_error_logistic$test_prob, test_error_logistic$true_outcome)

MLmetrics::LogLoss(train_error$train_prob, train_error$true_outcome)
MLmetrics::LogLoss(train_error_logistic$train_prob, train_error_logistic$true_outcome)


#d <- AmesHousing::make_ames()
#
# nt <- seq(1, 501, 10)
# 
# oob_mse <- vector("numeric", length(nt))
# 
# for(i in 1:length(nt)){
#   rf <- ranger(within_dist_ofBC_frames_ahead2 ~ 
#                  dist_to_ball_carrier + 
#                  min_proj_dist_to_ball_carrier + 
#                  NumberOfBlockers + BlockedScore,
#                data = defenders_data, num.trees = nt[i], write.forest = FALSE)
#   oob_mse[i] <- rf$prediction.error
#   print(i)
# }
# 
# 
# plot(x = nt, y = oob_mse, col = "red", type = "l")
# 
# summary(mod2)
# v<-as.vector(mod2$variable.importance$Importance)
# w<-(as.vector((row.names(df))))
# DF<-cbind(w,v)
# DF<-as.data.frame(DF)
#  DF

##line charts
library(viridis)
chubb_run %>%
  filter(dist_to_ball_carrier<=10 & club==defensiveTeam) %>%
  mutate(time_in_secs = frameId*0.1) %>%
  ggplot( aes(x=time_in_secs, y = pred, group=displayName, color=displayName)) +
  geom_line() +
  geom_point() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Player's Probability of Being within 1 Yard of Ball Carrier \n
          Five Frames into the Future, via Random Forest Model") +
  #theme_ipsum() +
  ylab("Probability") +
  transition_reveal(time_in_secs)

chubb_run %>%
  filter(dist_to_ball_carrier <= 10 & club == defensiveTeam) %>%
  mutate(time_in_secs = frameId*0.1) %>%
  ggplot( aes(x=time_in_secs, y = pred_logistic, group=displayName, color=displayName)) +
  geom_line() +
  geom_point() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Player's Probability of Being within 1 Yard of Ball Carrier \n
          Five Frames into the Future, via Logistic Regression Model") +
  #theme_ipsum() +
  ylab("Probability") +
  transition_reveal(time_in_secs)

# Here's the code for the all-22 moving dots of the Aaron Jones rush
#Sample plays and corresponding visualizations
set.seed(1)

# This would be replaced by whichever play you want to look into (Jones rush in our case)
example_play <- modeling_data %>%
  sample_n(1)

example_game <- example_play$gameId
example_play <- example_play$playId
# games <- read_csv("games.csv")
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
xmax <- 53.3
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
  scale_fill_manual(values = c("red", 'blue', "black"), guide=T) +
  guides(fill = guide_legend(override.aes=list(shape=21))) +
  #scale_colour_manual(values = c("red", 'blue', "pink"), guide = FALSE) +
 
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
                                      fill = Player_Role,
                                      group = Player_Role,
                                      size = club),
             alpha = 0.7) +  
  #ggforce::geom_circle(data = example_play, aes(x0=X_ball_carrier, y0=Y_ball_carrier, r=10)) +
 
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
  labs(title = plot_title,
       fill = 'Player Role') +
 
  #setting animation parameters
  transition_time(frameId)  +
  ease_aes('linear') +
  NULL



