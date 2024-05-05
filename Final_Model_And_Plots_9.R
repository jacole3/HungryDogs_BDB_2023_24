# Each of these CSVs were built from the "full_modeling_code" file
# I.e. that file was one-week sample of how we built the model, this file combines all weeks for the final product
week_1 <- read_csv('Final Week 1.csv')
week_2 <- read_csv('Final Week 2.csv')
week_3 <- read_csv('Final Week 3.csv')
week_4 <- read_csv('Final Week 4.csv')
week_5 <- read_csv('Final Week 5.csv')
week_6 <- read_csv('Final Week 6.csv')
week_7 <- read_csv('Final Week 7.csv')
week_8 <- read_csv('Final Week 8.csv')
week_9 <- read_csv('Final Week 9.csv')

final_merged_data <- bind_rows(week_1, week_2, week_3, week_4, week_5, week_6, week_7, week_8, week_9)

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

modeling_data <- final_merged_data_sub %>%
  mutate(game_play_combination = paste0(gameId, '-', playId)) %>% 
  group_by(gameId, playId) %>%
  filter(frameId!=max(frameId)) %>%
  mutate(within_lead1 = lead(within_dist_ofBC, 1),
         within_lead2 = lead(within_dist_ofBC, 2),
         within_lead3 = lead(within_dist_ofBC, 3),
         within_lead4 = lead(within_dist_ofBC, 4),
         within_lead5 = lead(within_dist_ofBC, 5)) %>%
  mutate(within_lead1 = ifelse(is.na(within_lead1), 0, within_lead1),
         within_lead2 = ifelse(is.na(within_lead2), 0, within_lead2),
         within_lead3 = ifelse(is.na(within_lead3), 0, within_lead3),
         within_lead4 = ifelse(is.na(within_lead4), 0, within_lead4),
         within_lead5 = ifelse(is.na(within_lead5), 0, within_lead5)) %>%
  mutate(within_dist_ofBC_frames_ahead = ifelse((within_lead1 + within_lead2 + within_lead3 + within_lead4 + within_lead5)>=1, 1, 0)) %>%
  ungroup()

modeling_data <- modeling_data %>% filter(!is.na(BlockedScore))

set.seed(4444)
unique_plays <- unique(modeling_data$game_play_combination)
train_indices <- sample(1:length(unique_plays), 0.8*length(unique_plays))
train_plays <- unique_plays[train_indices]
test_plays <- unique_plays[-train_indices]

train_data <- modeling_data %>%
  filter(game_play_combination %in% train_plays)

test_data <- modeling_data %>%
  filter(game_play_combination %in% test_plays)

# Model to predict whether a defender gets within the one-yard radius in the next five frames
# We tested dozens of models, including logistic regression and random forest
# Other models we tested can be seen in the "RandomFor_vs_Logit" and "full_modeling"code GitHub files
mod_logistic_train <- glm(within_dist_ofBC_frames_ahead ~ dist_to_ball_carrier*min_proj_dist_to_ball_carrier +
                            TotDistFromBall_Rank_OVR + NumberOfBlockers + min_proj_dist_to_ball_carrier*BlockedScore, 
                    data = train_data, family = 'binomial')
summary(mod_logistic_train)

logistic_train_preds <- predict(mod_logistic_train, data = train_data, type ='response')
logistic_test_preds <- predict(mod_logistic_train, newdata = test_data, type ='response')

train_error_logistic <- data.frame(true_outcome = train_data$within_dist_ofBC_frames_ahead, train_prob = logistic_train_preds) 
train_error_logistic <- train_error_logistic %>%
  mutate(prediction = ifelse(train_prob >= 0.5, 1, 0),
         match = ifelse(prediction==true_outcome, 1, 0))
mean(train_error_logistic$true_outcome != train_error_logistic$prediction) #training error logistic

test_error_logistic <- data.frame(true_outcome = test_data$within_dist_ofBC_frames_ahead, test_prob = logistic_test_preds) 
test_error_logistic <- test_error_logistic %>%
  mutate(prediction = ifelse(test_prob >= 0.5, 1, 0),
         match = ifelse(prediction==true_outcome, 1, 0))
mean(test_error_logistic$true_outcome != test_error_logistic$prediction) # test error logistic

MLmetrics::LogLoss(test_error_logistic$test_prob, test_error_logistic$true_outcome)

modeling_data$pred_logistic <- predict(mod_logistic_train,newdata =  modeling_data, type = 'response')

final_merged_data <- final_merged_data %>%
  left_join(modeling_data %>% select(pred_logistic, gameId, playId, nflId, frameId), by = c("gameId", "playId", "nflId", 'frameId'))

example_play <- final_merged_data %>%
  filter(gameId==2022102306 & playId==1531)

library(viridis)
example_play %>%
  filter(dist_to_ball_carrier<=10 & club==defensiveTeam) %>%
  mutate(time_in_secs = frameId*0.1) %>%
  ggplot( aes(x=time_in_secs, y=pred_logistic, group=displayName, color=displayName)) +
  geom_line() +
  geom_point() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Probability of Being Within 1 Yard of Ball Carrier \n
          in the Next Half Second") +
  #theme_ipsum() +
  xlab("Time in Seconds")+
  ylab("Probability") +
  transition_reveal(time_in_secs)


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
  scale_fill_manual(values = c("red", 'blue', "black"), guide=T) + 
  guides(fill=guide_legend(override.aes=list(shape=21))) +
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

handoff <- example_play %>%
  filter(event=='handoff') %>%
  distinct(frameId) %>%
  pull()

first_contact <- example_play %>%
  filter(event=='first_contact') %>%
  distinct(frameId) %>%
  pull()

tackle <- example_play %>%
  filter(event=='tackle') %>%
  distinct(frameId) %>%
  pull()

##line charts
library(viridis)
example_play %>%
  filter(dist_to_ball_carrier<=10 & club==defensiveTeam) %>%
  mutate(time_in_secs = frameId*0.1,
         name_num = paste0(displayName, ' - ', jerseyNumber)) %>%
  rename(`Player Name/Number` = name_num) %>%
  ggplot( aes(x=time_in_secs, y=pred_logistic, group=displayName, color=`Player Name/Number`)) +
  geom_line() +
  geom_point() +
  scale_color_viridis(discrete = T) +
  ggtitle("Probability of Being Within 1 Yard of Ball Carrier \n
          in the Next Half Second") +
  #theme_ipsum() +
  ylab("Probability") +
  transition_reveal(time_in_secs) +
  theme_bw() +
  geom_vline(aes(xintercept = handoff*0.1), linetype='dashed', color = 'black') +
  geom_vline(aes(xintercept = first_contact*0.1), linetype='dashed', color = 'black') +
  geom_vline(aes(xintercept = tackle*0.1), linetype='dashed', color = 'black') +
  geom_text(aes(x = 1, y = 0.6, label = 'Handoff'), color = 'black')+
  geom_text(aes(x = 3.5, y = 0.85, label = 'First Contact'), color = 'black') +
  #geom_text(aes(x = 7.4, y = 0.85, label = 'Tackle')) +
  labs(x = 'Time in Seconds')


 #---------------------------- Cole's model code
final_merged_data_sub$pred_within_dist_ofBC_logistic <- predict(mod_logistic, final_merged_data_sub, type = 'response')
final_merged_data_sub <- final_merged_data_sub %>%
  mutate(pred_near_BC_error_logistic = within_dist_ofBC_frames_ahead - pred_within_dist_ofBC_logistic)

final_merged_data_sub <- final_merged_data_sub %>% mutate(FrameNumber_FiveBeforeDefSurge =
                                                            ifelse(FirstDefSurge_Frame - FirstFrame_WithBC >= 5, FirstDefSurge_Frame - 5, FirstFrame_WithBC))

Max_NearBC_FiveFramesEarly_DF <- final_merged_data_sub %>% filter(frameId <= FrameNumber_FiveBeforeDefSurge) %>%
  group_by(gameId, playId, nflId) %>%
  mutate(NearBC_rank = rank(-pred_within_dist_ofBC_logistic, ties.method = "first")) %>%
  ungroup()
Max_NearBC_FiveFramesEarly_DF <- Max_NearBC_FiveFramesEarly_DF %>% 
  select(c("gameId", "playId", "nflId", "displayName", "frameId", "pred_within_dist_ofBC_logistic", "NearBC_rank"))
Max_NearBC_FiveFramesEarly_DF <- Max_NearBC_FiveFramesEarly_DF %>% filter(NearBC_rank == 1)
Max_NearBC_FiveFramesEarly_DF <- Max_NearBC_FiveFramesEarly_DF %>%
  rename(max_pred_near_BC_FiveFramesEarly = pred_within_dist_ofBC_logistic)
Max_NearBC_FiveFramesEarly_DF <- Max_NearBC_FiveFramesEarly_DF %>%
  select(-"NearBC_rank")

final_merged_data_sub <- final_merged_data_sub %>%
  left_join(Max_NearBC_FiveFramesEarly_DF, by = c("gameId", "playId", "nflId", "displayName", "frameId"))

# This would be for tackle probability, if we use the logistic model there
TackleProb_FiveFramesEarly_DF <- final_merged_data_sub %>% filter(frameId <= FrameNumber_FiveBeforeEndOfPlay) %>%
  group_by(gameId, playId, nflId) %>%
  mutate(PredTackle_rank = rank(-pred_tackle_logistic, ties.method = "first")) %>%
  ungroup()
TackleProb_FiveFramesEarly_DF <- TackleProb_FiveFramesEarly_DF %>% 
  select(c("gameId", "playId", "nflId", "displayName", "frameId", "pred_tackle_logistic", "PredTackle_rank"))
TackleProb_FiveFramesEarly_DF <- TackleProb_FiveFramesEarly_DF %>% filter(PredTackle_rank == 1)
TackleProb_FiveFramesEarly_DF <- TackleProb_FiveFramesEarly_DF %>%
  rename(max_pred_tackle_FiveFramesEarly = pred_tackle_logistic)
TackleProb_FiveFramesEarly_DF <- TackleProb_FiveFramesEarly_DF %>%
  select(-"PredTackle_rank")

final_merged_data_sub <- final_merged_data_sub %>%
  left_join(TackleProb_FiveFramesEarly_DF, by = c("gameId", "playId", "nflId", "displayName", "frameId"))
rm(Max_NearBC_FiveFramesEarly_DF, TackleProb_FiveFramesEarly_DF)

# Now, merge final_merged_data_sub with final_merged_data again
# I.e., the "sub" version was only needed during modeling, now that's done
final_merged_data_sub <- final_merged_data_sub %>% 
  select("gameId", "playId", "nflId", "displayName", "frameId",
         "pred_within_dist_ofBC_logistic", "pred_tackle_logistic", 
         "max_pred_near_BC_FiveFramesEarly", "max_pred_tackle_FiveFramesEarly")
final_merged_data <- merge(x = final_merged_data, y = final_merged_data_sub,
                           by = c("gameId", "playId", "nflId", "displayName", "frameId"), all.x = TRUE)
rm(final_merged_data_sub)
