# All of this starts off where we left off after the "full_modeling_code" file, where we defined final_merged_data_sub

# First, get each player's "maximum" probabilities, excluding the final five frames before play ended
# Premise: if player X makes tackle on frame 40, tackle probability will inherently be high on frame 39
# First, start with "surge" probability (i.e. getting near ball-carrier)
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

# But, we can still remove some columns from final_merged_data
# This is only really needed to make the pre-snap alignment code go smoothly for designed runs
# DO NOT run this section if you plan to analyze completions or scrambles independently
colnames(final_merged_data)
final_merged_data <- final_merged_data %>%
  select(-c("roof", "surface", "wind", "Ball_DistFromGoalLine", "Ball_DistFromSideline",
            "BallNearGoalLine", "BallNearSideline", "Season_MaxSpeed", "Temperature", 
            "offenseFormation", "Y_AbsDistFromBall_Rank_BySide",
            "X_AbsDistFromBall_Rank_BySide", "Y_NetDistFromBall_Rank_BySide",
            "X_NetDistFromBall_Rank_BySide", "Y_AbsDistFromBall_Rank_OVR",
            "X_AbsDistFromBall_Rank_OVR", "Y_NetDistFromBall_Rank_OVR", 
            "X_NetDistFromBall_Rank_OVR", "TeamTackle_FiveFramesAhead",       
            "IndivSoloTackle_FiveFramesAhead", "IndivAssist_FiveFramesAhead",      
            "IndivTotTackles_FiveFramesAhead", "TeamTackle_Within0.5Sec",          
            "IndivSoloTackle_Within0.5Sec", "IndivAssist_Within0.5Sec",         
            "IndivTotTackles_Within0.5Sec", "TeamTackle_CurrentFrame",          
            "IndivSoloTackle_CurrentFrame", "IndivAssist_CurrentFrame",         
            "IndivTotTackles_CurrentFrame", "X_ball_carrier", "Y_ball_carrier",                  
            "dist_to_ball_carrier", "ball_carrier_speed", "ball_carrier_acc", 
            "ball_carrier_dist_ran", "ball_carrier_orient", "ball_carrier_direction",           
            "ball_carrier_weight", "ball_carrier_height", "BC_Season_MaxSpeed", "ball_carrier_momentum",            
            "Rel_Speed_ToBC", "Rel_Acc_ToBC", "Rel_Weight_ToBC", "Rel_Height_ToBC",                  
            "Rel_SeasonMaxSpeed_ToBC", "Momentum","Rel_Momentum_ToBC", "CosSimilarity_Orient_ToBC",        
            "CosSimilarity_Dir_ToBC", "Rel_Velocity_ToBC", "X_proj_1", "X_proj_2", "X_proj_3", "X_proj_4",                         
            "X_proj_5", "Y_proj_1", "Y_proj_2", "Y_proj_3", "Y_proj_4", "Y_proj_5",                         
            "ball_carrier_X_proj_1", "ball_carrier_X_proj_2", "ball_carrier_X_proj_3", 
            "ball_carrier_X_proj_4", "ball_carrier_X_proj_5", "ball_carrier_Y_proj_1",            
            "ball_carrier_Y_proj_2", "ball_carrier_Y_proj_3", "ball_carrier_Y_proj_4",
            "ball_carrier_Y_proj_5", "In_BallCarrier_Radius", "NumberOfBlockers",
            "BlockedScore", "TeamDefSurge_InFrame", "FirstDefSurge_Frame",
            "min_proj_dist_to_ball_carrier", "min_dist_opp_player", "num_opp_players_same_dist",
            "min_dist_opp_index", "second_closest_dist_opp_player", 
            "second_closest_opp_index", "closest_opp_player_name", "closest_opp_player_nflID",  
            "second_closest_opp_player_name", "second_closest_opp_player_nflID",  
            "dir_of_closest_opp_player", "dir_of_second_closest_opp_player",
            "homeTeamAbbr", "visitorTeamAbbr", "possessionTeam",
            "defensiveTeam", "sp", "foulName1", "foulName2", "foulNFLId1", "foulNFLId2",
            "td_prob", "safety_prob", "air_epa", "yac_epa", "comp_air_epa", "comp_yac_epa", "wp",                               
            "def_wp", "air_wpa", "yac_wpa", "comp_air_wpa", "comp_yac_wpa", "xyac_mean_yardage",
            "xyac_epa", "xyac_median_yardage", "xyac_success", "xyac_fd", "tackle_for_loss_1_player_id",
            "tackle_for_loss_2_player_id", "tackle_for_loss_1_player_name", "tackle_for_loss_2_player_name",
            "week", "penaltyYards", "prePenaltyPlayResult", "playNullifiedByPenalty",
            "air_yards", "yards_after_catch", "return_touchdown", "penalty_team",
            "penalty_type", "jerseyNumber", "weight", "height_inches",
            "YACOE", "YAC_EPA_OE", "YAC_Success_OE", "YAC_FD_OE", "FrameNumber_EndOfPlay",
            "FrameNumber_FiveBeforeEndOfPlay"))

# Re-split the play types, now that we have the model predictions
DesignedRuns_Merged <- final_merged_data %>% filter(pass == 0)
# Scrambles_Merged <- final_merged_data %>% filter(passResult == "R")
# AllRushes_Merged <- final_merged_data %>% filter(pass == 0 | passResult == "R")
# Completions_Merged <- final_merged_data %>% filter(passResult == "C")

# Pre-snap alignment code has to be for designed runs separately
# This is because we don't have "start of snap" data for the dropbacks

# Reminder from data cleansing: highest Y value is to offense's left
# Highest X value is end zone offense is aiming at
# So the offensive players lined up on LOS will have highest X

# For starters, get rid of X_LOS_Approx (no need since these plays begin at snap of ball)
DesignedRuns_Merged <- DesignedRuns_Merged %>% select(-c("X_LOS_Approx", "X_distFromLOS_Approx"))
# Recall that X_dist_FromBallFrame1 already exists, which is equivalent to X distance from LOS

# Add "x" pre-snap rank for each side of ball (e.g. who is closest to opposing end zone)
DesignedRuns_AtSnap <- DesignedRuns_Merged %>%
  group_by(gameId, playId, PlayerSideOfBall) %>%
  filter(event %in% c("ball_snap", "snap_direct")) %>% 
  mutate(X_PreSnap_Rank = rank(-x, ties.method = "first")) %>%
  ungroup() %>%
  select(gameId, playId, nflId, displayName, PlayerSideOfBall, X_PreSnap_Rank, 
         PreSnap_x = x, PreSnap_y = y)
# table(FirstFrameOfPlay_DesignedRuns$X_PreSnap_Rank); rank is never higher than 11

# Note that the highest "x" rank pre-snap is NOT always the center
# View(DesignedRuns_AtSnap %>% filter(X_PreSnap_Rank == 1, PlayerSideOfBall == "offense"))

DesignedRuns_Merged <- DesignedRuns_Merged %>%
  merge(DesignedRuns_AtSnap, by = c("gameId", "playId", "nflId", "displayName", "PlayerSideOfBall"))
DesignedRuns_Merged <- DesignedRuns_Merged %>% arrange(gameId, playId, nflId, frameId)
rm(DesignedRuns_AtSnap)

# Now rank the "y" column for OL specifically
# Both in terms of highest y (e.g. farthest left) & smallest y dist from ball (who is snapping)
# This helps us adjust for two players being listed as C (e.g. BUF Mitch Morse, Ryan Bates)
DesignedRuns_Merged <- DesignedRuns_Merged %>% mutate(OL_Label =
                          ifelse(PlayerSideOfBall == "offense" & PosGroup %in% "OL", "OL",
                               ifelse(PlayerSideOfBall == "offense" & !PosGroup %in% "OL", "Offense_Skill", "Defense")))
table(DesignedRuns_Merged$OL_Label)

DesignedRuns_By_OL_Label <- DesignedRuns_Merged %>%
  group_by(gameId, playId, OL_Label) %>%
  filter(event %in% c("ball_snap", "snap_direct")) %>% 
  mutate(Y_PreSnap_Rank_By_OL_Label = rank(-y, ties.method = "first"),
         Y_PreSnap_Rank_ClosestOL_ToBall = rank(abs(Y_dist_FromBallFrame1), ties.method = "first"),
         DistFromBall_PreSnap_Rank_By_OL = rank(TotDistFromBall, ties.method = "first")) %>%
  ungroup() %>%
  select(gameId, playId, nflId, displayName, PlayerSideOfBall, position, 
         OL_Label, Y_PreSnap_Rank_By_OL_Label, Y_PreSnap_Rank_ClosestOL_ToBall,
         DistFromBall_PreSnap_Rank_By_OL)

# Note that ranking by "tot dist from ball" actually doesn't always get centers
# View(DesignedRuns_By_OL_Label %>% filter(OL_Label == "OL", DistFromBall_PreSnap_Rank_By_OL == 1))
# I.e. most accurate way is to use strictly the "Y dist from ball"

DesignedRuns_Merged <- merge(x = DesignedRuns_Merged, y = DesignedRuns_By_OL_Label, 
                             by = c("gameId", "playId", "nflId", "displayName", "position", "OL_Label", "PlayerSideOfBall"),
                             all.x = TRUE)
DesignedRuns_Merged <- DesignedRuns_Merged %>% arrange(gameId, playId, nflId, frameId)
rm(DesignedRuns_By_OL_Label)

# From there, use center to retroactively define other OL spots
DesignedRuns_Merged <- DesignedRuns_Merged %>% mutate(AlignedPos_Box =
       ifelse(OL_Label == "OL" & Y_PreSnap_Rank_ClosestOL_ToBall == 1, "C", NA))

# Now, we want it where "if Y rank is one ahead of center's Y rank, then LG"
# "If Y rank is two ahead of center's Y rank, then LT", etc.
# Will have to group_by() playId to do so
DesignedRuns_Center_PreSnap <- DesignedRuns_Merged %>%
  group_by(gameId, playId, OL_Label) %>%
  filter(event %in% c("ball_snap", "snap_direct")) %>% 
  mutate(Y_PreSnap_Rank_By_OL_Label = rank(-y, ties.method = "first")) %>%
  ungroup() %>%
  select(gameId, playId, nflId, displayName, PlayerSideOfBall, 
         C_PreSnap_YRank_AmongOL = Y_PreSnap_Rank_By_OL_Label, AlignedPos_Box) %>%
  filter(AlignedPos_Box == "C")

# Now get rid of the unnecessary columns before we merge
DesignedRuns_Center_PreSnap <- DesignedRuns_Center_PreSnap %>%
  select(-c("nflId", "displayName", "PlayerSideOfBall", "AlignedPos_Box"))

DesignedRuns_Merged <- DesignedRuns_Merged %>%
  merge(DesignedRuns_Center_PreSnap, by = c("gameId", "playId"))
DesignedRuns_Merged <- DesignedRuns_Merged %>% arrange(gameId, playId, nflId, frameId)
rm(DesignedRuns_Center_PreSnap)

# Now, we can compare each player's pre-snap Y rank to the center's Y rank, to define positions
# I.e., it's not as simple as "center always has Y rank of 3", b/c some plays have 6+ OL
# View(DesignedRuns_Merged %>% filter(event == "ball_snap", playId == 3379, PlayerSideOfBall == "offense"))
# This is example of unbalanced formation - Drew Dalman is C, three OL are to his left
DesignedRuns_Merged <- DesignedRuns_Merged %>% mutate(AlignedPos_Box =
    ifelse(OL_Label == "OL" & (Y_PreSnap_Rank_By_OL_Label - C_PreSnap_YRank_AmongOL == (-1)), "LG", AlignedPos_Box))
DesignedRuns_Merged <- DesignedRuns_Merged %>% mutate(AlignedPos_Box =
    ifelse(OL_Label == "OL" & (Y_PreSnap_Rank_By_OL_Label - C_PreSnap_YRank_AmongOL == (-2)), "LT", AlignedPos_Box))
DesignedRuns_Merged <- DesignedRuns_Merged %>% mutate(AlignedPos_Box =
    ifelse(OL_Label == "OL" & (Y_PreSnap_Rank_By_OL_Label - C_PreSnap_YRank_AmongOL == 1), "RG", AlignedPos_Box))
DesignedRuns_Merged <- DesignedRuns_Merged %>% mutate(AlignedPos_Box =
    ifelse(OL_Label == "OL" & (Y_PreSnap_Rank_By_OL_Label - C_PreSnap_YRank_AmongOL == 2), "RT", AlignedPos_Box))
table(DesignedRuns_Merged$AlignedPos_Box)

# create a distance from LT column
LT_coordinates <- DesignedRuns_Merged %>%
  filter(AlignedPos_Box == "LT") %>%
  select(gameId, playId, nflId_LT = nflId, x, y, frameId) %>%
  rename(xLT = x,
         yLT = y)

# create a distance from LG column
LG_coordinates <- DesignedRuns_Merged %>%
  filter(AlignedPos_Box == "LG") %>%
  select(gameId, playId, nflId_LG = nflId, x, y, frameId) %>%
  rename(xLG = x,
         yLG = y)

# create a distance from RT column
RT_coordinates <- DesignedRuns_Merged %>%
  filter(AlignedPos_Box == "RT") %>%
  select(gameId, playId, nflId_RT = nflId, x, y, frameId) %>%
  rename(xRT = x,
         yRT = y)

# create a distance from RG column
RG_coordinates <- DesignedRuns_Merged %>%
  filter(AlignedPos_Box == "RG") %>%
  select(gameId, playId, nflId_RG = nflId, x, y, frameId) %>%
  rename(xRG = x,
         yRG = y)

# create a distance from Center column
C_coordinates <- DesignedRuns_Merged %>%
  filter(AlignedPos_Box == "C") %>%
  select(gameId, playId, nflId_C = nflId, x, y, frameId) %>%
  rename(xCenter = x,
         yCenter = y)

DesignedRuns_Merged <- DesignedRuns_Merged %>%
  left_join(LT_coordinates, by = c("playId", "gameId", "frameId")) %>%
  left_join(RT_coordinates, by = c("playId", "gameId", "frameId")) %>%
  left_join(RG_coordinates, by = c("playId", "gameId", "frameId")) %>%
  left_join(LG_coordinates, by = c("playId", "gameId", "frameId")) %>%
  left_join(C_coordinates, by = c("playId", "gameId", "frameId"))

# clean up environment
rm(C_coordinates, LT_coordinates, LG_coordinates, RT_coordinates, RG_coordinates)

# Define remaining LG/LT/RG/RT below, incorporating is.na() when needed
# E.G. LG would have is.na for AlignedPos_Box, and y barely higher than C's y, etc.
DesignedRuns_LG_Identify_Snap <- DesignedRuns_Merged %>%
  filter(PlayerSideOfBall == "offense", position != "QB", is.na(AlignedPos_Box),
         event %in% c("ball_snap", "snap_direct"), y > yCenter, y <= 1.5 + yCenter, x >= xCenter - 1) %>%
  mutate(Is_LG = TRUE) %>%
  select(gameId, playId, nflId, displayName, PlayerSideOfBall, Is_LG)

DesignedRuns_Merged <- DesignedRuns_Merged %>% left_join(DesignedRuns_LG_Identify_Snap, 
                                                         by = c("gameId", "playId", "nflId", "displayName", "PlayerSideOfBall"))
DesignedRuns_Merged <- DesignedRuns_Merged %>% arrange(gameId, playId, nflId, frameId)
DesignedRuns_Merged <- DesignedRuns_Merged %>% 
  mutate(Is_LG = ifelse(!is.na(Is_LG), TRUE, FALSE))

DesignedRuns_Merged <- DesignedRuns_Merged %>% mutate(AlignedPos_Box =
            ifelse(Is_LG == TRUE, "LG", AlignedPos_Box))

# Update the distance from LG column
LG_coordinates <- DesignedRuns_Merged %>%
  filter(AlignedPos_Box == "LG") %>%
  select(gameId, playId, nflId_LG = nflId, x, y, frameId) %>%
  rename(xLG = x,
         yLG = y)

DesignedRuns_Merged <- DesignedRuns_Merged %>%
  left_join(LG_coordinates, by = c("playId", "gameId", "frameId")) 
rm(LG_coordinates, DesignedRuns_LG_Identify_Snap)

# Use group_by to see if any play has multiple players listed at same position
LGNumbers_ByPlay <- DesignedRuns_Merged %>% group_by(gameId, playId) %>% 
  filter(AlignedPos_Box == "LG", event %in% c("ball_snap", "snap_direct")) %>% summarize(n = n()) %>% arrange(desc(n))
rm(LGNumbers_ByPlay)
table(DesignedRuns_Merged$AlignedPos_Box)

# And get rid of the new "extra" columns
DesignedRuns_Merged <- DesignedRuns_Merged %>%
  select(-c("nflId_LG.x", "xLG.x", "yLG.x"))
DesignedRuns_Merged <- DesignedRuns_Merged %>%
  rename(nflId_LG = `nflId_LG.y`, xLG = `xLG.y`, yLG = `yLG.y`)

# Repeat the process for LT
DesignedRuns_LT_Identify_Snap <- DesignedRuns_Merged %>%
  filter(PlayerSideOfBall == "offense", position != "QB", is.na(AlignedPos_Box),
         event %in% c("ball_snap", "snap_direct"), y > yLG, y <= 1.5 + yLG, x >= xCenter - 1) %>%
  mutate(Is_LT = TRUE) %>%
  select(gameId, playId, nflId, displayName, PlayerSideOfBall, Is_LT)

DesignedRuns_Merged <- DesignedRuns_Merged %>% left_join(DesignedRuns_LT_Identify_Snap, 
                                                         by = c("gameId", "playId", "nflId", "displayName", "PlayerSideOfBall"))
DesignedRuns_Merged <- DesignedRuns_Merged %>% arrange(gameId, playId, nflId, frameId)
DesignedRuns_Merged <- DesignedRuns_Merged %>% 
  mutate(Is_LT = ifelse(!is.na(Is_LT), TRUE, FALSE))

DesignedRuns_Merged <- DesignedRuns_Merged %>% mutate(AlignedPos_Box =
     ifelse(Is_LT == TRUE, "LT", AlignedPos_Box))

# Update the distance from LT column
LT_coordinates <- DesignedRuns_Merged %>%
  filter(AlignedPos_Box == "LT") %>%
  select(gameId, playId, nflId_LT = nflId, x, y, frameId) %>%
  rename(xLT = x,
         yLT = y)

DesignedRuns_Merged <- DesignedRuns_Merged %>%
  left_join(LT_coordinates, by = c("playId", "gameId", "frameId")) 
rm(LT_coordinates, DesignedRuns_LT_Identify_Snap)

# Use group_by to see if any play has multiple players listed at same position
LTNumbers_ByPlay <- DesignedRuns_Merged %>% group_by(gameId, playId) %>% 
  filter(AlignedPos_Box == "LT", event %in% c("ball_snap", "snap_direct")) %>% summarize(n = n()) %>% arrange(desc(n))
rm(LTNumbers_ByPlay)
table(DesignedRuns_Merged$AlignedPos_Box)

# And get rid of the new "extra" columns
DesignedRuns_Merged <- DesignedRuns_Merged %>%
  select(-c("nflId_LT.x", "xLT.x", "yLT.x"))
DesignedRuns_Merged <- DesignedRuns_Merged %>%
  rename(nflId_LT = `nflId_LT.y`, xLT = `xLT.y`, yLT = `yLT.y`)

# Repeat the process for RG
DesignedRuns_RG_Identify_Snap <- DesignedRuns_Merged %>%
  filter(PlayerSideOfBall == "offense", position != "QB", is.na(AlignedPos_Box),
         event %in% c("ball_snap", "snap_direct"), y < yCenter, y >= yCenter - 1.5, x >= xCenter - 1) %>%
  mutate(Is_RG = TRUE) %>%
  select(gameId, playId, nflId, displayName, PlayerSideOfBall, Is_RG)

DesignedRuns_Merged <- DesignedRuns_Merged %>% left_join(DesignedRuns_RG_Identify_Snap, 
                                                         by = c("gameId", "playId", "nflId", "displayName", "PlayerSideOfBall"))
DesignedRuns_Merged <- DesignedRuns_Merged %>% arrange(gameId, playId, nflId, frameId)
DesignedRuns_Merged <- DesignedRuns_Merged %>% 
  mutate(Is_RG = ifelse(!is.na(Is_RG), TRUE, FALSE))

DesignedRuns_Merged <- DesignedRuns_Merged %>% mutate(AlignedPos_Box =
         ifelse(Is_RG == TRUE, "RG", AlignedPos_Box))

# Update the distance from RG column
RG_coordinates <- DesignedRuns_Merged %>%
  filter(AlignedPos_Box == "RG") %>%
  select(gameId, playId, nflId_RG = nflId, x, y, frameId) %>%
  rename(xRG = x,
         yRG = y)

DesignedRuns_Merged <- DesignedRuns_Merged %>%
  left_join(RG_coordinates, by = c("playId", "gameId", "frameId")) 
rm(RG_coordinates, DesignedRuns_RG_Identify_Snap)

# Use group_by to see if any play has multiple players listed at same position
RGNumbers_ByPlay <- DesignedRuns_Merged %>% group_by(gameId, playId) %>% 
  filter(AlignedPos_Box == "RG", event %in% c("ball_snap", "snap_direct")) %>% summarize(n = n()) %>% arrange(desc(n))
rm(RGNumbers_ByPlay)
table(DesignedRuns_Merged$AlignedPos_Box)

# And get rid of the new "extra" columns
DesignedRuns_Merged <- DesignedRuns_Merged %>%
  select(-c("nflId_RG.x", "xRG.x", "yRG.x"))
DesignedRuns_Merged <- DesignedRuns_Merged %>%
  rename(nflId_RG = `nflId_RG.y`, xRG = `xRG.y`, yRG = `yRG.y`)

# Repeat the process for RT
DesignedRuns_RT_Identify_Snap <- DesignedRuns_Merged %>%
  filter(PlayerSideOfBall == "offense", position != "QB", is.na(AlignedPos_Box),
         event %in% c("ball_snap", "snap_direct"), y < yRG, y >= yRG - 1.5, x >= xCenter - 1) %>%
  mutate(Is_RT = TRUE) %>%
  select(gameId, playId, nflId, displayName, PlayerSideOfBall, Is_RT)

DesignedRuns_Merged <- DesignedRuns_Merged %>% left_join(DesignedRuns_RT_Identify_Snap, 
                                                         by = c("gameId", "playId", "nflId", "displayName", "PlayerSideOfBall"))
DesignedRuns_Merged <- DesignedRuns_Merged %>% arrange(gameId, playId, nflId, frameId)
DesignedRuns_Merged <- DesignedRuns_Merged %>% 
  mutate(Is_RT = ifelse(!is.na(Is_RT), TRUE, FALSE))

DesignedRuns_Merged <- DesignedRuns_Merged %>% mutate(AlignedPos_Box =
        ifelse(Is_RT == TRUE, "RT", AlignedPos_Box))

# Update the distance from RT column
RT_coordinates <- DesignedRuns_Merged %>%
  filter(AlignedPos_Box == "RT") %>%
  select(gameId, playId, nflId_RT = nflId, x, y, frameId) %>%
  rename(xRT = x,
         yRT = y)

DesignedRuns_Merged <- DesignedRuns_Merged %>%
  left_join(RT_coordinates, by = c("playId", "gameId", "frameId")) 
rm(RT_coordinates, DesignedRuns_RT_Identify_Snap)

# Use group_by to see if any play has multiple players listed at same position
RTNumbers_ByPlay <- DesignedRuns_Merged %>% group_by(gameId, playId) %>% 
  filter(AlignedPos_Box == "RT", event %in% c("ball_snap", "snap_direct")) %>% summarize(n = n()) %>% arrange(desc(n))
rm(RTNumbers_ByPlay)
table(DesignedRuns_Merged$AlignedPos_Box)

# And get rid of the new "extra" columns
DesignedRuns_Merged <- DesignedRuns_Merged %>%
  select(-c("nflId_RT.x", "xRT.x", "yRT.x"))
DesignedRuns_Merged <- DesignedRuns_Merged %>%
  rename(nflId_RT = `nflId_RT.y`, xRT = `xRT.y`, yRT = `yRT.y`)

# Now define TEs using Y and X coordinates
DesignedRuns_TEL1_Identify_Snap <- DesignedRuns_Merged %>%
  filter(PlayerSideOfBall == "offense", is.na(AlignedPos_Box),
  event %in% c("ball_snap", "snap_direct"), y > yLT, y <= 2 + yLT, x >= xLT - 1.25) %>%
  mutate(Is_TEL1 = TRUE) %>%
  select(gameId, playId, nflId, displayName, PlayerSideOfBall, y, Is_TEL1)

# Use group_by to see if any play has multiple players listed at same position
TEL1Numbers_ByPlay <- DesignedRuns_TEL1_Identify_Snap %>% group_by(gameId, playId) %>% 
  filter(Is_TEL1 == TRUE) %>% summarize(n = n()) %>% arrange(desc(n))
# There are a few plays with multiple TEL1 by this definition (likely QB sneaks)
# An example: View(DesignedRuns_Merged %>% filter(event == "ball_snap", PlayerSideOfBall == "offense", playId == 361))

DesignedRuns_TEL1_Identify_Snap <- DesignedRuns_TEL1_Identify_Snap %>% 
  group_by(gameId, playId) %>% 
  filter(Is_TEL1 == TRUE) %>%
  mutate(Y_Rank_Among_TEL1 = rank(y, ties.method = "first")) %>% 
  ungroup()

# Now, for the few "double" cases, make the player with the higher y not be TEL1
DesignedRuns_TEL1_Identify_Snap <- DesignedRuns_TEL1_Identify_Snap %>% 
  mutate(Is_TEL1 = ifelse(Y_Rank_Among_TEL1 == 1, TRUE, FALSE))
# Then get rid of the players that are no longer considered TEL1
DesignedRuns_TEL1_Identify_Snap <- DesignedRuns_TEL1_Identify_Snap %>% 
  filter(Is_TEL1 == TRUE)
# And now get rid of unnecessary columns before the left_join
DesignedRuns_TEL1_Identify_Snap <- DesignedRuns_TEL1_Identify_Snap %>%
  select(-c("y", "Y_Rank_Among_TEL1"))

DesignedRuns_Merged <- DesignedRuns_Merged %>% left_join(DesignedRuns_TEL1_Identify_Snap, 
          by = c("gameId", "playId", "nflId", "displayName", "PlayerSideOfBall"))
DesignedRuns_Merged <- DesignedRuns_Merged %>% arrange(gameId, playId, nflId, frameId)
DesignedRuns_Merged <- DesignedRuns_Merged %>% 
  mutate(Is_TEL1 = ifelse(!is.na(Is_TEL1), TRUE, FALSE))

DesignedRuns_Merged <- DesignedRuns_Merged %>% mutate(AlignedPos_Box =
          ifelse(Is_TEL1 == TRUE, "TEL1", AlignedPos_Box))

# create a distance from TEL1 column
TEL1_coordinates <- DesignedRuns_Merged %>%
  filter(AlignedPos_Box == "TEL1") %>%
  select(gameId, playId, nflId_TEL1 = nflId, x, y, frameId) %>%
  rename(xTEL1 = x,
         yTEL1 = y)

DesignedRuns_Merged <- DesignedRuns_Merged %>%
  left_join(TEL1_coordinates, by = c("playId", "gameId", "frameId")) 
rm(TEL1_coordinates, DesignedRuns_TEL1_Identify_Snap)

# Run a final check to see if any play has multiple players listed at same position
TEL1Numbers_ByPlay <- DesignedRuns_Merged %>% group_by(gameId, playId) %>% 
  filter(AlignedPos_Box == "TEL1", event %in% c("ball_snap", "snap_direct")) %>% summarize(n = n()) %>% arrange(desc(n))
rm(TEL1Numbers_ByPlay)
table(DesignedRuns_Merged$AlignedPos_Box)

# Now define TEL2
DesignedRuns_TEL2_Identify_Snap <- DesignedRuns_Merged %>%
  filter(PlayerSideOfBall == "offense", is.na(AlignedPos_Box),
         event %in% c("ball_snap", "snap_direct"), y > yTEL1, y <= 2 + yTEL1, x >= xLT - 1.25) %>%
  mutate(Is_TEL2 = TRUE) %>%
  select(gameId, playId, nflId, displayName, PlayerSideOfBall, y, Is_TEL2)

# Use group_by to see if any play has multiple players listed at same position
TEL2Numbers_ByPlay <- DesignedRuns_TEL2_Identify_Snap %>% group_by(gameId, playId) %>% 
  filter(Is_TEL2 == TRUE) %>% summarize(n = n()) %>% arrange(desc(n))

DesignedRuns_TEL2_Identify_Snap <- DesignedRuns_TEL2_Identify_Snap %>% 
  group_by(gameId, playId) %>% 
  filter(Is_TEL2 == TRUE) %>%
  mutate(Y_Rank_Among_TEL2 = rank(y, ties.method = "first")) %>% 
  ungroup()

# Now, for the few "double" cases, make the player with the higher y not be TEL2
DesignedRuns_TEL2_Identify_Snap <- DesignedRuns_TEL2_Identify_Snap %>% 
  mutate(Is_TEL2 = ifelse(Y_Rank_Among_TEL2 == 1, TRUE, FALSE))
# Then get rid of the players that are no longer considered TEL2
DesignedRuns_TEL2_Identify_Snap <- DesignedRuns_TEL2_Identify_Snap %>% 
  filter(Is_TEL2 == TRUE)
# And now get rid of unnecessary columns before the left_join
DesignedRuns_TEL2_Identify_Snap <- DesignedRuns_TEL2_Identify_Snap %>%
  select(-c("y", "Y_Rank_Among_TEL2"))

DesignedRuns_Merged <- DesignedRuns_Merged %>% left_join(DesignedRuns_TEL2_Identify_Snap, 
                    by = c("gameId", "playId", "nflId", "displayName", "PlayerSideOfBall"))
DesignedRuns_Merged <- DesignedRuns_Merged %>% arrange(gameId, playId, nflId, frameId)
DesignedRuns_Merged <- DesignedRuns_Merged %>% 
  mutate(Is_TEL2 = ifelse(!is.na(Is_TEL2), TRUE, FALSE))

DesignedRuns_Merged <- DesignedRuns_Merged %>% mutate(AlignedPos_Box =
                                     ifelse(Is_TEL2 == TRUE, "TEL2", AlignedPos_Box))

# create a distance from TEL2 column
TEL2_coordinates <- DesignedRuns_Merged %>%
  filter(AlignedPos_Box == "TEL2") %>%
  select(gameId, playId, nflId_TEL2 = nflId, x, y, frameId) %>%
  rename(xTEL2 = x,
         yTEL2 = y)

DesignedRuns_Merged <- DesignedRuns_Merged %>%
  left_join(TEL2_coordinates, by = c("playId", "gameId", "frameId")) 
rm(TEL2_coordinates, DesignedRuns_TEL2_Identify_Snap)

# Run a final check to see if any play has multiple players listed at same position
TEL2Numbers_ByPlay <- DesignedRuns_Merged %>% group_by(gameId, playId) %>% 
  filter(AlignedPos_Box == "TEL2", event %in% c("ball_snap", "snap_direct")) %>% summarize(n = n()) %>% arrange(desc(n))
rm(TEL2Numbers_ByPlay)
table(DesignedRuns_Merged$AlignedPos_Box)

# Now define TEL3
DesignedRuns_TEL3_Identify_Snap <- DesignedRuns_Merged %>%
  filter(PlayerSideOfBall == "offense", is.na(AlignedPos_Box),
         event %in% c("ball_snap", "snap_direct"), y > yTEL2, y <= 2 + yTEL2, x >= xLT - 1.25) %>%
  mutate(Is_TEL3 = TRUE) %>%
  select(gameId, playId, nflId, displayName, PlayerSideOfBall, y, Is_TEL3)

# Use group_by to see if any play has multiple players listed at same position
TEL3Numbers_ByPlay <- DesignedRuns_TEL3_Identify_Snap %>% group_by(gameId, playId) %>% 
  filter(Is_TEL3 == TRUE) %>% summarize(n = n()) %>% arrange(desc(n))

DesignedRuns_TEL3_Identify_Snap <- DesignedRuns_TEL3_Identify_Snap %>% 
  group_by(gameId, playId) %>% 
  filter(Is_TEL3 == TRUE) %>%
  mutate(Y_Rank_Among_TEL3 = rank(y, ties.method = "first")) %>% 
  ungroup()

# Now, for the few "double" cases, make the player with the higher y not be TEL3
DesignedRuns_TEL3_Identify_Snap <- DesignedRuns_TEL3_Identify_Snap %>% 
  mutate(Is_TEL3 = ifelse(Y_Rank_Among_TEL3 == 1, TRUE, FALSE))
# Then get rid of the players that are no longer considered TEL3
DesignedRuns_TEL3_Identify_Snap <- DesignedRuns_TEL3_Identify_Snap %>% 
  filter(Is_TEL3 == TRUE)
# And now get rid of unnecessary columns before the left_join
DesignedRuns_TEL3_Identify_Snap <- DesignedRuns_TEL3_Identify_Snap %>%
  select(-c("y", "Y_Rank_Among_TEL3"))

DesignedRuns_Merged <- DesignedRuns_Merged %>% left_join(DesignedRuns_TEL3_Identify_Snap, 
                by = c("gameId", "playId", "nflId", "displayName", "PlayerSideOfBall"))
DesignedRuns_Merged <- DesignedRuns_Merged %>% arrange(gameId, playId, nflId, frameId)
DesignedRuns_Merged <- DesignedRuns_Merged %>% 
  mutate(Is_TEL3 = ifelse(!is.na(Is_TEL3), TRUE, FALSE))

DesignedRuns_Merged <- DesignedRuns_Merged %>% mutate(AlignedPos_Box = 
                                      ifelse(Is_TEL3 == TRUE, "TEL3", AlignedPos_Box))

# create a distance from TEL3 column
TEL3_coordinates <- DesignedRuns_Merged %>%
  filter(AlignedPos_Box == "TEL3") %>%
  select(gameId, playId, nflId_TEL3 = nflId, x, y, frameId) %>%
  rename(xTEL3 = x,
         yTEL3 = y)

DesignedRuns_Merged <- DesignedRuns_Merged %>%
  left_join(TEL3_coordinates, by = c("playId", "gameId", "frameId")) 
rm(TEL3_coordinates, DesignedRuns_TEL3_Identify_Snap)

# Run a final check to see if any play has multiple players listed at same position
TEL3Numbers_ByPlay <- DesignedRuns_Merged %>% group_by(gameId, playId) %>% 
  filter(AlignedPos_Box == "TEL3", event %in% c("ball_snap", "snap_direct")) %>% summarize(n = n()) %>% arrange(desc(n))
rm(TEL3Numbers_ByPlay)
table(DesignedRuns_Merged$AlignedPos_Box)

# Now define TER1
DesignedRuns_TER1_Identify_Snap <- DesignedRuns_Merged %>%
  filter(PlayerSideOfBall == "offense", is.na(AlignedPos_Box),
         event %in% c("ball_snap", "snap_direct"), y < yRT, y >= yRT - 2, x >= xRT - 1.25) %>%
  mutate(Is_TER1 = TRUE) %>%
  select(gameId, playId, nflId, displayName, PlayerSideOfBall, y, Is_TER1)

# Use group_by to see if any play has multiple players listed at same position
TER1Numbers_ByPlay <- DesignedRuns_TER1_Identify_Snap %>% group_by(gameId, playId) %>% 
  filter(Is_TER1 == TRUE) %>% summarize(n = n()) %>% arrange(desc(n))

DesignedRuns_TER1_Identify_Snap <- DesignedRuns_TER1_Identify_Snap %>% 
  group_by(gameId, playId) %>% 
  filter(Is_TER1 == TRUE) %>%
  mutate(Y_Rank_Among_TER1 = rank(y, ties.method = "first")) %>% 
  ungroup()

# Now, for the few "double" cases, make the player with the higher y not be TER1
DesignedRuns_TER1_Identify_Snap <- DesignedRuns_TER1_Identify_Snap %>% 
  mutate(Is_TER1 = ifelse(Y_Rank_Among_TER1 == 1, TRUE, FALSE))
# Then get rid of the players that are no longer considered TER1
DesignedRuns_TER1_Identify_Snap <- DesignedRuns_TER1_Identify_Snap %>% 
  filter(Is_TER1 == TRUE)
# And now get rid of unnecessary columns before the left_join
DesignedRuns_TER1_Identify_Snap <- DesignedRuns_TER1_Identify_Snap %>%
  select(-c("y", "Y_Rank_Among_TER1"))

DesignedRuns_Merged <- DesignedRuns_Merged %>% left_join(DesignedRuns_TER1_Identify_Snap, 
               by = c("gameId", "playId", "nflId", "displayName", "PlayerSideOfBall"))
DesignedRuns_Merged <- DesignedRuns_Merged %>% arrange(gameId, playId, nflId, frameId)
DesignedRuns_Merged <- DesignedRuns_Merged %>% 
  mutate(Is_TER1 = ifelse(!is.na(Is_TER1), TRUE, FALSE))

DesignedRuns_Merged <- DesignedRuns_Merged %>% mutate(AlignedPos_Box =
                                       ifelse(Is_TER1 == TRUE, "TER1", AlignedPos_Box))

# create a distance from TER1 column
TER1_coordinates <- DesignedRuns_Merged %>%
  filter(AlignedPos_Box == "TER1") %>%
  select(gameId, playId, nflId_TER1 = nflId, x, y, frameId) %>%
  rename(xTER1 = x,
         yTER1 = y)

DesignedRuns_Merged <- DesignedRuns_Merged %>%
  left_join(TER1_coordinates, by = c("playId", "gameId", "frameId")) 
rm(TER1_coordinates, DesignedRuns_TER1_Identify_Snap)

# Run a final check to see if any play has multiple players listed at same position
TER1Numbers_ByPlay <- DesignedRuns_Merged %>% group_by(gameId, playId) %>% 
  filter(AlignedPos_Box == "TER1", event %in% c("ball_snap", "snap_direct")) %>% summarize(n = n()) %>% arrange(desc(n))
rm(TER1Numbers_ByPlay)
table(DesignedRuns_Merged$AlignedPos_Box)

# Now define TER2
DesignedRuns_TER2_Identify_Snap <- DesignedRuns_Merged %>%
  filter(PlayerSideOfBall == "offense", is.na(AlignedPos_Box),
         event %in% c("ball_snap", "snap_direct"), y < yTER1, y >= yTER1 - 2, x >= xRT - 1.25) %>%
  mutate(Is_TER2 = TRUE) %>%
  select(gameId, playId, nflId, displayName, PlayerSideOfBall, y, Is_TER2)

# Use group_by to see if any play has multiple players listed at same position
TER2Numbers_ByPlay <- DesignedRuns_TER2_Identify_Snap %>% group_by(gameId, playId) %>% 
  filter(Is_TER2 == TRUE) %>% summarize(n = n()) %>% arrange(desc(n))

DesignedRuns_TER2_Identify_Snap <- DesignedRuns_TER2_Identify_Snap %>% 
  group_by(gameId, playId) %>% 
  filter(Is_TER2 == TRUE) %>%
  mutate(Y_Rank_Among_TER2 = rank(y, ties.method = "first")) %>% 
  ungroup()

# Now, for the few "double" cases, make the player with the higher y not be TER2
DesignedRuns_TER2_Identify_Snap <- DesignedRuns_TER2_Identify_Snap %>% 
  mutate(Is_TER2 = ifelse(Y_Rank_Among_TER2 == 1, TRUE, FALSE))
# Then get rid of the players that are no longer considered TER2
DesignedRuns_TER2_Identify_Snap <- DesignedRuns_TER2_Identify_Snap %>% 
  filter(Is_TER2 == TRUE)
# And now get rid of unnecessary columns before the left_join
DesignedRuns_TER2_Identify_Snap <- DesignedRuns_TER2_Identify_Snap %>%
  select(-c("y", "Y_Rank_Among_TER2"))

DesignedRuns_Merged <- DesignedRuns_Merged %>% left_join(DesignedRuns_TER2_Identify_Snap, 
         by = c("gameId", "playId", "nflId", "displayName", "PlayerSideOfBall"))
DesignedRuns_Merged <- DesignedRuns_Merged %>% arrange(gameId, playId, nflId, frameId)
DesignedRuns_Merged <- DesignedRuns_Merged %>% 
  mutate(Is_TER2 = ifelse(!is.na(Is_TER2), TRUE, FALSE))

DesignedRuns_Merged <- DesignedRuns_Merged %>% mutate(AlignedPos_Box =
                                      ifelse(Is_TER2 == TRUE, "TER2", AlignedPos_Box))

# create a distance from TER2 column
TER2_coordinates <- DesignedRuns_Merged %>%
  filter(AlignedPos_Box == "TER2") %>%
  select(gameId, playId, nflId_TER2 = nflId, x, y, frameId) %>%
  rename(xTER2 = x,
         yTER2 = y)

DesignedRuns_Merged <- DesignedRuns_Merged %>%
  left_join(TER2_coordinates, by = c("playId", "gameId", "frameId")) 
rm(TER2_coordinates, DesignedRuns_TER2_Identify_Snap)

# Run a final check to see if any play has multiple players listed at same position
TER2Numbers_ByPlay <- DesignedRuns_Merged %>% group_by(gameId, playId) %>% 
  filter(AlignedPos_Box == "TER2", event %in% c("ball_snap", "snap_direct")) %>% summarize(n = n()) %>% arrange(desc(n))
rm(TER2Numbers_ByPlay)
table(DesignedRuns_Merged$AlignedPos_Box)

# Now define TER3
DesignedRuns_TER3_Identify_Snap <- DesignedRuns_Merged %>%
  filter(PlayerSideOfBall == "offense", is.na(AlignedPos_Box),
         event %in% c("ball_snap", "snap_direct"), y < yTER2, y >= yTER2 - 2, x >= xRT - 1.25) %>%
  mutate(Is_TER3 = TRUE) %>%
  select(gameId, playId, nflId, displayName, PlayerSideOfBall, y, Is_TER3)

# Use group_by to see if any play has multiple players listed at same position
TER3Numbers_ByPlay <- DesignedRuns_TER3_Identify_Snap %>% group_by(gameId, playId) %>% 
  filter(Is_TER3 == TRUE) %>% summarize(n = n()) %>% arrange(desc(n))

DesignedRuns_TER3_Identify_Snap <- DesignedRuns_TER3_Identify_Snap %>% 
  group_by(gameId, playId) %>% 
  filter(Is_TER3 == TRUE) %>%
  mutate(Y_Rank_Among_TER3 = rank(y, ties.method = "first")) %>% 
  ungroup()

# Now, for the few "double" cases, make the player with the higher y not be TER3
DesignedRuns_TER3_Identify_Snap <- DesignedRuns_TER3_Identify_Snap %>% 
  mutate(Is_TER3 = ifelse(Y_Rank_Among_TER3 == 1, TRUE, FALSE))
# Then get rid of the players that are no longer considered TER3
DesignedRuns_TER3_Identify_Snap <- DesignedRuns_TER3_Identify_Snap %>% 
  filter(Is_TER3 == TRUE)
# And now get rid of unnecessary columns before the left_join
DesignedRuns_TER3_Identify_Snap <- DesignedRuns_TER3_Identify_Snap %>%
  select(-c("y", "Y_Rank_Among_TER3"))

DesignedRuns_Merged <- DesignedRuns_Merged %>% left_join(DesignedRuns_TER3_Identify_Snap, 
            by = c("gameId", "playId", "nflId", "displayName", "PlayerSideOfBall"))
DesignedRuns_Merged <- DesignedRuns_Merged %>% arrange(gameId, playId, nflId, frameId)
DesignedRuns_Merged <- DesignedRuns_Merged %>% 
  mutate(Is_TER3 = ifelse(!is.na(Is_TER3), TRUE, FALSE))

DesignedRuns_Merged <- DesignedRuns_Merged %>% mutate(AlignedPos_Box =
                             ifelse(Is_TER3 == TRUE, "TER3", AlignedPos_Box))

# create a distance from TER3 column
TER3_coordinates <- DesignedRuns_Merged %>%
  filter(AlignedPos_Box == "TER3") %>%
  select(gameId, playId, nflId_TER3 = nflId, x, y, frameId) %>%
  rename(xTER3 = x,
         yTER3 = y)

DesignedRuns_Merged <- DesignedRuns_Merged %>%
  left_join(TER3_coordinates, by = c("playId", "gameId", "frameId")) 
rm(TER3_coordinates, DesignedRuns_TER3_Identify_Snap)

# Run a final check to see if any play has multiple players listed at same position
TER3Numbers_ByPlay <- DesignedRuns_Merged %>% group_by(gameId, playId) %>% 
  filter(AlignedPos_Box == "TER3", event %in% c("ball_snap", "snap_direct")) %>% summarize(n = n()) %>% arrange(desc(n))
rm(TER3Numbers_ByPlay)
table(DesignedRuns_Merged$AlignedPos_Box)

# Use unique() to get rid of any duplicates
DesignedRuns_Merged <- unique(DesignedRuns_Merged)

# Now classify how many total TEs there are on each play
left_tes <- DesignedRuns_Merged %>% 
  filter(event %in% c("ball_snap", "snap_direct"),
         AlignedPos_Box %in% c("TEL1", "TEL2", "TEL3")) %>% 
  group_by(gameId, playId) %>% 
  summarize(num_left_tes = n())

right_tes <- DesignedRuns_Merged %>% 
  filter(event %in% c("ball_snap", "snap_direct"),
         AlignedPos_Box %in% c("TER1", "TER2", "TER3")) %>% 
  group_by(gameId, playId) %>% 
  summarize(num_right_tes = n())

DesignedRuns_Merged <- DesignedRuns_Merged %>% 
  left_join(left_tes, by = c("gameId","playId")) %>% 
  left_join(right_tes, by = c("gameId","playId"))

DesignedRuns_Merged <- DesignedRuns_Merged %>% 
  mutate(num_left_tes = ifelse(is.na(num_left_tes), 0, num_left_tes),
         num_right_tes = ifelse(is.na(num_right_tes), 0, num_right_tes))
rm(left_tes, right_tes)
DesignedRuns_Merged <- DesignedRuns_Merged %>%
  mutate(num_total_tes = num_left_tes + num_right_tes)

# Define the outer regions of the "box" by using furthest distance from C
# Recall that maximum "y" is to the offense's left
table(DesignedRuns_Merged$AlignedPos_Box)

DesignedRuns_BoxEnd_Left <- DesignedRuns_Merged %>%
  group_by(gameId, playId) %>%
  filter(event %in% c("ball_snap", "snap_direct") & !is.na(AlignedPos_Box)) %>% 
  mutate(FarLeft_Box = ifelse(PreSnap_y == max(PreSnap_y), TRUE, FALSE)) %>%
  ungroup() %>%
  select(gameId, playId, nflId, displayName, AlignedPos_Box, FarLeft_Box)

DesignedRuns_Merged <- DesignedRuns_Merged %>%
  left_join(DesignedRuns_BoxEnd_Left, by = c("gameId", "playId", "nflId", "displayName", "AlignedPos_Box"))

DesignedRuns_BoxEnd_Right <- DesignedRuns_Merged %>%
  group_by(gameId, playId) %>%
  filter(event %in% c("ball_snap", "snap_direct") & !is.na(AlignedPos_Box)) %>% 
  mutate(FarRight_Box = ifelse(PreSnap_y == min(PreSnap_y), TRUE, FALSE)) %>%
  ungroup() %>%
  select(gameId, playId, nflId, displayName, AlignedPos_Box, FarRight_Box)

DesignedRuns_Merged <- DesignedRuns_Merged %>%
  left_join(DesignedRuns_BoxEnd_Right, by = c("gameId", "playId", "nflId", "displayName", "AlignedPos_Box"))

# create columns for where the edges of the box are (only y is relevant)
FarLeft_Box_Location <- DesignedRuns_Merged %>%
  filter(FarLeft_Box == TRUE, event %in% c("ball_snap", "snap_direct")) %>%
  select(gameId, playId, y) %>%
  rename(FarLeft_Box_Y_Snap = y)

DesignedRuns_Merged <- DesignedRuns_Merged %>%
  left_join(FarLeft_Box_Location, by = c("playId", "gameId"))

FarRight_Box_Location <- DesignedRuns_Merged %>%
  filter(FarRight_Box == TRUE, event %in% c("ball_snap", "snap_direct")) %>%
  select(gameId, playId, y) %>%
  rename(FarRight_Box_Y_Snap = y)

DesignedRuns_Merged <- DesignedRuns_Merged %>%
  left_join(FarRight_Box_Location, by = c("playId", "gameId"))
rm(FarLeft_Box_Location, FarRight_Box_Location, DesignedRuns_BoxEnd_Left, DesignedRuns_BoxEnd_Right)

# Then treat defenders similarly, where "x" within 1.5 yards of LOS means "on the line"
# And use 6 yards of "x" from the LOS to be "in the box"
# Likewise, "y" must be within 2 yards of edge (or 3.5, if on LOS)
# Example of why we stretch to 3.5 for EDGE: View(DesignedRuns_Merged %>% filter(playId == 101, displayName == "Justin Hollins"))
DesignedRuns_Defenders_InBox <- DesignedRuns_Merged %>%
  group_by(gameId, playId, nflId) %>%
  filter(event %in% c("ball_snap", "snap_direct"), PlayerSideOfBall == "defense") %>% 
  mutate(DefenderInBox = ifelse(x - Ball_X_Frame1 <= 1.5 & y <= FarLeft_Box_Y_Snap + 3.5 & y >= FarRight_Box_Y_Snap - 3.5, "Box_LOS",
                            ifelse(x - Ball_X_Frame1 <= 6 & y <= FarLeft_Box_Y_Snap + 2 & y >= FarRight_Box_Y_Snap - 2, "Box_OffBall", NA))) %>%
  ungroup() %>%
  select(gameId, playId, nflId, displayName, DefenderInBox,
         PreSnap_x = x, PreSnap_y = y, Ball_X_Frame1)

DesignedRuns_Merged <- DesignedRuns_Merged %>%
  left_join(DesignedRuns_Defenders_InBox, by = c("gameId", "playId", "nflId", "displayName", "Ball_X_Frame1", "PreSnap_x", "PreSnap_y"))
DesignedRuns_Merged <- DesignedRuns_Merged %>% arrange(gameId, playId, nflId, frameId)
rm(DesignedRuns_Defenders_InBox)

# Then gap alignments can proceed from there
# Note that these gaps are based on offensive orientation (e.g. b/w C and LG is left A-gap)
# And only the box defenders have gaps (e.g., weird to say FS 15 yards off-ball is in "B-gap")
pre_snap_gaps <- DesignedRuns_Merged %>% 
  filter(event %in% c("ball_snap", "snap_direct"),
         PlayerSideOfBall == "defense", !is.na(DefenderInBox)) %>% 
  select(gameId, playId, nflId, displayName, position, AlignedPos_Box, DefenderInBox, 
         Ball_X_Frame1, X_dist_FromBallFrame1, Y_distFromMOF, x, y, yLT, yLG, 
         yCenter, yRG, yRT, PlayerSideOfBall, yTEL1, yTER1, yTEL2, yTER2, yTEL3, yTER3,
         num_right_tes, num_left_tes) %>% 
  mutate(pre_snap_gap =
           case_when(y >= yCenter & y <= yLG ~ "L-A",
                     y >= yLG & y <= yLT ~ "L-B",
                     num_left_tes == 0 & y >= yLT ~ "L-C",
                     y < yCenter & y >= yRG ~ "R-A",
                     y <= yRG & y >= yRT ~ "R-B",
                     num_right_tes == 0 & y <= yRT ~ "R-C",
                     num_left_tes == 1 & y >= yLT & y <= yTEL1 ~ "L-C",
                     num_left_tes == 1 & y >= yTEL1 ~ "L-D",
                     num_left_tes == 2 & y >= yLT & y <= yTEL1 ~ "L-C",
                     num_left_tes == 2 & y >= yTEL1 & y <= yTEL2 ~ "L-D",
                     num_left_tes == 2 & y >= yTEL2 ~ "L-E",
                     num_left_tes == 3 & y >= yLT & y <= yTEL1 ~ "L-C", 
                     num_left_tes == 3 & y >= yTEL1 & y <= yTEL2 ~ "L-D",
                     num_left_tes == 3 & y >= yTEL2 & y <= yTEL3 ~ "L-E",
                     num_left_tes == 3 & y >= yTEL3 ~ "L-F",
                     num_right_tes == 1 & y <= yRT & y >= yTER1 ~ "R-C",
                     num_right_tes == 1 & y <= yTER1 ~ "R-D",
                     num_right_tes == 2 & y <= yRT & y >= yTER1 ~ "R-C",
                     num_right_tes == 2 & y <= yTER1 & y >= yTER2 ~ "R-D",
                     num_right_tes == 2 & y <= yTER2 ~ "R-E",
                     num_right_tes == 3 & y <= yRT & y >= yTER1 ~ "R-C",
                     num_right_tes == 3 & y <= yTER1 & y >= yTER2 ~ "R-D",
                     num_right_tes == 3 & y <= yTER2 & y >= yTER3 ~ "R-E",
                     num_right_tes == 3 & y <= yTER3 ~ "R-F"))

pre_snap_gaps <- pre_snap_gaps %>% 
  select(gameId, playId, nflId, displayName, pre_snap_gap) %>% 
  filter(nflId != "NA")

DesignedRuns_Merged <- DesignedRuns_Merged %>%
  left_join(pre_snap_gaps, by = c("gameId", "playId", "nflId", "displayName"))
DesignedRuns_Merged <- DesignedRuns_Merged %>% arrange(gameId, playId, nflId, frameId)
rm(pre_snap_gaps)
DesignedRuns_Merged <- DesignedRuns_Merged %>% 
  select(-c("Is_LG", "nflId_LG", "xLG", "yLG", "Is_LT", "nflId_LT", "xLT", "yLT", "Is_RG",                            
            "nflId_RG", "xRG", "yRG", "Is_RT", "nflId_RT", "xRT", "yRT", "nflId_TEL1", "xTEL1",                            
            "yTEL1", "Is_TEL2", "nflId_TEL2", "xTEL2", "yTEL2", "Is_TEL3", "nflId_TEL3", "xTEL3",                            
            "yTEL3", "Is_TER1", "nflId_TER1", "xTER1", "yTER1", "Is_TER2", "nflId_TER2", "xTER2",                            
            "yTER2", "Is_TER3", "nflId_TER3", "xTER3", "yTER3"))

# View(DesignedRuns_Merged %>% filter(frameId == 1, playId == 146))
# This is a good example of "gap moving" on trap/power play w/ pulling tackle
# The run_gap is "left/tackle," though it was RT who pulled toward left

# View(DesignedRuns_Merged %>% filter(is.na(run_gap)))
# View(DesignedRuns_Merged %>% filter(is.na(run_location)))
# Fix these few with str_detect
DesignedRuns_Merged <- DesignedRuns_Merged %>% mutate(run_gap =
   ifelse(is.na(run_gap) & str_detect(playDescription, "(up the middle)"), "center",
      ifelse(is.na(run_gap) & str_detect(playDescription, "(left end)"), "end",
           ifelse(is.na(run_gap) & str_detect(playDescription, "(left tackle)"), "tackle",
              ifelse(is.na(run_gap) & str_detect(playDescription, "(left guard)"), "guard",
                   ifelse(is.na(run_gap) & str_detect(playDescription, "(right guard)"), "guard",
                     ifelse(is.na(run_gap) & str_detect(playDescription, "(right tackle)"), "tackle",
                        ifelse(is.na(run_gap) & str_detect(playDescription, "(right end)"), "end", run_gap))))))))

DesignedRuns_Merged <- DesignedRuns_Merged %>% mutate(run_location =
  ifelse(is.na(run_location) & str_detect(playDescription, "(up the middle)"), "middle",
     ifelse(is.na(run_location) & str_detect(playDescription, "(left end)"), "left",
        ifelse(is.na(run_location) & str_detect(playDescription, "(left tackle)"), "left",
           ifelse(is.na(run_location) & str_detect(playDescription, "(left guard)"), "left",
              ifelse(is.na(run_location) & str_detect(playDescription, "(right guard)"), "right",
                 ifelse(is.na(run_location) & str_detect(playDescription, "(right tackle)"), "right",
                    ifelse(is.na(run_location) & str_detect(playDescription, "(right end)"), "right", run_location))))))))

# Now, turn run_gap into something numeric, e.g. left end is 7, left tackle is 6 ...
# View(DesignedRuns_Merged %>% filter(run_gap == "end" & num_total_tes == 0))
# This is NOT empty (i.e., there can be an "end" run-gap even without any TEs on field)
DesignedRuns_Merged <- DesignedRuns_Merged %>% mutate(RunGap_Numeric =
    ifelse(run_location == "left" & run_gap == "end", 7, 
        ifelse(run_location == "left" & run_gap == "tackle", 6,
              ifelse(run_location == "left" & run_gap == "guard", 5,
                    ifelse(run_location == "middle" & run_gap == "center", 4,
                         ifelse(run_location == "right" & run_gap == "guard", 3,
                               ifelse(run_location == "right" & run_gap == "tackle", 2, 1)))))))

# Make a similar adjustment relating to defensive pre-snap alignments
table(DesignedRuns_Merged$pre_snap_gap)

DesignedRuns_Merged <- DesignedRuns_Merged %>% mutate(DefAlign_Numeric =
    ifelse(pre_snap_gap %in% c("L-A", "R-A"), 4,
        ifelse(pre_snap_gap %in% c("L-B"), 5,
             ifelse(pre_snap_gap %in% c("L-C"), 6,
                  ifelse(pre_snap_gap %in% c("L-D", "L-E", "L-F"), 7,
                      ifelse(pre_snap_gap %in% c("R-B"), 3,
                           ifelse(pre_snap_gap %in% c("R-C"), 2,
                               ifelse(pre_snap_gap %in% c("R-D", "R-E", "R-F"), 1, pre_snap_gap))))))))
class(DesignedRuns_Merged$DefAlign_Numeric) <- "numeric"

# Also add a column for IDL vs. EDGE vs. off-ball LB
DesignedRuns_Merged <- DesignedRuns_Merged %>% mutate(Box_IDLvsEDGE =
  ifelse(DefenderInBox %in% "Box_OffBall", "OffBall_LB",
      ifelse(DefenderInBox %in% "Box_LOS" & DefAlign_Numeric %in% 3:5, "IDL",
            ifelse(DefenderInBox %in% "Box_LOS" & DefAlign_Numeric %in% c(1, 2, 6, 7), "EDGE", NA))))

# Do a group-by to create each player's "Primary_BoxPosition"
# I.e. each player's general most-often played position, even if not on this specific play
PrimPosition_Label <- DesignedRuns_Merged %>% filter(event %in% c("ball_snap", "snap_direct")) %>%
  group_by(nflId, displayName) %>%
  summarize(Plays = n(), 
            IDLSnaps = sum(Box_IDLvsEDGE == "IDL", na.rm = TRUE), IDLSnap_Rate = IDLSnaps / Plays,
            EDGESnaps = sum(Box_IDLvsEDGE == "EDGE", na.rm = TRUE), EDGESnap_Rate = EDGESnaps / Plays,
            OffBall_LBSnaps = sum(Box_IDLvsEDGE == "OffBall_LB", na.rm = TRUE), OffBall_LBSnap_Rate = OffBall_LBSnaps / Plays,
            BoxSnaps = IDLSnaps + EDGESnaps + OffBall_LBSnaps, BoxSnapRate = BoxSnaps / Plays)
PrimPosition_Label <- PrimPosition_Label %>% filter(BoxSnapRate >= 0.5)
PrimPosition_Label <- PrimPosition_Label %>% mutate(Primary_BoxPosition =
    ifelse(EDGESnaps >= OffBall_LBSnaps & EDGESnaps >= IDLSnaps, "EDGE",
           ifelse(IDLSnaps >= EDGESnaps & IDLSnaps >= OffBall_LBSnaps, "IDL",
                  ifelse(OffBall_LBSnaps >= EDGESnaps & OffBall_LBSnaps >= IDLSnaps, "OffBall_LB", NA))))
# View(PrimPosition_Label %>% filter(is.na(Primary_BoxPosition))) # it's empty

PrimPosition_Label <- PrimPosition_Label %>% select(nflId, displayName, Primary_BoxPosition, BoxSnaps)
DesignedRuns_Merged <- merge(x = DesignedRuns_Merged, y = PrimPosition_Label,
                             by = c("nflId", "displayName"), all.x = TRUE)
DesignedRuns_Merged <- DesignedRuns_Merged %>% select(-"BoxSnaps")
rm(PrimPosition_Label)

# Get rid of frames where ball-carrier doesn't have ball yet
# For designed runs, we had to wait until after we defined all the gap alignments
# Recall that we already did this for dropbacks in "excluding early frames" file
Frames_AtHandoff <- DesignedRuns_Merged %>%
  filter(event %in% c("run", "handoff", "lateral")) %>%
  select(gameId, playId, nflId, displayName, frameId) %>%
  rename(FrameNumber_AtHandoff = frameId)

# Account for plays that could have multiple of these events
Frames_AtHandoff <- Frames_AtHandoff %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(Frame_Rank = rank(FrameNumber_AtHandoff, ties.method = "first")) %>%
  ungroup() 
Frames_AtHandoff <- Frames_AtHandoff %>% filter(Frame_Rank == 1)
Frames_AtHandoff <- Frames_AtHandoff %>% select(-"Frame_Rank")

DesignedRuns_Merged <- merge(x = DesignedRuns_Merged, y = Frames_AtHandoff, 
                             by = c("playId", "gameId", "nflId", "displayName"))
DesignedRuns_Merged <- DesignedRuns_Merged %>% arrange(gameId, playId, nflId, frameId)

DesignedRuns_Merged <- DesignedRuns_Merged %>% group_by(gameId, playId, nflId) %>% 
  mutate(Unnecessary_Early = ifelse(frameId < FrameNumber_AtHandoff, TRUE, FALSE)) %>% 
  ungroup()

DesignedRuns_Merged <- DesignedRuns_Merged %>% filter(Unnecessary_Early == FALSE)
rm(Frames_AtHandoff)
DesignedRuns_Merged <- DesignedRuns_Merged %>% select(c(-"Unnecessary_Early", "FirstFrame_WithBC"))

