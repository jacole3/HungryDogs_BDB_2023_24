# Recall that DesignedRuns_Merged was originally made in the "DataCleansing_Code" file
# And then it was further mutated in the PreSnap_Alignment_Code file

# View(StatsByPlay_DesignedRuns %>% filter(IndivTackle == 0 & IndivAssist == 0 & Indiv_MissedTackle == 0))
# This is empty, i.e. it's impossible to have a 0 in all three columns
# In other words, players who didn't have a "tackle attempt" simply have NA in all three

# Note that "Frame1" refers to actual start of tracking data
# NOT just the frames where the ball-carrier already has ball
# But, everything "Initial" refers just to frames when ball-carrier has ball

# Stats by play for all plays in data set
StatsByPlay_final_merged_data <- final_merged_data %>% 
  group_by(gameId, playId, nflId, displayName) %>%
  summarize(Frames = n(), down = max(down), distance = max(ydstogo), Team = max(club),
            PlayerSideOfBall = PlayerSideOfBall[1], HomeTeam = max(homeTeamAbbr), AwayTeam = max(visitorTeamAbbr),
            BallCarrierID = max(ballCarrierId), BallCarrierName = max(ballCarrierDisplayName),
            IsBallCarrier = max(IsBallCarrier), Description = max(playDescription), 
            PosTeam = max(possessionTeam), DefTeam = max(defensiveTeam),
            PenaltyYards = max(penaltyYards),
            PrePenaltyYardage = max(prePenaltyPlayResult), NetYardage = max(playResult),
            YdsBeforeContact = max(YdsBeforeContact), YdsAfterContact = max(YdsAfterContact),
            OffFormation = max(offenseFormation), TeamDefendersInBox = max(defendersInTheBox),
            EPA = max(expectedPointsAdded),
            posteam_type = max(posteam_type), yardline_100 = max(yardline_100),
            sp = max(sp), goal_to_go = max(goal_to_go),
            DropbackType = max(passResult),
            air_yards = max(air_yards), yards_after_catch = max(yards_after_catch), 
            air_epa = max(air_epa), yac_epa = max(yac_epa), 
            comp_air_epa = max(comp_air_epa), comp_yac_epa = max(comp_yac_epa),
            pass_touchdown = max(pass_touchdown), Dropback = max(pass),
            xyac_epa = max(xyac_epa), xyac_mean_yardage = max(xyac_mean_yardage), 
            xyac_median_yardage = max(xyac_median_yardage), xyac_success = max(xyac_success), 
            xyac_fd = max(xyac_fd), YACOE = max(YACOE), YAC_EPA_OE = max(YAC_EPA_OE), 
            YAC_Success_OE = max(YAC_Success_OE), YAC_FD_OE = max(YAC_FD_OE), 
            TeamQBHit = max(qb_hit),
            run_location = max(run_location), run_gap = max(run_gap),
            td_team = max(td_team), WPA = max(wpa), DefWPA = max(DefWPA),
            DefTeam_SoloTackle = max(solo_tackle),
            DefTeam_TFL = max(tackled_for_loss), DefTeam_MissedTackles = max(TeamMT_FullPlay),
            TeamTouchdown = max(touchdown), TeamRushTD = max(rush_touchdown),
            TeamReturnTD = max(return_touchdown), OffTeam_Fumble = max(fumble),
            DefTeam_Assist_Tackle = max(assist_tackle),
            DefTeam_Penalized_Tackle = max(TeamDef_Tkl_Pen), TeamDef_Tackle_Clean = max(TeamDef_Tackle_Clean),
            penalty_team = max(penalty_team), penalty_type = max(penalty_type),
            Temperature = max(Temperature), roof = max(roof),
            surface = max(surface), EPSuccess = max(success),
            first_down = max(first_down), out_of_bounds = max(out_of_bounds),
            IndivSoloTackle = max(tackle), IndivAssist = max(assist),
            IndivTotTackles = max(IndivTotTackles), Indiv_MadeTackle = max(Indiv_MadeTackle),
            Indiv_ForcedFumble = max(forcedFumble), Indiv_PenalizedTackle = max(IndivTackle_Penalized),
            IndivTotTackle_Clean = max(IndivTotTackle_Clean),
            Indiv_MissedTackle = max(pff_missedTackle), Indiv_TackleAttempt = max(IndivTackleAttempt),
            PlayDirection = max(playDirection), Initial_X = x[1],
            Initial_Y = y[1], MaxSpeed = max(s), AvgSpeed = mean(s),
            MaxAcceleration = max(a), AvgAcceleration = mean(a),
            TotDistance = sum(dis), InitialOrientation = o[1],
            InitialDirection = dir[1], Ball_X_Frame1 = max(Ball_X_Frame1),
            Ball_Y_Frame1 = max(Ball_Y_Frame1), Initial_X_DistFromBall = X_DistFromBall[1],
            Initial_Y_DistFromBall = Y_DistFromBall[1], Initial_Tot_DistFromBall = TotDistFromBall[1],
            Initial_Y_DistFromMOF = Y_distFromMOF[1], Max_X_AbsDistFromBall = max(X_AbsDistFromBall),
            Max_Y_AbsDistFromBall = max(Y_AbsDistFromBall), Max_Tot_DistFromBall = max(TotDistFromBall),
            height = max(height_inches), weight = max(weight), 
            WPSuccess = max(WPSuccess), QBAlignment = max(QBAlignment), PosGroup = max(PosGroup),
            position = max(position), PlayEPA_TackleAttemptOnly = max(PlayEPA_TackleAttemptOnly),
            PlayEPA_TacklerOnly = max(PlayEPA_TacklerOnly),
            PlayDefWPA_TackleAttemptOnly = max(PlayDefWPA_TackleAttemptOnly),
            PlayDefWPA_TacklerOnly = max(PlayDefWPA_TacklerOnly),
            PlayEPASuccess_TackleAttemptOnly = max(PlayEPASuccess_TackleAttemptOnly),
            PlayEPASuccess_TacklerOnly = max(PlayEPASuccess_TacklerOnly),
            PlayWPASuccess_TackleAttemptOnly = max(PlayWPASuccess_TackleAttemptOnly),
            PlayWPASuccess_TacklerOnly = max(PlayWPASuccess_TacklerOnly),
            Surge = ifelse(PlayerSideOfBall == "offense", NA, max(within_dist_ofBC)),
            Surge_Prob_Logistic = ifelse(PlayerSideOfBall == "offense", NA,
                      ifelse(max(within_dist_ofBC) == 0, max(pred_within_dist_ofBC_logistic), max_pred_near_BC_FiveFramesEarly)),
            Tackle_Prob_Logistic = ifelse(PlayerSideOfBall == "offense", NA,
                      ifelse(max(IndivTotTackles) == 0, max(pred_tackle_logistic), max_pred_tackle_FiveFramesEarly)))

StatsByPlay_final_merged_data <- StatsByPlay_final_merged_data %>% 
  mutate(Surges_OE_Logistic = Surge - Surge_Prob_Logistic,
         Tackles_OE_Logistic = IndivTotTackles - Tackle_Prob_Logistic)

# Mutate a variable for missing a tackle on a play that still was successful for defense
# Recall EPSuccess and WPSuccess are from offense's point of view (so defense wants them to be 0)
StatsByPlay_final_merged_data <- StatsByPlay_final_merged_data %>% mutate(IndivMT_DefEPSuccess =
    ifelse(Indiv_MissedTackle > 0 & EPSuccess == 0, 1,
           ifelse(Indiv_MissedTackle > 0 & EPSuccess > 0, 0, NA)))
StatsByPlay_final_merged_data <- StatsByPlay_final_merged_data %>% mutate(IndivMT_DefWPSuccess =
    ifelse(Indiv_MissedTackle > 0 & WPSuccess == 0, 1,
           ifelse(Indiv_MissedTackle > 0 & WPSuccess > 0, 0, NA)))

# Example of seeing who has highest surge rate in general (no play type or position filters)
IndivStats_final_merged_data <- StatsByPlay_final_merged_data %>%
  group_by(nflId, displayName) %>%
  summarize(Plays = n(), TackleAttempts = sum(Indiv_TackleAttempt, na.rm = TRUE),
            SoloTkl_PerPlay = sum(IndivSoloTackle, na.rm = TRUE) / Plays,
            TotalTkl_PerPlay = sum(IndivTotTackles, na.rm = TRUE) / Plays,
            MissedTackleRate = sum(Indiv_MissedTackle, na.rm = TRUE) / sum(Indiv_TackleAttempt, na.rm = TRUE),
            AvgTotalEPA_TackleAttemptsOnly = mean(PlayEPA_TackleAttemptOnly, na.rm = TRUE),
            AvgTotalEPA_TacklesOnly = mean(PlayEPA_TacklerOnly, na.rm = TRUE),
            AvgDefWPA_TackleAttemptsOnly = mean(PlayDefWPA_TackleAttemptOnly, na.rm = TRUE),
            AvgDefWPA_TacklesOnly = mean(PlayDefWPA_TacklerOnly, na.rm = TRUE),
            EPASuccessRate_TackleAttemptsOnly = mean(PlayEPASuccess_TackleAttemptOnly, na.rm = TRUE),
            EPASuccessRate_TacklesOnly = mean(PlayEPASuccess_TacklerOnly, na.rm = TRUE),
            WPASuccessRate_TackleAttemptsOnly = mean(PlayWPASuccess_TackleAttemptOnly, na.rm = TRUE),
            WPASuccessRate_TacklesOnly = mean(PlayWPASuccess_TacklerOnly, na.rm = TRUE),
            MissedTackles_DefEPASuccess = sum(IndivMT_DefEPSuccess, na.rm = TRUE),
            MissedTackles_DefWPASuccess = sum(IndivMT_DefWPSuccess, na.rm = TRUE),
            Surges = sum(Surge, na.rm = TRUE),
            Surges_OverExpected = sum(Surges_OE_Logistic, na.rm = TRUE),
            SurgeRate = sum(Surge, na.rm = TRUE) / Plays,
            SurgeRate_OverExpected = sum(Surges_OE_Logistic, na.rm = TRUE) / Plays,
            Tackles_OverExpected = sum(Tackles_OE_Logistic, na.rm = TRUE),
            TackleRate_OverExpected = sum(Tackles_OE_Logistic, na.rm = TRUE) / Plays) %>%
  filter(Plays >= 5) %>% # insert your own number here
  arrange(desc(SurgeRate)) %>%
  select(1:4, "SurgeRate", "SurgeRate_OverExpected", 5:21)

# Stats by play for completions
StatsByPlay_Completions <- Completions_Merged %>% 
  group_by(gameId, playId, nflId, displayName) %>%
  summarize(Frames = n(), down = max(down), distance = max(ydstogo), Team = max(club),
            PlayerSideOfBall = PlayerSideOfBall[1], HomeTeam = max(homeTeamAbbr), AwayTeam = max(visitorTeamAbbr),
            BallCarrierID = max(ballCarrierId), BallCarrierName = max(ballCarrierDisplayName),
            IsBallCarrier = max(IsBallCarrier), Description = max(playDescription), 
            PosTeam = max(possessionTeam), DefTeam = max(defensiveTeam),
            PenaltyYards = max(penaltyYards),
            PrePenaltyYardage = max(prePenaltyPlayResult), NetYardage = max(playResult),
            YdsBeforeContact = max(YdsBeforeContact), YdsAfterContact = max(YdsAfterContact),
            OffFormation = max(offenseFormation), TeamDefendersInBox = max(defendersInTheBox),
            EPA = max(expectedPointsAdded),
            foulName1 = max(foulName1), foulName2 = max(foulName2),
            foulNFLId1 = max(foulNFLId1), foulNFLId2 = max(foulNFLId2),
            posteam_type = max(posteam_type), yardline_100 = max(yardline_100),
            sp = max(sp), goal_to_go = max(goal_to_go), 
            air_yards = max(air_yards), yards_after_catch = max(yards_after_catch), 
            air_epa = max(air_epa), yac_epa = max(yac_epa), 
            comp_air_epa = max(comp_air_epa), comp_yac_epa = max(comp_yac_epa),
            pass_touchdown = max(pass_touchdown), 
            xyac_epa = max(xyac_epa), xyac_mean_yardage = max(xyac_mean_yardage), 
            xyac_median_yardage = max(xyac_median_yardage), xyac_success = max(xyac_success), 
            xyac_fd = max(xyac_fd), YACOE = max(YACOE), YAC_EPA_OE = max(YAC_EPA_OE), 
            YAC_Success_OE = max(YAC_Success_OE), YAC_FD_OE = max(YAC_FD_OE), 
            TeamQBHit = max(qb_hit),
            td_team = max(td_team), td_player_name = max(td_player_name),
            td_player_id = max(td_player_id), WPA = max(wpa), DefWPA = max(DefWPA),
            DefTeam_SoloTackle = max(solo_tackle), DefTeam_Safety = max(safety),
            DefTeam_TFL = max(tackled_for_loss), DefTeam_MissedTackles = max(TeamMT_FullPlay),
            TeamTouchdown = max(touchdown),
            TeamReturnTD = max(return_touchdown), OffTeam_Fumble = max(fumble),
            DefTeam_Assist_Tackle = max(assist_tackle),
            DefTeam_Penalized_Tackle = max(TeamDef_Tkl_Pen), TeamDef_Tackle_Clean = max(TeamDef_Tackle_Clean),
            TFL_player_id_1 = max(tackle_for_loss_1_player_id),
            TFL_player_name_1 = max(tackle_for_loss_1_player_name),
            TFL_player_id_2 = max(tackle_for_loss_2_player_id),
            TFL_player_name_2 = max(tackle_for_loss_2_player_name),
            penalty_team = max(penalty_team), penalty_type = max(penalty_type),
            Temperature = max(Temperature), roof = max(roof),
            surface = max(surface), EPSuccess = max(success),
            first_down = max(first_down), out_of_bounds = max(out_of_bounds),
            IndivSoloTackle = max(tackle), IndivAssist = max(assist),
            IndivTotTackles = max(IndivTotTackles), Indiv_MadeTackle = max(Indiv_MadeTackle),
            Indiv_ForcedFumble = max(forcedFumble), Indiv_PenalizedTackle = max(IndivTackle_Penalized),
            IndivTotTackle_Clean = max(IndivTotTackle_Clean),
            Indiv_MissedTackle = max(pff_missedTackle), Indiv_TackleAttempt = max(IndivTackleAttempt),
            PlayDirection = max(playDirection), Initial_X = x[1],
            Initial_Y = y[1], MaxSpeed = max(s), AvgSpeed = mean(s),
            MaxAcceleration = max(a), AvgAcceleration = mean(a),
            TotDistance = sum(dis), InitialOrientation = o[1],
            InitialDirection = dir[1], Ball_X_Frame1 = max(Ball_X_Frame1),
            Ball_Y_Frame1 = max(Ball_Y_Frame1), Initial_X_DistFromBall = X_DistFromBall[1],
            Initial_Y_DistFromBall = Y_DistFromBall[1], Initial_Tot_DistFromBall = TotDistFromBall[1],
            Initial_Y_DistFromMOF = Y_distFromMOF[1], Max_X_AbsDistFromBall = max(X_AbsDistFromBall),
            Max_Y_AbsDistFromBall = max(Y_AbsDistFromBall), Max_Tot_DistFromBall = max(TotDistFromBall),
            height = max(height_inches), weight = max(weight), 
            WPSuccess = max(WPSuccess), QBAlignment = max(QBAlignment), PosGroup = max(PosGroup),
            position = max(position), PlayEPA_TackleAttemptOnly = max(PlayEPA_TackleAttemptOnly),
            PlayEPA_TacklerOnly = max(PlayEPA_TacklerOnly),
            PlayDefWPA_TackleAttemptOnly = max(PlayDefWPA_TackleAttemptOnly),
            PlayDefWPA_TacklerOnly = max(PlayDefWPA_TacklerOnly),
            PlayEPASuccess_TackleAttemptOnly = max(PlayEPASuccess_TackleAttemptOnly),
            PlayEPASuccess_TacklerOnly = max(PlayEPASuccess_TacklerOnly),
            PlayWPASuccess_TackleAttemptOnly = max(PlayWPASuccess_TackleAttemptOnly),
            PlayWPASuccess_TacklerOnly = max(PlayWPASuccess_TacklerOnly))

# Mutate a variable for missing a tackle on a play that still was successful for defense
# Recall EPSuccess and WPSuccess are from offense's point of view (so defense wants them to be 0)
StatsByPlay_Completions <- StatsByPlay_Completions %>% mutate(IndivMT_DefEPSuccess =
      ifelse(Indiv_MissedTackle > 0 & EPSuccess == 0, 1,
            ifelse(Indiv_MissedTackle > 0 & EPSuccess > 0, 0, NA)))
StatsByPlay_Completions <- StatsByPlay_Completions %>% mutate(IndivMT_DefWPSuccess =
      ifelse(Indiv_MissedTackle > 0 & WPSuccess == 0, 1,
            ifelse(Indiv_MissedTackle > 0 & WPSuccess > 0, 0, NA)))

# Example of lowest MT rate on passes with negative air yards
StatsByPlay_Completions_BehindLOS = StatsByPlay_Completions %>%
  filter(air_yards < 0)
IndivStats_Completions_BehindLOS <- StatsByPlay_Completions_BehindLOS %>%
  group_by(nflId, displayName) %>%
  summarize(Plays = n(), TackleAttempts = sum(Indiv_TackleAttempt, na.rm = TRUE),
            SoloTkl_PerPlay = sum(IndivSoloTackle, na.rm = TRUE) / Plays,
            TotalTkl_PerPlay = sum(IndivTotTackles, na.rm = TRUE) / Plays,
            MissedTackleRate = sum(Indiv_MissedTackle, na.rm = TRUE) / sum(Indiv_TackleAttempt, na.rm = TRUE),
            AvgTotalEPA_TackleAttemptsOnly = mean(PlayEPA_TackleAttemptOnly, na.rm = TRUE),
            AvgTotalEPA_TacklesOnly = mean(PlayEPA_TacklerOnly, na.rm = TRUE),
            AvgDefWPA_TackleAttemptsOnly = mean(PlayDefWPA_TackleAttemptOnly, na.rm = TRUE),
            AvgDefWPA_TacklesOnly = mean(PlayDefWPA_TacklerOnly, na.rm = TRUE),
            EPASuccessRate_TackleAttemptsOnly = mean(PlayEPASuccess_TackleAttemptOnly, na.rm = TRUE),
            EPASuccessRate_TacklesOnly = mean(PlayEPASuccess_TacklerOnly, na.rm = TRUE),
            WPASuccessRate_TackleAttemptsOnly = mean(PlayWPASuccess_TackleAttemptOnly, na.rm = TRUE),
            WPASuccessRate_TacklesOnly = mean(PlayWPASuccess_TacklerOnly, na.rm = TRUE),
            MissedTackles_DefEPASuccess = sum(IndivMT_DefEPSuccess, na.rm = TRUE),
            MissedTackles_DefWPASuccess = sum(IndivMT_DefWPSuccess, na.rm = TRUE)) %>%
  filter(TackleAttempts >= 2) %>% # insert your own number here
  arrange(MissedTackleRate)

# Stats by play for scrambles
Scrambles_Merged <- Scrambles_Merged %>% mutate(RunGap_Numeric =
    ifelse(run_location == "left" & run_gap == "end", 7, 
       ifelse(run_location == "left" & run_gap == "tackle", 6,
          ifelse(run_location == "left" & run_gap == "guard", 5,
             ifelse(run_location == "middle" & run_gap == "center", 4,
                  ifelse(run_location == "right" & run_gap == "guard", 3,
                       ifelse(run_location == "right" & run_gap == "tackle", 2, 1)))))))

StatsByPlay_Scrambles <- Scrambles_Merged %>% 
  group_by(gameId, playId, nflId, displayName) %>%
  summarize(Frames = n(), down = max(down), distance = max(ydstogo), Team = max(club),
            PlayerSideOfBall = PlayerSideOfBall[1], HomeTeam = max(homeTeamAbbr), AwayTeam = max(visitorTeamAbbr),
            BallCarrierID = max(ballCarrierId), BallCarrierName = max(ballCarrierDisplayName),
            IsBallCarrier = max(IsBallCarrier), Description = max(playDescription), 
            PosTeam = max(possessionTeam), DefTeam = max(defensiveTeam),
            PenaltyYards = max(penaltyYards),
            PrePenaltyYardage = max(prePenaltyPlayResult), NetYardage = max(playResult),
            YdsBeforeContact = max(YdsBeforeContact), YdsAfterContact = max(YdsAfterContact),
            OffFormation = max(offenseFormation), TeamDefendersInBox = max(defendersInTheBox),
            EPA = max(expectedPointsAdded),
            foulName1 = max(foulName1), foulName2 = max(foulName2),
            foulNFLId1 = max(foulNFLId1), foulNFLId2 = max(foulNFLId2),
            posteam_type = max(posteam_type), yardline_100 = max(yardline_100),
            sp = max(sp), goal_to_go = max(goal_to_go),
            TeamQBHit = max(qb_hit),
            run_location = max(run_location), run_gap = max(run_gap),
            td_team = max(td_team), td_player_name = max(td_player_name),
            td_player_id = max(td_player_id), WPA = max(wpa), DefWPA = max(DefWPA),
            DefTeam_SoloTackle = max(solo_tackle), DefTeam_Safety = max(safety),
            DefTeam_TFL = max(tackled_for_loss), DefTeam_MissedTackles = max(TeamMT_FullPlay),
            TeamTouchdown = max(touchdown), TeamRushTD = max(rush_touchdown),
            TeamReturnTD = max(return_touchdown), OffTeam_Fumble = max(fumble),
            DefTeam_Assist_Tackle = max(assist_tackle),
            DefTeam_Penalized_Tackle = max(TeamDef_Tkl_Pen), TeamDef_Tackle_Clean = max(TeamDef_Tackle_Clean),
            TFL_player_id_1 = max(tackle_for_loss_1_player_id),
            TFL_player_name_1 = max(tackle_for_loss_1_player_name),
            TFL_player_id_2 = max(tackle_for_loss_2_player_id),
            TFL_player_name_2 = max(tackle_for_loss_2_player_name),
            penalty_team = max(penalty_team), penalty_type = max(penalty_type),
            safety_player_name = max(safety_player_name), safety_player_id = max(safety_player_id),
            Temperature = max(Temperature), roof = max(roof),
            surface = max(surface), EPSuccess = max(success),
            first_down = max(first_down), out_of_bounds = max(out_of_bounds),
            IndivSoloTackle = max(tackle), IndivAssist = max(assist),
            IndivTotTackles = max(IndivTotTackles), Indiv_MadeTackle = max(Indiv_MadeTackle),
            Indiv_ForcedFumble = max(forcedFumble), Indiv_PenalizedTackle = max(IndivTackle_Penalized),
            IndivTotTackle_Clean = max(IndivTotTackle_Clean),
            Indiv_MissedTackle = max(pff_missedTackle), Indiv_TackleAttempt = max(IndivTackleAttempt),
            PlayDirection = max(playDirection), Initial_X = x[1],
            Initial_Y = y[1], MaxSpeed = max(s), AvgSpeed = mean(s),
            MaxAcceleration = max(a), AvgAcceleration = mean(a),
            TotDistance = sum(dis), InitialOrientation = o[1],
            InitialDirection = dir[1], Ball_X_Frame1 = max(Ball_X_Frame1),
            Ball_Y_Frame1 = max(Ball_Y_Frame1), Initial_X_DistFromBall = X_DistFromBall[1],
            Initial_Y_DistFromBall = Y_DistFromBall[1], Initial_Tot_DistFromBall = TotDistFromBall[1],
            Initial_Y_DistFromMOF = Y_distFromMOF[1], Max_X_AbsDistFromBall = max(X_AbsDistFromBall),
            Max_Y_AbsDistFromBall = max(Y_AbsDistFromBall), Max_Tot_DistFromBall = max(TotDistFromBall),
            height = max(height_inches), weight = max(weight),
            WPSuccess = max(WPSuccess), QBAlignment = max(QBAlignment), PosGroup = max(PosGroup),
            RunGap_Numeric = max(RunGap_Numeric), position = max(position),
            PlayEPA_TackleAttemptOnly = max(PlayEPA_TackleAttemptOnly),
            PlayEPA_TacklerOnly = max(PlayEPA_TacklerOnly),
            PlayDefWPA_TackleAttemptOnly = max(PlayDefWPA_TackleAttemptOnly),
            PlayDefWPA_TacklerOnly = max(PlayDefWPA_TacklerOnly),
            PlayEPASuccess_TackleAttemptOnly = max(PlayEPASuccess_TackleAttemptOnly),
            PlayEPASuccess_TacklerOnly = max(PlayEPASuccess_TacklerOnly),
            PlayWPASuccess_TackleAttemptOnly = max(PlayWPASuccess_TackleAttemptOnly),
            PlayWPASuccess_TacklerOnly = max(PlayWPASuccess_TacklerOnly))

# Mutate a variable for missing a tackle on a play that still was successful for defense
# Recall EPSuccess and WPSuccess are from offense's point of view (so defense wants them to be 0)
StatsByPlay_Scrambles <- StatsByPlay_Scrambles %>% mutate(IndivMT_DefEPSuccess =
      ifelse(Indiv_MissedTackle > 0 & EPSuccess == 0, 1,
            ifelse(Indiv_MissedTackle > 0 & EPSuccess > 0, 0, NA)))
StatsByPlay_Scrambles <- StatsByPlay_Scrambles %>% mutate(IndivMT_DefWPSuccess =
      ifelse(Indiv_MissedTackle > 0 & WPSuccess == 0, 1,
            ifelse(Indiv_MissedTackle > 0 & WPSuccess > 0, 0, NA)))

# Stats by play for designed runs
StatsByPlay_DesignedRuns <- DesignedRuns_Merged %>% 
  group_by(gameId, playId, nflId, displayName) %>%
  summarize(Frames = n(), down = max(down), distance = max(ydstogo), Team = max(club),
            PlayerSideOfBall = PlayerSideOfBall[1], HomeTeam = max(homeTeamAbbr), AwayTeam = max(visitorTeamAbbr),
            BallCarrierID = max(ballCarrierId), BallCarrierName = max(ballCarrierDisplayName),
            IsBallCarrier = max(IsBallCarrier), Description = max(playDescription), 
            PosTeam = max(possessionTeam), DefTeam = max(defensiveTeam),
            PenaltyYards = max(penaltyYards),
            PrePenaltyYardage = max(prePenaltyPlayResult), NetYardage = max(playResult),
            YdsBeforeContact = max(YdsBeforeContact), YdsAfterContact = max(YdsAfterContact),
            OffFormation = max(offenseFormation), TeamDefendersInBox = max(defendersInTheBox),
            EPA = max(expectedPointsAdded),
            foulName1 = max(foulName1), foulName2 = max(foulName2),
            foulNFLId1 = max(foulNFLId1), foulNFLId2 = max(foulNFLId2),
            posteam_type = max(posteam_type), yardline_100 = max(yardline_100),
            sp = max(sp), goal_to_go = max(goal_to_go),
            run_location = max(run_location), run_gap = max(run_gap),
            td_team = max(td_team), td_player_name = max(td_player_name),
            td_player_id = max(td_player_id), WPA = max(wpa), DefWPA = max(DefWPA),
            DefTeam_SoloTackle = max(solo_tackle), DefTeam_Safety = max(safety),
            DefTeam_TFL = max(tackled_for_loss), DefTeam_MissedTackles = max(TeamMT_FullPlay),
            TeamTouchdown = max(touchdown), TeamRushTD = max(rush_touchdown),
            TeamReturnTD = max(return_touchdown), OffTeam_Fumble = max(fumble),
            DefTeam_Assist_Tackle = max(assist_tackle),
            DefTeam_Penalized_Tackle = max(TeamDef_Tkl_Pen), TeamDef_Tackle_Clean = max(TeamDef_Tackle_Clean),
            TFL_player_id_1 = max(tackle_for_loss_1_player_id),
            TFL_player_name_1 = max(tackle_for_loss_1_player_name),
            TFL_player_id_2 = max(tackle_for_loss_2_player_id),
            TFL_player_name_2 = max(tackle_for_loss_2_player_name),
            penalty_team = max(penalty_team), penalty_type = max(penalty_type),
            safety_player_name = max(safety_player_name), safety_player_id = max(safety_player_id),
            Temperature = max(Temperature), roof = max(roof),
            surface = max(surface), EPSuccess = max(success),
            first_down = max(first_down), out_of_bounds = max(out_of_bounds),
            IndivSoloTackle = max(tackle), IndivAssist = max(assist),
            IndivTotTackles = max(IndivTotTackles), Indiv_MadeTackle = max(Indiv_MadeTackle),
            Indiv_ForcedFumble = max(forcedFumble), Indiv_PenalizedTackle = max(IndivTackle_Penalized),
            IndivTotTackle_Clean = max(IndivTotTackle_Clean),
            Indiv_MissedTackle = max(pff_missedTackle), Indiv_TackleAttempt = max(IndivTackleAttempt),
            PlayDirection = max(playDirection), Initial_X = x[1],
            Initial_Y = y[1], MaxSpeed = max(s), AvgSpeed = mean(s),
            MaxAcceleration = max(a), AvgAcceleration = mean(a),
            TotDistance = sum(dis), InitialOrientation = o[1],
            InitialDirection = dir[1], Ball_X_Frame1 = max(Ball_X_Frame1),
            Ball_Y_Frame1 = max(Ball_Y_Frame1), Initial_X_DistFromBall = X_DistFromBall[1],
            Initial_Y_DistFromBall = Y_DistFromBall[1], Initial_Tot_DistFromBall = TotDistFromBall[1],
            Initial_Y_DistFromMOF = Y_distFromMOF[1], Max_X_AbsDistFromBall = max(X_AbsDistFromBall),
            Max_Y_AbsDistFromBall = max(Y_AbsDistFromBall), Max_Tot_DistFromBall = max(TotDistFromBall),
            height = max(height_inches), weight = max(weight),
            WPSuccess = max(WPSuccess), QBAlignment = max(QBAlignment), PosGroup = max(PosGroup),
            Initial_TotDistFromBall_Rank_BySide = TotDistFromBall_Rank_BySideOfBall[1],
            Initial_Y_AbsDistFromBall_Rank_BySide = Y_AbsDistFromBall_Rank_BySide[1],
            Initial_X_AbsDistFromBall_Rank_BySide = X_AbsDistFromBall_Rank_BySide[1],
            position = max(position), AlignedPos_Box = AlignedPos_Box[1],
            num_left_tes = num_left_tes[1], num_right_tes = num_right_tes[1],
            num_total_tes = num_total_tes[1], IndivDefenderInBox = DefenderInBox[1],
            pre_snap_gap = pre_snap_gap[1], RunGap_Numeric = max(RunGap_Numeric),
            DefAlign_Numeric = max(DefAlign_Numeric), 
            Play_BoxPosition = max(Box_IDLvsEDGE), Primary_BoxPosition = max(Primary_BoxPosition),
            PlayEPA_TackleAttemptOnly = max(PlayEPA_TackleAttemptOnly),
            PlayEPA_TacklerOnly = max(PlayEPA_TacklerOnly),
            PlayDefWPA_TackleAttemptOnly = max(PlayDefWPA_TackleAttemptOnly),
            PlayDefWPA_TacklerOnly = max(PlayDefWPA_TacklerOnly),
            PlayEPASuccess_TackleAttemptOnly = max(PlayEPASuccess_TackleAttemptOnly),
            PlayEPASuccess_TacklerOnly = max(PlayEPASuccess_TacklerOnly),
            PlayWPASuccess_TackleAttemptOnly = max(PlayWPASuccess_TackleAttemptOnly),
            PlayWPASuccess_TacklerOnly = max(PlayWPASuccess_TacklerOnly))

# Mutate a variable for missing a tackle on a play that still was successful for defense
# Recall EPSuccess and WPSuccess are from offense's point of view (so defense wants them to be 0)
StatsByPlay_DesignedRuns <- StatsByPlay_DesignedRuns %>% mutate(IndivMT_DefEPSuccess =
     ifelse(Indiv_MissedTackle > 0 & EPSuccess == 0, 1,
           ifelse(Indiv_MissedTackle > 0 & EPSuccess > 0, 0, NA)))
StatsByPlay_DesignedRuns <- StatsByPlay_DesignedRuns %>% mutate(IndivMT_DefWPSuccess =
     ifelse(Indiv_MissedTackle > 0 & WPSuccess == 0, 1,
           ifelse(Indiv_MissedTackle > 0 & WPSuccess > 0, 0, NA)))

# Make code for "run was near defender's gap"
StatsByPlay_DesignedRuns <- StatsByPlay_DesignedRuns %>%
  mutate(Numeric_RunDistFromDefenderGap = abs(RunGap_Numeric - DefAlign_Numeric))
StatsByPlay_DesignedRuns <- StatsByPlay_DesignedRuns %>%
  mutate(Run_NearDefender = ifelse(Numeric_RunDistFromDefenderGap <= 1, TRUE, FALSE))

StatsByPlay_DesignedRuns_NearDefender = StatsByPlay_DesignedRuns %>%
  filter(Run_NearDefender > 0)
NearbyBoxDefender_Stats_DesignedRuns <- StatsByPlay_DesignedRuns_NearDefender %>%
  group_by(nflId, displayName) %>%
  summarize(Plays = n(), TackleAttempts = sum(Indiv_TackleAttempt, na.rm = TRUE),
            SoloTkl_PerPlay = sum(IndivSoloTackle, na.rm = TRUE) / Plays,
            TotalTkl_PerPlay = sum(IndivTotTackles, na.rm = TRUE) / Plays,
            MissedTackleRate = sum(Indiv_MissedTackle, na.rm = TRUE) / sum(Indiv_TackleAttempt, na.rm = TRUE),
            AvgTotalEPA_TackleAttemptsOnly = mean(PlayEPA_TackleAttemptOnly, na.rm = TRUE),
            AvgTotalEPA_TacklesOnly = mean(PlayEPA_TacklerOnly, na.rm = TRUE),
            AvgDefWPA_TackleAttemptsOnly = mean(PlayDefWPA_TackleAttemptOnly, na.rm = TRUE),
            AvgDefWPA_TacklesOnly = mean(PlayDefWPA_TacklerOnly, na.rm = TRUE),
            EPASuccessRate_TackleAttemptsOnly = mean(PlayEPASuccess_TackleAttemptOnly, na.rm = TRUE),
            EPASuccessRate_TacklesOnly = mean(PlayEPASuccess_TacklerOnly, na.rm = TRUE),
            WPASuccessRate_TackleAttemptsOnly = mean(PlayWPASuccess_TackleAttemptOnly, na.rm = TRUE),
            WPASuccessRate_TacklesOnly = mean(PlayWPASuccess_TacklerOnly, na.rm = TRUE),
            MissedTackles_DefEPASuccess = sum(IndivMT_DefEPSuccess, na.rm = TRUE),
            MissedTackles_DefWPASuccess = sum(IndivMT_DefWPSuccess, na.rm = TRUE)) %>%
  filter(Plays >= 5) %>% # insert your own number here
  arrange(desc(TotalTkl_PerPlay))
