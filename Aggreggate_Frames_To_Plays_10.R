# Recall that final_merged_data came from the end of the "Final_Model_And_Plots" file

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
            TeamDefendersInBox = max(defendersInTheBox),
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
            Surge_To_EndOfPlay_Frames = max(Surge_To_EndOfPlay_Frames),
            Surge = ifelse(PlayerSideOfBall == "offense", NA, max(within_dist_ofBC)),
            Surge_Prob_Logistic = ifelse(PlayerSideOfBall == "offense", NA,
                      ifelse(max(within_dist_ofBC) == 0, max(pred_within_dist_ofBC_logistic), max_pred_near_BC_FiveFramesEarly)),
            Tackle_Prob_Logistic = ifelse(PlayerSideOfBall == "offense", NA,
                      ifelse(max(IndivTotTackles) == 0, max(pred_tackle_logistic), max_pred_tackle_FiveFramesEarly)))

StatsByPlay_final_merged_data <- StatsByPlay_final_merged_data %>% 
  mutate(Surges_OE_Logistic = Surge - Surge_Prob_Logistic,
         Tackles_OE_Logistic = Indiv_MadeTackle - Tackle_Prob_Logistic)

# Mutate a variable for missing a tackle on a play that still was successful for defense
# Recall EPSuccess and WPSuccess are from offense's point of view (so defense wants them to be 0)
StatsByPlay_final_merged_data <- StatsByPlay_final_merged_data %>% mutate(IndivMT_DefEPSuccess =
    ifelse(Indiv_MissedTackle > 0 & EPSuccess == 0, 1,
           ifelse(Indiv_MissedTackle > 0 & EPSuccess > 0, 0, NA)))
StatsByPlay_final_merged_data <- StatsByPlay_final_merged_data %>% mutate(IndivMT_DefWPSuccess =
    ifelse(Indiv_MissedTackle > 0 & WPSuccess == 0, 1,
           ifelse(Indiv_MissedTackle > 0 & WPSuccess > 0, 0, NA)))

IndivStats_final_merged_data <- StatsByPlay_final_merged_data %>%
  group_by(nflId, displayName) %>% filter(PlayerSideOfBall == "defense") %>%
  summarize(Plays = n(), TackleAttempts = sum(Indiv_TackleAttempt, na.rm = TRUE),
            SoloTkl_PerPlay = sum(IndivSoloTackle, na.rm = TRUE) / Plays,
            SoloTackles = sum(IndivSoloTackle, na.rm = TRUE),
            TotalTkl_PerPlay = sum(IndivTotTackles, na.rm = TRUE) / Plays,
            TotalTackles = sum(IndivTotTackles, na.rm = TRUE),
            MissedTackleRate = sum(Indiv_MissedTackle, na.rm = TRUE) / sum(Indiv_TackleAttempt, na.rm = TRUE),
            AvgTotalEPA = mean(EPA, na.rm = TRUE),
            AvgTotalDefWPA = mean(DefWPA, na.rm = TRUE),
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
            Avg_Surge_To_EndOfPlay_Frames = mean(Surge_To_EndOfPlay_Frames, na.rm = TRUE),
            Tackles_OverExpected = sum(Tackles_OE_Logistic, na.rm = TRUE),
            TackleRate_OverExpected = sum(Tackles_OE_Logistic, na.rm = TRUE) / Plays) %>%
  filter(Plays >= 5) # insert your own number here
 
# Leaderboard for highest surge rate on all plays  
SurgeRate_AllPlays_Leaders <- IndivStats_final_merged_data %>%
  arrange(desc(SurgeRate)) %>%
  select(1:4, "SurgeRate", "SurgeRate_OverExpected", "Surges", "TotalTackles") %>%
  mutate(TacklesToSurge_Ratio = TotalTackles / Surges)

# Leaderboard for most total surges
TotSurges_AllPlays_Leaders <- IndivStats_final_merged_data %>%
  arrange(desc(Surges)) %>% 
  select(1:4, "Surges", "SurgeRate", "SurgeRate_OverExpected", "TotalTackles") %>%
  mutate(TacklesToSurge_Ratio = TotalTackles / Surges)

# Leaderboard for highest surge rate over expected
SurgeRateOE_AllPlays_Leaders <- IndivStats_final_merged_data %>%
  arrange(desc(SurgeRate_OverExpected)) %>% 
  select(1:4, "SurgeRate_OverExpected", "Surges", "SurgeRate", "TotalTackles") %>%
  mutate(TacklesToSurge_Ratio = TotalTackles / Surges)

# Use tab_options() w/ gt package for fancy table for surge rate and surge rate OE
class(SurgeRate_AllPlays_Leaders) <- "data.frame"
SurgeRate_FancyTable <- SurgeRate_AllPlays_Leaders %>%
  select(c("displayName", "Plays", "Surges", "SurgeRate")) %>%
  head(20) %>%
  mutate(Rank = row_number(desc(SurgeRate))) %>%
  select("Rank", 1:4)

SurgeRate_FancyTable <- SurgeRate_FancyTable |>
  # filter(officialPosition %in% c("OLB", "DE")) |> 
  # mutate(rank = row_number()) |> 
  # select(1:4, "SurgeRate", "SurgeRate_OverExpected", "Surges", "TotalTackles") |> 
  head(20) |> 
  gt() |>
  tab_options(
    table.border.top.color = "white",
    row.striping.include_table_body = FALSE
  ) |>
  opt_table_font(
    font = list(
      google_font("Chivo"),
      default_fonts()
    )
  ) |>
  fmt_number(
    columns = c(SurgeRate),
    decimals = 2,
  ) |>
  data_color(
    columns = c(SurgeRate),
    colors = scales::col_numeric(
    palette = c("#FEE0D2", "#67000D"),
    domain = NULL
    )
  ) |> 
  cols_label(
    Rank = md("**Rank**"), # the md is what makes the headers show up as bold
    displayName = md("**Player**"),
    # team = md("**Team**"),
    # officialPosition = md("**Position**"),
    Plays = md("**Snaps**"),
    Surges = md("**Surges**"),
    SurgeRate = html('<span style="text-decoration:overline; font-weight:bold">Surge Rate</span>')
  ) |> 
  cols_align(
    align = "center",
    columns = Plays:SurgeRate
  ) |> 
  tab_header(md("**Top 20 Players in Surge Rate**"),
             md("(Minimum XXX Defensive Snaps; Weeks 1-9, 2022)")) |> # don't forget to adjust your minimum
  tab_style(style = cell_borders(sides = "top"),
            locations = cells_title("title")) |> 
  tab_options(
    table.border.top.style = "a"
  ) |> 
  tab_footnote(
    footnote = "Surge Rate: Surges / Snaps Played",
    locations = cells_column_labels(
      columns = SurgeRate
    )
  )

gtsave(SurgeRate_FancyTable, "SurgeRate_FancyTable.png")

class(SurgeRateOE_AllPlays_Leaders) <- "data.frame"
options(digits = 3)
SurgeRateOE_FancyTable <- SurgeRateOE_AllPlays_Leaders %>%
  select(c("displayName", "Plays", "Surges", "SurgeRate", "SurgeRate_OverExpected")) %>%
  head(20) %>%
  mutate(Rank = row_number(desc(SurgeRate_OverExpected))) %>%
  select("Rank", 1:5)

SurgeRateOE_FancyTable <- SurgeRateOE_FancyTable |>
  # filter(officialPosition %in% c("OLB", "DE")) |> 
  # mutate(rank = row_number()) |> 
  # select(1:4, "SurgeRate", "SurgeRate_OverExpected", "Surges", "TotalTackles") |> 
  head(20) |> 
  gt() |>
  tab_options(
    table.border.top.color = "white",
    row.striping.include_table_body = FALSE
  ) |>
  opt_table_font(
    font = list(
      google_font("Chivo"),
      default_fonts()
    )
  ) |>
  fmt_number(
    columns = c(SurgeRate_OverExpected),
    decimals = 2,
  ) |>
  data_color(
    columns = c(SurgeRate_OverExpected),
    colors = scales::col_numeric(
      palette = c("#FEE0D2", "#67000D"),
      domain = NULL
    )
  ) |> 
  cols_label(
    Rank = md("**Rank**"), # the md is what makes the headers show up as bold
    displayName = md("**Player**"),
    # team = md("**Team**"),
    # officialPosition = md("**Position**"),
    Plays = md("**Snaps**"),
    Surges = md("**Surges**"),
    SurgeRate = md("**Surge Rate**"),
    SurgeRate_OverExpected = html('<span style="text-decoration:overline; font-weight:bold">Surge Rate OE</span>')
  ) |> 
  cols_align(
    align = "center",
    columns = Plays:SurgeRate_OverExpected
  ) |> 
  tab_header(md("**Top 20 Players in Surge Rate Over Expected**"),
             md("(Minimum XXX Defensive Snaps; Weeks 1-9, 2022)")) |> # don't forget to adjust your minimum
  tab_style(style = cell_borders(sides = "top"),
            locations = cells_title("title")) |> 
  tab_options(
    table.border.top.style = "a"
  ) |> 
  tab_footnote(
    footnote = "Surge Rate Over Expected: Total Surges - Expected Surges",
    locations = cells_column_labels(
      columns = SurgeRate_OverExpected
    )
  )

gtsave(SurgeRateOE_FancyTable, "SurgeRateOE_FancyTable.png")

# combine tables
library(magick)
image_append(c(image_read("SurgeRate_FancyTable.png"), image_read("SurgeRateOE_FancyTable.png")))
# Create function to combine two images
image_append_combine <- function(images) {
  combined_image <- image_append(images)
  return(combined_image)
}

# Image file paths
SurgeRate_FancyTable_path <- "~/Desktop/SurgeRate_FancyTable.png"
SurgeRateOE_FancyTable_path <- "~/Desktop/SurgeRateOE_FancyTable.png"

# Read images
SurgeRate_FancyTable_Image <- image_read(SurgeRate_FancyTable_path)
SurgeRateOE_FancyTable_Image <- image_read(SurgeRateOE_FancyTable_path)

# Combine images using your function
combined_SurgeRate_SurgeRateOE_image <- image_append_combine(c(SurgeRate_FancyTable_Image, SurgeRateOE_FancyTable_Image))

# Save the combined image to your desktop
combined_SurgeRate_SurgeRateOE_image_path <- "~/Desktop/combined_SurgeRate_SurgeRateOE_image.png"  # Change the filename or extension as needed
image_write(combined_SurgeRate_SurgeRateOE_image, path = combined_SurgeRate_SurgeRateOE_image_path)

# Leaderboard for highest tackle rate over expected
TackleRateOE_AllPlays_Leaders <- IndivStats_final_merged_data %>%
  arrange(desc(TackleRate_OverExpected)) %>% select(1:4, "TackleRate_OverExpected", "TotalTackles", "Tackles_OverExpected", "TotalTkl_PerPlay")
# Get weighted mean to help scale the metric
TacklesOE_Constant_AllPlays <- weighted.mean(TackleRateOE_AllPlays_Leaders$TackleRate_OverExpected, w = (c(TackleRateOE_AllPlays_Leaders$Plays)))
TacklesOE_Constant_Plays_Mean <- mean(TackleRateOE_AllPlays_Leaders$Plays)
TackleRateOE_AllPlays_Leaders <- TackleRateOE_AllPlays_Leaders %>%
  mutate(TackleRate_OverExpected = TackleRate_OverExpected - (TacklesOE_Constant_AllPlays * Plays / TacklesOE_Constant_Plays_Mean))
TackleRateOE_AllPlays_Leaders <- TackleRateOE_AllPlays_Leaders %>% arrange(desc(TackleRate_OverExpected))

# Get the stats when the defender in question has a surge
StatsByPlay_DefenderHasSurge = StatsByPlay_final_merged_data %>%
  filter(!is.na(Surge) & Surge > 0)

IndivStats_DefenderHasSurge <-  StatsByPlay_DefenderHasSurge %>%
  group_by(nflId, displayName) %>% 
  summarize(Plays = n(), TackleAttempts = sum(Indiv_TackleAttempt, na.rm = TRUE),
            SoloTkl_PerPlay = sum(IndivSoloTackle, na.rm = TRUE) / Plays,
            SoloTackles = sum(IndivSoloTackle, na.rm = TRUE),
            TotalTkl_PerPlay = sum(IndivTotTackles, na.rm = TRUE) / Plays,
            TotalTackles = sum(IndivTotTackles, na.rm = TRUE),
            MissedTackleRate = sum(Indiv_MissedTackle, na.rm = TRUE) / sum(Indiv_TackleAttempt, na.rm = TRUE),
            AvgTotalEPA = mean(EPA, na.rm = TRUE),
            AvgTotalDefWPA = mean(DefWPA, na.rm = TRUE),
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
            Avg_Surge_To_EndOfPlay_Frames = mean(Surge_To_EndOfPlay_Frames, na.rm = TRUE),
            Tackles_OverExpected = sum(Tackles_OE_Logistic, na.rm = TRUE),
            TackleRate_OverExpected = sum(Tackles_OE_Logistic, na.rm = TRUE) / Plays) %>%
  filter(Plays >= 5) # insert your own number here

# Leaderboard for lowest offensive EPA/play when the given defensive player has a surge
LowestEPA_OnSurges_Leaders <- IndivStats_DefenderHasSurge %>%
  arrange(AvgTotalEPA) %>%
  select(1:4, "AvgTotalEPA", "Avg_Surge_To_EndOfPlay_Frames", "TotalTackles") %>%
  mutate(TacklesPerSurge = TotalTackles / Plays) %>%
  rename(Surges = Plays)

# Leaderboard for lowest time remaining to end of snap when the given defensive player has a surge
LowestTimeToEnd_OnSurges_Leaders <- IndivStats_DefenderHasSurge %>%
  arrange(Avg_Surge_To_EndOfPlay_Frames) %>%
  select(1:4, "Avg_Surge_To_EndOfPlay_Frames", "AvgTotalEPA", "TotalTackles") %>%
  mutate(TacklesPerSurge = TotalTackles / Plays) %>%
  rename(Surges = Plays)

# Leaderboard for most tackles per surge
TacklesPerSurge_Leaders <- IndivStats_DefenderHasSurge %>%
  mutate(TacklesPerSurge = TotalTackles / Plays) %>%
  arrange(desc(TacklesPerSurge)) %>%
  select(1:4, "TacklesPerSurge", "Avg_Surge_To_EndOfPlay_Frames", "AvgTotalEPA", "TotalTackles") %>%
  rename(Surges = Plays)

# Use tab_options() w/ gt package for fancy table for tackles per surge and lowest team EPA on surges
class(TacklesPerSurge_Leaders) <- "data.frame"
TacklesPerSurge_FancyTable <- TacklesPerSurge_Leaders %>%
  select(c("displayName", "Surges", "TotalTackles", "TacklesPerSurge")) %>%
  head(20) %>%
  mutate(Rank = row_number(desc(TacklesPerSurge))) %>%
  select("Rank", 1:4)

TacklesPerSurge_FancyTable <- TacklesPerSurge_FancyTable |>
  # filter(officialPosition %in% c("OLB", "DE")) |> 
  # mutate(rank = row_number()) |> 
  # select(1:4, "Surges", "TotalTackles", "TacklesPerSurge") |> 
  head(20) |> 
  gt() |>
  tab_options(
    table.border.top.color = "white",
    row.striping.include_table_body = FALSE
  ) |>
  opt_table_font(
    font = list(
      google_font("Chivo"),
      default_fonts()
    )
  ) |>
  fmt_number(
    columns = c(TacklesPerSurge),
    decimals = 2,
  ) |>
  data_color(
    columns = c(TacklesPerSurge),
    colors = scales::col_numeric(
      palette = c("#FEE0D2", "#67000D"),
      domain = NULL
    )
  ) |> 
  cols_label(
    Rank = md("**Rank**"), # the md is what makes the headers show up as bold
    displayName = md("**Player**"),
    # team = md("**Team**"),
    # officialPosition = md("**Position**"),
    Surges = md("**Surges**"),
    TotalTackles = md("**Surge Tkls**"),
    TacklesPerSurge = html('<span style="text-decoration:overline; font-weight:bold">Tackles Per Surge</span>')
  ) |> 
  cols_align(
    align = "center",
    columns = Surges:TacklesPerSurge
  ) |> 
  tab_header(md("**Top 20 Players in Tackles Per Surge**"),
             md("(Minimum XXX Surges; Weeks 1-9, 2022)")) |> # don't forget to adjust your minimum
  tab_style(style = cell_borders(sides = "top"),
            locations = cells_title("title")) |> 
  tab_options(
    table.border.top.style = "a"
  ) |> 
  tab_footnote(
    footnote = "Surge: Getting Within 1 Yard of Ball-Carrier",
    locations = cells_column_labels(
      columns = TacklesPerSurge
    )
  )

gtsave(TacklesPerSurge_FancyTable, "TacklesPerSurge_FancyTable.png")

class(LowestEPA_OnSurges_Leaders) <- "data.frame"
LowestSurgeEPA_FancyTable <- LowestEPA_OnSurges_Leaders %>%
  select(c("displayName", "Surges", "TacklesPerSurge", "AvgTotalEPA")) %>%
  head(20) %>%
  mutate(Rank = row_number(AvgTotalEPA)) %>%
  select("Rank", 1:5)

LowestSurgeEPA_FancyTable <- LowestSurgeEPA_FancyTable |>
  # filter(officialPosition %in% c("OLB", "DE")) |> 
  # mutate(rank = row_number()) |> 
  # select(1:4, "Surges", "TacklesPerSurge", "AvgTotalEPA") |> 
  head(20) |> 
  gt() |>
  tab_options(
    table.border.top.color = "white",
    row.striping.include_table_body = FALSE
  ) |>
  opt_table_font(
    font = list(
      google_font("Chivo"),
      default_fonts()
    )
  ) |>
  fmt_number(
    columns = c(AvgTotalEPA),
    decimals = 2,
  ) |>
  data_color(
    columns = c(AvgTotalEPA),
    colors = scales::col_numeric(
      palette = c("#FEE0D2", "#67000D"),
      domain = NULL
    )
  ) |> 
  cols_label(
    Rank = md("**Rank**"), # the md is what makes the headers show up as bold
    displayName = md("**Player**"),
    # team = md("**Team**"),
    # officialPosition = md("**Position**"),
    Surges = md("**Surges**"),
    TacklesPerSurge = md("**Tackles Per Surge**"),
    AvgTotalEPA = html('<span style="text-decoration:overline; font-weight:bold">Avg EPA Allowed</span>')
  ) |> 
  cols_align(
    align = "center",
    columns = Surges:AvgTotalEPA
  ) |> 
  tab_header(md("**Top 20 Players with Lowest Team EPA on Their Surges**"),
             md("(Minimum XXX Surges; Weeks 1-9, 2022)")) |> # don't forget to adjust your minimum
  tab_style(style = cell_borders(sides = "top"),
            locations = cells_title("title")) |> 
  tab_options(
    table.border.top.style = "a"
  ) |> 
  tab_footnote(
    footnote = "EPA: Expected Points Added, from Offense's Perspective",
    locations = cells_column_labels(
      columns = AvgTotalEPA
    )
  )

gtsave(LowestSurgeEPA_FancyTable, "LowestSurgeEPA_FancyTable.png")

# combine tables
library(magick)
image_append(c(image_read("TacklesPerSurge_FancyTable.png"), image_read("LowestSurgeEPA_FancyTable.png")))
# Create function to combine two images
image_append_combine <- function(images) {
  combined_image <- image_append(images)
  return(combined_image)
}

# Image file paths
TacklesPerSurge_FancyTable_path <- "~/Desktop/TacklesPerSurge_FancyTable.png"
LowestSurgeEPA_FancyTable_path <- "~/Desktop/LowestSurgeEPA_FancyTable.png"

# Read images
TacklesPerSurge_FancyTable_Image <- image_read(TacklesPerSurge_FancyTable_path)
LowestSurgeEPA_FancyTable_Image <- image_read(LowestSurgeEPA_FancyTable_path)

# Combine images using your function
combined_StatsOnSurges_image <- image_append_combine(c(TacklesPerSurge_FancyTable_Image, LowestSurgeEPA_FancyTable_Image))

# Save the combined image to your desktop
combined_StatsOnSurges_image_path <- "~/Desktop/combined_StatsOnSurges_image.png"  # Change the filename or extension as needed
image_write(combined_StatsOnSurges_image, path = combined_StatsOnSurges_image_path)

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
            TeamDefendersInBox = max(defendersInTheBox),
            EPA = max(expectedPointsAdded),
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
            DefTeam_SoloTackle = max(solo_tackle), 
            DefTeam_TFL = max(tackled_for_loss), DefTeam_MissedTackles = max(TeamMT_FullPlay),
            TeamTouchdown = max(touchdown),
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
            Surge_To_EndOfPlay_Frames = max(Surge_To_EndOfPlay_Frames),
            Surge = ifelse(PlayerSideOfBall == "offense", NA, max(within_dist_ofBC)),
            Surge_Prob_Logistic = ifelse(PlayerSideOfBall == "offense", NA,
                                         ifelse(max(within_dist_ofBC) == 0, max(pred_within_dist_ofBC_logistic), max_pred_near_BC_FiveFramesEarly)),
            Tackle_Prob_Logistic = ifelse(PlayerSideOfBall == "offense", NA,
                                          ifelse(max(IndivTotTackles) == 0, max(pred_tackle_logistic), max_pred_tackle_FiveFramesEarly)))

StatsByPlay_Completions <- StatsByPlay_final_Completions %>% 
  mutate(Surges_OE_Logistic = Surge - Surge_Prob_Logistic,
         Tackles_OE_Logistic = Indiv_MadeTackle - Tackle_Prob_Logistic)

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
  group_by(nflId, displayName) %>% filter(PlayerSideOfBall == "defense") %>%
  summarize(Plays = n(), TackleAttempts = sum(Indiv_TackleAttempt, na.rm = TRUE),
            SoloTkl_PerPlay = sum(IndivSoloTackle, na.rm = TRUE) / Plays,
            SoloTackles = sum(IndivSoloTackle, na.rm = TRUE),
            TotalTkl_PerPlay = sum(IndivTotTackles, na.rm = TRUE) / Plays,
            TotalTackles = sum(IndivTotTackles, na.rm = TRUE),
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
            Avg_Surge_To_EndOfPlay_Frames = mean(Surge_To_EndOfPlay_Frames, na.rm = TRUE),
            Tackles_OverExpected = sum(Tackles_OE_Logistic, na.rm = TRUE),
            TackleRate_OverExpected = sum(Tackles_OE_Logistic, na.rm = TRUE) / Plays) %>%
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
            TeamDefendersInBox = max(defendersInTheBox),
            EPA = max(expectedPointsAdded),
            posteam_type = max(posteam_type), yardline_100 = max(yardline_100),
            sp = max(sp), goal_to_go = max(goal_to_go),
            TeamQBHit = max(qb_hit),
            run_location = max(run_location), run_gap = max(run_gap),
            td_team = max(td_team), td_player_name = max(td_player_name),
            td_player_id = max(td_player_id), WPA = max(wpa), DefWPA = max(DefWPA),
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
            RunGap_Numeric = max(RunGap_Numeric), position = max(position),
            PlayEPA_TackleAttemptOnly = max(PlayEPA_TackleAttemptOnly),
            PlayEPA_TacklerOnly = max(PlayEPA_TacklerOnly),
            PlayDefWPA_TackleAttemptOnly = max(PlayDefWPA_TackleAttemptOnly),
            PlayDefWPA_TacklerOnly = max(PlayDefWPA_TacklerOnly),
            PlayEPASuccess_TackleAttemptOnly = max(PlayEPASuccess_TackleAttemptOnly),
            PlayEPASuccess_TacklerOnly = max(PlayEPASuccess_TacklerOnly),
            PlayWPASuccess_TackleAttemptOnly = max(PlayWPASuccess_TackleAttemptOnly),
            PlayWPASuccess_TacklerOnly = max(PlayWPASuccess_TacklerOnly),
            Surge_To_EndOfPlay_Frames = max(Surge_To_EndOfPlay_Frames),
            Surge = ifelse(PlayerSideOfBall == "offense", NA, max(within_dist_ofBC)),
            Surge_Prob_Logistic = ifelse(PlayerSideOfBall == "offense", NA,
                                         ifelse(max(within_dist_ofBC) == 0, max(pred_within_dist_ofBC_logistic), max_pred_near_BC_FiveFramesEarly)),
            Tackle_Prob_Logistic = ifelse(PlayerSideOfBall == "offense", NA,
                                          ifelse(max(IndivTotTackles) == 0, max(pred_tackle_logistic), max_pred_tackle_FiveFramesEarly)))

StatsByPlay_Scrambles <- StatsByPlay_Scrambles %>% 
  mutate(Surges_OE_Logistic = Surge - Surge_Prob_Logistic,
         Tackles_OE_Logistic = Indiv_MadeTackle - Tackle_Prob_Logistic)

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
            TeamDefendersInBox = max(defendersInTheBox),
            EPA = max(expectedPointsAdded),
            posteam_type = max(posteam_type), yardline_100 = max(yardline_100),
            sp = max(sp), goal_to_go = max(goal_to_go), 
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
            PlayWPASuccess_TacklerOnly = max(PlayWPASuccess_TacklerOnly),
            Surge_To_EndOfPlay_Frames = max(Surge_To_EndOfPlay_Frames),
            Surge = ifelse(PlayerSideOfBall == "offense", NA, max(within_dist_ofBC)),
            Surge_Prob_Logistic = ifelse(PlayerSideOfBall == "offense", NA,
                                         ifelse(max(within_dist_ofBC) == 0, max(pred_within_dist_ofBC_logistic), max_pred_near_BC_FiveFramesEarly)),
            Tackle_Prob_Logistic = ifelse(PlayerSideOfBall == "offense", NA,
                                          ifelse(max(IndivTotTackles) == 0, max(pred_tackle_logistic), max_pred_tackle_FiveFramesEarly)))

StatsByPlay_DesignedRuns <- StatsByPlay_DesignedRuns %>% 
  mutate(Surges_OE_Logistic = Surge - Surge_Prob_Logistic,
         Tackles_OE_Logistic = Indiv_MadeTackle - Tackle_Prob_Logistic)

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
  group_by(nflId, displayName) %>% filter(PlayerSideOfBall == "defense") %>%
  summarize(Plays = n(), TackleAttempts = sum(Indiv_TackleAttempt, na.rm = TRUE),
            SoloTkl_PerPlay = sum(IndivSoloTackle, na.rm = TRUE) / Plays,
            SoloTackles = sum(IndivSoloTackle, na.rm = TRUE),
            TotalTkl_PerPlay = sum(IndivTotTackles, na.rm = TRUE) / Plays,
            TotalTackles = sum(IndivTotTackles, na.rm = TRUE),
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
            Avg_Surge_To_EndOfPlay_Frames = mean(Surge_To_EndOfPlay_Frames, na.rm = TRUE),
            Tackles_OverExpected = sum(Tackles_OE_Logistic, na.rm = TRUE),
            TackleRate_OverExpected = sum(Tackles_OE_Logistic, na.rm = TRUE) / Plays) %>%
  filter(Plays >= 5) %>% # insert your own number here
  
# Leaderboard for who has highest surge rate on rushes within one gap of them
SurgeRate_NearbyRuns_Leaders <- NearbyBoxDefender_Stats_DesignedRuns %>%
arrange(desc(SurgeRate)) %>%
select(1:4, "SurgeRate", "SurgeRate_OverExpected", "Surges", "TotalTackles", "TotalTkl_PerPlay")

# Use tab_options() w/ gt package for fancy table for highest surge rate on nearby runs
class(SurgeRate_NearbyRuns_Leaders) <- "data.frame"
SurgeRate_NearbyRuns_FancyTable <- SurgeRate_NearbyRuns_Leaders %>%
  select(c("displayName", "Plays", "Surges", "SurgeRate")) %>%
  head(20) %>%
  mutate(Rank = row_number(desc(SurgeRate))) %>%
  select("Rank", 1:5)

SurgeRate_NearbyRuns_FancyTable <- SurgeRate_NearbyRuns_FancyTable |>
  # filter(officialPosition %in% c("OLB", "DE")) |> 
  # mutate(rank = row_number()) |> 
  # select(1:4,  "Plays", "Surges", "SurgeRate") |> 
  head(20) |> 
  gt() |>
  tab_options(
    table.border.top.color = "white",
    row.striping.include_table_body = FALSE
  ) |>
  opt_table_font(
    font = list(
      google_font("Chivo"),
      default_fonts()
    )
  ) |>
  fmt_number(
    columns = c(SurgeRate),
    decimals = 2,
  ) |>
  data_color(
    columns = c(SurgeRate),
    colors = scales::col_numeric(
      palette = c("#FEE0D2", "#67000D"),
      domain = NULL
    )
  ) |> 
  cols_label(
    Rank = md("**Rank**"), # the md is what makes the headers show up as bold
    displayName = md("**Player**"),
    # team = md("**Team**"),
    # officialPosition = md("**Position**"),
    Plays = md("**Nearby Runs**"),
    Surges = md("**Surges**"),
    SurgeRate = html('<span style="text-decoration:overline; font-weight:bold">Surge Rate</span>')
  ) |> 
  cols_align(
    align = "center",
    columns = Plays:SurgeRate
  ) |> 
  tab_header(md("**Top 20 Players in Surge Rate on Nearby Runs**"),
             md("(Minimum XXX Nearby Runs; Weeks 1-9, 2022)")) |> # don't forget to adjust your minimum
  tab_style(style = cell_borders(sides = "top"),
            locations = cells_title("title")) |> 
  tab_options(
    table.border.top.style = "a"
  ) |> 
  tab_footnote(
    footnote = "Nearby Runs: Within One Gap of Defender's Pre-Snap Alignment",
    locations = cells_column_labels(
      columns = SurgeRate
    )
  )

gtsave(SurgeRate_NearbyRuns_FancyTable, "SurgeRate_NearbyRuns_FancyTable.png")

StatsByPlay_DesignedRuns_NearDefender_PrimIDL <- StatsByPlay_DesignedRuns_NearDefender %>%
  filter(Primary_BoxPosition == "IDL")
Nearby_Stats_DesignedRuns_PrimIDL <- StatsByPlay_DesignedRuns_NearDefender_PrimIDL %>%
  group_by(nflId, displayName) %>%
  summarize(Plays = n(), TackleAttempts = sum(Indiv_TackleAttempt, na.rm = TRUE),
            SoloTkl_PerPlay = sum(IndivSoloTackle, na.rm = TRUE) / Plays,
            SoloTackles = sum(IndivSoloTackle, na.rm = TRUE),
            TotalTkl_PerPlay = sum(IndivTotTackles, na.rm = TRUE) / Plays,
            TotalTackles = sum(IndivTotTackles, na.rm = TRUE),
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
            Avg_Surge_To_EndOfPlay_Frames = mean(Surge_To_EndOfPlay_Frames, na.rm = TRUE),
            Tackles_OverExpected = sum(Tackles_OE_Logistic, na.rm = TRUE),
            TackleRate_OverExpected = sum(Tackles_OE_Logistic, na.rm = TRUE) / Plays) %>%
  filter(Plays >= 5) %>% # insert your own number here
  arrange(desc(TotalTkl_PerPlay))

StatsByPlay_DesignedRuns_NearDefender_IDLOnPlay <- StatsByPlay_DesignedRuns_NearDefender %>%
  filter(Play_BoxPosition == "IDL")
Nearby_Stats_DesignedRuns_IDLOnPlay <- StatsByPlay_DesignedRuns_NearDefender_IDLOnPlay %>%
  group_by(nflId, displayName)
  summarize(Plays = n(), TackleAttempts = sum(Indiv_TackleAttempt, na.rm = TRUE),
            SoloTkl_PerPlay = sum(IndivSoloTackle, na.rm = TRUE) / Plays,
            SoloTackles = sum(IndivSoloTackle, na.rm = TRUE),
            TotalTkl_PerPlay = sum(IndivTotTackles, na.rm = TRUE) / Plays,
            TotalTackles = sum(IndivTotTackles, na.rm = TRUE),
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
            Avg_Surge_To_EndOfPlay_Frames = mean(Surge_To_EndOfPlay_Frames, na.rm = TRUE),
            Tackles_OverExpected = sum(Tackles_OE_Logistic, na.rm = TRUE),
            TackleRate_OverExpected = sum(Tackles_OE_Logistic, na.rm = TRUE) / Plays) %>%
  filter(Plays >= 5) %>% # insert your own number here
  arrange(desc(TotalTkl_PerPlay))
