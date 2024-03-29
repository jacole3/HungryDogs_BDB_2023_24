# Recall that MergedData was created in the "Initial_DataCleansing_Code" file, and adjusted more in "distances_vectorized"

# Create subsets based on play types (designed runs go back to snap of ball)
MergedData <- MergedData %>% arrange(gameId, playId, nflId, frameId)
DesignedRuns_Merged <- MergedData %>% filter(pass == 0)
Scrambles_Merged <- MergedData %>% filter(passResult == "R")
AllRushes_Merged <- MergedData %>% filter(pass == 0 | passResult == "R")
Completions_Merged <- MergedData %>% filter(passResult == "C")

# Getting rid of pre-snap frames for designed runs
Frames_AtSnap <- DesignedRuns_Merged %>%
  filter(event %in% c("ball_snap", "snap_direct")) %>%
  select(gameId, playId, nflId, displayName, frameId) %>%
  rename(FrameNumber_AtSnap = frameId)

DesignedRuns_Merged <- merge(x = DesignedRuns_Merged, y = Frames_AtSnap, 
                             by = c("playId", "gameId", "nflId", "displayName"), all.x = TRUE)
DesignedRuns_Merged <- DesignedRuns_Merged %>% arrange(gameId, playId, nflId, frameId)

DesignedRuns_Merged <- DesignedRuns_Merged %>% group_by(gameId, playId, nflId) %>% 
  mutate(Unnecessary_Early = ifelse(frameId < FrameNumber_AtSnap, TRUE, FALSE)) %>% 
  ungroup()

DesignedRuns_Merged <- DesignedRuns_Merged %>% filter(Unnecessary_Early == FALSE)
rm(Frames_AtSnap)
DesignedRuns_Merged <- DesignedRuns_Merged %>% select(-c("Unnecessary_Early", "FrameNumber_AtSnap"))

# For dropbacks, get rid of frames where ball-carrier doesn't have ball yet
# For designed runs, this will go in pre-snap alignment code file, b/c we need to define pre-snap gaps first
Scrambles_StartOfRun <- Scrambles_Merged %>%
  filter(event %in% c("run", "lateral")) %>%
  select(gameId, playId, nflId, displayName, frameId) %>%
  rename(FrameNumber_AtStart = frameId)

# Account for plays that could have multiple of these events
Scrambles_StartOfRun <- Scrambles_StartOfRun %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(Frame_Rank = rank(FrameNumber_AtStart, ties.method = "first")) %>%
  ungroup() 
Scrambles_StartOfRun <- Scrambles_StartOfRun %>% filter(Frame_Rank == 1)
Scrambles_StartOfRun <- Scrambles_StartOfRun %>% select(-"Frame_Rank")

Scrambles_Merged <- merge(x = Scrambles_Merged, y = Scrambles_StartOfRun, 
                             by = c("playId", "gameId", "nflId", "displayName"))
Scrambles_Merged <- Scrambles_Merged %>% arrange(gameId, playId, nflId, frameId)

Scrambles_Merged <- Scrambles_Merged %>% group_by(gameId, playId, nflId) %>% 
  mutate(Unnecessary_Early = ifelse(frameId < FrameNumber_AtStart, TRUE, FALSE)) %>% 
  ungroup()

Scrambles_Merged <- Scrambles_Merged %>% filter(Unnecessary_Early == FALSE)
rm(Scrambles_StartOfRun)
Scrambles_Merged <- Scrambles_Merged %>% select(-c("Unnecessary_Early", "FrameNumber_AtStart"))

Completions_Arrival <- Completions_Merged %>%
  filter(event %in% c("pass_outcome_caught")) %>%
  select(gameId, playId, nflId, displayName, frameId) %>%
  rename(FrameNumber_AtStart = frameId)

Completions_Merged <- merge(x = Completions_Merged, y = Completions_Arrival, 
                          by = c("playId", "gameId", "nflId", "displayName"))
Completions_Merged <- Completions_Merged %>% arrange(gameId, playId, nflId, frameId)

Completions_Merged <- Completions_Merged %>% group_by(gameId, playId, nflId) %>% 
  mutate(Unnecessary_Early = ifelse(frameId < FrameNumber_AtStart, TRUE, FALSE)) %>% 
  ungroup()

Completions_Merged <- Completions_Merged %>% filter(Unnecessary_Early == FALSE)
rm(Completions_Arrival)
Completions_Merged <- Completions_Merged %>% select(-c("Unnecessary_Early", "FrameNumber_AtStart"))

MergedData <- rbind(DesignedRuns_Merged, Scrambles_Merged, Completions_Merged)
MergedData <- MergedData %>% arrange(gameId, playId, nflId, frameId)

# Also account for first frame of each play not necessarily being 1 anymore
# I.e., now we start plays when the ball-carrier has the ball
FirstFrames_WithBC <- MergedData %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(Frame_Rank = rank(frameId, ties.method = "first")) %>%
  ungroup() 
FirstFrames_WithBC <- FirstFrames_WithBC %>% filter(Frame_Rank == 1)
FirstFrames_WithBC <- FirstFrames_WithBC %>% 
  select(c("gameId", "playId", "nflId", "displayName", "frameId")) %>%
  rename(FirstFrame_WithBC = frameId)

MergedData <- merge(x = MergedData, y = FirstFrames_WithBC, 
                           by = c("playId", "gameId", "nflId", "displayName"))
MergedData <- MergedData %>% arrange(gameId, playId, nflId, frameId)
rm(FirstFrames_WithBC)
