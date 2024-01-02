#nessacary packages
import pandas as pd
import matplotlib.pyplot as plt

import math as m
import numpy as np
from scipy.spatial import Voronoi, voronoi_plot_2d
from matplotlib.patches import Circle

#enables me to 
from google.colab import drive
drive.mount('/content/gdrive')

#Week 1 of Data which I got from Justin's code
#all paths for connecting to google drive follow this format
T1Update=pd.read_csv("/content/gdrive/MyDrive/Big Data Bowl/nfl-big-data-bowl-2024/TW1_Updated12.csv")
#These are pretty self explanoatory 
GamesDF=pd.read_csv("/content/gdrive/MyDrive/Big Data Bowl/nfl-big-data-bowl-2024/games.csv")
PlayersDF=GamesDF=pd.read_csv("/content/gdrive/MyDrive/Big Data Bowl/nfl-big-data-bowl-2024/players.csv")
PlaysDF=GamesDF=pd.read_csv("/content/gdrive/MyDrive/Big Data Bowl/nfl-big-data-bowl-2024/plays.csv")


# Function to identify players within a certain radius of the ball carrier
#The current Radius is 10 yds although this can be changed
def is_in_radius(df):
    defensive_df = df.loc[(df.IsPlayerOnOffense == 'defense') & (df.displayName != 'football')]
    defensive_df['In_BallCarrier_Radius'] = (defensive_df['dist_to_ball_carrier'] < 10).astype(int)
    offensive_df = df.loc[df.IsPlayerOnOffense == 'offense']
    df_main = offensive_df.append(defensive_df)
    return df_main

# Function to calculate the angle between three points using the law of cosine inverse
def law_of_cosine_inverse(a, b, c):
    return np.degrees(np.arccos((c**2 - (a**2 + b**2)) / (-2 * a * b)))

# Function to determine the number of blockers for potential tacklers
def number_blockers(frame):
    #Creates dataframe of players where defenders in the Ball Carrier Radius are 1 and defenders outside the radius are zero
    frame = is_in_radius(frame)
    #filters out all none potential tacklers
    potential_tacklers = frame.loc[(frame.In_BallCarrier_Radius == 1)]
    blockersDFlist = []
    unblockedDefenders = []
    number_of_blockers = []
    
    #For each defender we find all offensive players who are potential blockers
    #if they are unblocked we say there are zero blockers 
    #If there are no tacklers in the radius we return none
    for i, ele in potential_tacklers.iterrows():
        radius = ele.dist_to_ball_carrier
        potential_blockers = frame.loc[
            (frame.dist_to_ball_carrier <= radius) & (frame.IsPlayerOnOffense == 'offense') &
            (frame.displayName != frame.ballCarrierDisplayName) & (frame.displayName != 'football')
        ]
        #Using the law of cosine we can find out if a player is in the defenders area
        potential_blockers['distance'] = ((potential_blockers.x - ele.x)**2 + (potential_blockers.x - ele.y)**2)**0.5
        potential_blockers['angles'] = law_of_cosine_inverse(radius, potential_blockers.dist_to_ball_carrier,
                                                             potential_blockers.distance)
        #This angle can be changed so that more are can be included for now lets leave as is
        blockers = (potential_blockers['angles'] < 22.5).sum()
        blockersDF = potential_blockers.loc[potential_blockers['angles'] < 22.5]
       
        if not blockersDF.empty:
            blockersDF['Defender'] = ele.displayName
            blockersDF = blockersDF[['displayName', 'distance', 'Defender']]
            blockersDFlist.append(blockersDF)
        else:
            unblockedDefenders.append(ele.displayName)

        number_of_blockers.append(blockers)

    # If there are blockers or unblocked defenders, find optimal pairs
    if blockersDFlist:
        FullBlockerDF = pd.concat(blockersDFlist, ignore_index=True)
        #Find optimal pairs is a greedy algorithim which matches up players with potential tacklers
        OptimalPairs, UnblockedDefenders2, ExtraBlockers = FindOptimalDefenders(FullBlockerDF)
        unblockedDefenders.extend(UnblockedDefenders2)
        potential_tacklers['NumberOfBlockers'] = number_of_blockers
        potential_tacklers = potential_tacklers[['displayName', 'club', 'NumberOfBlockers']]
        frame = frame.merge(potential_tacklers, on=['displayName', 'club'], how='left')
        return frame, OptimalPairs, unblockedDefenders, ExtraBlockers
    elif unblockedDefenders:
        potential_tacklers['NumberOfBlockers'] = number_of_blockers
        potential_tacklers = potential_tacklers[['displayName', 'club', 'NumberOfBlockers']]
        frame = frame.merge(potential_tacklers, on=['displayName', 'club'], how='left')
        return frame, unblockedDefenders, [], []
    else:
        return None

# Function to calculate blocking mechanics for a given play frame
def BlockingMechanics(PlayFrame):
    #this is the situation if there are potential tacklers 
    if number_blockers(PlayFrame) is not None:
        Frame, OptimalPairs, unblockedDefenders, ExtraBlockers = number_blockers(PlayFrame)
        Frame['BlockedScore'] = None
        OptimalPairs.extend(ExtraBlockers)
        OptimalPairs.extend(unblockedDefenders)

        for pair in OptimalPairs:
            if type(pair) == tuple:
                index_to_insert_into = Frame.index[Frame['displayName'] == pair[0]].tolist()[0]
                DefenderRow = Frame.loc[index_to_insert_into]
                blocker_index = Frame.index[Frame['displayName'] == pair[1]].tolist()[0]
                BlockerRow = Frame.loc[blocker_index]

                # Assuming there's a PlayersDF with columns 'nflId', 'o', 'a', 'weight'
                BlockedScore = float(np.log(
                    (1 - np.cos(DefenderRow.o - BlockerRow.o)) * max(1, BlockerRow.a) * PlayersDF.weight.loc[
                        PlayersDF.nflId == BlockerRow.nflId]))
                if Frame.at[index_to_insert_into, 'BlockedScore'] is None:
                    Frame.at[index_to_insert_into, 'BlockedScore'] = BlockedScore
                else:
                    Frame.at[index_to_insert_into, 'BlockedScore'] += BlockedScore
            else:
                index_to_insert_into = Frame.index[Frame['displayName'] == pair].tolist()[0]
                Frame.at[index_to_insert_into, 'BlockedScore'] = 0

        Frame.BlockedScore.fillna(25, inplace=True)
        Frame.In_BallCarrier_Radius.fillna(-1, inplace=True)
        Frame.NumberOfBlockers.fillna(-1, inplace=True)
        return Frame
    else:
        PlayFrame['In_BallCarrier_Radius'] = 0
        PlayFrame['BlockedScore'] = 25
        PlayFrame['NumberOfBlockers'] = -1
        return PlayFrame

# Function to find optimal defenders based on distances
def FindOptimalDefenders(df):
    df = df.sort_values(by='distance')

    used_X = set()
    used_Y = set()

    optimal_pairs = []
    UnblockedDefenders = []
    ExtraBlockers = []

    for _, row in df.iterrows():
        x, y, distance = row['Defender'], row['displayName'], row['distance']

        if x not in used_X and y not in used_Y:
            optimal_pairs.append((x, y))
            used_X.add(x)
            used_Y.add(y)

    for _, row in df.iterrows():
        x, y, distance = row['Defender'], row['displayName'], row['distance']

        if x not in used_X:
            UnblockedDefenders.append(x)
            used_X.add(x)
        if y not in used_Y:
            ExtraBlockers.append((x, y))
            used_Y.add(y)

    return optimal_pairs, UnblockedDefenders, ExtraBlockers

# Function to process blocking data for a given week
def week_of_blocking_data(df):
    df_temp = df[['gameId', 'playId', 'frameId']].drop_duplicates()
    dfs = []

    for i, j in df_temp.iterrows():
        df_frame = df.loc[(df.gameId == j.gameId) & (df.playId == j.playId) & (df.frameId == j.frameId)]
        df_blockers = BlockingMechanics(df_frame)
        dfs.append(df_blockers)

    result_df = pd.concat(dfs, ignore_index=True)
    return result_df
