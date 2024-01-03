#necessary packages
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
import numpy as np
import pandas as pd

def is_in_radius(df):
    defensive_df = df.loc[(df.IsPlayerOnOffense == 'defense') & (df.displayName != 'football')]
    defensive_df['In_BallCarrier_Radius'] = (defensive_df['dist_to_ball_carrier'] < 10).astype(int)
    offensive_df = df.loc[df.IsPlayerOnOffense == 'offense']
    df_main = offensive_df.append(defensive_df)
    return df_main

def law_of_cosine_inverse(a, b, c):
    return np.degrees(np.arccos(((a**2 + b**2-c**2 ) / (2 * a * b))))

def number_blockers(frame):
    frame = is_in_radius(frame)
    potential_tacklers = frame.loc[(frame.In_BallCarrier_Radius == 1)]
    blockersDFlist=[]
    unblockedDefenders=[]
    number_of_blockers = []
    for i, ele in potential_tacklers.iterrows():
        radius = ele.dist_to_ball_carrier
        potential_blockers = frame.loc[
            (frame.dist_to_ball_carrier < radius) & (frame.IsPlayerOnOffense == 'offense') &
            (frame.displayName != frame.ballCarrierDisplayName) & (frame.displayName != 'football')
        ]

        potential_blockers['distance']=((potential_blockers.x-ele.x)**2+(potential_blockers.y-ele.y)**2)**.5
        potential_blockers['angles']=law_of_cosine_inverse(radius, potential_blockers.dist_to_ball_carrier,
                                     potential_blockers.distance)

        blockers = (potential_blockers['angles'] < 30).sum()
        blockersDF=potential_blockers.loc[potential_blockers['angles'] < 30]
        if blockersDF.empty==False:
            blockersDF['Defender']=ele.displayName
            blockersDF=blockersDF[['displayName','distance','Defender']]
            blockersDFlist.append(blockersDF)
            
        else:
            unblockedDefenders.append(ele.displayName)
        number_of_blockers.append(blockers)
    #grabbing pairs of blockers
    #Once set up drop returning a Frame
    if blockersDFlist:
        FullBlockerDF=pd.concat(blockersDFlist, ignore_index=True)
        OptimalPairs, UnblockedDefenders2, ExtraBlockers = FindOptimalDefenders(FullBlockerDF)
        unblockedDefenders.extend(UnblockedDefenders2)
        potential_tacklers['NumberOfBlockers'] = number_of_blockers
        potential_tacklers = potential_tacklers[['displayName', 'club', 'NumberOfBlockers']]
        frame = frame.merge(potential_tacklers, on=['displayName', 'club'], how='left')
        return frame,OptimalPairs,unblockedDefenders, ExtraBlockers
    elif unblockedDefenders:
        potential_tacklers['NumberOfBlockers'] = number_of_blockers
        potential_tacklers = potential_tacklers[['displayName', 'club', 'NumberOfBlockers']]
        frame = frame.merge(potential_tacklers, on=['displayName', 'club'], how='left')
        return frame,unblockedDefenders,[],[]
    else:
        return None

def BlockingMechanics(PlayFrame):
    if number_blockers(PlayFrame)!=None:
        Frame,OptimalPairs,unblockedDefenders, ExtraBlockers=number_blockers(PlayFrame)
        Frame['BlockedScore']=None
        OptimalPairs.extend(ExtraBlockers)
        OptimalPairs.extend(unblockedDefenders)
        for pair in OptimalPairs:
            if type(pair)==tuple:
                index_to_insert_into=Frame.index[Frame['displayName']==pair[0]].tolist()[0]
                DefenderRow=Frame.loc[index_to_insert_into]
                blocker_index=Frame.index[Frame['displayName']==pair[1]].tolist()[0]
                BlockerRow=Frame.loc[blocker_index]
                #I am Calling the Players data frame Players DF. If you do not call it this there will be an error
                # either change the code to match your name or vice versa
                BlockedScore=float(np.log(((1-np.cos(DefenderRow.o-BlockerRow.o))*max(1,BlockerRow.a)*PlayersDF.weight.loc[PlayersDF.nflId==BlockerRow.nflId])))
                if Frame.at[index_to_insert_into,'BlockedScore'] is None:
                    Frame.at[index_to_insert_into,'BlockedScore']=BlockedScore
                else:
                    Frame.at[index_to_insert_into,'BlockedScore']+=BlockedScore
            else:
                index_to_insert_into=Frame.index[Frame['displayName']==pair].tolist()[0]
                Frame.at[index_to_insert_into,'BlockedScore']=0
        Frame.BlockedScore.fillna(25,inplace=True)
        Frame.In_BallCarrier_Radius.fillna(-1,inplace=True)
        Frame.NumberOfBlockers.fillna(-1,inplace=True)
        return Frame
    else:
        PlayFrame['In_BallCarrier_Radius']=0
        PlayFrame['BlockedScore']=25
        PlayFrame['NumberOfBlockers']=-1
        return PlayFrame

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
def week_of_blocking_data(df):
    df_temp = df[['gameId', 'playId', 'frameId']].drop_duplicates()
    dfs = []

    for i, j in df_temp.iterrows():
        df_frame = df.loc[(df.gameId == j.gameId) & (df.playId == j.playId) & (df.frameId == j.frameId)]
        df_blockers = BlockingMechanics(df_frame)
        dfs.append(df_blockers)

    result_df = pd.concat(dfs, ignore_index=True)
    return result_df
