from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score, classification_report
import xgboost as xgb
import numpy as np
import pandas as pd
import math
def FixingBlockingScore(DF):
  DFTemp=DF.loc[(DF.BlockedScore!=0) & (DF.BlockedScore!=25)&(DF.a>=1)]
  DFTemp2=DF.loc[(DF.BlockedScore!=0) & (DF.BlockedScore!=25)&(DF.a<1)]
  DFTemp2.BlockedScore=DFTemp2.BlockedScore*DFTemp2.s
  DFTemp.BlockedScore=(DFTemp.BlockedScore/DFTemp.a)*DFTemp.s
  DFTempWhole=pd.concat([DFTemp,DFTemp2],ignore_index=True)
  DF=DF.merge(DFTempWhole, on=list(DFTempWhole.columns),how='left')
  DF.BlockedScore.replace(25,np.nan,inplace=True)
  return DF
def Alpha(Ax,Ay,Bx,By,Cx,Cy,Dx,Dy):
  Top=(Cy-Ay)*Dx-(Cx-Ax)*Dy
  Bottom=(Bx*Dy)-(By*Dx)
  if Bottom==0:
    return -1
  return Top / Bottom
def Beta(Ax,Ay,Bx,By,Cx,Cy,Dx,Dy):
  Top=(Cy-Ay)*Bx-(Cx-Ax)*By
  Bottom=(Bx*Dy)-(By*Dx)
  if Bottom==0:
    return -1
  return Top / Bottom
def PlayerTackleIntersection(Frame,nflId):
    Row=Frame.loc[Frame.nflId==nflId]
    Week1Offense=Frame.loc[(Frame.IsPlayerOnOffense=='offense') & (Frame.dist_to_ball_carrier<10)]
    RowX=(Row.x)
    RowY=(Row.y)
    RowXProj=(Row.X_proj)
    RowYProj=(Row.Y_proj)
    Week1Offense['AlphaTop']=(Week1Offense.y-(RowY))*Week1Offense.X_proj - (Week1Offense.x-(RowX))*Week1Offense.Y_proj
    Week1Offense['AlphaBottom']=((RowXProj)*Week1Offense.Y_proj)-((RowYProj)-Week1Offense.X_proj)
    Week1Offense['Alpha']=Week1Offense['AlphaTop'] / Week1Offense['AlphaBottom']
    Week1Offense['BetaTop']=(Week1Offense.y-(RowY))*(RowXProj) - (Week1Offense.x-(RowX))*(RowYProj)
    Week1Offense['BetaBottom']=((RowXProj)*Week1Offense.Y_proj)-((RowYProj)-Week1Offense.X_proj)
    Week1Offense['Beta']=Week1Offense['BetaTop'] / Week1Offense['BetaBottom']
    Intersection=Week1Offense.loc[(Week1Offense.Beta>0)& (Week1Offense.Beta<1) & (Week1Offense.Alpha>0)& (Week1Offense.Alpha<1)]
    Intersection['x0']=Intersection.x+Intersection.Beta*(Intersection.X_proj-Intersection.x)
    Intersection['y0']=Intersection.y+Intersection.Beta*(Intersection.Y_proj-Intersection.y)
    Intersection['DistanceFromStart']=((Intersection.x0-Intersection.x)**2+(Intersection.y0-Intersection.y)**2)**.5
    Intersection['DefenseDistance']=((Intersection.x0-(RowX))**2+(Intersection.y0-(RowY))**2)**.5
    Intersection['PositiveOffenseTime']=-1*(Intersection.s)+((Intersection.s**2)+2*Intersection.a*Intersection.DistanceFromStart)**.5
    Intersection['NegativeOffenseTime']=-1*(Intersection['s'])-(Intersection['s']**2+2*Intersection['a']*Intersection['DistanceFromStart'])**.5
    Intersection['PositiveDefesneTime']=-1*((Row['s']))+((Row['s'])**2+2*(Row['a'])*Intersection['DefenseDistance'])**.5
    Intersection['NegativeDefenseTime']=-1*((Row['s']))-((Row['s'])**2+2*(Row['a'])*Intersection['DefenseDistance'])**.5
    Intersection['OffenseTime']=Intersection[['PositiveOffenseTime','NegativeOffenseTime']].max(axis=1)
    Intersection['DefenseTime']=Intersection[['PositiveDefesneTime','NegativeDefenseTime']].max(axis=1)
    Intersection=Intersection.loc[Intersection.DefenseTime>Intersection.OffenseTime]
    if Intersection.empty:
      Row['Intersection']=0
    else:
      #print(Intersection[['Alpha','Beta','OffenseTime','DefenseTime']])
      Row['Intersection']=1
    return Row
def updateintersection(DF):
  DFDefense=DF.loc[(DF.IsPlayerOnOffense=='defense') & (DF.In_BallCarrier_Radius==1)]
  rowlist=[]
  for i,j in DFDefense.iterrows():
    Frame=DF.loc[(DF.playId==int(j.playId)& (DF.gameId==int(j.gameId))&(DF.frameId==int(j.frameId)))]
    row=PlayerTackleIntersection(Frame,int(j.nflId))
    rowlist.append(row)
  modelDF=pd.concat(rowlist,ignore_index=True)
  return modelDF