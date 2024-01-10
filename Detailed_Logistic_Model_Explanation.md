Below is a more detailed explanation of the model that we discussed in our Kaggle Notebook (https://www.kaggle.com/code/colejacobson/surge-a-measurement-for-hidden-defensive-impact). 

As an integral part of our overall logistic regression model, our group created a measure of blocking, which measured the degree to which each defender was blocked. Additional factors in the larger model included the defender’s distance to the ball, his relative velocity and orientation to the ball-carrier, and the defender’s projected distance from the ball-carrier throughout the next half-second. In more detail:

To account for field congestion, we measured the number of potential blockers between a defender and the ball-carrier. We only considered defenders within 10 yards of the ball-carrier, and for each of those defenders, we drew a sector of a circle from the ball-carrier toward the defender. The ball-carrier represented the center of these circle sectors, and the sector angles totaled 60 degrees, with 30 degrees extending each way from the ball-carrier, as shown below. We arbitrarily chose 60 degrees as being the defender’s region of influence based on the eye test, which is one potential weakness of this approach. Additionally, we could’ve dynamically adjusted the sector angles based on the defender’s distance to the ball-carrier.

Understanding blocking is more than understanding the number of potential blockers between the defender and the ball-carrier. Our group used a greedy optimization algorithm to pair up defenders and blockers. We took all of the potential pairs of blockers and their defenders and sorted them by distance; blockers who are closest to a defender are most likely to block them. This pair is then set in stone, meaning that blocker cannot be blocking any other defender. (We understand George Kittle will disagree, but he’s the exception, not the rule.) Furthermore, unassigned blockers who have the ability to block an unblocked defender will be matched to that defender instead of an already blocked defender. Once all unique pairs have been selected, any remaining blockers are assigned to the closest defender, while any remaining defenders are left unblocked. The overall score for how well a defender is blocked, which we named “Blocked Score,” is quantified by the equation:

 ⅀ln(1-cos(O2-O1)* Mass of Blocker * Speed of Blocker)
 
This represents the blocker’s momentum (aka speed * mass), multiplied by his relative orientation to the defender. A blocker whose orientation is facing the defender will have the maximum blocked score. We then normalized these scores to be between zero and one (i.e., one means a player is as blocked as possible, while zero means he is completely unblocked).

Using basic Pythagorean Theorem principles, we tracked the current distance of the defender to the ball-carrier. 

While accounting for players’ angle and speed, we also created a projected future X/Y position. In other words, if we were projecting a player’s location 0.5 seconds into the future, we would multiply his current speed by that time value. (We also tested a kinematics-based approach that involved acceleration, but tests showed this was a less accurate approach.) However, looking exactly half a second into the future wouldn’t properly account for situations where a defender was already close to the ball-carrier and moving very fast, as the large projected distance would be overstated. To resolve this issue, we looked at the projected coordinates 0.1, 0.2, 0.3, 0.4, and 0.5 seconds into the future and took the minimum projected distance between each defender and the ball-carrier over that time frame. This was an extremely valuable metric because it captures many details about player movement. While it obviously accounts for speed and direction, it also can measure blocking to a degree, as more heavily impeded players should have trouble lowering the distance between themselves and the ball-carrier.

Lastly, we give each player within the 10-yard radius a ranking based off of his distance to the ball-carrier (e.g., if there is one defensive player and one offensive player between Defender X and the ball-carrier, Defender X’s rank in this regard would be 3).

Using these features, our group used logistic regression to predict whether a player will be within a yard of the ball-carrier -- which we defined as a "Surge" in our project -- in the next half-second. We ultimately settled on the following predictor variables:

Current distance from defender to ball-carrier, minimum projected distance from defender to ball-carrier over the next half-second, an interaction between the current distance and minimum projected distance, the defender’s rank in terms of current distance from the ball among all players, number of blockers, Blocked Score , an interaction between Blocked Score and minimum projected distance from defender to ball-carrier

The actual R code for the model was the following:

mod_logistic <- glm(within_dist_ofBC_frames_ahead ~ dist_to_ball_carrier * min_proj_dist_to_ball_carrier +
                      TotDistFromBall_Rank_OVR + NumberOfBlockers + min_proj_dist_to_ball_carrier * BlockedScore, 
                    data = final_merged_data_sub, family = 'binomial')
summary(mod_logistic)
