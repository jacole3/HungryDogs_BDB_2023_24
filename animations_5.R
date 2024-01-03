setwd("C:/Users/justi/OneDrive/Penn/BigDataBowl")
library(tidyverse)
library(gganimate)
source("https://raw.githubusercontent.com/mlfurman3/gg_field/main/gg_field.R")

#reading in all four files:
tracking_w1 <- read_csv("tracking_week_1.csv")
games<- read_csv("games.csv")
players <- read_csv("players.csv")
plays <- read_csv("plays.csv")


##joining tracking data to the plays data:

tracking_w1 <- tracking_w1 %>%
  left_join(plays, by = c("gameId", "playId")) %>%
  mutate(ball_carrier_indicator = ifelse(ballCarrierId==nflId,1,0)) %>%
  mutate(x = ifelse(playDirection == "left", 120-x, x),
         y = ifelse(playDirection == "left", 53.3 - y, y))

#setting variables: (you can adjust these on your own)
ballcarrier <- 'CeeDee Lamb'
game_id <- 2022091113
play_id <-155


sample_ceedee_pass<- tracking_w1%>%
  filter(ballCarrierDisplayName==ballcarrier & gameId==game_id & playId==play_id)


##disregard this plot, use the next one:
sample_ceedee_pass %>%
  ggplot(aes(x=x, y = y, col=club)) +
  geom_point() +
  transition_states(frameId,
                    transition_length = 1,
                    state_length = 1) +
  ease_aes("linear") +
  theme_bw() +
  geom_text(aes(x = 80, 
                y = 8, label = as.factor(frameId)),
            color='black',
            size=5)


#attributes used for plot. first is away, second is football, third is home.
cols_fill <- c("dodgerblue1", "#663300", "firebrick1")
cols_col <- c("#000000", "#663300", "#000000")
size_vals <- c(6, 4, 6)
shape_vals <- c(21, 16, 21)
plot_title <- sample_ceedee_pass$playDescription
nFrames <- max(sample_ceedee_pass$frameId)

#plotting
anim <- ggplot() +
  
  
  #creating field underlay
  gg_field(yardmin = 0, yardmax = 122) +
  
  #filling forest green for behind back of endzone
  theme(panel.background = element_rect(fill = "forestgreen",
                                        color = "forestgreen"),
        panel.grid = element_blank()) +
  
  
  #setting size and color parameters
  scale_size_manual(values = size_vals, guide = FALSE) + 
  scale_shape_manual(values = shape_vals, guide = FALSE) +
  scale_fill_manual(values = cols_fill, guide = FALSE) + 
  scale_colour_manual(values = cols_col, guide = FALSE) +
  
  
  #adding players
  geom_point(data = sample_ceedee_pass, aes(x = x,
                                                y = y, 
                                                shape = club,
                                                fill = club,
                                                group = nflId,
                                                size = club,
                                                colour = club), 
             alpha = 0.7) +  
  
  #adding jersey numbers
  geom_text(data = sample_ceedee_pass,
            aes(x = x, y = y, label = jerseyNumber),
            colour = "white", 
            vjust = 0.36, size = 3.5) + 
  
  
  #titling plot with play description
  labs(title = plot_title) +
  
  #setting animation parameters
  transition_time(frameId)  +
  ease_aes("linear") + 
  NULL


anim
