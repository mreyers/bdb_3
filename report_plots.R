set.seed(3459)
#libraries
library(tidyverse)
library(mvtnorm) #might be possible with MASS
library(zoo)
plays <- read_csv("plays.csv")
players <- read_csv("players.csv")
games <- read_csv("games.csv")

source("define_pc2.R")
source("plot_field.R")

files <- 
  tibble(files = list.files()) %>%
  filter(str_detect(files, "^week\\d{1,2}\\.csv"))   

tracking_data <- 
  read_csv("week1.csv") %>%
  mutate(x = if_else(playDirection == "left", 120-x, x),
         y = if_else(playDirection == "left", 160/3 - y, y),
         dir = if_else(playDirection == "left", (180 + dir) %% 360, dir)) 

ball_data <- 
  tracking_data %>%
  filter(team == "football") %>%
  select(frameId, gameId, playId, ball_x = x, ball_y = y)

nfl_tracking_data <-
  tracking_data %>%
  filter(team != "football") %>%
  left_join(ball_data) %>%
  mutate(time = frameId/10) %>%
  select(gameId, playId, playDirection, time, frameId, nflId, jerseyNumber, position, route, x, y, s, a, dis, o, dir, event, team, ball_x, ball_y) %>%
  filter(position != "QB")


#again very simple for illustrative purposes
get_theta <- function(dir) {
  
  #theta = (dir - 90) %% 360
  #theta = if_else(theta > 180, 360 - theta, theta) * pi/180
  theta = dir * pi / 180
  return(theta)
}
#first add in the lead x/y to ease processing 
nfl_tracking_data <- 
  nfl_tracking_data %>%
  group_by(nflId, team, playId, gameId) %>%
  #player x,y and time at t + n
  mutate(next_x = lead(x), next_y = lead(y), next_time = lead(time)) %>%
  #to develop velocity arrows per player
  mutate(forward_x = lead(x, 10), forward_y = lead(y, 10),
         ball_dist = sqrt((x - ball_x)^2 + (y - ball_y)^2),
         theta = get_theta(dir)) %>%
  ungroup() 


#no real reason for these to be functions, but just to
#make it more obvious what we're doing
get_speed_x <- function(speed, theta) {
  #speed in meters per second
  speed = speed * sin(theta)
  return(speed)
}

get_speed_y <- function(speed, theta) {
  #speed in meters per second
  speed = speed * cos(theta)
  return(speed)
}

#another simple function to find mu
get_mu <- function(location, speed, ball_dist, s) {
  #mu = location + speed * ball_dist/(20 - s)
  mu = location + speed * 0.5
  return(mu)
}

nfl_tracking_data %>%
  filter(s == max(s))
get_srat <- function(s) {
  #find total velocity
  srat = (s / 12)^2
  return(srat)
}


#little bit more complicated but still easy
get_ri <- function(ball_dist) {
  ri = 4 + ((ball_dist^3) / ((18^3) / 6))
  return(min(ri, 10))  
  #ri = 3 + ((ball_dist^3) / ((18^3) / 6))
  #return(if_else(ri > 18, ball_dist * 0.7 + 0.15, ri))  
}

get_R <- function(theta) {
  #R fills down first so these aren't the wrong way round
  R = matrix(c(sin(theta), cos(theta), -cos(theta), sin(theta)), nrow = 2)
  return(R)
}

get_S <- function(ri, srat) {
  top_left <- ri * (1 + srat) / 2
  bottom_right <- ri * (1-srat) / 2
  S = matrix(c(top_left, 0, 0, bottom_right), nrow = 2)
}


get_Sigma <- function(R, S) {
  inv_R <- solve(R)
  Sigma = R %*% S %*% S %*% inv_R
  return(Sigma)
}


#use statsbomb coords - 120m x 80m pitch
#split into 200x200 rectangles
pitch <- expand.grid(seq(0, 120, length.out = 200), seq(0, 53.3, length.out = 200)) %>%
  rename(x = Var1, y = Var2)


#function to calculate I as in equation 1/13
calc_I <- function(pitch_area, x, y, mu_x, mu_y, Sigma) {
  #create vectors
  mu <- c(mu_x, mu_y)
  player_loc <- c(x, y)
  
  numerator <- dmvnorm(as.matrix(pitch_area), mu, Sigma)
  denominator <- dmvnorm(t(mu), mu, Sigma)
  #and normalise
  norm_pdf = numerator/denominator
  return(norm_pdf)
}

#function to calculate I as in equation 1/13
calc_I_old <- function(pitch_area, x, y, mu_x, mu_y, Sigma) {
  #create vectors
  mu <- c(mu_x, mu_y)
  player_loc <- c(x, y)
  
  numerator <- dmvnorm(as.matrix(pitch_area), mu, Sigma)
  denominator <- dmvnorm(t(matrix(player_loc)), mu, Sigma)
  #and normalise
  norm_pdf = numerator/denominator
  return(norm_pdf)
}

#test our functions on one frame of the tracking data
testing_data <- nfl_tracking_data %>%
  filter(event == "pass_forward", playId == 3264, gameId == 2018090903) 

#sum all our little functions into one bigger function
calc_PC <- function(time, next_time, ball_x, ball_y, ball_dist, x, y, s, theta, team, nflId, pitch_area) {
  speed_x <- get_speed_x(s, theta)
  speed_y <- get_speed_y(s, theta)
  srat <- get_srat(s)
  
  mu_x <- get_mu(x, speed_x, ball_dist, s)
  mu_y <- get_mu(y, speed_y, ball_dist, s)
  
  ri <- get_ri(ball_dist)
  
  R <- get_R(theta)
  S <- get_S(ri, srat)
  
  Sigma <- get_Sigma(R, S)
  
  pitch_area$I <- calc_I_old(as.matrix(pitch), x, y, mu_x, mu_y, Sigma)
  pitch_area$team <- team
  pitch_area$time <- time
  pitch_area$nflId <- nflId
  return(pitch_area)
}

#run the pitch control function
pitch_control_old <- 
  testing_data %>%
  select(time, next_time, ball_x, ball_y, ball_dist, x, y, s, theta, nflId, team) %>% 
  #run func
  pmap_df(., calc_PC, pitch_area = pitch) %>%
  #sum by team and area
  group_by(team, x, y) %>%
  #summarise(team_sum = max(I)) %>%
  summarise(team_sum = sum(I)) %>%
  pivot_wider(names_from = team, values_from = team_sum) %>%
  #σ - logistic function
  mutate(PC = 1 / (1 + exp(home - away)))

#run the pitch control function
pitch_control <- 
  testing_data %>%
  select(time, next_time, ball_x, ball_y, ball_dist, x, y, s, theta, nflId, team) %>% 
  #run func
  pmap_df(., calc_PC2, pitch_area = pitch) %>%
  #sum by team and area
  group_by(team, x, y) %>%
  #summarise(team_sum = max(I)) %>%
  summarise(team_sum = max(I)) %>%
  pivot_wider(names_from = team, values_from = team_sum) %>%
  #σ - logistic function
  mutate(PC = 1 / (1 + exp(home - away)))



pitch_control_new <-
  testing_data %>%
  select(time, next_time, ball_x, ball_y, ball_dist, x, y, s, theta, nflId, team) %>% 
  #run func
  pmap_df(., calc_PC2, pitch_area = pitch) %>%
  filter(I > 1e-10) %>%
  group_by(time, x, y, team) %>%
  filter(I == max(I)) %>%
  group_by(time, x, y) %>%
  mutate(PC = 1/ ( 1 + exp(max(I) - min(I))),
         PC = if_else(team == "away", PC, 1 - PC)) %>%
  filter(I == max(I)) %>%
  filter(I > 0.1) 

#get the position of the ball for this frame
ball_location <- testing_data %>%
  select(ball_x, ball_y) %>%
  unique()

#plot it all
p6 <- 
  plot_field(y_min = 22, y_max = 95) +
  #pitch layout background
  #annotate_pitch(dimensions = pitch_statsbomb) +
  #pitch control raster
  geom_tile(data = pitch_control_new, aes(x = 160/3 - y, y = x, fill = PC), alpha = 0.7) +
  scale_fill_gradient2(low = "black", high = "red", mid = "white", midpoint = 0.5) +
  #players for each team
  #also add in little vector arrows
  geom_segment(data = testing_data, aes(x =  160/3 - y, y = x, yend = x + s * sin(theta), xend =  160/3 - (y  + s* cos(theta)), colour = team),
               size = 1, arrow = arrow(length = unit(0.01, "npc"))) +
  geom_point(data = testing_data, aes(x = 160/3 -  y, y = x, colour = team), size = 6) +
  #geom_point(data = arrival, aes(x = x, y = y, colour = team), size = 3) +
  geom_text(data = testing_data, aes(x =  160/3 - y, y = x, label = jerseyNumber), colour = "white", size = 3) +
  scale_colour_manual(values = c("red", "black"), breaks = c("home", "away")) +
  #ball location
  geom_point(data = ball_location, aes(y = ball_x, x = 160/3 -  ball_y),
             colour = "black", fill = "white", shape = 21, size = 2.5, stroke = 2)+
  labs(title = "New Completion Ownership Model")

p6_old <- 
  plot_field(y_min = 22, y_max = 95) +
  #pitch layout background
  #annotate_pitch(dimensions = pitch_statsbomb) +
  #pitch control raster
  geom_tile(data = pitch_control_old, aes(x = 160/3 -  y, y = x, fill = 1 - PC), alpha = 0.7) +
  scale_fill_gradient2(low = "black", high = "red", mid = "white", midpoint = 0.5) +
  #players for each team
  #also add in little vector arrows
  geom_segment(data = testing_data, aes(x =  160/3 - y, y = x, yend = x + s * sin(theta), xend =  160/3 - (y  + s* cos(theta)), colour = team),
               size = 1, arrow = arrow(length = unit(0.01, "npc"))) +
  geom_point(data = testing_data, aes(x =  160/3 - y, y = x, colour = team), size = 6) +
  #geom_point(data = arrival, aes(x = x, y = y, colour = team), size = 3) +
  geom_text(data = testing_data, aes(x =  160/3 - y, y = x, label = jerseyNumber), colour = "white", size = 3) +
  scale_colour_manual(values = c("red", "black"), breaks = c("home", "away")) +
  #ball location
  geom_point(data = ball_location, aes(y = ball_x, x =  160/3 -  ball_y),
             colour = "black", fill = "white", shape = 21, size = 2.5, stroke = 2)+
  labs(title = "Soccer Pitch Ownership Model")
#theme_pitch()



p6_old + p6





ggsave("space_generation_num1.png")








#test our functions on one frame of the tracking data
testing_data <- nfl_tracking_data %>%
  filter(time == 2.8, playId == 3264, gameId == 2018090903) 


pitch_control_new <-
  testing_data %>%
  select(time, next_time, ball_x, ball_y, ball_dist, x, y, s, theta, nflId, team) %>% 
  #run func
  pmap_df(., calc_PC2, pitch_area = pitch) %>%
  filter(I > 1e-10) %>%
  group_by(time, x, y, team) %>%
  filter(I == max(I)) %>%
  group_by(time, x, y) %>%
  mutate(PC = 1/ ( 1 + exp(max(I) - min(I))),
         PC = if_else(team == "away", PC, 1 - PC)) %>%
  filter(I == max(I)) %>%
  filter(I > 0.1) 

#get the position of the ball for this frame
ball_location <- testing_data %>%
  select(ball_x, ball_y) %>%
  unique()

#plot it all
p28 <- 
  plot_field(y_min = 22, y_max = 95) +
  #pitch layout background
  #annotate_pitch(dimensions = pitch_statsbomb) +
  #pitch control raster
  geom_tile(data = pitch_control_new, aes(x =  160/3 - y, y = x, fill = PC), alpha = 0.7) +
  scale_fill_gradient2(low = "black", high = "red", mid = "white", midpoint = 0.5) +
  #players for each team
  #also add in little vector arrows
  geom_segment(data = testing_data, aes(x =  160/3 - y, y = x, yend = x + s * sin(theta), xend =  160/3 - (y  + s* cos(theta)), colour = team),
               size = 1, arrow = arrow(length = unit(0.01, "npc"))) +
  geom_point(data = testing_data, aes(x =  160/3 - y, y = x, colour = team), size = 6) +
  #geom_point(data = arrival, aes(x = x, y = y, colour = team), size = 3) +
  geom_text(data = testing_data, aes(x =  160/3 - y, y = x, label = jerseyNumber), colour = "white", size = 3) +
  scale_colour_manual(values = c("red", "black"), breaks = c("home", "away")) +
  #ball location
  geom_point(data = ball_location, aes(y = ball_x, x =  160/3 - ball_y),
             colour = "black", fill = "white", shape = 21, size = 2.5, stroke = 2)+
  labs(title = "1.7 Seconds after Snap")

p28



#test our functions on one frame of the tracking data
testing_data <- nfl_tracking_data %>%
  filter(time == 3.3, playId == 3264, gameId == 2018090903) 


pitch_control_new <-
  testing_data %>%
  select(time, next_time, ball_x, ball_y, ball_dist, x, y, s, theta, nflId, team) %>% 
  #run func
  pmap_df(., calc_PC2, pitch_area = pitch) %>%
  filter(I > 1e-10) %>%
  group_by(time, x, y, team) %>%
  filter(I == max(I)) %>%
  group_by(time, x, y) %>%
  mutate(PC = 1/ ( 1 + exp(max(I) - min(I))),
         PC = if_else(team == "away", PC, 1 - PC)) %>%
  filter(I == max(I)) %>%
  filter(I > 0.1) 

#get the position of the ball for this frame
ball_location <- testing_data %>%
  select(ball_x, ball_y) %>%
  unique()

#plot it all
p33 <- 
  plot_field(y_min = 22, y_max = 95) +
  #pitch layout background
  #annotate_pitch(dimensions = pitch_statsbomb) +
  #pitch control raster
  geom_tile(data = pitch_control_new, aes(x =  160/3 - y, y = x, fill = PC), alpha = 0.7) +
  scale_fill_gradient2(low = "black", high = "red", mid = "white", midpoint = 0.5) +
  #players for each team
  #also add in little vector arrows
  geom_segment(data = testing_data, aes(x =  160/3 - y, y = x, yend = x + s * sin(theta), xend =  160/3 - (y  + s* cos(theta)), colour = team),
               size = 1, arrow = arrow(length = unit(0.01, "npc"))) +
  geom_point(data = testing_data, aes(x =  160/3 - y, y = x, colour = team), size = 6) +
  #geom_point(data = arrival, aes(x = x, y = y, colour = team), size = 3) +
  geom_text(data = testing_data, aes(x =  160/3 - y, y = x, label = jerseyNumber), colour = "white", size = 3) +
  scale_colour_manual(values = c("red", "black"), breaks = c("home", "away")) +
  #ball location
  geom_point(data = ball_location, aes(y = ball_x, x =  160/3 - ball_y),
             colour = "black", fill = "white", shape = 21, size = 2.5, stroke = 2)+
  labs(title = "2.2 Seconds after Snap")

p28 + p33



#test our functions on one frame of the tracking data
testing_data <- nfl_tracking_data %>%
  filter(time == 3.8, playId == 3264, gameId == 2018090903) 


pitch_control_new <-
  testing_data %>%
  select(time, next_time, ball_x, ball_y, ball_dist, x, y, s, theta, nflId, team) %>% 
  #run func
  pmap_df(., calc_PC2, pitch_area = pitch) %>%
  filter(I > 1e-10) %>%
  group_by(time, x, y, team) %>%
  filter(I == max(I)) %>%
  group_by(time, x, y) %>%
  mutate(PC = 1/ ( 1 + exp(max(I) - min(I))),
         PC = if_else(team == "away", PC, 1 - PC)) %>%
  filter(I == max(I)) %>%
  filter(I > 0.1) 

#get the position of the ball for this frame
ball_location <- testing_data %>%
  select(ball_x, ball_y) %>%
  unique()

#plot it all
p38 <- 
  plot_field(y_min = 22, y_max = 95) +
  #pitch layout background
  #annotate_pitch(dimensions = pitch_statsbomb) +
  #pitch control raster
  geom_tile(data = pitch_control_new, aes(x =  160/3 - y, y = x, fill = PC), alpha = 0.7) +
  scale_fill_gradient2(low = "black", high = "red", mid = "white", midpoint = 0.5) +
  #players for each team
  #also add in little vector arrows
  geom_segment(data = testing_data, aes(x =  160/3 - y, y = x, yend = x + s * sin(theta), xend =  160/3 - (y  + s* cos(theta)), colour = team),
               size = 1, arrow = arrow(length = unit(0.01, "npc"))) +
  geom_point(data = testing_data, aes(x =  160/3 - y, y = x, colour = team), size = 6) +
  #geom_point(data = arrival, aes(x = x, y = y, colour = team), size = 3) +
  geom_text(data = testing_data, aes(x =  160/3 - y, y = x, label = jerseyNumber), colour = "white", size = 3) +
  scale_colour_manual(values = c("red", "black"), breaks = c("home", "away")) +
  #ball location
  geom_point(data = ball_location, aes(y = ball_x, x =  160/3 - ball_y),
             colour = "black", fill = "white", shape = 21, size = 2.5, stroke = 2)+
  labs(title = "2.7 Seconds after Snap")

p28 + p33 + p38


ggsave("space_generation_num2.png")


