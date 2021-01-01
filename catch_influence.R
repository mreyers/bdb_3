# Need data in main folder named week1, week2 etc
# Need define_pc2R in main folder
# Need targetedReceiver.csv in main folder

set.seed(3459)
#libraries
library(tidyverse)
library(mvtnorm) #might be possible with MASS
library(zoo)
plays <- read_csv("plays.csv")
players <- read_csv("players.csv")
games <- read_csv("games.csv")

source("define_pc2.R")

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
  select(gameId, playId, playDirection, time, frameId, nflId, position, route, x, y, s, a, dis, o, dir, event, team, ball_x, ball_y) %>%
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
  mu = location + speed * ball_dist/(20 - s)
  #mu = location + speed * 0.5
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
  #ri = 4 + ((ball_dist^3) / ((18^3) / 6))
  #return(min(ri, 10))  
  ri = 3 + ((ball_dist^3) / ((18^3) / 6))
  return(if_else(ri > 18, ball_dist * 0.7 + 0.15, ri))  
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
  
  pitch_area$I <- calc_I(as.matrix(pitch), x, y, mu_x, mu_y, Sigma)
  pitch_area$team <- team
  pitch_area$time <- time
  pitch_area$nflId <- nflId
  return(pitch_area)
}

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

#get the position of the ball for this frame
ball_location <- testing_data %>%
  select(ball_x, ball_y) %>%
  unique()

#plot it all
p6 <- ggplot() +
  #pitch layout background
  #annotate_pitch(dimensions = pitch_statsbomb) +
  #pitch control raster
  geom_tile(data = pitch_control, aes(x = x, y = y, fill = PC), alpha = 0.7) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0.5) +
  #players for each team
  #also add in little vector arrows
  geom_segment(data = testing_data, aes(x = x, y = y, xend = x + s * sin(theta), yend = y  + s* cos(theta), colour = team),
               size = 1, arrow = arrow(length = unit(0.01, "npc"))) +
  geom_point(data = testing_data, aes(x = x, y = y, colour = team), size = 3) +
  #geom_point(data = arrival, aes(x = x, y = y, colour = team), size = 3) +
  geom_text(data = testing_data, aes(x = x, y = y, label = nflId, vjust = 1), size = 3) +
  scale_colour_manual(values = c("gold", "black"), breaks = c("home", "away")) +
  #ball location
  geom_point(data = ball_location, aes(x = ball_x, y = ball_y),
             colour = "black", fill = "white", shape = 21, size = 2.5, stroke = 2) +
  labs(title = "Mu based on distance to ball") +
  coord_fixed()
#theme_pitch()

p6

testing_data %>%
  select(time, next_time, ball_x, ball_y, ball_dist, x, y, s, theta, nflId, team) %>% 
  #run func
  pmap_df(., calc_PC2, pitch_area = pitch) %>%
  filter(nflId == 2540202) %>% 
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = I), alpha = 0.7)  +
  geom_segment(data = testing_data %>% filter(nflId == 2540202), aes(x = x, y = y, xend = s*sin(theta) + x, yend = s*cos(theta) + y, colour = team),
               size = 1, arrow = arrow(length = unit(0.01, "npc"))) +
  geom_point(data = testing_data %>% filter(nflId == 2540202), aes(x = x, y = y, colour = team), size = 3) +
  #geom_point(data = arrival %>% filter(nflId == 2540202), aes(x = x, y = y), size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white") +
  facet_wrap(~ nflId + team) +
  coord_fixed()

testing_data %>%
  select(time, next_time, ball_x, ball_y, ball_dist, x, y, s, theta, nflId, team) %>% 
  #run func
  pmap_df(., calc_PC2, pitch_area = pitch) %>%
  filter(I > 0.001) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = I), alpha = 0.7)  +
  geom_segment(data = testing_data, aes(x = x, y = y, xend = s*sin(theta) + x, yend = s*cos(theta) + y, colour = team),
               size = 1, arrow = arrow(length = unit(0.01, "npc"))) +
  geom_point(data = testing_data, aes(x = x, y = y, colour = team), size = 3) +
  #geom_point(data = arrival, aes(x = x, y = y), size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white") +
  facet_wrap(~ nflId + team) +
  coord_fixed()




# Space Generation --------------------------------------------------------

# GO RUN SPACE GENERATION FILE
# 
# big_testing_data <- nfl_tracking_data %>%
#   filter(playId == 3264, gameId == 2018090903, frameId >= 11, frameId <= 45)
# 
# PC <- 
#   big_testing_data %>%
#   select(time, next_time, ball_x, ball_y, ball_dist, x, y, s, theta, nflId, team) %>% 
#   #run func
#   pmap_df(., calc_PC2, pitch_area = pitch) %>%
#   filter(I > 1e-10) %>%
#   group_by(time, x, y, team) %>%
#   filter(I == max(I)) %>%
#   group_by(time, x, y) %>%
#   mutate(PC = 1/ ( 1 + exp(max(I) - min(I))),
#          PC = if_else(team == "home", PC, 1 - PC)) %>%
#   filter(I == max(I))
# 
# pitch_control_more <- 
#   PC %>%
#   mutate(yard_gain = (x - 28.2 - 10)/(100 - 28.2),
#          Q = yard_gain * PC)
# 
# pitch_control_more2 <- 
#   PC %>%
#   #filter(time %in% c(1.5, 2.5, 3.5, 4.5)) %>%
#   #filter(PC < 0.49 | PC > 0.51) %>%
#   mutate(yard_gain = (x - 28.2 - 10)/(100 - 28.2),
#          Q = yard_gain * PC) 
# 
#   
# ggplot() +
#   #pitch layout background
#   #annotate_pitch(dimensions = pitch_statsbomb) +
#   #pitch control raster
#   #geom_tile(data = pitch_control_more2, aes(x = x, y = y, fill = PC), alpha = 0.7) +
#   #scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0.5) +
#   #players for each team
#   #also add in little vector arrows
#   geom_segment(data = big_testing_data, aes(x = x, y = y, xend = x + s * sin(theta), yend = y  + s* cos(theta), colour = team),
#                size = 1, arrow = arrow(length = unit(0.01, "npc"))) +
#   geom_point(data = big_testing_data, aes(x = x, y = y, colour = team), size = 3) +
#   #geom_point(data = arrival, aes(x = x, y = y, colour = team), size = 3) +
#   #geom_text(data = big_testing_data, aes(x = x, y = y, label = nflId, vjust = 1), size = 3) +
#   scale_colour_manual(values = c("gold", "black"), breaks = c("home", "away")) +
#   #ball location
#   geom_point(data = ball_location, aes(x = ball_x, y = ball_y),
#              colour = "black", fill = "white", shape = 21, size = 2.5, stroke = 2) +
#   labs(title = "Mu based on distance to ball") +
#   coord_fixed() +
#   facet_wrap(~ time)
# 
# 
# 
# 
# dQ <- 
#   pitch_control_more %>%
#   group_by(team, time, nflId) %>%
#   summarise(sQ = sum(Q)) %>%
#   group_by(team, nflId) %>%
#   mutate(dQ = sQ - lag(sQ))
#   
# 
# library(slider)
# epsilon <- 1
# 
# 
# soGL <- 
#   dQ %>%
#   mutate(G = slide_dbl(dQ, ~ mean(., na.rm = T), .after = 5),
#          SOG = if_else(G < epsilon, 0, G),
#          SOL = if_else(G > -epsilon, 0, G))
# 
# soGL_joined <-
#   soGL %>%
#   left_join(big_testing_data, by = c("team", "time", "nflId")) %>%
#   select(team, time, nflId, G, SOG, SOL, x, y, next_x, next_y, s, a, dis, o, dir, event)
# 
# delta <- 20
# 
# soGL_joined %>%
#   filter(team == "home")%>%
#   left_join(., ., by = c("team", "time", "event"), suffix = c("", "_ip")) %>%
#   filter(nflId != nflId_ip) %>%
#   left_join(soGL_joined %>% filter(team == "away"), by = c("time", "event"), suffix = c("", "_j")) %>%
#   filter(nflId == 2550272, nflId_ip == 2540202, nflId_j == 2555158) %>%
#   
#   mutate(sqrt((x_ip - x_j)^2 + (y_ip - y_j)^2),
#          sqrt((next_x - x_j)^2 + (next_y - y_j)^2),
#          sqrt((next_x_ip - next_x_j)^2 + (next_y_ip - next_y_j)^2)) %>% View
#   mutate(SGG_i_ip = if_else(G_ip > epsilon, G_ip, 0)) %>%
#   group_by(nflId, nflId_ip, nflId_j) %>%
#   summarise(s = sum(SGG_i_ip)) %>%
#   arrange(desc(s))
#   
# soGL %>% group_by(nflId, team) %>%
#   summarise(SOG_n = sum(SOG >0),
#             SOL_n = sum(SOL < 0),
#             SOG_sum = sum(SOG),
#             SOL_sum = sum(SOL),
#             SOG_mean = mean(SOG),
#             SOL_mean = mean(SOL)) %>%
#   arrange(team, -SOG_sum)
# 
# 
# library(Rcpp)
# library(RcppArmadillo)
# 
# sourceCpp("pitch_control.cpp")
# 
# 
# # ugly packaged up function
# calc_PC_cpp <- function(time, next_time, ball_x, ball_y, x, y, next_x, next_y, team, player, pitch_area) {
#   #blargh terribly written- run out of energy to improve
#   pitch_area$I <- calc_I_cpp(c(x, y), c(next_x, next_y), c(ball_x, ball_y), time, next_time, as.matrix(pitch_area), t(c(x, y)))
#   pitch_area$team <- team
#   pitch_area$time <- time
#   pitch_area$player <- player
#   return(pitch_area)
# }
# 
# #sample 10 seconds worth of data
# animation_data <- tracking_data %>%
#   filter(time %in% 600:610) %>%
#   dplyr::select(time, next_time, ball_x, ball_y, x, y, next_x, next_y, team, player) 
# 
# #run the function over the data
# anim_pitch_control <- animation_data %>%
#   #run func
#   pmap_df(., calc_PC_cpp, pitch_area = pitch) %>%
#   #sum by team and area
#   group_by(team, x, y, time) %>%
#   summarise(team_sum = sum(I)) %>%
#   pivot_wider(names_from = team, values_from = team_sum) %>%
#   #σ - logistic function
#   mutate(PC = 1 / (1 + exp(Home_Team - Away_Team)))
# 
# #plot
# p7 <- ggplot(anim_pitch_control, aes(x = x, y = y, colour = PC)) +
#   annotate_pitch(dimensions = pitch_statsbomb) +
#   geom_point(alpha = 0.7, shape = 15) +
#   scale_colour_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0.5) +
#   theme_pitch() +
#   labs(title = "pitch control rasters by match time (s)") +
#   facet_wrap(~time)
# p7
