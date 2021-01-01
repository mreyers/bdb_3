# If you want to run for all plays
# Space Generation --------------------------------------------------------
# 
# tracking_plays <- 
#   nfl_tracking_data %>%
#   group_by(gameId, playId) %>%
#   mutate(ball_snap = if_else(event == "ball_snap", time, min(time)),
#          pass_forward = if_else(event == "pass_forward", time, max(time))) %>%
#   mutate(ball_snap = max(ball_snap),
#          pass_forward = min(pass_forward)) %>%
#   filter(time >= ball_snap, time<= pass_forward) %>%
#   nest(-c(gameId, playId))
# 
# 
# yards <- 
#   plays %>% 
#   mutate(yard_line = if_else(yardlineSide != possessionTeam, 100 - yardlineNumber, yardlineNumber)) %>%
#   left_join(games) %>%
#   mutate(off_team = if_else(possessionTeam == homeTeamAbbr, "home", "away"),
#          def_team = if_else(possessionTeam == homeTeamAbbr, "away", "home")) %>%
#   select(gameId, playId, yard_line, off_team, def_team)
# 
# library(furrr)
# plan("multicore")
# 
# spaces_df <-
#   tracking_plays %>%
#   left_join(yards) %>%
#   ungroup()
#   
# tictoc::tic()
# spaces = 
#   future_pmap(list(spaces_df$data, spaces_df$yard_line, spaces_df$off_team, spaces_df$def_team, spaces_df$gameId, spaces_df$playId), 
#               possibly(generate_space_list, NA))
# tictoc::toc()

#generate_space_list(big_testing_data, 28.2)


# if you want to test on Kenny Stills touchdown run this and step inside the function
play_data <- nfl_tracking_data %>%
  filter(playId == 3264, gameId == 2018090903, frameId >= 11, frameId <= 45)
yard_line = 28
off_team = "home"
def_team = "away"


generate_space_list <- function(play_data, yard_line, off_team, def_team, gameId, playId){
  
  player_PC <-
    play_data %>%
    select(time, next_time, ball_x, ball_y, ball_dist, x, y, s, theta, nflId, team) %>% 
    #run func
    pmap_df(., calc_PC2, pitch_area = pitch)
  
  write_csv(player_PC, paste0("output/player_PC/", gameId, "_", playId, ".csv"))
  
  PC <- 
    player_PC %>%
    filter(I > 1e-10) %>%
    group_by(time, x, y, team) %>%
    filter(I == max(I)) %>%
    group_by(time, x, y) %>%
    mutate(PC = 1/ ( 1 + exp(max(I) - min(I))),
           PC = if_else(team == def_team, PC, 1 - PC)) %>%
    filter(I == max(I)) %>%
    filter(I > 0.1) 
  
  pitch_control_more <- 
    PC %>%
    mutate(yard_gain = (x - yard_line - 10)/(100 -  yard_line),
           Q = yard_gain * PC)
  dQ <- 
    pitch_control_more %>%
    group_by(team, time, nflId) %>%
    summarise(sQ = sum(Q)) %>%
    group_by(team, nflId) %>%
    mutate(dQ = sQ - lag(sQ))
  
  
  library(slider)
  epsilon <- 1
  
  soGL <- 
    dQ %>%
    mutate(G = slide_dbl(dQ, ~ mean(., na.rm = T), .after = 5),
           SOG = if_else(G < epsilon, 0, G),
           SOL = if_else(G > -epsilon, 0, G))
  
  SGL <- 
    soGL %>% group_by(nflId, team) %>%
    summarise(SOG_n = sum(SOG > 0),
              SOL_n = sum(SOL < 0),
              SOG_sum = sum(SOG),
              SOL_sum = sum(SOL),
              SOG_mean = mean(SOG),
              SOL_mean = mean(SOL)) %>%
    arrange(team, -SOG_sum)
  
  player_catch_means <- 
    player_PC %>%
    group_by(team, time, nflId, mu_x, mu_y) %>%
    summarise() %>%
    group_by(team, nflId) %>%
    mutate(next_mu_x = lead(mu_x), 
           next_mu_y = lead(mu_y))
  
  soGL_joined <-
    soGL %>%
    left_join(play_data, by = c("team", "time", "nflId")) %>%
    select(team, time, nflId, G, SOG, SOL, x, y, next_x, next_y, s, a, dis, o, dir, event, ball_dist) %>%
    left_join(player_catch_means)
  
  SGG <- 
    soGL_joined %>%
    filter(team == off_team)%>%
    left_join(., ., by = c("team", "time", "event"), suffix = c("", "_ip")) %>%
    filter(nflId != nflId_ip) %>%
    left_join(soGL_joined %>% filter(team == def_team), by = c("time", "event"), suffix = c("", "_j")) %>%
    #filter(nflId == 2550272, nflId_ip == 2540202, nflId_j == 2555158) %>%
    mutate(d_ip_j = sqrt((mu_x_ip - mu_x_j)^2 + (mu_y_ip - mu_y_j)^2),
           d_i_j  = sqrt((mu_x - mu_x_j)^2 + (mu_y - mu_y_j)^2),
           next_d_i_j  = sqrt((next_mu_x - next_mu_x_j)^2 + (next_mu_y - next_mu_y_j)^2),
           next_d_ip_j  = sqrt((next_mu_x_ip - next_mu_x_j)^2 + (next_mu_y_ip - next_mu_y_j)^2),
           delta = (ball_dist + ball_dist_ip + ball_dist_j )/5) %>%
    mutate(SG = 
             # defender is close to receiver of space at current time
             d_ip_j < delta &
             # defender is farther away from receiver of space at next time
             #next_d_ip_j > d_ip_j &
             # defender is far away from receiver of space at next time
             next_d_ip_j > delta &
             # defender is close to generator of space at current time
             #d_i_j < delta &
             # defender is closer to generator of space at next time,
             #next_d_i_j < d_i_j &
             # defender is close to generator of space at next time
             next_d_i_j < delta &
             # distance change towards generator is greater than distance change towards receiver
             #next_d_i_j - d_i_j < next_d_ip_j - d_ip_j
             #  difference in distance between generator and defender at the time step
             next_d_i_j - d_i_j < 1,
    ) %>% 
    mutate(SGG_i_ip = if_else(G_ip > epsilon & SG, G_ip, 0)) %>%
    group_by(nflId, nflId_ip, nflId_j) %>%
    summarise(SGG_sum = sum(SGG_i_ip),
              #SGG_mean = mean(SGG_i_ip),
              SGG_n = sum(SGG_i_ip > 0)) %>%
    arrange(desc(SGG_sum)) %>%
    filter(SGG_n != 0)
  
  
  write_csv(SGL, paste0("output/SGL/", gameId, "_", playId, ".csv"))
  write_csv(SGG, paste0("output/SGG/", gameId, "_", playId, ".csv"))
  
  return(list(SGL, SGG))
  
}
