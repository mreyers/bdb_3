# Second EDA
# Lets think about the general idea of defence in football (passing)

# What is the objective?
  # Stop the offence from progressing
# How can this be accomplished?
  # Interceptions, incompletions, and sacks (although we dont have those in here) plus
  # completions that dont accomplish much
# What does 'accomplish much' mean?
  # Increase the likelihood of the offence scoring a touchdown, i.e.
  # provides more value in field position than it does in the time and down it cost
# What situations are prone to allowing a lot to be accomplished?
  # First downs, deep passes, and defensive penalties
# How does a defence prevent surrendering these positive plays?
  # Team defence, individual exceptional performances, offence mistakes
# How can we measure the quantity of influence a defence has on the outcome of a play?
  # Player by player level or at a unit level

# What about defensive alignment?
  # Some kernels on Kaggle help to identify player-wise coverage, man or zone

# What even is an interesting question about defence?
  # Maybe value attribution to players but many will do this, maybe as dessert?
  # Scheme effectiveness but that is an appetizer, not the main course

# What are Sarah's thoughts?
  # Reaction times are critical, understanding these is critical to understanding defense
  # Value attribution to units or individuals is worthwhile
  # Identifying situations where miscues can be reduced is worthwhile


# # # #
# Main
# # # #
library(tidyverse)
library(mvtnorm)
source("src/utils/tracking_helpers.R")

one_week <- read_csv("Data/week1.csv") %>%
  janitor::clean_names()

games <- read_csv("Data/games.csv", col_types = cols()) %>%
  janitor::clean_names(case = "snake")
players <- read_csv("Data/players.csv", col_types = cols()) %>%
  janitor::clean_names(case = "snake") 
plays <- read_csv("Data/plays.csv", col_types = cols()) %>%
  janitor::clean_names(case = "snake")

my_play <- one_week %>%
  filter(game_id == first(game_id), play_id == first(play_id)) %>%
  rename(velocity = s)
# Lets try and rewrite the old code for more efficiency
# The old version requires a frame by frame approach which loses out on some easy vectorization
# The below should be a bit faster

new_zone_influence <- function(data){
  
  my_play <- data
  
  pass_condition <- c("pass_forward", "pass_shovel")
  
  ball_loc <- my_play %>%
    filter(team %in% "football") %>%
    select(frame_id, ball_x = x, ball_y = y)
  
  # Get possession team info
  poss_team <- my_play %>%
    slice(1) %>%
    filter(position %in% c("QB", "WR", "RB", "TE")) %>%
    group_by(team) %>%
    summarize(n = n(), .groups = "drop") %>%
    arrange(desc(n)) %>%
    slice(1) %>%
    pull(team)
  
  # Calculate mu_i, Sratio, distance to ball, and non-zero elements of S matrix
  my_play_with_features <- 
    my_play %>%
    left_join(ball_loc, by = "frame_id") %>%
    mutate(mu_i_x = x + velocity * cos(dir * pi / 180) * 0.5,
           mu_i_y = y + velocity * sin(dir * pi / 180) * 0.5,
           # Max observed speed was 22.11 mph, say 24 mph. Denom is in m/s
           speed_rat = velocity ^ 2 / (10.729^2),
           speed_x = velocity * cos(dir * pi / 180),
           speed_y = velocity * sin(dir * pi / 180),
           dis_to_ball = sqrt((x - ball_x) ^ 2 +
                                (y - ball_y) ^ 2)) %>% 
    ungroup() %>%
    mutate(poss_team_o_d = if_else(team == poss_team, "Off", "Def"),
           R = map(dir, makeMatrix),
           R_i_t = get_R_i_t(dis_to_ball),
           S_i_t = map2(speed_rat, R_i_t, ~ matrix(
             c((.y + .y * .x) / 2,
               0,
               0,
               (.y - .y * .x) / 2),
             byrow = T, nrow = 2)),
           mu_i = map2(mu_i_x, mu_i_y, ~ matrix(c(.x, .y), nrow = 2)),
           cov_struc = map2(R, S_i_t, ~ .x %*% .y %*% .y %*% solve(.x)))
  
  
  
  play_dir <- my_play %>%
    slice(1) %>%
    pull(play_direction)
  
  # Get boundary x values for each play, just pull min / max
  x_bounds <- my_play %>%
    summarize(min_x = floor(min(x)),
              max_x = ceiling(max(x)))
  
  
  # We can now calculate the density of a player over the whole field
  data_grid <- expand.grid(s_1 = seq(x_bounds$min_x, x_bounds$max_x, by = 1),
                           s_2 = seq(0, 53, by = 1)) # previously res, now 1
  
  # Now we will try to do the same thing over all the players in one play
  # This needs to be normalized by player position at handoff
  density_calc <- my_play_with_features %>% 
    mutate(density_val = map2(mu_i, cov_struc,
                              ~ dmvnorm(data_grid, mean = .x, sigma = .y)))
  
  density_field <-
    density_calc %>% 
    dplyr::select(density_val, nfl_id, frame_id, team, jersey_number) %>% 
    mutate(density_val = map(density_val, ~ bind_cols(as_tibble(.x), as_tibble(data_grid)))) %>% 
    unnest(cols = c(density_val))
  
  
  # Next is the player influence, standardize by max player value
  influence <- density_field %>% 
    group_by(nfl_id) %>% 
    dplyr::arrange(nfl_id, desc(value)) %>%
    mutate(normalized_inf = value / first(value)) %>%
    ungroup() %>% 
    filter(normalized_inf > 1.30e-10) %>%
    group_by(frame_id, s_1, s_2, team) %>%
    summarize(team_total = sum(normalized_inf), .groups = "drop") %>%
    group_by(frame_id, s_1, s_2) %>%
    arrange(team) %>%
    summarize(away_inf = 1 / (1 + exp(-(first(team_total) - last(team_total)) )), 
              home_inf = 1 / (1 + exp(-(last(team_total) - first(team_total)) )),
              .groups = "drop") %>%
    filter(away_inf != 0.5, home_inf != 0.5)
  
  return(influence)
}


library(furrr)
plan(multisession, workers = availableCores() - 2)

# 96k, 69k, 65k, 186k, 38k
for(i in 2:17){
  # Read in each week, add influence data, save, go next
  one_week <- read_csv(paste0("Data/week", i, ".csv")) %>%
    janitor::clean_names()
  
  my_plays <- one_week %>%
    rename(velocity = s)
  
  tictoc::tic()
  holder <- my_plays %>%
    nest(data = c(time, x, y, velocity, a, dis, o, dir, event, nfl_id, display_name, 
                  jersey_number, position, frame_id, team, play_direction, 
                  route)) %>%
    mutate(inf_check = future_map(data, purrr::safely(new_zone_influence))) %>%
    select(game_id, play_id, inf_check)
  tictoc::toc()
  
  saveRDS(holder, paste0("Data/additional_data/week", i, "_influence.rds"))
  
  rm(one_week, my_plays, holder)
}
# 1200 seconds for week 1, only 20 minutes