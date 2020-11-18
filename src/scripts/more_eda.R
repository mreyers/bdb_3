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
target <- read_csv("Data/additional_data/targetedReceiver.csv", col_types = cols()) %>%
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
for(i in 1:1){
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
  
  rm(one_week, my_plays)
}

# The above is to populate influence values
# I have additional things to work on now
# A few things of interest are how we can get reaction time
# Lucas is going to handle nearest defender and coverage
# Im going to handle some really simple summary reaction times
possession <- my_play %>%
  filter(team != "football") %>%
  group_by(game_id, play_id, team) %>%
  summarize(n_off_players = sum(position %in% c("QB", "RB", "WR", "TE")),
            .groups = "drop_last") %>%
  arrange(desc(n_off_players)) %>%
  mutate(team_role = if_else(team == first(team), "off", "def")) %>%
  select(game_id, play_id, team, team_role)
  
play_pass_start <- c("pass_forward", "pass_shovel")
play_pass_arrive <- c("pass_outcome_caught",
                      "pass_outcome_incomplete", "pass_outcome_interception",
                      "pass_outcome_interception", "pass_outcome_touchdown")
# How?
  # Take throw frame and arrival frame
  # Pull receiver and all defenders
  # Measure distances
  # Calculate delta distance
my_play_with_receiver <- my_play %>%
  left_join(target, by = c("game_id", "play_id")) %>%
  left_join(possession, by = c("game_id", "play_id", "team")) %>%
  filter(team_role %in% "def" | nfl_id == target_nfl_id) %>%
  mutate(pass_start = (event %in% play_pass_start),
         pass_arrival = (event %in% play_pass_arrive)) %>%
  filter(pass_start | pass_arrival) %>%
  # TRUE and FALSE groups, grabbing first frame_id will get the first frame in which
  # there was a pass_start event and the first frame with a pass_arrival event
  # Dont want multiple of each event as that becomes cumbersome
  group_by(pass_start) %>%
  filter(frame_id == first(frame_id)) %>%
  mutate(receiver_x = sum((team_role == "off") * x, na.rm = TRUE),
         receiver_y = sum((team_role == "off") * y, na.rm = TRUE),
         dist_to_receiver = sqrt((x - receiver_x) ^ 2 + (y - receiver_y) ^ 2)) %>%
  group_by(nfl_id,  display_name) %>%
  arrange(desc(frame_id)) %>%
  # Negative is good
  summarize(closed_distance = first(dist_to_receiver) - last(dist_to_receiver),
            distance_to_go_still = first(dist_to_receiver),
            time_elapsed_in_frames = first(frame_id) - last(frame_id),
            time_elapsed_in_seconds = time_elapsed_in_frames / 10,
            .groups = "drop")

all_weeks_combos <- tibble()
for(i in 1:17){
  one_week <- read_csv(paste0("Data/week", i, ".csv")) %>%
    janitor::clean_names()
  
  my_play <- one_week %>%
    rename(velocity = s)
  
  possession <- my_play %>%
    filter(team != "football") %>%
    group_by(game_id, play_id, team) %>%
    summarize(n_off_players = sum(position %in% c("QB", "RB", "WR", "TE")),
              .groups = "drop_last") %>%
    arrange(desc(n_off_players)) %>%
    mutate(team_role = if_else(team == first(team), "off", "def")) %>%
    select(game_id, play_id, team, team_role)
  
  my_play_with_receiver <- my_play %>%
    left_join(target, by = c("game_id", "play_id")) %>%
    left_join(possession, by = c("game_id", "play_id", "team")) %>%
    filter(team_role %in% "def" | nfl_id == target_nfl_id) %>%
    mutate(pass_start = (event %in% play_pass_start),
           pass_arrival = (event %in% play_pass_arrive)) %>%
    filter(pass_start | pass_arrival) %>%
    # TRUE and FALSE groups, grabbing first frame_id will get the first frame in which
    # there was a pass_start event and the first frame with a pass_arrival event
    # Dont want multiple of each event as that becomes cumbersome
    group_by(game_id, play_id, pass_start) %>%
    filter(frame_id == first(frame_id)) %>%
    mutate(receiver_x = sum((team_role == "off") * x, na.rm = TRUE),
           receiver_y = sum((team_role == "off") * y, na.rm = TRUE),
           dist_to_receiver = sqrt((x - receiver_x) ^ 2 + (y - receiver_y) ^ 2)) %>%
    group_by(game_id, play_id, nfl_id,  display_name, team_role) %>%
    arrange(desc(frame_id)) %>%
    # Negative is good
    summarize(closed_distance = first(dist_to_receiver) - last(dist_to_receiver),
              distance_to_go_still = first(dist_to_receiver),
              time_elapsed_in_frames = first(frame_id) - last(frame_id),
              time_elapsed_in_seconds = time_elapsed_in_frames / 10,
              .groups = "drop")
  
  all_weeks_combos <- all_weeks_combos %>%
    bind_rows(my_play_with_receiver)
}

all_weeks_combos

# I wonder what some basic summary info would look like
# Things such as how much distance a player closed in on the receiver, how close they were
# Note there are some clear hail mary plays in here
  # This is observed by the weirdly large avg_start_distance values and having receivers (Logan Thomas)
  # on defence. This is the hands team, like the play Gronk screwed up against the Dolphins
am_a_receiver <- target %>%
  group_by(target_nfl_id) %>%
  summarize(n = n()) %>%
  pull(target_nfl_id)

# This isnt quite right as its not always the closest defender to the receiver
# Its all defenders within a play, thus the large distances
  # Dont use avg_distance_to_go
all_weeks_combos %>%
  # remove receivers
  filter(team_role %in% "def",
         !(nfl_id %in% am_a_receiver)) %>%
  group_by(nfl_id, display_name) %>%
  summarize(avg_closed_distance = mean(closed_distance),
            avg_distance_to_go = mean(distance_to_go_still),
            avg_start_distance = mean(distance_to_go_still - avg_closed_distance)) %>%
  arrange((avg_closed_distance)) %>%
  left_join(players %>% select(nfl_id, position),
            by = "nfl_id")
