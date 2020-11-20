# New parallel_observed setup
# Lets just create a large data file for each of the data sets, 2017 and 2018
# Then I can remove some of these functions and simplify this process
# Contingency is just that this can only be run on computers with some
# stronger RAM allowances

# Parallel stuff
flog.appender(appender.file('logs/parallel_all_frames.log'), 'par_obs')
flog.info('Start of simple_covariates.R. This will establish the
           necessary components for building a completion probability
           model at any frame. Of course we will only build the model with respect
           to the observed passes and then just expand over observed set.', name = 'par_obs')


default_path <- "Data_new/"
select_cols <- c("time", "x", "y", "s",
                 "dis", "dir", "event",
                 "nflId", "displayName", "jerseyNumber",
                 "team", "frameId", "gameId", "playId")
file_list <- list.files(default_path, pattern = "week[0-9]+.csv")

all_data_set <- tibble()
for(i in 1:length(file_list)){
  new_data <- read_csv(paste0(default_path, file_list[i])) %>%
    select_at(select_cols) %>%
    janitor::clean_names()
  
  all_data_set <- all_data_set %>%
    bind_rows(new_data)
}

# Load the games, players, and list of plays in the data set
games <- read_csv(paste0(default_path, "games.csv"), col_types = cols()) %>%
  janitor::clean_names()
players <- read_csv(paste0(default_path, "players.csv"), col_types = cols()) %>%
  janitor::clean_names()
plays <- read_csv(paste0(default_path, "plays.csv"), col_types = cols()) %>%
  janitor::clean_names()

# Reduce players, we dont care about college (A&M what?)
players <- players %>%
  dplyr::select(nfl_id, display_name, position)

# Gather positions that I need for analysis, eligible receivers and defensive players
player_pos_id_key <- players %>%
  dplyr::select(nfl_id, position)

route_runners_pos_id_key <- player_pos_id_key %>%
  filter(position %in% c("RB", "FB", "WR", "TE", "OLB", "SS", "ILB", "DE", "CB", "NT",
                         "MLB", "FS", "DT", "LB", "DB"))

# For play standardization
reorient <- FALSE

# Also need the play_ids which are the plays that had passes
play_ids <- plays %>%
  filter(!is.na(pass_result)) %>% 
  select(game_id, play_id, pass_result)

# Generic event labels
pass_air_end <- 
  c(# "pass_arrived",
    "pass_outcome_caught",
    "pass_outcome_incomplete",
    # "pass_tipped",
    "touchdown",
    "pass_outcome_interception",
    "pass_outcome_touchdown"
  )

pass_air_start <-
  c("pass_forward",
    "pass_shovel"
  )

# Plays and games that had specific, unresolvable issues (generally broken tracking data for player/ball)
exempt_plays <- tibble(game_id = c(2017091700,2017091706, 2017091711, 2017092407, 2017092407, 2017091007), 
                       play_id = c(2288, 2126, 3386, 190, 270, 1702))
flog.info('Setup complete for general purpose variables. Defining cluster now.', name = 'par_obs')

num_cores <- parallel::detectCores() - 2
plan(multisession, workers = num_cores)

flog.info('Cluster prepared, running loop.', name = 'par_obs')

#  # # # # #  The replacement for the loop # # #  # # # # # # 

# I dont think I need display_name here, looks like its in tracking data
tracking <- all_data_set %>%
  left_join(players %>% select(-display_name), by = "nfl_id") %>%
  rename(velocity = s)

# Need possession team for current format
tictoc::tic()
possession <- get_possession_team(tracking)

# Fix nesting that doesnt have a few variables at lower level
tracking_nest <- tracking %>%
  left_join(possession) %>%
  nest(-game_id, -play_id) %>%
  inner_join(play_ids) %>%
  mutate(data = pmap(list(data, game_id, play_id), ~quick_nest_fix(..1, ..2, ..3)))

# Remove plays with no snap recorded
tracking_rm_no_snaps <- tracking_nest %>%
  mutate(snapped = map_lgl(data, is_snap)) %>%
  filter(snapped) %>%
  select(-snapped)

tracking_standard <- tracking_rm_no_snaps %>%
  mutate(data = map(data, ~standardize_play(., reorient)),
         cleaning = map_dbl(data,
                            ~handle_no_ball(., is_football = new_age_tracking_data))) %>%
  filter(!is.na(cleaning), pass_result %in% c('C', 'I', 'IN')) 

# Error on 5996th object
# Result 5996 must be a single double, not a double vector of length 0
# Input `target` is `map_dbl(data, intended_receiver, is_football = new_age_tracking_data)
# Fixed, problem was a play had no WR / TE / RB to be the target
tracking_filter <- tracking_standard %>%
  mutate(data = map(data, ball_fix_2),
         target = map_dbl(data, intended_receiver, is_football = TRUE),
         complete = map_lgl(data, play_success),
         fake_pt = map_dbl(data, fake_punt),
         qb_check = map_dbl(data, no_qb),
         pass_recorded = map_lgl(data, ~pass_start_event(.))) %>%
  left_join(players %>% select(nfl_id, display_name), by = c("target" = "nfl_id")) %>%
  filter(!is.na(target), !is.na(fake_pt), !is.na(qb_check), pass_recorded) %>%
  mutate(receiver = display_name) %>%
  select(-display_name)

tracking_exempt <- tracking_filter %>%
  anti_join(exempt_plays) %>%
  filter(!(game_id == 2017092407))

# All the parallel stuff
parallel_res <- tracking_exempt %>%
  ungroup() %>% 
  mutate(air_dist = map2_dbl(data, target, ~air_distance(.x, .y)),
         separation = map2(data, target, ~separation(.x, .y)),
         sideline_sep = map2_dbl(data, target, ~sideline_sep(.x, .y)),
         passrush_sep = map(data, ~pass_rush_sep(.))) 

parallel_res_scalar <- parallel_res %>%
  mutate(qb_vel = map_dbl(data, ~qb_speed(.)),
         first_elig = map_dbl(data, ~first_elig_frame(.)),
         last_elig = map_dbl(data, ~last_elig_frame(.))) %>%
  mutate(time_to_throw = map_dbl(data, ~release_time(.)),
         dist_from_pocket = map_dbl(data, ~pocket(.)),
         qb_disp_name = map_chr(data, ~qb_name(.)))

# Add influence to the play
# Hit a writing to connection error, try with sequential to avoid
# Had to remove future_map altogether due to memory error on globals
plan(sequential)
parallel_res_inf <- parallel_res_scalar %>%
  mutate(inf_at_pass = map(data,
                           ~ add_influence(., is_football = new_age_tracking_data)))

# A few extra covariates and unnesting
parallel_res_unnest <- parallel_res_inf %>%
  mutate(ball_speed_arrival = map_dbl(data,
                                      ~ball_speed_at_arrival(.,
                                                             is_football = new_age_tracking_data)),
         air_time_ball = map_dbl(data, ~air_time(.)),
         air_yards_x = map_dbl(data, ~air_yards(.,
                                                is_football = new_age_tracking_data))) %>%
  dplyr::select(-data) %>%
  unnest(separation) %>%
  unnest(passrush_sep) %>%
  unnest(inf_at_pass)

# Save the data to a permanent location as to not have to rerun much
parallel_res_unnest %>% write_rds(paste0(default_path, "observed_covariates.rds"))
tictoc::toc()

rm(parallel_res, parallel_res_scalar,
   parallel_res_inf, parallel_res_unnest)
rm(all_data_set, tracking)
gc(verbose = FALSE)


flog.info('Parallel observed complete.', name = 'par_obs')