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


default_path <- "Data/"
select_cols <- c("time", "x", "y", "s",
                 "dis", "dir", "event",
                 "nflId", "displayName", "jerseyNumber",
                 "team", "frameId", "gameId", "playId")


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
  mutate(yardline_100 = case_when(possession_team == yardline_side ~ 100 - yardline_number,
                                  possession_team != yardline_side ~ yardline_number)) %>%
  select(game_id, play_id, pass_result,
         down, yards_to_go, yardline_100)

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

for(i in 4:17){
  
  this_week <-  read_csv(paste0(default_path, "week", i, ".csv")) %>%
    select_at(select_cols) %>%
    janitor::clean_names()
  
  # I dont think I need display_name here, looks like its in tracking data
  tracking <- this_week %>%
    left_join(players %>% select(-display_name), by = "nfl_id") %>%
    rename(velocity = s)
  
  # Need possession team for current format
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
                              ~handle_no_ball(., is_football = TRUE))) %>%
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
  
  parallel_res_setup <- tracking_exempt %>%
    mutate(first_elig = map_int(data, ~first_elig_frame(.)),
           last_elig = map_int(data, ~last_elig_frame(.)) + 0, # previously epsilon
           pocket_dist = pmap(list(data, first_elig, last_elig),
                              ~pocket_fixed(..1, ..2, ..3)))
  
  flog.info('Generating parallel_res', name = 'par_obs')
  parallel_res <- parallel_res_setup %>%
    mutate(basic_covariates = future_pmap(list(data, first_elig, last_elig),
                                   ~ simple_covariates(..1, ..2, ..3, run = TRUE)),
           basic_covariates = map2(basic_covariates, pocket_dist,
                                   ~ .x %>% mutate(pocket_dist = list(.y)))) %>%
    dplyr::select(-pocket_dist) # remove from exterior, now mapped it back
  
  flog.info('Generating parallel_res_temp', name = 'par_obs')
  parallel_res_temp <- parallel_res %>%
    mutate(additional_basic_covariates = future_pmap(list(data, first_elig, last_elig),
                                                     ~ additional_basic_covariates_wrapper(..1, ..2, ..3,
                                                                                           is_football = TRUE)))
  
  flog.info('Joining and unnesting, making parallel_res_joined', name = 'par_obs')
  parallel_res_joined <- parallel_res_temp %>%
    mutate(all_basic_covariates = map2(basic_covariates, additional_basic_covariates,
                                       ~ .x %>% left_join(.y, by = "frame_id_2"))) %>%
    select(game_id, play_id, pass_result, target, complete,
           first_elig, last_elig, all_basic_covariates) %>%
    unnest(all_basic_covariates) %>%
    mutate(joined_all_basic_covariates = pmap(list(rec_sep, sideline_sep, additional_metrics),
                                              ~ ..1 %>% 
                                                select(-c(x, y)) %>% # in sideline_sep
                                                left_join(..2, by = c( "nfl_id", "display_name")) %>%
                                                left_join(..3, by = c( "nfl_id")))) %>%
    select(-c(rec_sep, sideline_sep, additional_metrics)) %>%
    unnest(joined_all_basic_covariates)
  
  flog.info('Ended iteration %s at time %s.', i, format(Sys.time(), '%X'), name = 'all_time')
  flog.info('Writing to all_frames_covariates_week%s.rds', i, name = 'all_time')
  
  parallel_res_joined %>%
    saveRDS(paste0(default_path, "all_frames_covariates_week", i, ".rds"))
  
  rm(parallel_res, parallel_res_temp, parallel_res_setup, parallel_res_joined,
     this_week, tracking)
}

flog.info('Parallel all complete.', name = 'par_obs')
