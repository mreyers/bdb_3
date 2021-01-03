
all_preds <- readRDS("Data/cp_predictions/all_predictions.rds")

play_pass_start <- c("pass_forward", "pass_shovel")
play_pass_arrive <- c("pass_outcome_caught",
                      "pass_outcome_incomplete", "pass_outcome_interception",
                      "pass_outcome_interception", "pass_outcome_touchdown")


nearest_def_and_rec <- tibble()
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
  
  # This will return the receiver and the nearest defender at time of pass arrival
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
    arrange(desc(frame_id)) %>%
    group_by(game_id, play_id, nfl_id, display_name) %>%
    mutate(throw_frame = last(frame_id),
           arrival_frame = first(frame_id),
           time_elapsed_in_frames = first(frame_id) - last(frame_id),
           time_elapsed_in_seconds = time_elapsed_in_frames / 10,
           closed_distance = first(dist_to_receiver) - last(dist_to_receiver),
           distance_to_go_still = first(dist_to_receiver)) %>%
    group_by(game_id, play_id, nfl_id, display_name, frame_id) %>%
    # Negative is good
    summarize(team_role = first(team_role),
              throw_frame = first(throw_frame),
              arrival_frame = first(arrival_frame),
              time_elapsed_in_frames = first(time_elapsed_in_frames),
              time_elapsed_in_seconds = first(time_elapsed_in_seconds),
              distance_to_receiver = first(dist_to_receiver),
              closed_distance = first(closed_distance),
              distance_to_go_still = first(distance_to_go_still),
              .groups = "drop") %>%
    arrange(distance_to_receiver) %>%
    filter(team_role %in% "def") %>%
    group_by(game_id, play_id, frame_id) %>%
    slice(1)
  
  nearest_def_and_rec <- nearest_def_and_rec %>%
    bind_rows(my_play_with_receiver)
}

# Now to convert it to just a list of nearest defender at release and arrival
release_and_arrival <- nearest_def_and_rec %>%
  mutate(arrival_or_release = if_else(frame_id == arrival_frame,
                                      "arrival",
                                      "release")) %>%
  select(game_id, play_id, nfl_id, arrival_or_release) %>%
  pivot_wider(names_from = arrival_or_release,
              values_from = nfl_id,
              id_cols = c(game_id, play_id))

saveRDS(release_and_arrival, "Data/release_and_arrival.rds")

# Additional piece, need to fix interceptions
# Best way is probably text parsing
plays_int <- read_csv("Data/plays.csv") %>%
  janitor::clean_names() %>%
  filter(pass_result %in% "IN")

interception_phrase <- plays_int %>%
  select(game_id, play_id, play_description) %>%
  mutate(interception_string = str_extract(play_description, "INTERCEPTED by [A-z\\.\\'\\-]+"),
         interception_f_last = str_extract(interception_string, "[A-z\\.\\'\\-]+$"))

# Now to soft map these back to the player IDs that they correspond with
  # Cant just do fuzzy join, too many F.Last similarities
interception_check <- tibble()
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
  
  # This will return the receiver and the nearest defender at time of pass arrival
  my_play_with_receiver <- my_play %>%
    left_join(target, by = c("game_id", "play_id")) %>%
    left_join(possession, by = c("game_id", "play_id", "team")) %>%
    filter(team_role %in% "def" | nfl_id == target_nfl_id)
  
  interception_check <- interception_check %>%
    bind_rows(my_play_with_receiver)
}

just_interceptions <- interception_check %>%
  right_join(interception_phrase,
             by = c("game_id", "play_id")) %>%
  mutate(pass_start = (event %in% play_pass_start),
         pass_arrival = (event %in% play_pass_arrive)) %>%
  filter(pass_start)

# Problems: Shaquil Griffin to SL.Griffin -> manual override
# Ha Ha Clinton-Dix to H.Ha -> manual override to H.Clinton-Dix
just_interceptions_name_check <- just_interceptions %>%
  mutate(first_init = str_extract(display_name, "^[A-Z]{1}"),
         last_name = substr(str_extract(display_name, " [A-z\\.\\'\\-]+"), 2, 100),
         f_last = paste0(first_init, ".", last_name),
         f_last = case_when(
           f_last == "S.Griffin" ~ "SL.Griffin",
           f_last == "H.Ha" ~ "H.Clinton-Dix",
           TRUE ~ f_last
         )) %>%
  filter(interception_f_last == f_last) %>%
  select(game_id, play_id, modified_nearest_defender = nfl_id)

saveRDS(just_interceptions_name_check, "Data/just_interceptions_name_check.rds")

# Check that all interception plays have a match
  # 95.4 %
# just_interceptions_name_check %>%
#   group_by(game_id, play_id) %>%
#   summarize(n_match = sum(interception_f_last == f_last, na.rm = TRUE)) %>%
#   ungroup() %>%
#   summarize(prop_match = sum(n_match) / n())
# 
# plays_to_fix <- just_interceptions_name_check %>%
#   group_by(game_id, play_id) %>%
#   summarize(n_match = sum(interception_f_last == f_last, na.rm = TRUE)) %>%
#   filter(n_match < 1)
# 
# just_interceptions_name_check %>%
#   right_join(plays_to_fix,
#              by = c("game_id", "play_id")) %>%
#   View()

# Looks like the rest are D-line interceptions, not coverage players