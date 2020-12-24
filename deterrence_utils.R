create_train_data <- function(track_week_data, cp_data) {
  #browser()
  
  cp_week_df <-
    cp_data %>% select(game_id, play_id, nfl_id, frame_id, air_yards_x, air_dist, rec_separation, sideline_sep, .pred_C) %>%
    rename(cp = .pred_C)
  
  week_x <-
    track_week_data[!duplicated(track_week_data[, c("nflId", "gameId", "playId", "frameId")]), ]
  
  nest_df <-
    week_x %>% janitor::clean_names() %>%
    left_join(cp_week_df) %>%
    mutate(
      play_dir = play_direction == "left",
      x = if_else(play_dir, 120 - x, x),
      y = if_else(play_dir, 160 / 3 - y, y),
      dir = if_else(play_dir, (180 + dir) %% 360, dir)
    ) %>%
    filter(event %in% c("pass_forward")) %>%
    filter(!is.na(x) | !is.na(y)) %>%
    group_by(nfl_id, game_id, play_id, display_name) %>%
    tidyr::nest(tracking_data = c(
      x,
      y,
      s,
      o,
      a,
      cp,
      air_yards_x,
      air_dist,
      rec_separation,
      sideline_sep,
      dis,
      dir,
      time,
      frame_id,
      event
    ))
  
  
  nest_test = nest_df %>%  left_join(game_deets) %>% left_join(play_df) %>%
    filter(!is.null(possession_team), !team == "football") %>%
    mutate(
      player_team = if_else(team == "away", visitor_team_abbr, home_team_abbr),
      offense = player_team == possession_team,
      defense = !offense
    )
  
  play_nest = nest_test %>% select(
    play_id,
    game_id,
    nfl_id,
    yardline_side,
    yardline_100,
    half_seconds_remaining,
    team,
    display_name,
    offense,
    defense,
    position,
    tracking_data
  ) %>% group_by(game_id, play_id) %>% tidyr::nest()
  
  play <-
    play_nest %>% group_by(game_id, play_id) %>%  mutate(new_df = purrr::map(data, extract_receiver)) 
  
  return(play)
}

extract_receiver <- function(play_dat){
  
  receivers_df <- play_dat %>% filter(offense,!team=="football") %>% unnest()
  
  defenders_df <- play_dat %>% filter(defense,!team=="football") %>% unnest()  
  
  
  if(nrow(receivers_df)==0 | nrow(defenders_df)==0){
    return(NA)
  }
  
  qb_id <- receivers_df %>% filter(position=="QB") %>% slice(1) %>% pull(nfl_id)
  
  receivers_df <- receivers_df %>% filter(!position=="QB")
  
  if(length(qb_id)==0){
    qb_id = NA
  }
  
  receivers_df <- 
    receivers_df %>% 
    group_by(nfl_id) %>%
    mutate(closest_defensive_player = 
             purrr::map2_dbl(x, y,  ~ extract_location_at_pass_release(.x, .y, defenders_df)),
           qb_id_f = qb_id)
  
  final_df =  receivers_df %>% select(nfl_id,team,display_name,closest_defensive_player,qb_id_f)
  return(final_df)
}

extract_defender <- function(play_dat){
 
  defenders_df <- play_dat %>% filter(defense,!team=="football") %>% unnest()  
  
  offense_df <- play_dat %>% filter(offense) %>% unnest()
  
  if(nrow(defenders_df)==0){
    return(NA)
  }
  
  qb_id <- offense_df %>% filter(position=="QB") %>% slice(1) %>% pull(nfl_id)
  
  if(length(qb_id)==0){
    qb_id = NA
  }
  
  defenders_df <-
    defenders_df %>%
    group_by(nfl_id) %>%
    mutate(closest_offensive_player = 
             purrr::map2_dbl(x, y,  ~ extract_location_at_pass_release(.x, .y, offense_df)),
           qb_id_f = qb_id)
  
  final_df = defenders_df %>% select(nfl_id,team,display_name,closest_offensive_player,qb_id_f)
  return(final_df)
}


extract_location_at_pass_release <- function(ref_x,ref_y,off_df){
  if(is.null(ref_x) | is.null(ref_y) | nrow(off_df)==0){
    return(NA)
  }
  
  off_distance = off_df %>% 
    mutate(dist_to_offense = sqrt((ref_x - x) ^
                                     2 + (ref_y - y) ^ 2)) 
  
  ## pull minimum value id 
  min_id <- which.min(off_distance$dist_to_offense)
  min_distance <- off_distance$dist_to_offense[min_id]
  if(min_distance<=5){
    closest_defender = off_distance$nfl_id[min_id]
    return(closest_defender)
  }
  return(NA)
}
