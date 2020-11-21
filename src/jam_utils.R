filter_func <- function(los_x,play_dir,track_df){
  #' Filter to make sure the tracking data 
  #' only considers points up until the first 
  #' five yards past the LOS
  if(play_dir){
    final_df = track_df %>% filter(x>=(los_x-5))
    return(final_df)
  }
  final_df = track_df %>% filter(x<=(los_x+5))
  return(final_df)
}



closest_defender <- function(x_loc_target,y_loc_target, def_track_df){
  #' Determine the closest defender based on euclidean distance
  defense_distance = def_track_df %>% 
    mutate(dist_to_receiver = sqrt((x_loc_target - x) ^
                                     2 + (y_loc_target - y) ^ 2)) %>% 
    arrange(dist_to_receiver) %>% filter(dist_to_receiver<=5)
  
  if(nrow(defense_distance)==0){
    return(NA)
  }
  closest_defender = defense_distance %>% arrange(dist_to_receiver) %>% slice(1) %>% pull(nflId)
  return(closest_defender)
}



create_train_data <- function(week_df) {
  #browser()
  # remove possible duplicates which seem to occur 
  week_df <- week_df[!duplicated(week_df[,c("nflId","gameId","playId","frameId")]),]
  
  # nest the tracking data by player
  print("Process the tracking data")
  nest_df <- week_df %>%
    mutate(play_dir = playDirection == "left") %>% group_by(nflId, gameId, playId, displayName) %>%
    tidyr::nest(tracking_data = c(x, y, s, o, a, dis, dir, time, frameId, event))
  
  # join game details + play information
  tracking = nest_df %>% left_join(game_deets) %>% left_join(play_df)
  
  # defense players at snap (grouped by play)
  print("Process the defense tracking data")
  def_tracking = tracking %>% mutate(player_team = if_else(team == "away", visitorTeamAbbr, homeTeamAbbr)) %>%
    filter(player_team != possessionTeam) %>%
    unnest() %>% filter(team %in% c("home", "away"), event == "ball_snap") %>%
    select(gameId, playId, nflId, displayName, x, y, s, o, a, dis, dir, time) %>%
    group_by(gameId, playId) %>%
    nest(def_tracking = c(nflId, displayName, x, y, s, o, a, dis, dir, time))
  
  ## Receivers Tracking
  rec_tracking = tracking %>% filter(!is.na(route),!is.na(los_x)) %>%
    mutate(
      five_yards_track = purrr::pmap(list(los_x, play_dir, tracking_data), filter_func),
      top_speed = purrr::map_dbl(five_yards_track, function(df) {
        max(df$s, na.rm = T)
      }),
      player_x_loc = purrr::map_dbl(five_yards_track, function(df) {
        snap_df = df %>% filter(event == "ball_snap")
        snap_df %>% slice(1) %>% pull(x) 
      }),
      player_y_loc = purrr::map_dbl(five_yards_track, function(df) {
        snap_df = df %>% filter(event == "ball_snap")
        snap_df %>% slice(1) %>% pull(y)
      })
    ) %>% left_join(def_tracking) %>% 
    filter(!map_lgl(def_tracking,is.null)) %>% 
    group_by(displayName) %>% 
    mutate(defender_id = purrr::pmap_dbl(
      list(player_x_loc, player_y_loc, def_tracking),
      closest_defender
    ))
  
  # can remove all the nested dataframes in future.
  return(rec_tracking)
}

create_z_scores <- function(full_df){
  ## create z-scores
  print("Create Z-Scores and Find Closest Defender")
  rec_z_scores = full_df %>% select(
    -homeTeamAbbr,
    -visitorTeamAbbr) %>%
    group_by(displayName, route) %>%
    mutate(top_speed_z = as.vector(scale(top_speed))) %>%
    ungroup() 
    
  return(rec_z_scores)
}
