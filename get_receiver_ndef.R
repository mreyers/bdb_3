library(tidyverse)

targeted_receiver <- read_csv("Data/targetedReceiver.csv")

process_tracking_data = function(raw_tracking_data){
  df_tracking <- raw_tracking_data %>%
    mutate(x = ifelse(playDirection == "left", 120-x, x),
           y = ifelse(playDirection == "left", 160/3 - y, y)) %>%
    # flip direction
    mutate(dir = if_else(playDirection == "left", (180 + dir) %% 360, dir)) %>% 
    inner_join(df_games %>%
                 select(gameId, homeTeamAbbr, visitorTeamAbbr)) %>%
    inner_join(df_plays %>%
                 select(gameId, playId, possessionTeam)) %>%
    mutate(
      team_off = case_when(
        team == "football" ~ "football",
        ((team == 'home') &
           (possessionTeam == homeTeamAbbr)) |
          
          ((team == 'away') &
             (possessionTeam == visitorTeamAbbr)) ~ 'offense',
        
        ((team == 'home') &
           (possessionTeam == visitorTeamAbbr)) |
          
          ((team == 'away') &
             (possessionTeam == homeTeamAbbr)) ~ 'defense',
        
      ))
}

get_ndef <- function(tracking_data){
  
  df_tracking = process_tracking_data(tracking_data)
  
  pass_arrival_events = c("pass_outcome_interception",
                          "pass_outcome_caught" ,
                          "pass_outcome_incomplete",
                          "pass_outcome_touchdown",
                          "pass_arrived")
  
  receiver_df = df_tracking %>%
    mutate(id = paste0(gameId,playId)) %>% 
    filter(id %in% yac_id$id) %>% 
    left_join(targeted_receiver) %>% 
    group_by(gameId, playId, frameId) %>%
    filter(event %in% pass_arrival_events) %>% 
    ungroup() %>% 
    filter(nflId == targetNflId) %>%
    select(gameId, playId, 
           outcome_frame = frameId, 
           event_outcome = event, 
           rec_s = s, rec_a = a, 
           rec_dis = dis, rec_o = o,
           rec_dir = dir, rec_name = displayName,
           rec_nflid = nflId,
           rec_position = position,
           rec_route = route,
           rec_x_end = x, rec_y_end = y ) %>% 
    arrange(gameId, playId, outcome_frame) %>% 
    group_by(gameId, playId) %>% 
    # get the first frame 
    filter(outcome_frame == first(outcome_frame)) %>% 
    ungroup()
  
  ndef_df = df_tracking %>%
    mutate(id = paste0(gameId,playId)) %>% 
    filter(id %in% yac_id$id) %>% 
    group_by(gameId, playId, frameId) %>%
    filter(event %in% pass_arrival_events) %>% 
    ungroup() %>% 
    group_by(gameId, playId) %>% 
    # get the first frame 
    filter(frameId == first(frameId)) %>% 
    ungroup() %>% 
    filter(team_off == "defense") %>%
    left_join(receiver_df,
              by = c("gameId" = "gameId",
                     "playId" = "playId",
                     "frameId" = "outcome_frame")) %>% 
    rename(def_x_end = x, def_y_end = y, 
           def_s = s, def_a = a, 
           def_dis = dis, def_o = o,
           def_dir = dir, 
           ndef_name = displayName,
           def_nflid = nflId,
           def_position = position,
           def_route = route) %>% 
    mutate(dist_to_receiver = sqrt_dist(def_x_end, rec_x_end, def_y_end, rec_y_end)) %>%
    group_by(gameId, playId, frameId) %>%
    filter(dist_to_receiver == min(dist_to_receiver)) %>% 
    ungroup()
  
  return(ndef_df)
}

week_num = seq(1,17,1)
ndef_data = c()
for ( i in 1:length(week_num)){
  tic()
  df_tracking = data.table::fread(glue::glue("Data/week{week_num[i]}.csv"))
  # df_tracking <- read_csv(glue::glue("Data/week{week_num[i]}.csv"), col_types = cols())
  ndef_data_temp = get_ndef(df_tracking)
  ndef_data_temp$week = glue::glue("week{week_num[i]}")
  ndef_data = bind_rows(ndef_data, ndef_data_temp)
  toc()
}
