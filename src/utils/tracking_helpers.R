# College submission helper functions 

# First, the functions from the original big data bowl and relevant stuff for whole play analysis
# These are going under a bit of a refactoring to accommodate not using lapply

# Fix the animate and plot functions for standardized data, primarily the gif for gut checking
animate_play <- function(example.play, standardized = FALSE, complete = NA){
  # Feed in one plays worth of complete data for both sides to see the beautiful gif
  # Does not return an object
  
  xmin <- 0
  xmax <- 160/3
  hash.right <- 38.35
  hash.left <- 12
  hash.width <- 3.3
  
  if(standardized){
    # Add back the line of scrimmage to all observations
    example.play <- example.play %>%
      mutate(play_dir = play_direction == "left",
             x = if_else(play_dir, x + 120 - line_of_scrimmage, x + line_of_scrimmage))
  }
  example.play$comp_perc <- NA 
  # Add completion probability to receivers
  if(!is.na(complete)){
    complete <- complete %>%
      dplyr::select(display_name, frame_id = frame_id_2, completion_prob_pred)
    
    example.play <- example.play %>%
      left_join(complete) %>%
      mutate(comp_perc = completion_prob_pred,
             col_f = as.factor(comp_perc %/% 20)) %>%
      dplyr::select(-completion_prob_pred)
  }
  
  ## Specific boundaries for a given play
  ymin <- max(round(min(example.play$x, na.rm = TRUE) - 10, -1), 0)
  ymax <- min(round(max(example.play$x, na.rm = TRUE) + 10, -1), 120)
  df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
  df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
  df.hash <- df.hash %>% filter(y < ymax, y > ymin)
  
  
  animate.play <- ggplot() +
    geom_point(data = example.play, aes(x = (xmax-y), y = x, 
                                        colour = team, group = nfl_id, pch = team, size = team)) + 
    geom_text(data = example.play, aes(x = (xmax-y), y = x, label = jersey_number), colour = "white", 
              vjust = 0.36, size = 3.5) + 
    geom_text(data = example.play, aes(x = (xmax-y), y = x + 3, label = comp_perc), colour = 'blue') +
    scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
    scale_shape_manual(values = c(19, 16, 19), guide = FALSE) +
    scale_colour_manual(values = c("#e31837", "#654321", "#002244"), guide = FALSE) + 
    annotate("text", x = df.hash$x[df.hash$x < 55/2], 
             y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
    annotate("text", x = df.hash$x[df.hash$x > 55/2], 
             y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
    annotate("segment", x = xmin, 
             y = seq(max(10, ymin), min(ymax, 110), by = 5), 
             xend =  xmax, 
             yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
    annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
             label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
             angle = 270, size = 4) + 
    annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
             label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
             angle = 90, size = 4) + 
    annotate("segment", x = c(xmin, xmin, xmax, xmax), 
             y = c(ymin, ymax, ymax, ymin), 
             xend = c(xmin, xmax, xmax, xmin), 
             yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
    ylim(ymin, ymax) + 
    coord_fixed() +  
    theme_nothing() + 
    transition_time(frame_id)  +
    ease_aes('linear') + 
    NULL
  
  ## Ensure timing of play matches 10 frames-per-second
  play.length.ex <- length(unique(example.play$frame_id))
  animate(animate.play, fps = 7, nframe = play.length.ex)
}

# The play by play tracking data is stored in one big csv for now
# Need a function to read in the data and parse it accordingly
read_tracking_data <- function(file_path){
  # Most efficient function I have found for this is the fread function from library(data.table)
  basic_data <- fread(file_path, header = T, sep = ',') %>%
    rename_all(to_snake_case) %>%
    rename(velocity = s)
  
  return(basic_data)
}

# Read multiple files simultaneously provided a directory
read_many_tracking_data <- function(data_directory, seq_files = NA){
  
  root <- paste0(getwd(), data_directory)
  
  games_tracking <- paste0(root, list.files(root, "^tracking"))
  
  if(is.na(seq_files)){
    read_in_games <- lapply(games_tracking, read_tracking_data) %>%
      bind_rows()
  } else{
    read_in_games <- lapply(games_tracking[seq_files], read_tracking_data) %>%
      bind_rows()
  }
  
  
  return(read_in_games)
}



# Join the tracking data with the other datasets (plays, players, and games)
join_additional_data <- function(tracking_data, data_directory,
                                 games = FALSE, players = FALSE, plays = FALSE){
  # Takes generic tracking data and joins it with other relevant input files
  # Only joins to the data sets as necessary, allows users to specify what matters to them
  root <- paste0(getwd(), data_directory)
  
  if(games){
    games <- read_csv(paste0(root, 'games.csv'), col_types = cols()) %>%
      rename_all(to_snake_case)
    
    tracking_data <- tracking_data %>%
      left_join(games, by = c('game_id'))
  }
  
  if(players){
    players <- read_csv(paste0(root, 'players.csv'), col_types = cols()) %>%
      rename_all(to_snake_case) %>%
      select(nfl_id, position = position_abbr, weight = weight)
    
    tracking_data <- tracking_data %>%
      left_join(players, by = c('nfl_id'), col_types = cols())
  }
  
  if(plays){
    plays <- read_csv(paste0(root, 'plays.csv')) %>%
      rename_all(to_snake_case)
    
    tracking_data <- tracking_data %>%
      left_join(plays, by = c("game_id", "play_id"))
  }
  
  return(tracking_data)
}

# Store the R matrix in each row
makeMatrix <- function(dir) {
  dir <- dir * pi / 180
  mat <- matrix(c(cos(dir), -sin(dir), sin(dir), cos(dir)), byrow = T, nrow = 2)
}

# Now to calculate R for each value
# Use Lucas' magic value calculator (3rd order polynomial, represents a max dist curve)
get_R_i_t = function(x){
  magic_value = ifelse(x<0,
                       yes = 0,
                       no = ifelse(x<=18.5,
                                   yes = 3.969599761 + 0.068431838*x - 0.008837561*x^2 + 0.001229134* x^3,
                                   no = 10))
  return(magic_value)
}

# Establish the zone influence on a frame by frame basis
# This will need to be changed, no rusher interest anymore
get_zone_influence <- function(data, standardized = FALSE, start_or_end = 'start',
                               lazy = FALSE, run = FALSE){
  
  if(standardized){
    # Has been standardized, rename 2 variables
    myPlay <- data %>%
      mutate(velocity = velocity_2, dir = angle)
  } else{
    myPlay <- data 
  }
  if(start_or_end %in% 'start'){
    # Want to filter down to the start of a pass
    pass_cond <- c('pass_forward', 'pass_shovel')
  } else{
    pass_cond <- 
      c("pass_arrived",
        "pass_outcome_caught",
        "pass_outcome_incomplete",
        # "pass_tipped",
        "touchdown",
        "pass_outcome_interception",
        "pass_outcome_touchdown"
      )
  }
  
  if(run){
    pass_cond <- c('run', 'tackle', 'first_contact', 'out_of_bounds', 
                   'qb_sack', 'qb_strip_sack')
  }
  # lazy is for my group mutate step, allows me to artificially make everything the throw frame
  if(lazy){
    throw_frame <- myPlay %>%
      ungroup() %>%
      slice(1) %>%
      pull(frame_id) %>%
      as.numeric()
    
  } else{
    # get the frame of throw
    throw_frame <- myPlay  %>% 
      filter(event %in% pass_cond) %>% 
      ungroup() %>%
      slice(1) %>%
      dplyr::select(frame_id) %>%
      summarize(frame = first(frame_id), .groups = "drop") %>%
      pull(frame) %>%
      as.numeric()
  }

  
  # Input speeds are in yards per second, convert to metres per second for formula
  throw_time_positions <- myPlay %>%
    filter(frame_id %in% throw_frame) 
  
  # Dont need qb location, need ball location instead
  ball_loc <- 
    throw_time_positions %>%
    filter(team %in% "football") %>%
    select(x, y)
  
  qb_loc <- 
    myPlay %>%
    filter(frame_id %in% throw_frame) %>%
    filter(position %in% 'QB') %>%
    slice(1)
  
  # Calculate mu_i, Sratio, distance to ball, and non-zero elements of S matrix
  throw_time_positions <- 
    throw_time_positions %>%
    mutate(mu_i_x = x + velocity * cos(dir * pi / 180) * 0.5,
           mu_i_y = y + velocity * sin(dir * pi / 180) * 0.5,
           # Max observed speed was 22.11 mph, say 24 mph. Denom is in m/s
           speed_rat = velocity ^ 2 / (10.729^2),
           speed_x = velocity * cos(dir * pi / 180),
           speed_y = velocity * sin(dir * pi / 180),
           dis_to_ball = sqrt((x - ball_loc$x) ^ 2 +
                                (y - ball_loc$y) ^ 2)) %>% 
    ungroup()
  
  # Get positions at time of handoff, really just the same as the individual frame we have
  throw_time_positions <- 
    throw_time_positions %>% 
    mutate(team2 = ifelse((team == poss_team), "Off","Def"))
  
  throw_time_positions <- 
    throw_time_positions %>%
    mutate(R = purrr::map(dir, makeMatrix)) #dir -> Dir
  
  # Use get_R_i_t to get the value in df
  throw_time_positions <- 
    throw_time_positions %>%
    mutate(R_i_t = get_R_i_t(dis_to_ball),
           S_i_t = map2(speed_rat, R_i_t, ~ matrix(
             c((.y + .y * .x) / 2,
               0,
               0,
               (.y - .y * .x) / 2),
             byrow = T, nrow = 2)),
           mu_i = map2(mu_i_x, mu_i_y, ~ matrix(c(.x, .y), nrow = 2)))
  
  throw_time_positions <- 
    throw_time_positions %>% 
    mutate(cov_struc = map2(R, S_i_t, ~ .x %*% .y %*% .y %*% solve(.x)))
  
  play_dir <- myPlay %>%
    slice(1) %>%
    pull(play_direction)
  
  # Restrict field for efficiency
  if(play_dir %in% 'left'){
    # QB is furthest right that we care about
    x_bounds <-
      throw_time_positions %>% 
      transmute(min_x = 0,
                max_x = qb_loc$x) %>% 
      slice(1)
  } else{
    # QB is furthest left that we care about
    x_bounds <-
      throw_time_positions %>% 
      transmute(min_x = qb_loc$x,
                max_x = 120) %>% 
      slice(1)
  }

  # We can now calculate the density of a player over the whole field
  data_grid <- expand.grid(s_1 = seq(x_bounds$min_x, x_bounds$max_x, by = 1),
                           s_2 = seq(-10, 53, by = 1)) # previously res, now 1
  
  # Now we will try to do the same thing over all the players in one play
  # This needs to be normalized by player position at handoff
  density_calc <- 
    throw_time_positions %>% 
    mutate(current_density = pmap_dbl(list(x, y, mu_i, cov_struc),
                                      ~ dmvnorm(c(..1, ..2), mean = ..3, sigma = ..4))) %>%
    mutate(density_val = pmap(list(mu_i, cov_struc, current_density),
                              ~ dmvnorm(data_grid, mean = ..1, sigma = ..2) / ..3))
  
  # Nested dataframe doesnt need to select play_id and game_id
  if("play_id" %in% colnames(myPlay)){
    density_field <-
      density_calc %>% 
      dplyr::select(density_val, nfl_id, play_id, game_id, team, team2, jersey_number) %>% 
      mutate(density_val = map(density_val, ~ bind_cols(as_tibble(.x), as_tibble(data_grid)))) %>% 
      unnest(cols = c(density_val))
  } else{
    density_field <-
      density_calc %>% 
      dplyr::select(density_val, nfl_id, team, team2, jersey_number) %>% 
      mutate(density_val = map(density_val, ~ bind_cols(as_tibble(.x), as_tibble(data_grid)))) %>% 
      unnest(cols = c(density_val))
  }
  
  
  # Next is the player influence
  influence <- density_field %>% 
    group_by(nfl_id) %>% 
    dplyr::arrange(nfl_id, desc(value)) %>%
    mutate(influence_unscaled = value / first(value)) %>%
    ungroup() %>% 
    filter(value > 1.30e-10) %>%
    group_by(s_1, s_2, team) %>%
    summarize(team_total = sum(value), .groups = "drop") %>%
    group_by(s_1, s_2) %>%
    arrange(s_1, s_2, team) %>%
    summarize(away_inf = 1 / (1 + exp(-(first(team_total) - last(team_total))/ sum(team_total) )), 
              home_inf = 1 / (1 + exp(-(last(team_total) - first(team_total))/ sum(team_total) )),
              .groups = "drop") 
  
  return(influence)
}

# Plot the zone influence for any given frame input
get_inf_plot <- function(influence_data, throw_time_positions){
  
  # Need to modify end positions based on play orientation, influence is done right, just plot fix
  if(throw_time_positions$play_direction[1] == "left"){
    mod <- 0
  } else{ mod <- pi}
  
  end.frame <- 
    throw_time_positions %>%
    mutate(x.end = x + velocity * cos(dir * pi / 180 + mod),
           y.end = y + velocity * sin(dir * pi / 180 + mod)) %>%
    ungroup()
  
  xmin <- 0
  xmax <- 160/3
  hash.right <- 38.35
  hash.left <- 12
  hash.width <- 3.3
  
  
  ## Specific boundaries for a given play
  ymin <- 0
  ymax <- 120
  
  df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
  df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
  df.hash <- df.hash %>% filter(y < ymax, y > ymin)
  
  poss_team <- throw_time_positions %>%
    ungroup() %>%
    slice(1) %>%
    pull(poss_team) # This is in the standardized_calculation now and can be pulled as such
  
  influence_data <- influence_data %>%
    ungroup() %>%
    mutate(inf = case_when(poss_team %in% 'home' ~ home_inf,
                           TRUE ~ away_inf)) %>%
    filter(inf != 0.5) # Deal with weird background shading for non-influenced points
  
  throw_time_positions <- throw_time_positions %>%
    mutate(team2 = case_when(poss_team == 'away' & team == 'away' ~ "Off",
                             poss_team == 'home' & team == 'home' ~ "Off",
                             poss_team == 'home' & team == 'away' ~ "Def",
                             poss_team == 'away' & team == 'home' ~ "Def",
                             TRUE ~ "ball"))
  
  inf_plot <- influence_data %>% 
    ggplot(aes(x = s_1, y = s_2)) +
    geom_tile(aes(fill = inf)) +
    scale_fill_gradientn(colours = c("blue","white","red"), 
                         values = rescale(c(0, 0.5, 1)),
                         guide = "colorbar") +
    coord_fixed(xlim = c(0, 120), ylim = c(-5, 53), ratio = 1 ) +
    geom_segment(data = throw_time_positions,
                 aes(x=x, y=y, xend=end.frame$x.end, yend=end.frame$y.end),
                 arrow = arrow(angle = 20, length = unit(0.4, "cm")),
                 size = 0.75, color="black") +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text  = element_blank(),
          panel.background = element_blank()) +
    annotate("text", y = df.hash$x[df.hash$x < 55/2], 
             x = df.hash$y[df.hash$x < 55/2], label = "I", vjust = 0, hjust = -0.2) + 
    annotate("text", y = df.hash$x[df.hash$x > 55/2], 
             x = df.hash$y[df.hash$x > 55/2], label = "I", vjust = 1, hjust = -0.2) + 
    annotate("segment", y = xmin, 
             x = seq(max(10, ymin), min(ymax, 110), by = 5), 
             yend =  xmax, 
             xend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
    annotate("text", 
             y = rep(hash.left, 11), 
             x = seq(10, 110, by = 10), 
             label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
             angle = 270, size = 4) + 
    annotate("text", 
             y = rep((xmax - hash.left), 11), 
             x = seq(10, 110, by = 10), 
             label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
             angle = 90, size = 4) + 
    annotate("segment", 
             y = c(xmin, xmin, xmax, xmax), 
             x = c(ymin, ymax, ymax, ymin), 
             yend = c(xmin, xmax, xmax, xmin), 
             xend = c(ymax, ymax, ymin, ymin), colour = "black") + 
    ylim(xmin, xmax) +
    geom_point(data = throw_time_positions,
               aes(x=x,y=y,color=team2), size =8) +
    scale_color_manual(values = c("Def" = "lightblue", "Off" = "red", "ball" = "brown")) +
    geom_text(data = throw_time_positions,
              aes(x=x,y=y,group=nfl_id,label=jersey_number),color='black')
  
  return(inf_plot)
}

# Calculate a gradient for the field given two reference frames
calc_gradient <- function(frame_influence, handoff_time_positions){
  
  # Use the gradient value derived from the sigmoid function since we are using sigmoid
  poss_team <- handoff_time_positions %>%
    slice(1) %>%
    pull(poss_team)
  
  if(poss_team %in% "home"){
    grad_f <- frame_influence %>%
      mutate(grad_f = home_inf * (1 - home_inf)) %>%
      select(s_1, s_2, grad_f) %>%
      filter(grad_f != 0.25)
  } else{
    grad_f <- frame_influence %>%
      mutate(grad_f = away_inf * (1 - away_inf)) %>%
      select(s_1, s_2, grad_f) %>%
      filter(grad_f != 0.25)
  }
  return(grad_f)
}

# Plot the gradient calculated previously
get_grad_plot <- function(gradient_results, handoff_time_positions){
  
  # Make minor modifications to the data frame to allow for the base zone influence plot to work
  gradient_results <- gradient_results %>%
    mutate(home_inf = grad_f, away_inf = grad_f)
  
  return(get_inf_plot(gradient_results, handoff_time_positions))
}

# GIF a collection of frames, be it a given play and influence or a gradient and influence
make_gif <- function(example_play, inf_or_grad = 'inf'){
  
  handoff_time_positions <- example_play %>%
    slice(1)
  
  xmin <- 0
  xmax <- 160/3
  hash.right <- 38.35
  hash.left <- 12
  hash.width <- 3.3
  
  
  ## Specific boundaries for a given play
  ymin <- 0
  ymax <- 120
  
  df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
  df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
  df.hash <- df.hash %>% filter(y < ymax, y > ymin)
  
  poss_team <- example_play %>%
    slice(1) %>%
    pull(poss_team)
  
  # Make influence / grad data for each frame in the play
  influence_data <- example_play %>%
    mutate(frame_id_2 = frame_id) %>%
    nest(-game_id, -play_id, -frame_id_2) %>%
    #filter(frame_id >= handoff_frame, frame_id <= (handoff_frame + n_frames)) %>%
    mutate(inf = map(data, ~get_zone_influence(., lazy = TRUE)),
           grad = map2(inf, data, ~ calc_gradient(.x, .y)))
  print(influence_data)
  if(inf_or_grad %in% 'inf'){
    
    influence_data <- influence_data %>%
      select(game_id, play_id, frame_id_2, inf) %>%
      unnest(cols = c(inf)) %>%
      filter(home_inf != 0.5, away_inf != 0.5)
  } else{
    
    influence_data <- influence_data %>%
      select(game_id, play_id, frame_id_2, grad) %>% 
      unnest() %>%
      filter(grad_f != 0.25) %>%
      mutate( home_inf = grad_f, away_inf = grad_f)
  }
  
  influence_data <- influence_data %>%
    mutate(inf = case_when(poss_team %in% 'home' ~ home_inf,
                           TRUE ~ away_inf)) %>%
    rename(frame_id = frame_id_2)
  
  # Dont currently have poss_team in my example_play, need to resolve to get this to work
  handoff_time_positions <- handoff_time_positions %>%
    mutate(team2 = case_when(poss_team == 'away' & team == 'away' ~ "Off",
                             poss_team == 'home' & team == 'home' ~ "Off",
                             poss_team == 'home' & team == 'away' ~ "Def",
                             poss_team == 'away' & team == 'home' ~ "Def",
                             TRUE ~ "ball"))
  
  example_play <- example_play %>%
    mutate(team2 = case_when(poss_team == 'away' & team == 'away' ~ "Off",
                             poss_team == 'home' & team == 'home' ~ "Off",
                             poss_team == 'home' & team == 'away' ~ "Def",
                             poss_team == 'away' & team == 'home' ~ "Def",
                             TRUE ~ "ball"))
  
  # Plotting the zone influence
  inf_plot <- 
    influence_data %>% 
    # Some have fractional values for some reason
    mutate(s_1 = round(s_1), s_2 = round(s_2)) %>% 
    ggplot(aes(x = s_1, y = s_2)) +
    # need to play around with home_inf and away_inf
    geom_tile(aes(fill = inf)) +
    scale_fill_gradientn(colours = c("blue","white","red"), 
                         values = rescale(c(0, 0.5, 1)), # If plot caps at 0.73, this might be why
                         guide = "colorbar") +
    coord_fixed(xlim = c(0, 120), ylim = c(-5, 53), ratio = 1 ) +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text  = element_blank(),
          panel.background = element_blank()) +
    annotate("text", y = df.hash$x[df.hash$x < 55/2], 
             x = df.hash$y[df.hash$x < 55/2], label = "I", vjust = 0, hjust = -0.2) + 
    annotate("text", y = df.hash$x[df.hash$x > 55/2], 
             x = df.hash$y[df.hash$x > 55/2], label = "I", vjust = 1, hjust = -0.2) + 
    annotate("segment", y = xmin, 
             x = seq(max(10, ymin), min(ymax, 110), by = 5), 
             yend =  xmax, 
             xend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
    annotate("text", 
             y = rep(hash.left, 11), 
             x = seq(10, 110, by = 10), 
             label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
             angle = 270, size = 4) + 
    annotate("text", 
             y = rep((xmax - hash.left), 11), 
             x = seq(10, 110, by = 10), 
             label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
             angle = 90, size = 4) + 
    annotate("segment", 
             y = c(xmin, xmin, xmax, xmax), 
             x = c(ymin, ymax, ymax, ymin), 
             yend = c(xmin, xmax, xmax, xmin), 
             xend = c(ymax, ymax, ymin, ymin), colour = "black") + 
    ylim(xmin, xmax) +
    #geom_point(data = example_play %>% filter(frame_id <= handoff_frame + n_frames,
    #                                          frame_id >= handoff_frame),
    #           aes(x=x,y=y,color=team2), size =8) +
    geom_point(data = example_play,
               aes(x=x,y=y,color=team2), size =8) +
    scale_color_manual(values = c("Def" = "lightblue", "Off" = "red", "ball" = "brown")) +
    #geom_text(data = example_play %>% filter(frame_id <= handoff_frame + n_frames,
    #                                         frame_id >= handoff_frame),
    #          aes(x=x,y=y,group=nfl_id,label=jersey_number),color='black') +
    geom_text(data = example_play,
              aes(x=x,y=y,group=nfl_id,label=jersey_number),color='black') +
    theme_nothing() +
    transition_time(frame_id)  +
    ease_aes('linear') +
    NULL

  play_length_ex <- length(unique(example_play$frame_id)) 
  animate(inf_plot, fps = 10, nframe = play_length_ex)
}

# Include summer work standardization for future modeling
standardize_play <- function(pass_play, reorient = FALSE){
  
  data <- pass_play
  
  line_of_scrimmage <-
    data %>% 
    filter(event %in% c("ball_snap", "snap_direct"), team %in% c("home", "away")) %>% 
    group_by(game_id, play_id, team) %>% 
    summarise(right_scrim = max(x, na.rm=TRUE), left_scrim = min(x, na.rm=TRUE),
              poss_team = first(poss_team)) %>%
    filter(team == poss_team) %>%
    select(game_id, play_id, right_scrim, left_scrim)
  
  play_direction <- data %>%
    filter(event %in% c("ball_snap", "snap_direct")) %>%
    group_by(game_id, play_id, team) %>%
    summarise(mean_team = mean(x, na.rm=TRUE)) %>%
    filter(team != "ball") %>%
    filter(mean_team == max(mean_team)) %>%
    dplyr::select(game_id, play_id, direction_left = team, -mean_team)

  possession <- plays %>%
    dplyr::select(game_id, play_id, possession_team) %>%
    left_join(games, by = "game_id") %>%
    mutate(possession_t = if_else(possession_team == home_team_abbr, "home", "away")) %>%
    dplyr::select(game_id, play_id, possession_t)

  data <- data %>%
    left_join(play_direction, by=c("game_id", "play_id")) %>%
    left_join(line_of_scrimmage, by = c("game_id", "play_id")) %>%
    left_join(possession, by = c("game_id", "play_id")) %>%
    mutate(line_of_scrimmage = if_else(possession_t == direction_left, left_scrim, right_scrim),
           play_direction = if_else(poss_team == direction_left, "left", "right")) %>%
    dplyr::select(-right_scrim, -left_scrim, -possession_t)%>%
    group_by(nfl_id) %>% 
    mutate(velocity_2 = sqrt((dplyr::lead(x, 2, default = 0) - dplyr::lag(x, 2, default = 0))^2 +
                               (dplyr::lead(y, 2, default = 0) - dplyr::lag(y, 2, default = 0)) ^ 2),
           angle = (base::atan2(dplyr::lead(y, 2, default = 0) - dplyr::lag(y, 2, default = 0), 
                                dplyr::lead(x, 2, default = 0) - dplyr::lag(x, 2, default = 0)) * 180/pi) %% 360,
           dir = angle) %>%
    ungroup() %>%
    drop_na(x)
  
  if(reorient){
    data <- data %>% 
      mutate(# flip the field to try and get all routes in the same direction
        play_dir = play_direction == "left",
        x = if_else(play_dir, 120 - x, x),
        y = if_else(play_dir, 160/3 - y, y),
        x = if_else(play_dir, x - (120 - line_of_scrimmage), x - line_of_scrimmage)) # really just x - line_of_scrimmage
  }
  
  return(data)
}

# Build a function to get EP for ownership
ep_for_field_ownership <- function(standardized_play, 
                                   appropriate_inf){
  # Store nfl_scrapr_playdata somewhere globally
  # Currently stored as nfl_2017_small
  line_of_scrimmage <- round(standardized_play$line_of_scrimmage[1]) # Need integers here
  play_dir <- standardized_play$play_direction[1]
  game_id_arg <- standardized_play$game_id[1]
  play_id_arg <- standardized_play$play_id[1]
  
  # Dont need runner location because field is already condensed to only be forward looking
  play_starting_params <- nfl_2017_small %>%
    filter(game_id == game_id_arg, play_id == play_id_arg) %>%
    select(posteam_type, down, ydstogo, yardline_100)
  
  # Fail condition: play not in both sets for some reason
  if(dim(play_starting_params)[1] == 0){
    return(NA_real_)
  }
  
  # Group influence frame by x coordinate and count
  if(play_starting_params$posteam_type %in% "home"){
    appropriate_inf <- appropriate_inf %>%
      mutate(off_inf = home_inf)
  } else{
    appropriate_inf <- appropriate_inf %>%
      mutate(off_inf = away_inf)
  }
  
  condensed <- appropriate_inf %>%
    filter(off_inf > 0.5) %>%
    group_by(s_1) %>%
    summarize(n_cells = n())
  
  denom <- sum(condensed$n_cells)
  
  condensed <- condensed %>%
    mutate(proportion = n_cells / denom)
  
  # Fail condition
  if(dim(condensed)[1] == 0){
    # no space owned by offence
    res <- play_starting_params %>%
      mutate(down = min(down + 1, 4)) %>%
      left_join(full_val_grid) %>%
      mutate(exp_ep_next = avg_ep)
    
    return(res %>% pull(exp_ep_next))
  }
  
  # Now calculate the new situation for each play
  # assuming line_of_scrimmage is in tracking coord system
  change_df <- data.frame(los = line_of_scrimmage,
                          yardlines = ceiling(condensed$s_1),
                          probabilities = condensed$proportion)
  
  if(play_dir %in% 'left'){
    change_df <- change_df %>%
      nest() %>%
      bind_cols(play_starting_params) %>%
      unnest() %>%
      rowwise() %>%
      mutate(net_yards = los - yardlines,
             new_ydstogo = ydstogo - net_yards,
             new_down = case_when(new_ydstogo <= 0 ~ 1,
                                  down == 4 ~ NA_real_,
                                  TRUE ~ down + 1),
             new_ydstogo = ifelse(new_ydstogo <= 0, 10, new_ydstogo),
             new_yardline_100 = yardline_100 - net_yards,
             new_yardline_100 = ifelse(is.na(new_down), 100 - new_yardline_100, new_yardline_100),
             new_ydstogo = ifelse(new_ydstogo <= 0 | new_yardline_100 < 10,
                                  min(10, new_yardline_100), new_ydstogo),
             new_ydstogo = ifelse(is.na(new_down), 10, new_ydstogo),
             modifier = ifelse(is.na(new_down), -1, 1), # Need to make new ep negative on turnovers
             new_down = ifelse(is.na(new_down), 
                               1, # Turnover case
                               new_down)) # Standard
    
  } else{
    change_df <- change_df %>%
      nest() %>%
      bind_cols(play_starting_params) %>%
      unnest() %>%
      rowwise() %>%
      mutate(net_yards = yardlines - los,
             new_ydstogo = ydstogo - net_yards,
             new_down = case_when(new_ydstogo <= 0 ~ 1,
                                  down == 4 ~ NA_real_,
                                  TRUE ~ down + 1),
             new_yardline_100 = yardline_100 - net_yards,
             new_yardline_100 = ifelse(is.na(new_down), 100 - new_yardline_100, new_yardline_100),
             new_ydstogo = ifelse(new_ydstogo <= 0 | new_yardline_100 < 10,
                                  min(10, new_yardline_100), new_ydstogo),
             new_ydstogo = ifelse(is.na(new_down), 10, new_ydstogo),
             modifier = ifelse(is.na(new_down), -1, 1), # Need to make new ep negative on turnovers
             new_down = ifelse(is.na(new_down), 
                               1, # Turnover case
                               new_down)) # Standard
  }
  
  # Now to get ep for each 
  change_df <- change_df %>%
    left_join(full_val_grid, by = c("new_ydstogo" = "ydstogo",
                                    "new_down" = "down",
                                    "new_yardline_100" = "yardline_100")) %>%
    mutate(avg_ep = avg_ep * modifier,
           exp_ep_next = case_when(
             yardlines > 10 & yardlines < 109 & new_yardline_100 > 0 ~ probabilities * avg_ep,
             yardlines <= 10 & play_dir == 'right' ~ probabilities * (-2 + -1.5),
             yardlines >= 109 & play_dir == 'left' ~ probabilities * (-2 + -1.5),
             TRUE ~ probabilities * 6.5))
  
  final_res <- change_df %>%
    ungroup() %>%
    summarize(exp_ep_next = sum(exp_ep_next)) %>%
    pull(exp_ep_next)
  
  return(final_res)
}

# Load and prepare data for the ep over expectation set up
ep_data_prep <- function(seq_files){
  
  test_read <- read_many_tracking_data("/Data/tracking_data/", seq_files)
  
  # Add additional data of interest
  test_read <- join_additional_data(test_read, "/Data/tracking_data/", 
                                    players = TRUE)
  
  # Running plays have a n_handoff value > 0
  run_plays <- running_plays(test_read)
  
  # Set possession team
  possession <- get_possession_team(test_read)
  
  # Filter the actual data to only be rushing plays
  test_read <- test_read %>%
    left_join(run_plays) %>%
    left_join(possession) %>%
    filter(n_handoff > 1/200) %>% # Filter out weird events where only one player sees handoff, no one else
    select(-n_handoff)
  
  # Grab just the run plays
  # Bug with dis_to_ball, fix, unsure which play is breaking
  run_plays <- test_read %>%
    nest(-game_id, -play_id) %>%
    mutate(nfl_id_rusher = map_dbl(data, ~make_nfl_id_rusher(.)))
  
  return(run_plays)
}

# Calculate our value over average metric for RB
ep_over_exp <- function(ep_data_prep_output, lead = 0){
  # First argument holds a single object that is all rushing plays of interest, unnested
  # Should be the output of ep_data_prep
  
  # Generate all of the variables
  ep_calc_start <- ep_data_prep_output %>%
    select(game_id, play_id, data, nfl_id_rusher) %>%
    mutate(play_n = row_number(),
           game_id_2 = game_id, 
           play_id_2 = play_id) %>%
    unnest() %>%
    nest(-play_n, -game_id_2, -play_id_2) %>%
    mutate(standardized_example = map(data, ~standardize_play(.))) %>%
    select(-data) %>%
    mutate(handoff_frame = map2(standardized_example, lead, ~get_frame_pos(.x, .y)),
           handoff_zone_inf = map(handoff_frame, ~get_zone_influence(.)),
           ep_for_ownership = map2_dbl(handoff_frame, handoff_zone_inf, ~ep_for_field_ownership(.x, .y)),
           play_dir = map_chr(standardized_example, ~.$play_direction[1]),
           line_of_scrim = map_dbl(standardized_example, ~.$line_of_scrimmage[1]))
  
  runs <- ep_data_prep_output %>%
    select(game_id, play_id)
  
  # Just some binding
  runs_with_ep <- runs %>% 
    left_join(nfl_2017_small) %>%
    select(ep, epa)
  
  # Some variables not already in dataset
  ep_calc_start <- ep_calc_start %>%
    mutate(start_of_play_ep = runs_with_ep$ep,
           epa_for_ownership = ep_for_ownership - start_of_play_ep,
           epa_for_nflscrapr = runs_with_ep$epa) 
  
  # Summary level data that I want
  merged_calc <- ep_calc_start %>%
    select(-handoff_frame, -handoff_zone_inf) %>%
    unnest() %>%
    group_by(game_id, play_id) %>%
    summarize(nfl_id_rusher = first(nfl_id_rusher),
              line_of_scrim = first(line_of_scrim),
              expected_ep = first(ep_for_ownership),
              epa_for_ownership = first(epa_for_ownership),
              epa_for_nflscrapr = first(epa_for_nflscrapr)) %>%
    left_join(nfl_2017_small) %>%
    mutate(epa_earned = epa_for_nflscrapr - epa_for_ownership,
           line_of_scrim = line_of_scrim - 10,
           field_location = case_when(line_of_scrim <= 5 | line_of_scrim >= 95 ~ "Goalline",
                                      line_of_scrim <= 20 | line_of_scrim >= 80 ~ "RedZone",
                                      TRUE ~ "BetweenTwenties"))
  
  return(merged_calc)
}

# 1. Comprehensive probability of catch model
# a) Air distance
# To debug: Some plays dont have a pass_forward/pass_shovel event despite being pass plays
# Find a way to remedy this through ball motion, can roughly estimate
air_distance <- function(pass_play, target_rec){
  # If not working, be sure to load pass_air_end and pass_air_start
  qb_loc_pass <- pass_play %>%
    filter(position %in% "QB", event %in% pass_air_start) %>%
    slice(1) %>% # just in case
    select(x, y)
  
  if(length(qb_loc_pass$x) == 0){
    # pass event wasnt recorded, approximate
    if(pass_play$play_direction[1] %in% 'left'){
      # new x < old x + 0.5 means pass
      qb_loc_pass <- pass_play %>%
        mutate(x_change = lead(x) - x) %>%
        filter(x_change > 0.5) %>% 
        arrange(frame_id) %>% 
        slice(1)
    } else{
      qb_loc_pass <- pass_play %>%
        mutate(x_change = lead(x) - x) %>%
        filter(x_change < -0.5) %>% 
        arrange(frame_id) %>% 
        slice(1)
    }
  }
  
  target_receiver_loc <- pass_play %>%
    filter(nfl_id == target_rec, event %in% pass_air_end) %>%
    slice(1) %>% # just in case
    select(x, y)
  
  if(length(target_receiver_loc$x) == 0){
    # complete/incomplete not recorded, use pass_arrived
    target_receiver_loc <- pass_play %>%
      filter(nfl_id == target_rec, event %in% 'pass_arrived') %>%
      slice(1) %>% # just in case
      select(x, y)
  }
  
  air_dist <- sqrt((qb_loc_pass$x - target_receiver_loc$x)^2 + (qb_loc_pass$y - target_receiver_loc$y)^2)
  
  return(air_dist)
}

# b) Target receiver separation
# Probably should be done at time of pass
separation <- function(pass_play, target_rec, start_or_end = "start"){
  # At time of pass arrival
  pass_play <- pass_play %>% ungroup() # Just in case
  
  pass_event <- c("pass_forward", "pass_shovel")
  if(start_or_end %in% "end"){
    pass_event <- c("pass_outcome_incomplete", "pass_outcome_caught",
                    "pass_outcome_interception", "pass_outcome_touchdown")
  }
  
  target_receiver_loc <- pass_play %>%
    filter(nfl_id == target_rec, event %in% pass_event) %>%
    slice(1) %>% # just in case
    select(x, y)
  
  if(dim(target_receiver_loc)[1] < 1){
    # pass arrived not in event data despite being a pass play, compromise down to diff result
    return(NA)
  }
  
  # Get closest defender
  defense <- pass_play %>%
    filter(!(team %in% poss_team)) %>%
    filter(team %in% c("home", "away")) %>% # no ball for this part
    mutate(dist_receiver = sqrt((x - target_receiver_loc$x)^2 + (y - target_receiver_loc$y)^2)) %>%
    arrange(dist_receiver) %>%
    slice(1)
  
  # Return distance to closest defender, nfl_id, and display_name
  return(defense %>% select(rec_separation = dist_receiver,
                            nearest_def_to_rec_id = nfl_id, 
                            nearest_def_to_rec_name = display_name))
}


# c) Sideline separation
sideline_sep <- function(pass_play, target_rec){
  # At time of pass arrival, grab sideline separation for nearest sideline
  pass_play <- pass_play %>% ungroup() # Just in case
  
  target_receiver_loc <- pass_play %>%
    filter(nfl_id == target_rec, event %in% pass_air_start) %>%
    slice(1) %>% # just in case
    select(x, y)
  
  if(dim(target_receiver_loc)[1] < 1){
    # pass arrived not in event data despite being a pass play, compromise down to diff result
    target_receiver_loc <- pass_play %>%
      filter(nfl_id == target_rec, frame_id == 30) %>% # Fall back if no pass_start
      slice(1) %>% # just in case
      select(x, y)
  }
  
  # Sidelines are at 0 and 53
  distance_close_sideline <- target_receiver_loc %>%
    mutate(dist_0 = sqrt((y - 0) ^ 2),
           dist_53 = sqrt((y - 53)^2),
           dist = if_else(dist_0 > dist_53, dist_53, dist_0)) %>%
    pull(dist)
  
  return(distance_close_sideline)
}

# d) Pass Rush Separation
pass_rush_sep <- function(pass_play){
  # Get distance from nearest defender and the qb at time of throw
  # Possibly also grab the rate of change in that distance from 5 frames prior
  qb_loc <- pass_play %>%
    filter(position %in% "QB", event %in% pass_air_start) %>%
    ungroup() %>%
    slice(1) %>% # just in case
    select(x, y, frame_id)
  
  # Corner case for when a pass event is not recorded despite existing
  if(length(qb_loc$x) == 0){
    # pass event wasnt recorded, approximate
    if(pass_play$play_direction[1] %in% 'left'){
      # new x < old x + 0.5 means pass
      qb_loc <- pass_play %>%
        mutate(x_change = lead(x) - x) %>%
        filter(x_change > 0.5) %>% 
        arrange(frame_id) %>% 
        slice(1)
    } else{
      qb_loc <- pass_play %>%
        mutate(x_change = lead(x) - x) %>%
        filter(x_change < -0.5) %>% 
        arrange(frame_id) %>% 
        slice(1)
    }
  }
  
  qb_loc_5_frames <- pass_play %>%
    filter(position %in% "QB", frame_id == (qb_loc$frame_id - 5)) %>%
    ungroup() %>%
    slice(1) %>%
    select(x, y, frame_id)
  
  def_loc <- pass_play %>%
    filter(!(team %in% poss_team), team %in% c("home", "away"), event %in% pass_air_start) %>%
    mutate(dist = sqrt((x - qb_loc$x)^2 + (y - qb_loc$y)^2)) %>%
    ungroup() %>%
    arrange(dist) %>%
    slice(1) %>%
    pull(dist)
  
  def_loc_5_frames <- pass_play %>%
    ungroup() %>%
    filter(!(team %in% poss_team), team %in% c("home", "away"), frame_id == qb_loc_5_frames$frame_id) %>%
    mutate(dist = sqrt((x - qb_loc_5_frames$x)^2 + (y - qb_loc_5_frames$y)^2)) %>%
    arrange(dist) %>%
    slice(1) %>%
    pull(dist)
  
  def_tot <- tibble(no_frame_rush_sep = def_loc, five_frame_rush_sep = def_loc_5_frames) %>%
    mutate(roc_rush = (no_frame_rush_sep - five_frame_rush_sep) / 0.5)
  
  return(def_tot)
}

# e) qb speed at time of release
qb_speed <- function(pass_play){
  
  qb_vel <- pass_play %>%
    filter(position %in% "QB", event %in% pass_air_start) %>%
    slice(1) %>% # just in case
    pull(velocity)
  
  if(length(qb_vel) == 0){
    return(NA_real_)
  }
  
  return(qb_vel)
}

# f) time from snap to release
release_time <- function(pass_play){
  
  time_to_throw <- pass_play %>%
    ungroup() %>%
    filter(position %in% "QB", event %in% c("ball_snap", "snap_direct", pass_air_start)) %>%
    slice(1:2) %>%
    summarize(time_to_throw = (last(frame_id) - first(frame_id)) / 10) %>%
    pull(time_to_throw)
  
  if(length(time_to_throw) == 0){
    return(NA_real_)
  }
  
  return(time_to_throw)
}

# g) pocket boolean
pocket_fixed <- function(pass_play, first_elig, last_elig){
  
  play_start_coords <- pass_play %>%
    filter(frame_id == first_elig, position %in% 'QB') %>%
    slice(1) %>% # just in case
    select(x = line_of_scrimmage, y, play_direction, frame_id)
  
  right_bound <- play_start_coords$y + 3.5
  left_bound <- play_start_coords$y - 3.5
  
  if(play_start_coords$play_direction %in% 'left'){
    upper_bound <- play_start_coords$x + 7
    lower_bound <- play_start_coords$x
  } else{
    upper_bound <- play_start_coords$x 
    lower_bound <- play_start_coords$x - 7 # pocket is behind los
  }
  
  dist_from_pocket <- pass_play %>%
    ungroup() %>%
    filter(position %in% "QB", frame_id >= first_elig, frame_id <= last_elig) %>%
    mutate(dist_from_pocket = case_when(left_bound <= y & y <= right_bound & lower_bound <= x & x <= upper_bound ~ 0, # in pocket
                                        y < left_bound ~ left_bound - y, # out of pocket, left side
                                        y > right_bound ~ y - right_bound,
                                        x < lower_bound ~ lower_bound - x,
                                        TRUE ~ x - upper_bound)) %>% # out of pocket, right side
    select(dist_from_pocket)
  
  if(length(dist_from_pocket) == 0){
    return(list(NA_real_))
  }
  
  return(dist_from_pocket)
}

pocket <- function(pass_play){
  
  throw_frame <- pass_play %>%
    filter(event %in% c('pass_forward', 'pass_shovel')) %>%
    slice(1) %>%
    pull(frame_id)
  
  if(is.na(throw_frame)){
    return(NA_real_)
  }
  
  dist_from_pocket <- pocket_fixed(pass_play, throw_frame, throw_frame)
  
  return(dist_from_pocket %>% slice(1) %>% pull(dist_from_pocket))
}


qb_name <- function(pass_play){
  
  qb <- pass_play %>%
    dplyr::filter(position %in% 'QB') %>%
    slice(1) %>%
    pull(display_name)
  
  if(length(qb) == 0){
    return(NA_character_)
  }
  return(qb)
}


# g) My features now. Incorporate Openness in to the model. These results already exist
# Be sure to use helper functions
add_influence <- function(pass_play){
  
  # Default is standardized = FALSE in zone inf but this data has been standardized
  # assuming that it comes from a ~standardize_play() call
  # Standardized changes only really make the plot look weird, this is an angle/velocity adj
  this_inf <- get_zone_influence(pass_play, standardized = TRUE) %>%
    mutate(x = round(s_1),
           y = round(s_2)) # sometimes get fractional values, need to correct
  
  future_inf <- get_zone_influence(pass_play, standardized = TRUE, start_or_end = 'end') %>%
    mutate(x = round(s_1),
           y = round(s_2))
  
  # Summarize into some variables
  # Amount of cells owned in a 5x5 circle around location ball will land
  pass_arrival_frame <- pass_play %>%
    filter(event %in% c(pass_air_end, 'pass_arrived')) %>%
    ungroup() %>%
    arrange(frame_id) %>%
    slice(1) %>%
    pull(frame_id) # Just need one frame for an arrival event
  
  ball_at_arrival <- pass_play %>%
    filter(frame_id == pass_arrival_frame, team %in% "football") %>%
    ungroup() %>%
    slice(1) %>%
    mutate(x = if_else(x < 0, 0, x),
           x = if_else(x > 120, 120, x),
           y = if_else(y < 0, 0, y),
           y = if_else(y > 53, 53, y))
  
  if(dim(ball_at_arrival)[1] == 0){
    
    # This shouldn't happen, I have filtered most of these cases out
    # Few exceptions for incompletions that never really have a pass arrival status
    return(NA_real_)
    
  } else{
    
    # Quick variable renaming dependent on possession team
    if(ball_at_arrival$poss_team %in% 'home'){
      this_inf <- this_inf %>% mutate(off_inf = home_inf)
      future_inf <- future_inf %>% mutate(off_inf = home_inf)
    } else{
      this_inf <- this_inf %>% mutate(off_inf = away_inf)
      future_inf <- future_inf %>% mutate(off_inf = away_inf)
    }
    
    pass_time_results <- this_inf %>%
      ungroup() %>%
      mutate(dist_from_ball = sqrt((x - ball_at_arrival$x)^2 + (y - ball_at_arrival$y)^2)) %>%
      filter(dist_from_ball <= 5) %>%
      summarize(n_close = n(),
                n_cells_at_throw = sum(off_inf > 0.5, na.rm = TRUE),
                own_intensity_at_throw = sum(off_inf, na.rm = TRUE),
                own_avg_intensity_at_throw = mean(off_inf, na.rm = TRUE))
    
    
    if(is.na(pass_time_results$own_avg_intensity_at_throw)){
      print('Failing for some reason')
    }
    
    pass_arrival_results <- future_inf %>%
      ungroup() %>%
      mutate(dist_from_ball = sqrt((x - ball_at_arrival$x)^2 + (y - ball_at_arrival$y)^2)) %>%
      filter(dist_from_ball <= 5) %>%
      summarize(n_cells_at_arrival = sum(off_inf > 0.5),
                own_intensity_at_arrival = sum(off_inf),
                own_avg_intensity_at_arrival = mean(off_inf))
    
    results <- bind_cols(pass_time_results, pass_arrival_results)
  }
  
  return(results)
}



# h) velocity of ball at pass_arrival
ball_speed_at_arrival <- function(pass_play){
  
  pass_arrival_frame <- pass_play %>%
    filter(event %in% c(pass_air_end, 'pass_arrived')) %>%
    ungroup() %>%
    arrange(frame_id) %>%
    slice(1) %>%
    pull(frame_id) # Just need one frame for an arrival event
  
  ball_at_arrival <- pass_play %>%
    filter(frame_id == pass_arrival_frame, team %in% "football") %>%
    ungroup() %>%
    slice(1) %>%
    pull(velocity)
  
  return(ball_at_arrival)
}


# i) time from pass to pass_arrival, possibly interaction of h) and i)
air_time <- function(pass_play){
  
  throw_frame <- pass_play %>%
    ungroup() %>%
    filter(event %in% pass_air_start) %>%
    slice(1) %>%
    pull(frame_id)
  
  arrival_frame <- pass_play %>%
    ungroup() %>%
    filter(event %in% c(pass_air_end, 'pass_arrived')) %>%
    slice(1) %>%
    pull(frame_id)
  
  return((arrival_frame - throw_frame) / 10)
}


# Previously used y but field length is measured by x, clearly meant to use that variable
# j) Air yards (just the x component)
air_yards <- function(pass_play){
  throw_frame <- pass_play %>%
    ungroup() %>%
    filter(event %in% pass_air_start) %>%
    slice(1) %>%
    pull(frame_id)
  
  arrival_frame <- pass_play %>%
    ungroup() %>%
    filter(event %in% c(pass_air_end, 'pass_arrived')) %>%
    slice(1) %>%
    pull(frame_id)
  
  ball_position <- pass_play %>%
    filter(team %in% "football", frame_id %in% c(throw_frame, arrival_frame)) %>%
    summarize(dist = abs(first(x) - last(x))) %>%
    pull(dist)
  
  return(ball_position)
}

# Other ancilliary functions
# This is breaking for something in game # 23
intended_receiver <- function(pass_play){
  # Using pass_over events, identify player closest to the ball on offense, label intended receiver
  
  pass_play <- pass_play %>% ungroup() 
  
  pass_air_end <- 
    c(# "pass_arrived",
      "pass_outcome_caught",
      "pass_outcome_incomplete",
      # "pass_tipped",
      "touchdown",
      "pass_outcome_interception",
      "pass_outcome_touchdown"
    )
  
  # First arrival event gets frame id
  arrival_frame <- pass_play %>%
    filter(team %in% poss_team, event %in% pass_air_end) %>%
    slice(1) %>% # Will grab first pass_air_end event incase of data problems
    pull(frame_id)
  
  if(length(arrival_frame) == 0){
    # Perhaps complete/incomplete wasnt recorded but pass arrived might have been
    arrival_frame <- pass_play %>%
      filter(team %in% poss_team, event %in% c("pass_arrived")) %>% # Weird corner case on tips
      slice(1) %>% # Will grab first pass_air_end event incase of data problems
      pull(frame_id)
    
    if(length(arrival_frame) == 0){
      # implies no pass_over event, must be qb_sack or qb_run, no target receiver
      return(NA_real_)
    }
    
  }
  
  # Grab ball location, may need to investigate data integrity on this one
  ball_loc <- pass_play %>%
    filter(team %in% "football", frame_id == arrival_frame) %>%
    select(x, y)
  
  if(length(ball_loc$x) == 0){
    # Ball isnt measured at time of arrival for some reason, remove these plays
    return(NA_real_)
  }

  # Closest to ball should reasonably be the receiver, can alternatively scrape names from text
  receiver <- pass_play %>%
    filter(position %in% c("WR", "RB", "TE"), frame_id == arrival_frame) %>%
    mutate(dist = sqrt((x - ball_loc$x) ^ 2 + (y - ball_loc$y)^2)) %>%
    arrange(dist) %>%
    slice(1) %>%
    pull(nfl_id)
  
  return(receiver)
}


# Now need to pull identifier for if pass was complete or incomplete
play_success <- function(data){
  # If any entry is pass_outcome_caught then the play had a successful catch
  if(any(data$event %in% c("pass_outcome_caught", "pass_outcome_touchdown")) |
     (any(data$event %in% c("pass_arrived")) & !any(data$event %in% "pass_outcome_incomplete"))){
    return(TRUE)
  }
  return(FALSE)
}

fake_punt <- function(data){
  if(any(data$event %in% "punt_fake")){
    return(NA_real_)
  } 
  return(1)
}

no_qb <- function(data){
  if(!any(data$position %in% 'QB')){
    return(NA_real_)
  }
  return(1)
}

# Trying to fix the ball tracking bug
# Assumption: The ball is moving forward quickly for the first time in the play
# when it is thrown. Since I filter out QB runs this should be a reasonable assumption
ball_fix <- function(pass_play){
  # Dont yet have a blanket clause that I could use to skip/ignore correct plays as 
  # I dont yet know what a correct play looks like wrt ball position due to those tap
  # passes
  # Instead probably decide skip/ignore based on number of frames I am off by
  
  # Ball stuff
  ball <- pass_play %>%
    filter(team %in% "football") %>%
    mutate(delta_x = x - lag(x, default = 9999))
  
  # No Ball stuff
  no_ball <- pass_play %>% 
    ungroup() %>%
    dplyr::filter(team %in% c('home', 'away')) 
  
  check_params <- no_ball %>%
    slice(1)
  
  # In case of event errors, grab the frame that the most players have as pass_play
  throw_frame_act <- no_ball %>%
    group_by(frame_id) %>%
    summarize(throw_frame = sum(event %in% c('pass_forward', 'pass_shovel'))) %>%
    arrange(desc(throw_frame)) %>%
    slice(1) %>%
    ungroup() %>%
    pull(frame_id)
  
  # Looks like a snap doesnt go much faster than a velocity of 8 units per second
  # Players do, however, go faster. Use 10 then as a reference as that probably covers problem
  if(check_params$play_direction %in% 'left'){
    # Going left, implies pass goes from large x to smaller x
    ball_frame_guess <- ball %>%
      dplyr::filter(delta_x > -9000) %>%
      mutate(going_forward_fast = if_else(delta_x < 0 & velocity > 10, 1, 0)) %>%
      ungroup() %>%
      dplyr::filter(going_forward_fast == 1) %>%
      arrange(frame_id) %>%
      slice(1) %>%
      pull(frame_id)
    
  } else{
    # Going right, implies pass goes from small x to larger x
    ball_frame_guess <- ball %>%
      dplyr::filter(delta_x > -9000) %>%
      mutate(going_forward_fast = if_else(delta_x > 0 & velocity > 10, 1, 0)) %>%
      ungroup() %>%
      dplyr::filter(going_forward_fast == 1) %>%
      arrange(frame_id) %>%
      slice(1) %>%
      pull(frame_id)
  }
  
  # Adjustment
  if(length(ball_frame_guess) > 0){
    # If exists, measure difference
    difference <- ball_frame_guess - throw_frame_act
    #print(difference)
    if(difference >= 5){
      
      # only do it if the difference is big enough
      pass_play <- pass_play %>%
        mutate(frame_id = case_when(team %in% "football" ~ frame_id - difference,
                                    TRUE ~ frame_id)) %>%
        dplyr::filter(frame_id >= 1)
    }
  } 
  # Sometimes this will just be completely unmodified
  return(pass_play)
}

ball_fix_2 <- function(pass_play){
  # Realistically the first time the ball moves more than 2 units per second is the snap
  # Just correct with respect to that event
  if(pass_play$game_id[1] == 2017100806){
    return(pass_play) # This game is weird for ball tracking
  }
  
  start_frame_player <- pass_play %>%
    ungroup() %>%
    filter(team %in% c('home', 'away'),
           event %in% c('ball_snap', 'snap_direct')) %>%
    slice(1) %>%
    pull(frame_id)
  
  start_frame_ball <- pass_play %>%
    ungroup() %>%
    filter(team %in% c('ball')) %>%
    filter(velocity > 0.85) %>%
    slice(1) %>%
    pull(frame_id)
  
  difference <- start_frame_ball - start_frame_player
  
  if(length(difference) == 0){
    # Some weird ball_snap problem, circumvent with just using the other fix
    return(ball_fix(pass_play))
  }
  
  # Continue as normal so long as a snap event exists
  if(difference > 5){
    temp <- pass_play %>%
      ungroup() %>%
      filter(team %in% c('home', 'away'),
             event %in% c('ball_snap', 'snap_direct')) %>%
      slice(1)
    
    pass_play <- pass_play %>%
      mutate(frame_id = if_else(team %in% "football", frame_id - difference, frame_id)) %>%
      filter(frame_id >0)
  }
  return(pass_play)
}


# # Load in some of the data to test with, do all after satisfied with functionality
quick_nest_fix <- function(data, game_id, play_id){
  data$game_id <- game_id
  data$play_id <- play_id
  
  return(data)
}

# Some of the observations have no ball data, remove these
handle_no_ball <- function(data){
  if(!any(data$team %in% "football")){
    return(NA_real_)
  } else{
    return(1)
  }
}

pass_start_event <- function(data){
  return(any(data$event %in% c('pass_forward', 'pass_shovel')))
}

all_rec_air_yards_if_caught <- function(one_frame, ball_speed = 20, run = FALSE){
  # Ball speed is set at 20 yards per second as an exploratory measure, used as argument to be tailored
  
  # QB position for my calculations
  qb_loc <- one_frame %>%
    filter(position %in% 'QB') %>%
    slice(1)
  
  position_group <- c('WR', 'TE', 'RB')
  if(run){
    position_group <- c('WR', 'TE', 'RB', 'QB')
  }
  # Receiver locations and anticipated locations
  receiver_loc <- one_frame %>%
    filter(position %in% position_group) %>%
    mutate(dist_to_qb = sqrt((x - qb_loc$x) ^ 2 + (y - qb_loc$y)^2), 
           speed_diff = ball_speed - velocity,
           time_to_arrival = dist_to_qb / speed_diff,
           speed_x = velocity * cos(dir * pi / 180),
           arrival_x = time_to_arrival * speed_x + x,
           arrival_x = case_when(arrival_x < 0 ~ 0,
                                 arrival_x > 120 ~ 120,
                                 TRUE ~ arrival_x),
           air_yards_x = abs(arrival_x - qb_loc$x),
           yards_downfield = if_else(play_direction %in% 'left', line_of_scrimmage - arrival_x, 
                                     arrival_x - line_of_scrimmage)) %>%
    select(nfl_id, display_name, dist_to_qb, arrival_x, air_yards_x, yards_downfield)
  
  return(receiver_loc)
}

# I can use most of these parameters with the actual play to define starting EP
# Now to adjust the function that I use to estimate EP currently
ep_for_receivers_exact <- function(all_frames_with_pbp, game_id_par, play_id_par,
                                   yardline_100, ydstogo, down){

  # Relies on global dependency
  play_params <- ep_requirements %>%
    filter(game_id == game_id_par, play_id == play_id_par) 
  
  if(game_id_par == 2017092100 & play_id_par == 3173){
    # Problematic case where snap isnt recorded but is not dealt with for some reason
    return(list(NA))
  }
  
  initial_ep <- calculate_expected_points(play_params, "half_seconds_remaining",
                                          "yardline_100", "down", "ydstogo", "goal_to_go")$ep
  
  # Store nfl_scrapr_playdata somewhere globally
  # Currently stored as nfl_2017_small
  line_of_scrimmage <- round(yardline_100)
  
  # Now calculate the new situation for each play, assuming line_of_scrimmage is in tracking coord system
  change_df <- data.frame(los = line_of_scrimmage,
                          ydstogo = ydstogo,
                          down = down,
                          yardlines = ceiling(all_frames_with_pbp$yards_downfield),
                          yac = all_frames_with_pbp$yac,
                          frame_id_2 = all_frames_with_pbp$frame_id_2,
                          display_name = as.character(all_frames_with_pbp$display_name),
                          receiver = as.character(all_frames_with_pbp$receiver))
  
  if(any(is.na(change_df$yardlines))){
    return(list(NA))
  }
  
  complete_params <- change_df %>%
    rowwise() %>%
    mutate(yards_gained = round(yardlines + yac), # yards gained on the play
           yac = yac,
           td_flag = (yards_gained >= los),
           new_ydstogo = ydstogo - yards_gained,
           new_down = case_when(new_ydstogo <= 0 ~ 1,
                                down == 4 ~ NA_real_,
                                TRUE ~ down + 1),
           new_ydstogo = ifelse(new_ydstogo <= 0, 10, new_ydstogo),
           new_yardline_100 = max(yardline_100 - yards_gained, 0),
           safety_flag = (new_yardline_100 >= 100),
           new_yardline_100 = ifelse(new_yardline_100 >= 100, 100, new_yardline_100),
           new_ydstogo = ifelse(new_ydstogo <= 0 | new_yardline_100 < 10,
                                min(10, new_yardline_100), new_ydstogo),
           new_ydstogo = ifelse(is.na(new_down), 10, new_ydstogo),
           new_ydstogo = ifelse(new_yardline_100 >= 100, 10, new_ydstogo),
           modifier = ifelse(is.na(new_down), -1, 1), # Need to make new ep negative on turnovers
           new_yardline_100 = ifelse(is.na(new_down), 100 - new_yardline_100, new_yardline_100), #turnover
           new_down = ifelse(is.na(new_down) | new_yardline_100 >= 100, 
                             1, # Turnover case
                             new_down),
           new_yardline_100 = as.numeric(ifelse(new_yardline_100 <= 0, 1, new_yardline_100)), #TD corrections, these 2
           new_ydstogo = ifelse(new_ydstogo <= 0, 1, new_ydstogo)) %>% # Standard
    ungroup() %>% # add on the play covariates
    mutate(half_seconds_remaining = play_params$half_seconds_remaining,
           goal_to_go = play_params$goal_to_go)
  
  complete_ep <- calculate_expected_points(complete_params, "half_seconds_remaining",
                                           "new_yardline_100", "new_down", "new_ydstogo",
                                           "goal_to_go") %>%
    mutate(adj_comp_ep = ifelse(td_flag, 
                                7,
                                modifier * ep)) %>%# To handle turnovers
    dplyr::select(-tidyselect::contains("_prob"))
  print('a')
  
  # What happens if the pass goes incomplete, there is some penalty to associated with this
  failure_params <- data.frame(yardline_100 = line_of_scrimmage,
                               ydstogo = ydstogo,
                               down = down,
                               display_name = as.character(all_frames_with_pbp$display_name))%>%
    mutate(new_down = ifelse(down == 4, NA_real_, down + 1),
           modifier = ifelse(is.na(new_down), -1, 1),
           new_yardline_100 = ifelse(is.na(new_down), 100 - yardline_100, yardline_100),
           new_ydstogo = ifelse(is.na(new_down), min(10, new_yardline_100), ydstogo),
           new_down = ifelse(is.na(new_down), 1, new_down)) %>% # add on the play covariates
    mutate(half_seconds_remaining = max(play_params$half_seconds_remaining - 7, 1),
           goal_to_go = play_params$goal_to_go)
  
  incomplete_ep <- calculate_expected_points(failure_params, "half_seconds_remaining",
                                             "yardline_100", "new_down", "new_ydstogo",
                                             "goal_to_go") %>%
    mutate(adj_inc_ep = modifier * ep) %>%
    dplyr::select(adj_inc_ep)
  
  print('b')
  # Revisit, this seems a little wonky as it sometimes increases when we lose a down
  # Perhaps this is because NFL teams throw more on 2nd down and this gets captured by EP model
  
  # What happens if the pass gets intercepted?
  interception_params <- data.frame(yardlines = ceiling(all_frames_with_pbp$yards_downfield)) %>%
    rowwise() %>%
    mutate(down = 1, # turnover, new set of downs for other team
           yardline_100 = 100 - (line_of_scrimmage - yardlines),
           yardline_100 = if_else(yardline_100 > 100, 80, yardline_100), # Safety check
           ydstogo = min(10, yardline_100), # add on the play covariates
           td_flag = (ydstogo <= 0),
           ydstogo = max(ydstogo, 1)) %>% # Small correction as we do log(ydstogo)
    mutate(half_seconds_remaining = play_params$half_seconds_remaining,
           goal_to_go = as.numeric(yardline_100 < 10)) # turn over happened within own 10 yardline
  
  interception_ep <- calculate_expected_points(interception_params, "half_seconds_remaining",
                                               "yardline_100", "down", "ydstogo", "goal_to_go") %>%
    mutate(adj_int_ep = if_else(td_flag, -7, -1 * ep)) %>%
    dplyr::select(adj_int_ep)
  print('c')
  # Now to get ep for each 
  change_df <- complete_ep %>%
    bind_cols(incomplete_ep) %>%
    bind_cols(interception_ep) %>%
    mutate(initial_ep = initial_ep,
           complete_epa = adj_comp_ep - initial_ep,
           incomplete_epa = adj_inc_ep - initial_ep,
           interception_epa = adj_int_ep - initial_ep)
  
  return(change_df)
}

# Frame eligibility within a play
first_elig_frame <- function(pass_play){
  # Grab the frame id for the first data point I want to consider
  frame <- pass_play %>%
    group_by(frame_id) %>%
    summarize(n_ball_snap = sum(event %in% c('ball_snap', 'direct_snap'), na.rm = TRUE)) %>%
    arrange(desc(n_ball_snap)) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(frame_id = if_else(n_ball_snap > 1, frame_id, 15L)) %>% # Change to arbitrary frame if we have a ball snap problem
    pull(frame_id)
  
  return(frame)
}

last_elig_frame <- function(pass_play, pass_type = 'C'){
  # Set default to be 'C' for legacy code
  
  last_event <- c('pass_forward', 'pass_shovel')
  if(pass_type %in% c('S', 'R')){
    last_event <- c('run', 'tackle', 'first_contact', 'out_of_bounds',
                    'qb_sack', 'qb_strip_sack')
  }
  
  frame <- pass_play %>%
    group_by(frame_id) %>%
    summarize(n_ball_released = sum(event %in% last_event, na.rm = TRUE)) %>%
    arrange(desc(n_ball_released)) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(frame_id = if_else(n_ball_released > 1, frame_id, 30L)) %>% # Change to arbitrary frame if we have a ball snap problem
    pull(frame_id)
  
  # Check if 10 frames after is available
  # If not, 5. If not, just this frame.
  max_frame <- pass_play %>%
    dplyr::select(frame_id) %>%
    slice(n()) %>%
    pull(frame_id)
  
  return(frame) # Changed my mind, only want up to time of throw
}

# intended_receiver() function just returns an nfl_id so this will do the same but for the qb
qb_tuck <- function(sack_or_run){
  
  qb_id <- sack_or_run %>%
    filter(position %in% 'QB') %>%
    slice(1) %>%
    pull(nfl_id)
  
  return(qb_id)
}
