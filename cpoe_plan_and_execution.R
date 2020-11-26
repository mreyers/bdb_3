# Skill / Value Estimation part 1
  # CPOE Model and relative change in CPOE from pass frame to arrival frame

# This one we have a bunch of infrastructure for
# I will lay out a TO DO list and a COMPLETE list
# Move things appropriately as they are completed

# TODO:
  # 4. Apply CP model to all frames within all plays using purrr::safely()
  # 5. Return dataset appending CP values for each eligible receiver (non-QB) and their corresponding nearest defender
  # 6. Use Outcome - CP to generate CPOE across dataset
  # 7. Estimate value per frame using Hypothetical EPA
  # 8. Assign value per frame based on Hypothetical EPA, the CP * EPA hybrid from thesis
  # 9. Upweight plays corresponding to large +/-WPA moments through scale_factors.R
  


# COMPLETE:
  # 1. Generate stacked ensemble for CP using the observed covariates  
  # 2. Generate framewise covariates for the CP model from my thesis
      # a) Simple covariates
      # b) Framewise influence
      # c) Additional framewise covariates that are not related to influence 
  # 3. Framewise labelling for nearest receiver
  

# # # #
# Main
# # # #
library(futile.logger)
library(readr)
library(dplyr)
library(tidyr)
library(future)
library(furrr)
library(purrr)
library(tidymodels)
library(stacks)

source("src/utils/tracking_helpers.R")
# 1. Source in observed model from nfl_tracking
  # Code and model are complete there, next time I run just save as RDS and load here
cp_at_release_model <- readRDS("~/GitHub/nfl_tracking/src/Data_new/release/comp_prob.rds")

# 2. Source in the covariates, either by loading or creating (do by week)
  # Simple covariates (done in simple_covariates.R)
    # 'air_dist', 'rec_separation', 'sideline_sep', 'no_frame_rush_sep', 
    # 'qb_vel', 'time_to_throw', 'dist_from_pocket', 
    # 'air_yards_x',
    # 'yardline_100', 'down', 'ydstogo'
week1_simple <- readRDS("Data/all_frames_covariates_week1.rds")

  # Ownership covariates
    # 'n_cells_at_throw',
    # 'own_intensity_at_throw', 
    # 'own_avg_intensity_at_throw',
  # Done in more_eda.R, to be moved to a better named file later
week1_inf <- readRDS("Data/additional_data/week1_influence.rds")
week1_inf <- week1_inf %>%
  unnest(inf_check) %>%
  group_by(game_id, play_id) %>%
  slice(1) %>%
  ungroup() %>%
  unnest(inf_check) %>%
  nest(-c(game_id, play_id, frame_id))

# Actually it looks like I still need to convert these to covariates, currently 
# just the pointwise ownership
# Can do this with arrival_x and arrival_y from week1_simple and pair it with
# week1_inf on a play level, applying the sub_fn the way I did in my thesis
sub_fn_bdb3 <- function(one_frame, influence){
  
  if(is.null(influence)){
    return(list(NULL))
  }
  influence <- influence %>%
    mutate(x = round(s_1),
           y = round(s_2))
  
  poss_team <- one_frame$poss_team[1]
  
  one_frame <- one_frame %>%
    select(nfl_id, proj_x = arrival_x, proj_y = arrival_y) %>%
    nest(data = c(nfl_id, proj_x, proj_y))
  
  if(poss_team[1] %in% 'home'){
    influence <- influence %>% mutate(off_inf = home_inf)
  } else{
    influence <- influence %>% mutate(off_inf = away_inf)
  }
  
  pass_time_results <- influence %>%
    ungroup() %>%
    bind_cols(one_frame) %>%
    unnest(data) %>%
    filter(!is.na(proj_x), !is.na(proj_y)) %>%
    mutate(dist_from_ball = sqrt((x - proj_x)^2 + (y - proj_y)^2)) %>%
    filter(dist_from_ball <= 5) %>%
    group_by(nfl_id) %>%
    summarize(n_close = n(),
              n_cells_at_throw = sum(off_inf > 0.5),
              own_intensity_at_throw = sum(off_inf),
              own_avg_intensity_at_throw = if_else(is.na(mean(off_inf)), 0, mean(off_inf)),
              .groups = "drop")
  
  return(pass_time_results)
}

possession_team <- plays %>%
  left_join(games %>% select(game_id, home_team_abbr)) %>%
  mutate(poss_team = if_else(possession_team == home_team_abbr, "home", "away")) %>%
  select(game_id, play_id, poss_team)

for(i in 1:17){
  # Actual covariate setup because Im a jackass that forgot to condense influence
  # Will require a bit more preprocessing to finalize bc I need to go do something
  # and want to set this running
  
  week1_simple <- readRDS(paste0("Data/all_frames_covariates_week", i, ".rds"))
  
  week1_inf <- readRDS(paste0("Data/additional_data/week", i, "_influence.rds"))
  week1_inf_unnest <- week1_inf %>%
    # Double nested for some reason, this should work to unbind issue
    unnest(inf_check) %>%
    group_by(game_id, play_id) %>%
    slice(1) %>%
    ungroup() %>%
    unnest(inf_check) %>%
    nest(-c(game_id, play_id, frame_id))
  
  week1_both <- week1_simple %>%
    left_join(possession_team, by = c("game_id", "play_id")) %>%
    select(-frame_id_2) %>%
    nest(-c(game_id, play_id, frame_id)) %>%
    rename(one_frame = data) %>%
    left_join(week1_inf_unnest %>%
                rename(influence = data), by = c("game_id", "play_id", "frame_id")) %>%
    mutate(ownership_summaries = map2(one_frame, influence,
                                      ~ sub_fn_bdb3(.x, .y)))
  
  week1_both_updated <- week1_both %>%
    mutate(null_check = map_dbl(ownership_summaries, ~ifelse(is.null(nrow(.)), 0, nrow(.)))) %>%
    filter(null_check > 0) %>%
    mutate(necessary_columns = map2(one_frame, ownership_summaries,
                                    ~ .x %>% left_join(.y, by = "nfl_id"))) %>%
    select(game_id, play_id, frame_id, necessary_columns) %>%
    unnest(necessary_columns)
  
  saveRDS(week1_both_updated, paste0("Data/simple_and_inf_covariates_week", i, ".rds"))
  
  print(i)
}

# Quick sub step, I saved the wrong files
# Just the last step is the issue

for(i in 1:17){
  one_week_covariates <- readRDS(paste0("Data/simple_and_inf_covariates_week", i, ".rds"))
  week1_both_updated <- one_week_covariates %>%
    mutate(null_check = map_dbl(ownership_summaries, ~ifelse(is.null(nrow(.)), 0, nrow(.)))) %>%
    filter(null_check > 0) %>%
    mutate(necessary_columns = map2(one_frame, ownership_summaries,
                                    ~ .x %>% left_join(.y, by = "nfl_id"))) %>%
    select(game_id, play_id, frame_id, necessary_columns) %>%
    unnest(necessary_columns)
  
  saveRDS(week1_both_updated, paste0("Data/simple_and_inf_covariates_fix_week", i, ".rds"))
}

# 4. Apply the CP model to all frames (skipped step 3 for a second)
one_week_covariates <- readRDS("Data/simple_and_inf_covariates_week1.rds")

head(one_week_covariates)
