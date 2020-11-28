# Skill / Value Estimation part 1
  # CPOE Model and relative change in CPOE from pass frame to arrival frame

# This one we have a bunch of infrastructure for
# I will lay out a TO DO list and a COMPLETE list
# Move things appropriately as they are completed

# TODO:
  # 3. Framewise labelling for nearest receiver
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
  # 4. Apply CP model to all frames within all plays using purrr::safely()

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

plays_essential <- read_csv("Data/plays.csv", col_types = cols()) %>%
  janitor::clean_names(case = "snake") %>%
  mutate(yardline_100 = if_else(possession_team == yardline_side,
                                100 - yardline_number,
                                yardline_number)) %>%
  select(game_id, play_id, down, ydstogo = yards_to_go, yardline_100)

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
one_week_covariates <- readRDS("Data/simple_and_inf_covariates_fix_week1.rds")
names(one_week_covariates)

one_week_renamed <- one_week_covariates %>%
  rename(qb_vel = qb_speed,
         time_to_throw = time_throw,
         dist_from_pocket = pocket_dist,
         n_cells = n_close,
         own_intensity = own_intensity_at_throw,
         own_avg_intensity = own_avg_intensity_at_throw) %>%
  # Remove old list column, dont think I need
  select(-ball_at_arrival_coords)

one_week_fixed_context <- one_week_renamed %>%
  nest(-c(dist_from_pocket, first_elig, last_elig)) %>%
  mutate(new_dist_from_pocket = map2(dist_from_pocket, first_elig,
                                     ~ .x %>% mutate(frame_id = row_number() - 1 + .y)),
         data = map2(data, new_dist_from_pocket, ~ .x %>% left_join(.y, by = "frame_id"))) %>%
  select(-c(dist_from_pocket, new_dist_from_pocket)) %>%
  unnest(data) %>%
  left_join(plays_essential, by = c("game_id", "play_id")) %>%
  select(-rush_sep) %>%
  filter(!is.na(own_avg_intensity), !is.na(yardline_100))

summary(one_week_fixed_context)
# Can I just predict like this? Might need to do a drop_na above
temp <- one_week_fixed_context %>% 
  bind_cols(predict(cp_at_release_model, ., type = "prob"))


for(i in 1:17){
  one_week_covariates <- readRDS(paste0("Data/simple_and_inf_covariates_fix_week",
                                        i,
                                        ".rds"))
  
  one_week_renamed <- one_week_covariates %>%
    rename(qb_vel = qb_speed,
           time_to_throw = time_throw,
           dist_from_pocket = pocket_dist,
           n_cells = n_close,
           own_intensity = own_intensity_at_throw,
           own_avg_intensity = own_avg_intensity_at_throw) %>%
    # Remove old list column, dont think I need
    select(-ball_at_arrival_coords)
  
  one_week_fixed_context <- one_week_renamed %>%
    nest(-c(dist_from_pocket, first_elig, last_elig)) %>%
    mutate(new_dist_from_pocket = map2(dist_from_pocket, first_elig,
                                       ~ .x %>% mutate(frame_id = row_number() - 1 + .y)),
           data = map2(data, new_dist_from_pocket, ~ .x %>% left_join(.y, by = "frame_id"))) %>%
    select(-c(dist_from_pocket, new_dist_from_pocket)) %>%
    unnest(data) %>%
    left_join(plays_essential, by = c("game_id", "play_id")) %>%
    select(-rush_sep) %>%
    filter(!is.na(own_avg_intensity), !is.na(yardline_100))
  
  # Can I just predict like this? Might need to do a drop_na above
  one_week_preds <- one_week_fixed_context %>% 
    bind_cols(predict(cp_at_release_model, ., type = "prob"))
  
  saveRDS(one_week_preds,
          paste0("Data/cp_predictions/covariates_and_predictions_week",
                 i,
                 ".rds"))
}

# 4b) quickly check calibration plot for the actual observed catches
View(one_week_preds %>%
       filter(game_id == first(game_id), play_id == first(play_id)))

  # Need to get event column into data set as I had cut previously
target <- read_csv("Data/additional_data/targetedReceiver.csv") %>%
  janitor::clean_names() 


target_and_frame <- tibble()
for(i in 1:17){
  week1 <- read_csv(paste0("Data/week", i, ".csv"))  %>%
    janitor::clean_names()
  
  week1_reduced <- week1 %>%
    left_join(target, by = c("game_id", "play_id")) %>%
    filter(nfl_id == target_nfl_id, event %in% c("pass_forward", "pass_shovel")) %>%
    select(game_id, play_id, frame_id, target_nfl_id) 
  
  target_and_frame <- target_and_frame %>%
    bind_rows(week1_reduced)
}

saveRDS(target_and_frame, "Data/target_and_frame.rds")

# Read in all predictions and such, should probably just make a full preds frame
all_preds <- tibble()
for(i in 1:17){
  all_preds <- all_preds %>%
    bind_rows(readRDS(paste0("Data/cp_predictions/covariates_and_predictions_week",
                             i,
                             ".rds")))
}

saveRDS(all_preds, "Data/cp_predictions/all_predictions.rds")

# Join on target_and_frame, filter
all_preds_actually_targeted <- all_preds %>%
  right_join(target_and_frame,
             by = c("game_id", "play_id", "frame_id", "nfl_id" = "target_nfl_id"))

all_preds_actually_targeted %>%
  ggplot(aes(x = .pred_C)) +
  geom_histogram() +
  xlim(c(0,1))

# I have lots of missing values in the above join, investigate
  # 9607 missing out of 17363, roughly 50%
all_preds_actually_targeted %>%
  filter(!is.na(.pred_C)) %>%
  arrange(.pred_C) %>%
  mutate(bins = floor((row_number() - 1) / n() * 10)) %>%
  group_by(bins) %>%
  summarize(obs = mean(pass_result == "C", na.rm = TRUE),
            pred = mean(.pred_C, na.rm = TRUE),
            n = n()) %>%
  ggplot(aes(x = pred, y = obs)) +
  geom_point(size =3) +
  geom_abline(intercept = 0, slope = 1, col = "red", lty = 2) +
  xlim(c(0,1)) + ylim(c(0, 1))
