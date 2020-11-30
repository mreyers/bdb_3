# Skill / Value Estimation part 1
  # CPOE Model and relative change in CPOE from pass frame to arrival frame

# This one we have a bunch of infrastructure for
# I will lay out a TO DO list and a COMPLETE list
# Move things appropriately as they are completed

# TODO:
  
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
  # 4. Apply CP model to all frames within all plays using purrr::safely()
  # 5. Return dataset appending CP values for each eligible receiver (non-QB) and their corresponding nearest defender
  # 6. Use Outcome - CP to generate CPOE across dataset

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

players <- read_csv("Data/players.csv", col_types = cols()) %>%
  janitor::clean_names(case = "snake") 

games <- read_csv("Data/games.csv", col_types = cols()) %>%
  janitor::clean_names(case = "snake")

games_reduced <- games %>%
  select(game_id, home_team_abbr, visitor_team_abbr)

plays_essential <- read_csv("Data/plays.csv", col_types = cols()) %>%
  janitor::clean_names(case = "snake") %>%
  left_join(games_reduced, by = "game_id") %>%
  mutate(yardline_100 = if_else(possession_team == yardline_side,
                                100 - yardline_number,
                                yardline_number),
         yardline_100 = if_else(is.na(yardline_100), 50, yardline_100),
         score_differential = if_else(possession_team == home_team_abbr,
                                      pre_snap_home_score - pre_snap_visitor_score,
                                      -1 * (pre_snap_home_score - pre_snap_visitor_score)),
         is_redzone = factor(yardline_100 < 20, levels = c(FALSE, TRUE))) %>%
  select(game_id, play_id, down, ydstogo = yards_to_go, yardline_100,
         score_differential, is_redzone, number_of_pass_rushers)

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
  
  # Supplement covariates with relevant play level info
  one_week_fixed_context <- one_week_renamed %>%
    nest(-c(dist_from_pocket, first_elig, last_elig)) %>%
    mutate(new_dist_from_pocket = map2(dist_from_pocket, first_elig,
                                       ~ .x %>% mutate(frame_id = row_number() - 1 + .y)),
           data = map2(data, new_dist_from_pocket, ~ .x %>% left_join(.y, by = "frame_id"))) %>%
    select(-c(dist_from_pocket, new_dist_from_pocket)) %>%
    unnest(data) %>%
    left_join(plays_essential, by = c("game_id", "play_id")) %>%
    select(-rush_sep) %>%
    # Sensible default exists for score differential and yardline_100, not intensity
    mutate(score_differential = if_else(is.na(score_differential), 0, score_differential),
           yardline_100 = if_else(is.na(yardline_100), 50, yardline_100)) %>%
    filter(!is.na(own_avg_intensity))
  
  # Add additional covariate for type of target receiver
  one_week_fixed_context <- one_week_fixed_context %>%
    left_join(players %>% select(nfl_id, position), by = c("nfl_id")) %>%
    mutate(position_f = factor(case_when(
      position == "WR" ~ "WR",
      position == "TE" ~ "TE",
      TRUE ~ "RB"
    ), levels = c("WR", "TE", "RB"))) %>%
    select(-position)
  
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

# Removed the Calibration Plot exercise from here as that is in my training file
  # nfl_tracking

# 3. Framewise labelling for nearest receiver
  # I guess not even necessarily framewise
  # Since this component is on CPOE, probably only really care about pass release frame
  # and the pass result. Also would care who the nearest defender is at release and who
  # is nearest at time of arrival. Only question is how to distribute credit.
  # I guess preliminary model would be to assign all credit to nearest defender at time
  # of arrival. This is the observed model so no deterrence question here. Simply
  # CP at time of pass release and nearest defender at pass arrival.

# Generate nearest defender at pass arrival

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
    group_by(game_id, play_id, nfl_id,  display_name) %>%
    arrange(desc(frame_id)) %>%
    # Negative is good
    summarize(team_role = first(team_role),
              throw_frame = last(frame_id),
              arrival_frame = first(frame_id),
              closed_distance = first(dist_to_receiver) - last(dist_to_receiver),
              distance_to_go_still = first(dist_to_receiver),
              time_elapsed_in_frames = first(frame_id) - last(frame_id),
              time_elapsed_in_seconds = time_elapsed_in_frames / 10,
              .groups = "drop") %>%
    arrange(distance_to_go_still) %>%
    group_by(game_id, play_id, team_role) %>%
    slice(1)
  
  nearest_def_and_rec <- nearest_def_and_rec %>%
    bind_rows(my_play_with_receiver)
}

# This holds the nearest defender to the targeted receiver on any given play in dataset
nearest_def_wide <- nearest_def_and_rec %>%
  group_by(game_id, play_id) %>%
  mutate(target_nfl_id = if_else(first(team_role) %in% "off",
                                 first(nfl_id),
                                 last(nfl_id))) %>%
  filter(team_role %in% "def")

# 5. Return dataset appending CP values for each targeted receiver (non-QB) and their corresponding nearest defender
all_preds_with_defenders <- all_preds %>%
  filter(nfl_id == target) %>%
  left_join(nearest_def_wide %>%
              select(game_id, play_id, defender_id = nfl_id, defender_name = display_name,
                     closed_distance, distance_to_go_still, throw_frame, arrival_frame),
            by = c("game_id", "play_id")) %>%
  filter(frame_id == throw_frame) 

# 6. Use Outcome - CP to generate CPOE across dataset
basic_cpoe <- all_preds_with_defenders %>%
  group_by(defender_id) %>%
  summarize(defender = first(defender_name),
            n_games = length(unique(game_id)),
            nearest_rec_targeted = n(),
            nearest_rec_completed = sum(pass_result == "C"),
            comp_perc_allowed = nearest_rec_completed / nearest_rec_targeted,
            exp_comp_perc_allowed = mean(.pred_C),
            cpoe = mean(as.numeric(pass_result == "C") - .pred_C),
            .groups = "drop"
  )

basic_cpoe %>% 
  filter(n_games > 10) %>%
  arrange(cpoe) %>%
  View()

  # 6.a) Estimate an adjusted CP based on relevant QB and Target
qb_play <- all_preds %>%
  group_by(game_id, play_id) %>%
  filter(frame_id == first(frame_id)) %>%
  left_join(players %>% select(nfl_id, position) %>% filter(position %in% "QB"), by = "nfl_id") %>%
  filter(!is.na(position)) %>%
  select(game_id, play_id, qb_id = nfl_id)

all_preds_with_defenders_and_qbs <- all_preds_with_defenders %>%
  left_join(qb_play, by = c("game_id", "play_id")) %>%
  mutate(pass_result_bin = if_else(pass_result %in% "C", 1, 0),
         qb_id_f = factor(qb_id),
         target_id_f = factor(nfl_id),
         defender_id_f = factor(defender_id),
         logit_pred_c = case_when(.pred_C < 0.01 ~ log(0.01 / 0.99),
                                  .pred_C > 0.99 ~ log(0.99 / 0.01),
                                  TRUE ~ log(.pred_C / (1 - .pred_C))))

library(brms)
# Super simple brms model to estimate adjustment to CP
tictoc::tic()
basic_brms_adjust <- brm(pass_result_bin ~ logit_pred_c +
                           (1|qb_id_f) + (1|target_id_f) + (1|defender_id_f),
                               data = all_preds_with_defenders_and_qbs,
                               family = bernoulli(),
                               chains = 2,
                               warmup = 1000,
                               iter = 1500,
                         cores = 2,
                         silent = FALSE)
tictoc::toc()

# Yeah ok this seems to work alright
summary(basic_brms_adjust)

# Lets get the predictions for each observation then from this
  # Will represent adjusted probability
adjusted_pred_c_all <- predict(basic_brms_adjust,
                               re_formula = ~ (1|qb_id_f) + (1|target_id_f) + (1|defender_id_f))
adjusted_pred_c_no_def <- predict(basic_brms_adjust,
                                  re_formula = ~ (1|qb_id_f) + (1|target_id_f))

  # 6 a) Do value attribution via result - adjusted_pred_c_no_def
value_attr <- all_preds_with_defenders_and_qbs %>%
  # First column is the prediction
  mutate(pred_c_no_def = adjusted_pred_c_no_def[,1]) %>%
  group_by(defender_id) %>%
  summarize(defender = first(defender_name),
            n_games = length(unique(game_id)),
            nearest_rec_targeted = n(),
            nearest_rec_completed = sum(pass_result == "C"),
            comp_perc_allowed = nearest_rec_completed / nearest_rec_targeted,
            exp_comp_perc_allowed = mean(.pred_C),
            exp_comp_perc_allowed_given_players = mean(pred_c_no_def),
            cpoe = sum(as.numeric(pass_result == "C") - .pred_C),
            cpoe_controlled =  sum(as.numeric(pass_result == "C") - pred_c_no_def),
            .groups = "drop"
  )

value_attr %>%
  arrange(cpoe_controlled) %>%
  slice(1:20) %>% View()

value_attr %>%
  ggplot(aes(x = cpoe, y = cpoe_controlled, size = nearest_rec_targeted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red", lty = 2) +
  ggtitle("Comparison of BRMS regulated vs uncontrolled CPOE") +
  theme_bw()

  # 6 b) Measure defensive skill via coefficient estimates in model
skill_measures <- ranef(basic_brms_adjust)
defender_skills <- skill_measures$defender_id_f[,,1] %>%
  data.frame() %>%
  rownames_to_column() %>%
  tibble() %>%
  left_join(players %>% select(nfl_id, display_name) %>%
              mutate(nfl_id = as.character(nfl_id)), by = c("rowname" = "nfl_id")) %>%
  arrange(Estimate)

qb_skills <- skill_measures$qb_id_f[,,1] %>%
  data.frame() %>%
  rownames_to_column() %>%
  tibble() %>%
  left_join(players %>% select(nfl_id, display_name) %>%
              mutate(nfl_id = as.character(nfl_id)), by = c("rowname" = "nfl_id")) %>%
  arrange(desc(Estimate))

target_skills <- skill_measures$target_id_f[,,1] %>%
  data.frame() %>%
  rownames_to_column() %>%
  tibble() %>%
  left_join(players %>% select(nfl_id, display_name) %>%
              mutate(nfl_id = as.character(nfl_id)), by = c("rowname" = "nfl_id")) %>%
  arrange(desc(Estimate))

# 7. Estimate value per frame using Hypothetical EPA
  # Going to source this one ine from ep_calculation.R
first_pass_ep_unnested <- readRDS("Data/first_pass_ep.rds")

# 8. Assign value per frame based on Hypothetical EPA, the CP * EPA hybrid from thesis
act_epa <- read_csv("Data/plays.csv") %>%
  janitor::clean_names() %>%
  select(game_id, play_id, observed_epa = epa)

# Add a join feature as I lose a few plays somewhere
join_holder <- all_preds_with_defenders_and_qbs %>%
  # First column is the prediction
  mutate(pred_c_no_def = adjusted_pred_c_no_def[,1]) %>%
  select(game_id, play_id, nfl_id, pred_c_no_def)

all_preds_with_defenders_and_epa <- first_pass_ep_unnested %>%
  filter(nfl_id == target) %>%
  left_join(nearest_def_wide %>%
              select(game_id, play_id, defender_id = nfl_id, defender_name = display_name,
                     closed_distance, distance_to_go_still, throw_frame, arrival_frame),
            by = c("game_id", "play_id")) %>%
  filter(frame_id == throw_frame) %>%
  left_join(act_epa, by = c("game_id", "play_id")) %>%
  left_join(join_holder, by = c("game_id", "play_id", "nfl_id")) %>%
  mutate(hypothetical_epa = pred_c_no_def * complete_epa + (1 - pred_c_no_def) * incomplete_epa,
         epa_allowed_above_hypothetical = observed_epa - hypothetical_epa)

# Lets see some summaries
ranked_2018_defenders <- all_preds_with_defenders_and_epa %>%
  # Problematic play, clearly something wrong in most calculations, gets duplicated a bunch
  filter(!(game_id == 2018092300 & play_id == 4480)) %>%
  group_by(defender_name) %>%
  summarize(defender = first(defender_name),
            n_games = length(unique(game_id)),
            nearest_rec_targeted = n(),
            nearest_rec_completed = sum(pass_result == "C"),
            comp_perc_allowed = nearest_rec_completed / nearest_rec_targeted,
            exp_comp_perc_allowed = mean(.pred_C),
            exp_comp_perc_allowed_given_players = mean(pred_c_no_def),
            cpoe = sum(as.numeric(pass_result == "C") - .pred_C),
            cpoe_controlled =  sum(as.numeric(pass_result == "C") - pred_c_no_def),
            tot_epa_allowed_above_exp = sum(epa_allowed_above_hypothetical),
            .groups = "drop") %>%
  arrange(tot_epa_allowed_above_exp)

# 9. Upweight plays corresponding to large +/-WPA moments through scale_factors.R