# Skill / Value Estimation part 3
  # Recovery model in terms of limiting yards / value after the catch

# This model may well be simple, just extending the SIS work to handle the idea
# Doing so would involve estimating YAC, either by reproducing nflfastR model or
# by pivoting thesis work over.

# TODO:
  # 1. Decide on YAC approach
  # 2. Calculate YACOE given the base model
  # 3. Estimate additional EPA gained between Observed and Expected YAC
  # 4. Append these values to our current setup
  # 5. Having completed all 3 models, find a way to appropriately combine the evaluation schemes


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

target <- read_csv("Data/additional_data/targetedReceiver.csv") %>%
  janitor::clean_names() 

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


# 1. Approach
# Current plan is to grab all plays with a completion on it, filter all frames at which
# the pass was completed, identify relevant covariates, identify the number of yards gained
# after the catch, and model with a discrete distribution (or pretend its continuous, made
# discrete by measurement error)

# Start by loading in plays and covariates
  # Might as well be lazy and grab the existing files
all_preds <- readRDS("Data/cp_predictions/all_predictions.rds")


# Append nearest defender and filter down to arrival frame in this case
  # Recycling but modifying steps 4 and 5 from CPOE
play_pass_start <- c("pass_forward", "pass_shovel")
play_pass_arrive <- c("pass_outcome_caught", "pass_outcome_incomplete",
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
    group_by(game_id, play_id, nfl_id, display_name) %>%
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
  filter(frame_id == arrival_frame) 

# This is currently missing some plays for some reason
  # e.g. game 2018090600, play 190, 33 yard catch
  # Occurs in plays.csv but not in all_preds_with_defenders
saveRDS(all_preds_with_defenders, "Data/yac/covariates.rds")

  # Before writing the tuning script, need to get the defender that eventually
  # makes the tackle / force out on any completion. I think I will ultimately
  # give benefit to the tackler and debit the nearest defender
  # Feedback will eventually come from team
  # Actually, I dont need to have an answer for this right now, I can still build
  # YAC model without knowing this.
# Skip over to go write a model tuning script for this
yac_preds_results <- readRDS("Data/yac/yac_preds.rds")

# Need to get the EPA associated with these plays now
# After will need to combine with CPOE
# I think I will have to do this 2 ways
# Modify completion probability to be 100 percent, use YAC = 0 and YAC = estimate
# Call YAC EPA to be EPA(estimate) - EPA(0)
