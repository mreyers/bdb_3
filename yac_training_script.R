# YAC tuning script, lets get it

all_preds_with_defenders <- readRDS("Data/yac/covariates.rds")

head(all_preds_with_defenders)
summary(all_preds_with_defenders)

temp <- read_csv("Data/plays.csv", col_types = cols()) %>%
  janitor::clean_names(case = "snake")

# No YAC in the plays
# Use yardline_100, arrival_x to estimate the yards gained in air
  # Then get YAC as the offense_play_result - air_yards_gained
# I think I have to do it this way because air_yards is with respect to ball distance
  # Since the QB is behind LOS we would have too many yards gained in the air, at
  # least as per how the NFL tabulates yards.

# Try with the code from ep_requirements
nfl_data <- read_csv("Data/plays.csv") %>%
  janitor::clean_names() %>%
  left_join(games_reduced, by = "game_id") %>%
  # It weirdly includes milliseconds so I modified
  mutate(numeric_time = as.POSIXct(game_clock, "%H:%M"),
         numeric_minutes = as.numeric(format(numeric_time, "%H")),
         numeric_seconds = as.numeric(format(numeric_time, "%M")),
         half_seconds_remaining = if_else(quarter == 1 | quarter == 3,
                                          15 * 60 + numeric_minutes * 60 + numeric_seconds,
                                          numeric_minutes * 60 + numeric_seconds),
         yardline_100 = if_else(possession_team == yardline_side,
                                100 - yardline_number,
                                yardline_number),
         yardline_100 = if_else(is.na(yardline_100), 50, yardline_100),
         # Pseudo goal to go detector, yards_to_go can only satisfy if endzone is FD
         goal_to_go = if_else(yards_to_go >= yardline_100, 1, 0)
  )

ep_requirements <- nfl_data %>%
  arrange(game_id, play_id) %>%
  dplyr::select(game_id, play_id, half_seconds_remaining,
                yardline_100, down, ydstogo = yards_to_go, goal_to_go,
                # Keep this for figuring out direction of play
                yardline_number, absolute_yardline_number) %>%
  mutate(half_seconds_remaining = if_else(is.na(half_seconds_remaining),
                                          # 500 seconds is arbitrary
                                          lag(half_seconds_remaining, default = 500),
                                          half_seconds_remaining),
         # 60 is midfield
         absolute_yardline_number = if_else(is.na(absolute_yardline_number),
                                            lag(absolute_yardline_number, default = 60),
                                            absolute_yardline_number)) %>%
  # Just default the remaining issues
  mutate(half_seconds_remaining = if_else(is.na(half_seconds_remaining), 500, half_seconds_remaining),
         absolute_yardline_number = if_else(is.na(absolute_yardline_number), 60, absolute_yardline_number))


# Need to extract some play level information from all preds as I have condensed things a bit
# for defenders
all_preds <- readRDS("Data/cp_predictions/all_predictions.rds")
all_preds_direction <- all_preds %>%
  left_join(ep_requirements %>% select(game_id, play_id,
                                       yardline_number, absolute_yardline_number),
            by = c("game_id", "play_id")) %>%
  group_by(game_id, play_id) %>%
  mutate(line_of_scrim_x = absolute_yardline_number) %>%
  # Field goes from 10 to 110 in x, be wary
  # This is fine, line_of_scrim_x is in the same units
  mutate(n_qb_left = sum(dist_to_qb == 0 & x < line_of_scrim_x, na.rm = TRUE),
         n_qb_right = sum(dist_to_qb == 0 & x > line_of_scrim_x, na.rm = TRUE),
         direction_of_play = if_else(n_qb_left > n_qb_right,
                                     "right",
                                     "left")) %>%
  mutate(yards_downfield = case_when(
    direction_of_play %in% "right" ~ arrival_x - line_of_scrim_x,
    direction_of_play %in% "left" ~ line_of_scrim_x - arrival_x
    ),
    yards_downfield = ceiling(yards_downfield),
    # Update yac later when we have it
    yac = 0,
    frame_id_2 = frame_id,
    receiver = nfl_id
  ) %>%
  # De-select as I have these in a global object that I'll call in fn
  select(-c(yardline_number, absolute_yardline_number)) %>%
  select(game_id, play_id, frame_id, nfl_id, direction_of_play, yards_downfield, line_of_scrim_x)


# Add in the summary information above
all_preds_with_defenders_imp <- all_preds_with_defenders %>%
  # left_join(all_preds_direction,
  #           by = c("game_id", "play_id", "frame_id", "nfl_id")) %>%
  group_by(game_id, play_id, frame_id, nfl_id) %>%
  slice(1) %>%
  ungroup()

# Pretty much the same but we now have line of scrimmage stuff
  # It seems like plays with NA number_of_pass_rushers are problematic
  # Perhaps they point out trick plays
all_preds_with_defenders_no_na_pr <- all_preds_with_defenders_imp %>%
  filter(!is.na(number_of_pass_rushers))

# Parse YAC
all_preds_defenders_yac <- all_preds_with_defenders_no_na_pr %>%
  # left_join(nfl_data %>% select(game_id, play_id, offense_play_result, play_result, penalty_codes),
  #           by = c("game_id", "play_id")) %>%
  mutate(is_comp = 1 * (pass_result == "C"),
         # Offense gains YAC on completion
         offense_yac = is_comp * (offense_play_result - yards_downfield),
         # Overall YAC can differ on fumbles
         overall_yac = is.na(penalty_codes) *
                               # Second term is frequently 0 except INT and FUM
                               (offense_yac - (offense_play_result - play_result)),
         # Also want YAC on non-penalty plays as a result of fumble
         fumble_yac = is.na(penalty_codes) * (play_result - offense_play_result))

# There are a few positive fumble YAC plays where a fumble happens and the ball progresses
# Probably those weird spillage plays, soft censor to 0
# I'll leave as is for now

all_preds_defenders_yac %>%
  ggplot(aes(x = offense_yac)) +
  geom_histogram()

# Hmm distribution isnt quite what I would expect, not horribly far off
# Skewed Gaussian likely, start with a preliminary fit on offense_yac and see where that goes
# Im just going to toss this in a glmer and see what happens
# Can come back and tailor things later
library(lme4)
defender_yac_setup <- all_preds_defenders_yac %>%
  mutate(logit_comp = case_when(.pred_C < 0.01 ~ log(0.01 / 0.99),
                                .pred_C > 0.99 ~ log(0.99 / 0.01),
                                TRUE ~ log(.pred_C / (1 - .pred_C))),
         yards_to_endzone_at_catch = yardline_100 - yards_downfield,
         defender_id_f = factor(defender_id)) %>%
  filter(!is.na(offense_yac), pass_result == "C")

# Lets try this
simple_yac <- lmer(offense_yac ~ logit_comp + rec_separation + velocity +
                     yards_to_endzone_at_catch + is_redzone + closed_distance +
                     distance_to_go_still + position_f + (1|defender_id_f),
                   data = defender_yac_setup)

summary(simple_yac)

# Full pred set
defender_yac_setup_all <- all_preds_defenders_yac %>%
  mutate(logit_comp = case_when(.pred_C < 0.01 ~ log(0.01 / 0.99),
                                .pred_C > 0.99 ~ log(0.99 / 0.01),
                                TRUE ~ log(.pred_C / (1 - .pred_C))),
         yards_to_endzone_at_catch = yardline_100 - yards_downfield,
         defender_id_f = factor(defender_id))

yac_preds <- predict(simple_yac, defender_yac_setup_all, re.form = NA)

# Bring it together
defender_yac_preds <- defender_yac_setup_all %>%
  mutate(yac_pred = yac_preds)

# Store results
saveRDS(defender_yac_preds, "Data/yac/yac_preds.rds")

# Lets see what a summary result looks like
defender_yac_preds %>%
  group_by(defender_id) %>%
  summarize(player = first(defender_name),
            n = n(),
            tot_yac = sum(offense_yac),
            tot_off_yacoe = sum(offense_yac - yac_pred),
            tot_yacoe = sum(overall_yac - yac_pred)) %>%
  arrange(desc(tot_yacoe)) %>%
  View()
