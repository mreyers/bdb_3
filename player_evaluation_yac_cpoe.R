# Note: I think that completions + interceptions should be valued together,
# incompletions valued alone. The completions and interceptions rely on end of
# play results while the incompletions just rely on the ball not being caught

# I am going to test this thought by bringing together the work from
# CPOE and YAC to investigate

# Load the required data
all_preds_with_defenders_and_epa <- readRDS("Data/all_preds_with_defenders_and_epa.rds")
  # Switch back to first_pass_ep_yac_all_3.rds for original results
first_pass_ep_yac_both <- readRDS("Data/yac/second_pass_ep_yac_all_3.rds")
deterrence_epa <- readRDS("Data/deterrence/deterrence_summary (1).RDS")
defender_assignment <- readRDS("Data/release_and_arrival.rds")

# Coordinate covariates, otherwise many dupes
first_pass_reduced <- first_pass_ep_yac_both %>%
  select(game_id, play_id, yac_epa_est = complete_epa, no_yac_epa_est = no_yac_epa,
         observed_yac_epa_est = observed_yac_epa,
         observed_yac_epa_est_int = observed_yac_epa_int)

# This is already a summary value, skip this one down to the WAR
deterrence_reduced <- deterrence_epa %>%
  select(nfl_id, deterrence_value = total_epa_value_over_expected)

# Separate into a completions + interceptions data set and an incompletions data set
complete_intercept <- all_preds_with_defenders_and_epa %>%
  filter(pass_result %in% c("C", "IN")) %>%
  left_join(first_pass_reduced, by = c("game_id", "play_id"))

incomplete <- all_preds_with_defenders_and_epa %>%
  filter(pass_result %in% c("I")) %>%
  left_join(first_pass_reduced, by = c("game_id", "play_id"))

# Now calculate EPA over expected in each scenario separately
  # I think for completions and interceptions there is no need for probability
  # Additionally, the yac epa estimates are currently full play and full play without yac
    # Need to difference them to get just the YAC
complete_intercept_value <- complete_intercept %>%
  mutate(yac_epa_est = if_else(is.na(yac_epa_est), 0, yac_epa_est),
         no_yac_epa_est = if_else(is.na(no_yac_epa_est), 0, no_yac_epa_est),
         observed_yac_epa_est = if_else(is.na(observed_yac_epa_est), 
                                        observed_epa,
                                        observed_yac_epa_est),
         observed_yac_epa_est_int = if_else(is.na(observed_yac_epa_est_int),
                                            observed_epa,
                                            observed_yac_epa_est_int),
         just_the_yac_epa = yac_epa_est - no_yac_epa_est,
         play_value_est = pred_c_no_def * (complete_epa + just_the_yac_epa),
         play_value_over_est = observed_yac_epa_est - play_value_est, # Check if replace with obs_yac_epa_est
         total_yac_epa = pred_c_no_def * just_the_yac_epa,
         total_complete_epa = pred_c_no_def * complete_epa,
         # Used to all be within if_else(int) for yac, now splitting
         yac_value_over_est = (pass_result != "IN") * (observed_yac_epa_est - yac_epa_est),
         int_value_over_est = (pass_result == "IN") * (observed_yac_epa_est_int - yac_epa_est),
         complete_value_over_est = no_yac_epa_est - total_complete_epa,
         play_value_over_est_2 = yac_value_over_est + complete_value_over_est + int_value_over_est,
         play_value_over_est_no_int = yac_value_over_est + complete_value_over_est)

# Continue revising above formulas

  # On incompletions though the value is 0 unless probability is included and should be
  # higher as the probability of pass completion rises
incomplete_value <- incomplete %>%
  mutate(play_value_est = (1 - pred_c_no_def) * incomplete_epa,
         play_value_over_est = observed_epa - play_value_est,
         play_value_over_est_2 = play_value_over_est,
         play_value_over_est_no_int = play_value_over_est)

# Now combine the necessary elements in one standardized summary
  # Adding in the 0 columns to allow row binding while also indicating what is not used
complete_intercept_value_reduced <- complete_intercept_value %>%
  select(game_id, play_id, pass_result, defender_id, defender_name,
         pred_c_no_def, complete_epa, incomplete_epa = 0, just_the_yac_epa, observed_epa,
         play_value_est,
         play_value_over_est, complete_value_over_est, yac_value_over_est, int_value_over_est,
         play_value_over_est_2,
         play_value_over_est_no_int)

incomplete_value_reduced <- incomplete_value %>%
  select(game_id, play_id, pass_result, defender_id, defender_name,
         pred_c_no_def, complete_epa = 0, incomplete_epa, just_the_yac_epa = 0, observed_epa,
         play_value_est, play_value_over_est, complete_value_over_est = 0,
         yac_value_over_est = 0, int_value_over_est = 0,
         play_value_over_est_2,
         play_value_over_est_no_int)

all_value_reduced <- complete_intercept_value_reduced %>%
  bind_rows(incomplete_value_reduced)

# Add in the defender IDs for release and arrival
all_value_reduced <- all_value_reduced %>%
  left_join(defender_assignment,
            by = c("game_id", "play_id"))

# Now summarize with respect to these outcomes
  # Adding in deterrence here
  # Now I need to switch the ID info 
  # Use closest at release for deterrence
all_values_long <- all_value_reduced %>%
  pivot_longer(cols = c(release, arrival),
               names_to = "frame_time",
               values_to = "nearest_defender_id")

# Separate by type of frame
release_summary <- all_values_long %>%
  filter(frame_time %in% "release")
  
arrival_summary <- all_values_long %>%
  filter(frame_time %in% "arrival")

# Modify nearest_defender_id to be intercepting_player_id
just_interceptions_name_check <- readRDS("Data/just_interceptions_name_check.rds")
arrival_summary <- arrival_summary %>%
  left_join(just_interceptions_name_check, by = c("game_id", "play_id")) %>%
  mutate(nearest_defender_id = if_else(!is.na(modified_nearest_defender),
                                       modified_nearest_defender,
                                       nearest_defender_id)) %>%
  select(-defender_name) %>%
  left_join(players %>% select(nearest_defender_id = nfl_id, defender_name = display_name),
            by = c("nearest_defender_id"))

# Check interceptions in arrival_summary
arrival_summary %>%
  filter(defender_name %in% "Damontae Kazee") %>% View()

# Release value should be deterrence
release_value_summarized <- release_summary %>%
  group_by(nearest_defender_id) %>%
  summarize(n_reps = n()) %>%
  left_join(deterrence_reduced %>%
              mutate(defender_id = as.numeric(as.character((nfl_id)))) %>%
              select(defender_id, total_deterrence_value_over_est = deterrence_value),
            by = c("nearest_defender_id" = "defender_id"))

arrival_value_summarized <- arrival_summary %>%
  group_by(nearest_defender_id) %>%
  summarize(defender_name = first(defender_name),
            n_target = n(),
            completion_percentage_allowed = sum(as.numeric(pass_result == "C")) / n_target,
            expected_completion_percentage_allowed = mean(pred_c_no_def),
            cpoe = completion_percentage_allowed - expected_completion_percentage_allowed,
            #total_play_value_observed = sum(observed_epa),
            #total_play_value_est = sum(play_value_est),
            #total_play_value_over_est = sum(play_value_over_est),
            total_yac_value_over_est = sum(yac_value_over_est, na.rm = TRUE),
            total_int_value_over_est = sum(int_value_over_est, na.rm = TRUE),
            total_play_value_over_est_2 = sum(play_value_over_est_2, na.rm = TRUE),
            total_play_value_over_est_no_int = sum(play_value_over_est_no_int, na.rm = TRUE),
            # Do as subtraction instead of sum(complete_val_over_est) due to incomplete passes
            total_complete_value_over_est = 
              total_play_value_over_est_2 - total_yac_value_over_est - total_int_value_over_est)
  
all_values_summarized <- arrival_value_summarized %>%
  left_join(release_value_summarized, by = "nearest_defender_id") %>%
  mutate(total_play_value_over_est_2 = if_else(!is.na(total_deterrence_value_over_est),
                                               total_play_value_over_est_2 + total_deterrence_value_over_est,
                                               total_play_value_over_est_2),
         total_play_value_over_est_no_int = if_else(!is.na(total_deterrence_value_over_est),
                                               total_play_value_over_est_no_int + total_deterrence_value_over_est,
                                               total_play_value_over_est_no_int))

# Old approach, updated now due to release/arrival discrepancy
# # Arrival value should be CP, YAC, etc
# all_values_summarized <- all_values_summarized %>%
#   group_by(nearest_defender_id) %>%
#   summarize(defender_id = first(defender_id), # This is at release
#             n_target = n(),
#             completion_percentage_allowed = sum(as.numeric(pass_result == "C")) / n_target,
#             expected_completion_percentage_allowed = mean(pred_c_no_def),
#             cpoe = completion_percentage_allowed - expected_completion_percentage_allowed,
#             #total_play_value_observed = sum(observed_epa),
#             #total_play_value_est = sum(play_value_est),
#             #total_play_value_over_est = sum(play_value_over_est),
#             total_yac_value_over_est = sum(yac_value_over_est, na.rm = TRUE),
#             total_int_value_over_est = sum(int_value_over_est, na.rm = TRUE),
#             total_play_value_over_est_2 = sum(play_value_over_est_2, na.rm = TRUE),
#             # Do as subtraction instead of sum(complete_val_over_est) due to incomplete passes
#             total_complete_value_over_est = 
#               total_play_value_over_est_2 - total_yac_value_over_est - total_int_value_over_est) %>%
#   # modify afterwards to not mess up complete_value estimation
#   left_join(deterrence_reduced %>%
#               mutate(defender_id = as.numeric(as.character((nfl_id)))) %>%
#               select(defender_id, total_deterrence_value_over_est = deterrence_value),
#             by = c("defender_id")) %>%
#   # Might need to also is.na swap the total_deterrence value
#   mutate(total_play_value_over_est_2 = if_else(!is.na(total_deterrence_value_over_est),
#                                                total_play_value_over_est_2 + total_deterrence_value_over_est,
#                                                total_play_value_over_est_2))

# New and old approach are highly correlated, I think I like the new one better
  # Now that I include interception run backs, differs a fair bit (0.578 corr)
# all_values_summarized %>%
#   arrange(total_play_value_over_est_2) %>%
#   View()
# 
# all_values_summarized %>%
#   ggplot(aes(x = total_play_value_over_est_2, y = total_play_value_observed)) +
#   geom_point() +
#   geom_vline(data = all_values_summarized %>%
#                summarize(mean_pred = mean(total_play_value_over_est)),
#              aes(xintercept = mean_pred), col = "red") +
#   geom_hline(data = all_values_summarized %>%
#                summarize(mean_obs = mean(total_play_value_observed)),
#              aes(yintercept = mean_obs), col = "red") +
#   xlab("Predicted EPA Surrendered") + ylab("Observed EPA Surrendered") +
#   ggtitle("Observed vs Predicted EPA Surrendered When Nearest Defender") +
#   theme_bw()


war_benchmarks <- all_values_summarized %>%
  summarize(avg_value_over_est = mean(total_play_value_over_est_2),
            # Need to do this one backwards because negative is good
            tenth_percentile_over_est = quantile(total_play_value_over_est_2, 0.9),
            # WAR without INT
            avg_value_over_est_no_int = mean(total_play_value_over_est_no_int),
            tenth_percentile_over_est_no_int = quantile(total_play_value_over_est_no_int, 0.9),
            # complete
            avg_value_over_est_complete = mean(total_complete_value_over_est),
            tenth_percentile_over_est_complete = quantile(total_complete_value_over_est, 0.9),
            # yac
            avg_value_over_est_yac = mean(total_yac_value_over_est),
            tenth_percentile_over_est_yac = quantile(total_yac_value_over_est, 0.9),
            # deterrence
            avg_value_over_est_deterrence = mean(total_deterrence_value_over_est, na.rm = TRUE),
            tenth_percentile_over_est_deterrence = quantile(total_deterrence_value_over_est, 0.9, na.rm = TRUE)
            )

# Now a WAR plot
  # Change to use the no_int option, used to be est_2
war_options <- all_values_summarized %>%
  mutate(total_war_per_384 = -1 * total_play_value_over_est_no_int / 38.4,
         total_war_per_avg = -1 * (total_play_value_over_est_no_int - war_benchmarks$avg_value_over_est) / 38.4,
         total_war_per_tenth = -1 * (total_play_value_over_est_no_int - war_benchmarks$tenth_percentile_over_est) / 38.4)

# war_options <- all_values_summarized %>%
#   mutate(total_war_per_384 = -1 * total_play_value_over_est_2 / 38.4,
#          total_war_per_avg = -1 * (total_play_value_over_est_2 - war_benchmarks$avg_value_over_est) / 38.4,
#          total_war_per_tenth = -1 * (total_play_value_over_est_2 - war_benchmarks$tenth_percentile_over_est) / 38.4)


saveRDS(war_options, "Data/war_options.rds")

war_options %>%
  ggplot() +
  geom_density(aes(x = total_war_per_384, col = "per_384")) +
  geom_density(aes(x = total_war_per_avg, col = "per_avg")) +
  geom_density(aes(x = total_war_per_tenth, col = "per_tenth")) +
  theme_bw() +
  ggtitle("Wins Options")

# Additionally there was a thought of turning these into grades
# Would do this by calculating Z-scores for each skill and converting to the percentile
# associated. Using same mock up as the GT table here, just renamed

  # Was getting weird SD NA values, just do separately for now
complete_value <- war_options$total_complete_value_over_est
complete_z <- (complete_value - mean(complete_value)) / sd(complete_value)
complete_p <- pnorm(complete_z, lower.tail = FALSE) # lower because negative is good
complete_grades <- (complete_p - min(complete_p)) / (max(complete_p) - min(complete_p))

yac_value <- war_options$total_yac_value_over_est
yac_z <- (yac_value - mean(yac_value)) / sd(yac_value)
yac_p <- pnorm(yac_z, lower.tail = FALSE)
yac_grades <- (yac_p - min(yac_p)) / (max(yac_p) - min(yac_p))

int_value <- war_options$total_int_value_over_est
int_z <- (int_value - mean(int_value)) / sd(int_value)
int_p <- pnorm(int_z, lower.tail = FALSE)
int_grades <- (int_p - min(int_p)) / (max(int_p) - min(int_p))

det_value <- if_else(is.na(war_options$total_deterrence_value_over_est),
                     0,
                     war_options$total_deterrence_value_over_est)
det_z <- (det_value - mean(det_value)) / sd(det_value)
det_p <- pnorm(det_z, lower.tail = FALSE)
det_grades <- (det_p - min(det_p)) / (max(det_p) - min(det_p))


z_score_setup <- war_options %>%
  select(defender_name,
         completion_percentage_allowed, expected_completion_percentage_allowed, cpoe,
         total_complete_value_over_est, total_yac_value_over_est,
         total_int_value_over_est, total_deterrence_value_over_est,
         total_play_value_over_est_2,
         total_war_per_tenth) %>%
  mutate(total_complete_value_grade = complete_grades,
         total_yac_value_grade = yac_grades,
         total_int_value_grade = int_grades,
         total_det_value_grade = det_grades)


# Setting up some prettier table options
library(gt)
library(paletteer)

# Need to calculate proportion of WAR to assign each category

# I think since I like the second approach better I will focus on that
  # Something is going weird, not summing back up to total WAR
  # Likely to do with the baselining that happens to setup WAR in the first place
  # Revise later
gt_table_setup <- z_score_setup %>%
  mutate(abs_magnitude = abs(total_complete_value_over_est) +
           abs(total_yac_value_over_est) + abs(total_int_value_over_est) + abs(total_deterrence_value_over_est),
         total_war_per_tenth_complete =
           (total_complete_value_over_est) /
           total_play_value_over_est_2 * total_war_per_tenth,
         total_war_per_tenth_yac =
           (total_yac_value_over_est) /
           total_play_value_over_est_2 * total_war_per_tenth,
         total_war_per_tenth_int =
           (total_int_value_over_est) /
           total_play_value_over_est_2 * total_war_per_tenth,
         total_war_per_tenth_det =
           (total_deterrence_value_over_est) /
           total_play_value_over_est_2 * total_war_per_tenth) %>%
    select(defender_name,
           completion_percentage_allowed, expected_completion_percentage_allowed, cpoe,
           total_complete_value_grade, total_yac_value_grade,
           total_int_value_grade, total_det_value_grade,
           #total_play_value_over_est_2,
           total_war_per_tenth,
           total_war_per_tenth_complete,
           total_war_per_tenth_yac,
           total_war_per_tenth_int,
           total_war_per_tenth_det)
  
gt_table_setup %>%
  select(-completion_percentage_allowed) %>%
  arrange(desc(total_war_per_tenth)) %>%
  slice(1:20) %>%
  gt() %>%
  cols_label(
    defender_name = "Defender",
    expected_completion_percentage_allowed = "xCP",
    cpoe = "CPOE",
    total_complete_value_grade = "Pass Defense Value Grade (WAR)",
    total_yac_value_grade = "Pursuit Value Grade (WAR)",
    total_int_value_grade = "Interception Value Grade (WAR)",
    total_det_value_grade = "Deterrence Value Grade (WAR)",
    #total_play_value_over_est_2 = "Combined Contribution Over Est",
    total_war_per_tenth = "Contribution as WAR"
  ) %>%
  # WAR is in the right direction, value isnt
  # Now that it is grades, remove the rev()
  data_color(
    columns = vars(total_complete_value_grade, total_yac_value_grade,
                   total_int_value_grade, total_det_value_grade),
                   #total_play_value_over_est_2),
    colors = scales::col_numeric(
      palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
      domain = NULL
      )
  ) %>%
  data_color(
    columns = vars(total_war_per_tenth, total_war_per_tenth_yac,
                   total_war_per_tenth_complete,
                   total_war_per_tenth_int, total_war_per_tenth_det),
    colors = scales::col_numeric(
      palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
      domain = NULL
    )
  ) %>%
  fmt_percent(
    columns = vars(expected_completion_percentage_allowed)
  ) %>%
  fmt_number(
    columns = vars(cpoe),
    decimals = 4
  ) %>%
  fmt_number(
    columns = vars(total_war_per_tenth_yac,
                   total_war_per_tenth_complete,
                   total_war_per_tenth_int,
                   total_war_per_tenth_det,
                   total_war_per_tenth),
    decimals = 2
  ) %>%
  fmt_number(
    columns = vars(total_complete_value_grade, total_yac_value_grade,
                   total_int_value_grade, total_det_value_grade),
    decimals = 0,
    scale_by = 100
  ) %>% 
  cols_merge(
    columns = vars(total_complete_value_grade, total_war_per_tenth_complete),
    hide_columns = vars(total_war_per_tenth_complete),
    pattern = "{1} ({2})"
  ) %>%
  cols_merge(
    columns = vars(total_yac_value_grade, total_war_per_tenth_yac),
    hide_columns = vars(total_war_per_tenth_yac),
    pattern = "{1} ({2})"
  ) %>%
  cols_merge(
    columns = vars(total_int_value_grade, total_war_per_tenth_int),
    hide_columns = vars(total_war_per_tenth_int),
    pattern = "{1} ({2})"
  ) %>%
  cols_merge(
    columns = vars(total_det_value_grade, total_war_per_tenth_det),
    hide_columns = vars(total_war_per_tenth_det),
    pattern = "{1} ({2})"
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "right",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = vars(defender_name, cpoe)
      )
    )
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "right",
        color = "black",
        weight = px(1)
      )
    ),
    locations = list(
      cells_body(
        columns = vars(total_complete_value_grade,
                       total_yac_value_grade,
                       total_int_value_grade,
                       total_det_value_grade,
                       total_war_per_tenth)
      )
    )
  ) %>%
  cols_move(
    columns = vars(total_det_value_grade, total_complete_value_grade,
                   total_yac_value_grade, total_int_value_grade),
    after = vars(cpoe)
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>%
  tab_header(
    title = md("**Defender Metric Summaries from the 2018 NFL Season**"),
    subtitle = "Top 20 Players by Wins Above Replacement (WAR)"
  )
