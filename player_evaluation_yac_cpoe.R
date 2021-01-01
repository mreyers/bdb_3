# Note: I think that completions + interceptions should be valued together,
# incompletions valued alone. The completions and interceptions rely on end of
# play results while the incompletions just rely on the ball not being caught

# I am going to test this thought by bringing together the work from
# CPOE and YAC to investigate

# Load the required data
all_preds_with_defenders_and_epa <- readRDS("Data/all_preds_with_defenders_and_epa.rds")
first_pass_ep_yac_both <- readRDS("Data/yac/first_pass_ep_yac_all_3.rds")

# Coordinate covariates, otherwise many dupes
first_pass_reduced <- first_pass_ep_yac_both %>%
  select(game_id, play_id, yac_epa_est = complete_epa, no_yac_epa_est = no_yac_epa,
         observed_yac_epa_est = observed_yac_epa,
         observed_yac_epa_est_int = observed_yac_epa_int)

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
         observed_yac_epa_est = if_else(is.na(observed_yac_epa_est), 0, observed_yac_epa_est),
         just_the_yac_epa = yac_epa_est - no_yac_epa_est,
         play_value_est = pred_c_no_def * (complete_epa + just_the_yac_epa),
         play_value_over_est = observed_yac_epa_est - play_value_est, # Check if replace with obs_yac_epa_est
         total_yac_epa = pred_c_no_def * just_the_yac_epa,
         total_complete_epa = pred_c_no_def * complete_epa,
         yac_value_over_est = if_else(pass_result != "IN",
                                      observed_yac_epa_est - yac_epa_est,
                                      observed_yac_epa_est_int - yac_epa_est),
         complete_value_over_est = no_yac_epa_est - total_complete_epa,
         play_value_over_est_2 = yac_value_over_est + complete_value_over_est)

# Continue revising above formulas

  # On incompletions though the value is 0 unless probability is included and should be
  # higher as the probability of pass completion rises
incomplete_value <- incomplete %>%
  mutate(play_value_est = (1 - pred_c_no_def) * incomplete_epa,
         play_value_over_est = observed_epa - play_value_est,
         play_value_over_est_2 = play_value_over_est)

# Check with plots
complete_intercept_value %>% ggplot(aes(x = play_value_est)) + geom_histogram()
complete_intercept_value %>% filter(pass_result %in% "C") %>%
  ggplot(aes(x = complete_epa, y = observed_epa)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red")
complete_intercept_value %>% ggplot(aes(x = play_value_est, y = observed_epa)) +
  geom_point(col = "red") +
  geom_point(aes(x = complete_epa + yac_epa_est), col = "blue")
complete_intercept_value %>% ggplot(aes(x = play_value_over_est)) + geom_histogram()
incomplete_value %>% ggplot(aes(x = play_value_over_est)) + geom_histogram()

complete_intercept_value %>% group_by(defender_id) %>%
  summarize(play_value_over_est = sum(play_value_over_est)) %>%
  ggplot(aes(x = play_value_over_est)) + geom_histogram()
# Now combine the necessary elements in one standardized summary
  # Adding in the 0 columns to allow row binding while also indicating what is not used
complete_intercept_value_reduced <- complete_intercept_value %>%
  select(game_id, play_id, pass_result, defender_id, defender_name,
         pred_c_no_def, complete_epa, incomplete_epa = 0, just_the_yac_epa, observed_epa,
         play_value_est, play_value_over_est, complete_value_over_est, yac_value_over_est,
         play_value_over_est_2)

incomplete_value_reduced <- incomplete_value %>%
  select(game_id, play_id, pass_result, defender_id, defender_name,
         pred_c_no_def, complete_epa = 0, incomplete_epa, just_the_yac_epa = 0, observed_epa,
         play_value_est, play_value_over_est, complete_value_over_est = 0,
         yac_value_over_est = 0,
         play_value_over_est_2)

all_value_reduced <- complete_intercept_value_reduced %>%
  bind_rows(incomplete_value_reduced)

# Now summarize with respect to these outcomes
all_values_summarized <- all_value_reduced %>%
  group_by(defender_id) %>%
  summarize(defender_name = first(defender_name),
            n_target = n(),
            completion_percentage_allowed = sum(as.numeric(pass_result == "C")) / n_target,
            expected_completion_percentage_allowed = mean(pred_c_no_def),
            cpoe = completion_percentage_allowed - expected_completion_percentage_allowed,
            total_play_value_observed = sum(observed_epa),
            total_play_value_est = sum(play_value_est),
            total_play_value_over_est = sum(play_value_over_est),
            total_yac_value_over_est = sum(yac_value_over_est, na.rm = TRUE),
            total_play_value_over_est_2 = sum(play_value_over_est_2, na.rm = TRUE),
            # Do as subtraction instead of sum(complete_val_over_est) due to incomplete passes
            total_complete_value_over_est = total_play_value_over_est_2 - total_yac_value_over_est)

# New and old approach are highly correlated, I think I like the new one better
  # Now that I include interception run backs, differs a fair bit (0.578 corr)
all_values_summarized %>%
  arrange(total_play_value_over_est_2) %>%
  View()

all_values_summarized %>%
  ggplot(aes(x = total_play_value_over_est_2, y = total_play_value_observed)) +
  geom_point() +
  geom_vline(data = all_values_summarized %>%
               summarize(mean_pred = mean(total_play_value_over_est)),
             aes(xintercept = mean_pred), col = "red") +
  geom_hline(data = all_values_summarized %>%
               summarize(mean_obs = mean(total_play_value_observed)),
             aes(yintercept = mean_obs), col = "red") +
  xlab("Predicted EPA Surrendered") + ylab("Observed EPA Surrendered") +
  ggtitle("Observed vs Predicted EPA Surrendered When Nearest Defender") +
  theme_bw()


all_values_summarized %>%
  ggplot(aes(x = total_play_value_over_est)) +
  geom_histogram()

war_benchmarks <- all_values_summarized %>%
  summarize(avg_value_over_est = mean(total_play_value_over_est_2),
            # Need to do this one backwards because negative is good
            tenth_percentile_over_est = quantile(total_play_value_over_est_2, 0.9))

# Now a WAR plot
war_options <- all_values_summarized %>%
  mutate(total_war_per_384 = -1 * total_play_value_over_est_2 / 38.4,
         total_war_per_avg = -1 * (total_play_value_over_est_2 - war_benchmarks$avg_value_over_est) / 38.4,
         total_war_per_tenth = -1 * (total_play_value_over_est_2 - war_benchmarks$tenth_percentile_over_est) / 38.4)

saveRDS(war_options, "Data/war_options.rds")

war_options %>%
  ggplot() +
  geom_density(aes(x = total_war_per_384, col = "per_384")) +
  geom_density(aes(x = total_war_per_avg, col = "per_avg")) +
  geom_density(aes(x = total_war_per_tenth, col = "per_tenth")) +
  theme_bw() +
  ggtitle("Wins Options")


# Setting up some prettier table options
library(gt)
library(paletteer)

# I think since I like the second approach better I will focus on that
gt_table_setup <- war_options %>%
    select(defender_name,
           completion_percentage_allowed, expected_completion_percentage_allowed, cpoe,
           total_complete_value_over_est, total_yac_value_over_est, 
           #total_play_value_over_est_2,
           total_war_per_tenth)

gt_table_setup %>%
  arrange(desc(total_war_per_tenth)) %>%
  slice(1:20) %>%
  gt() %>%
  cols_label(
    defender_name = "Defender",
    completion_percentage_allowed = "CP",
    expected_completion_percentage_allowed = "xCP",
    cpoe = "CPOE",
    total_complete_value_over_est = "Pass Coverage Over Est",
    total_yac_value_over_est = "Pursuit Over Est",
    #total_play_value_over_est_2 = "Combined Contribution Over Est",
    total_war_per_tenth = "Contribution as WAR"
  ) %>%
  # WAR is in the right direction, value isnt
  data_color(
    columns = vars(total_complete_value_over_est, total_yac_value_over_est),
                   #total_play_value_over_est_2),
    colors = scales::col_numeric(
      palette = rev(c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab")),
      domain = NULL
      )
  ) %>%
  data_color(
    columns = vars(total_war_per_tenth),
    colors = scales::col_numeric(
      palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
      domain = NULL
    )
  ) %>%
  fmt_percent(
    columns = vars(completion_percentage_allowed, expected_completion_percentage_allowed)
  ) %>%
  fmt_number(
    columns = vars(cpoe),
    decimals = 4
  ) %>%
  fmt_number(
    columns = vars(total_complete_value_over_est, total_yac_value_over_est,
                   #total_play_value_over_est_2,
                   total_war_per_tenth),
    decimals = 2
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
        weight = px(2)
      )
    ),
    locations = list(
      cells_body(
        columns = vars(total_complete_value_over_est,
                       total_yac_value_over_est,
                       total_war_per_tenth)
      )
    )
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
