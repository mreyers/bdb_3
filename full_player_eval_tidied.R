# Note: I think that completions + interceptions should be valued together,
# incompletions valued alone. The completions and interceptions rely on end of
# play results while the incompletions just rely on the ball not being caught

# Setting up some prettier table options
library(gt)
library(paletteer)


# I am going to test this thought by bringing together the work from
# CPOE and YAC to investigate

# Target info to supplement defender_assignment_lucas
target <- read_csv("Data/additional_data/targetedReceiver.csv") %>%
  janitor::clean_names() %>%
  mutate(target = 1)

# Load the required data
# Switch back to all_preds_with_defenders_and_epa for non-similarity based assignment
all_preds_with_defenders_and_epa <- readRDS("Data/all_preds_with_defenders_and_epa_similarity.rds")
# Switch back to first_pass_ep_yac_all_3.rds for original results
first_pass_ep_yac_both <- readRDS("Data/yac/second_pass_ep_yac_all_3.rds")
# Switch back to deterrence_summary (1) for original results
deterrence_epa <- readRDS("Data/deterrence/deterrence_summary_update_v2.RDS") 
defender_assignment <- readRDS("Data/release_and_arrival.rds")

# New Lucas approach to calculating nearest defender, better allocation scheme
defender_assignment_lucas <- readRDS("Data/additional_data/most_similar.rds")

# Coordinate covariates, otherwise many dupes
first_pass_reduced <- first_pass_ep_yac_both %>%
  select(game_id, play_id, yac_epa_est = complete_epa, no_yac_epa_est = no_yac_epa,
         observed_yac_epa_est = observed_yac_epa,
         observed_yac_epa_est_int = observed_yac_epa_int)

# This is already a summary value, skip this one down to the WAR
deterrence_reduced <- deterrence_epa %>%
  # Different columns if back on original file
  select(nfl_id, deterrence_value = total_det_value)

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
         # Unsure if I should use yac_epa_est for int_value with pred_c or no_yac_epa with 1 - pred_c
         yac_value_over_est = (pass_result != "IN") * (observed_yac_epa_est - pred_c_no_def * yac_epa_est),
         # Actually lets replace yac_epa_est with incomplete_epa, stating the assumption that if
         # the ball was not intercepted it would have been at least deflected and incomplete
         # Also use probability of incomplete instead, changing baseline
         int_value_over_est = (pass_result == "IN") * (observed_yac_epa_est_int - (1 - pred_c_no_def) * incomplete_epa),
         complete_value_over_est = (pass_result == "C") * (no_yac_epa_est -  total_complete_epa),
         play_value_over_est_2 = yac_value_over_est + complete_value_over_est + int_value_over_est,
         play_value_over_est_no_int = yac_value_over_est + complete_value_over_est)

# Continue revising above formulas

# On incompletions though the value is 0 unless probability is included and should be
# higher as the probability of pass completion rises
incomplete_value <- incomplete %>%
  mutate(incomplete_value_over_est = (1 - pred_c_no_def) * incomplete_epa - hypothetical_epa,
         play_value_over_est_2 = incomplete_value_over_est,
         play_value_over_est_no_int = play_value_over_est_2)

# Now combine the necessary elements in one standardized summary
# Adding in the 0 columns to allow row binding while also indicating what is not used
# Changed defender_id to similarity_defender_id but kept the naming scheme
complete_intercept_value_reduced <- complete_intercept_value %>%
  select(game_id, play_id, pass_result, defender_id, defender_name,
         pred_c_no_def, complete_epa, incomplete_epa = 0, just_the_yac_epa, observed_epa,
         #play_value_est,
         #play_value_over_est,
         complete_value_over_est, yac_value_over_est, int_value_over_est,
         incomplete_value_over_est = 0,
         play_value_over_est_2,
         play_value_over_est_no_int,
         similarity_defender_id)

incomplete_value_reduced <- incomplete_value %>%
  select(game_id, play_id, pass_result, defender_id, defender_name,
         pred_c_no_def, complete_epa = 0, incomplete_epa, just_the_yac_epa = 0, observed_epa,
         #play_value_est,
         #play_value_over_est,
         complete_value_over_est = 0,
         yac_value_over_est = 0, int_value_over_est = 0,
         incomplete_value_over_est,
         play_value_over_est_2,
         play_value_over_est_no_int,
         similarity_defender_id)

all_value_reduced <- complete_intercept_value_reduced %>%
  bind_rows(incomplete_value_reduced)

# Add in the defender IDs for release and arrival
# Since this is with similarity ranking, defender is the same at release and arrival
all_value_reduced <- all_value_reduced %>%
  left_join(defender_assignment,
            by = c("game_id", "play_id")) %>%
  mutate(release = similarity_defender_id,
         arrival = similarity_defender_id)

# Additionally, add in the defensive team a player is on
plays_map <- read_csv("Data/plays.csv", col_types = cols()) %>%
  janitor::clean_names(case = "snake") %>%
  group_by(game_id) %>%
  arrange(possession_team) %>%
  filter(!is.na(possession_team)) %>%
  # Mutate to get the opposite team
  mutate(def_team = if_else(possession_team == first(possession_team),
                            last(possession_team),
                            first(possession_team))) %>%
  ungroup() %>%
  select(game_id, play_id, def_team)

all_value_reduced <- all_value_reduced %>%
  left_join(plays_map, by = c("game_id", "play_id"))

new_lower_level_summary <- all_value_reduced %>%
  group_by(similarity_defender_id) %>%
  summarize(cpoe = mean(pass_result %in% "C") - mean(pred_c_no_def),
            tot_comp_value = sum(complete_value_over_est, na.rm = TRUE),
            tot_inc_value = sum(incomplete_value_over_est, na.rm =TRUE),
            tot_int_value = sum(int_value_over_est, na.rm = TRUE),
            tot_yac_value = sum(yac_value_over_est, na.rm = TRUE)) %>%
  mutate(contesting = tot_comp_value + tot_inc_value + tot_int_value,
         total_epa = tot_comp_value + tot_inc_value + tot_int_value + tot_yac_value) %>%
  left_join(players %>% select(similarity_defender_id = nfl_id, display_name)) %>%
  arrange(total_epa)

# Add in deterrence here
new_lower_level_with_det <- new_lower_level_summary %>%
  left_join(deterrence_reduced %>%
              mutate(defender_id = as.numeric(as.character((nfl_id)))) %>%
              select(defender_id, total_deterrence_value_over_est = deterrence_value),
            by = c("similarity_defender_id" = "defender_id")) %>%
  mutate(total_epa_with_det = total_epa + total_deterrence_value_over_est)

# Okay these values look nice
# Now convert each to WAR and finalize an initial table
# Make it pretty tomorrow

grades_fn <- function(column){
  complete_value <- column
  complete_z <- (complete_value - mean(complete_value, na.rm=TRUE)) / sd(complete_value, na.rm=TRUE)
  complete_p <- pnorm(complete_z, lower.tail = FALSE) # lower because negative is good
  complete_grades <- (complete_p - min(complete_p, na.rm = TRUE)) /
    (max(complete_p, na.rm = TRUE) - min(complete_p, na.rm = TRUE))
  
  return(complete_grades)
}

contesting_grades <- grades_fn(new_lower_level_with_det$contesting)
yac_grades <- grades_fn(new_lower_level_with_det$tot_yac_value)
det_grades <- grades_fn(new_lower_level_with_det$total_deterrence_value_over_est)

revised_setup <- new_lower_level_with_det %>%
  select(similarity_defender_id, display_name,
         cpoe,
         total_deterrence_value_over_est,
         contesting,
         tot_yac_value,
         total_epa_with_det) %>%
  mutate(#war_det = -1 * total_deterrence_value_over_est / 38.4,
         #war_contest = -1 * contesting / 38.4,
         #war_yac = -1 * tot_yac_value / 38.4,
         #war_total = -1 * total_epa_with_det / 38.4,
         total_contesting_grade = contesting_grades, # complete_grades,
         total_yac_value_grade = yac_grades,
         total_det_value_grade = det_grades)




revised_setup %>%
  select(-similarity_defender_id) %>%
  arrange((total_epa_with_det)) %>%
  slice(1:20) %>%
  gt() %>%
  cols_label(
    display_name = "Defender",
    cpoe = "CPOE",
    total_contesting_grade = "Pass Contesting Grade",
    total_yac_value_grade = "Pursuit Grade",
    total_det_value_grade = "Deterrence Grade",
    #total_play_value_over_est_2 = "Combined Contribution Over Est",
    total_deterrence_value_over_est = "Deterring Contribution",
    contesting = "Contesting Contribution",
    tot_yac_value = "Pursuing Contribution",
    total_epa_with_det = "Total Contribution"
  ) %>%
  # WAR is in the right direction, value isnt
  # Now that it is grades, remove the rev()
  data_color(
    columns = vars(total_contesting_grade, total_yac_value_grade,
                   total_det_value_grade),
    #total_play_value_over_est_2),
    colors = scales::col_numeric(
      palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = vars(total_deterrence_value_over_est,
                   contesting,
                   tot_yac_value,
                   total_epa_with_det),
    colors = scales::col_numeric(
      palette = rev(c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab")),
      domain = NULL
    )
  ) %>%
  fmt_number(
    columns = vars(cpoe),
    decimals = 4
  ) %>%
  fmt_number(
    columns = vars(total_deterrence_value_over_est,
                   contesting,
                   tot_yac_value,
                   total_epa_with_det),
    decimals = 2
  ) %>%
  fmt_number(
    columns = vars(total_contesting_grade, total_yac_value_grade,
                   total_det_value_grade),
    decimals = 0,
    scale_by = 100
  ) %>% 
  cols_merge(
    columns = vars(contesting, total_contesting_grade),
    hide_columns = vars(total_contesting_grade),
    pattern = "{1} ({2})"
  ) %>%
  cols_merge(
    columns = vars(tot_yac_value, total_yac_value_grade),
    hide_columns = vars(total_yac_value_grade),
    pattern = "{1} ({2})"
  ) %>%
  cols_merge(
    columns = vars(total_deterrence_value_over_est, total_det_value_grade),
    hide_columns = vars(total_det_value_grade),
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
        columns = vars(display_name, cpoe)
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
        columns = vars(total_deterrence_value_over_est,
                       contesting,
                       tot_yac_value,
                       total_epa_with_det)
      )
    )
  ) %>%
  cols_move(
    columns = vars(total_deterrence_value_over_est,
                   contesting,
                   tot_yac_value),
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
    subtitle = "Top 20 Players by EPA Above Expected"
  )
