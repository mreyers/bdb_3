# Try to apply valuation to deterrence in different ways
# Some ideas in my slack chat

# deterrence_everything old file, deterrence_unsummary new
deterrence_all <- readRDS("Data/deterrence/deterrence_unsummary.RDS")

deterrence_all %>%
  head(20) %>%
  View()

# Assuming the brms model from player_evaluation
library(brms)
basic_brms_adjust <- readRDS("Data/cp_predictions/brms_adjustment_model.rds")
brms_factors <- basic_brms_adjust$data %>%
  select(qb_id_f, target_id_f, defender_id_f)

qb_levels <- levels(brms_factors$qb_id_f)
target_levels <- levels(brms_factors$target_id_f)
def_levels <- levels(brms_factors$defender_id_f)

# Make minor adjustments to get deterrence_all to be compatible
deterrence_brms_preds <- deterrence_all %>%
  mutate(logit_pred_c = logit_cp,
         qb_id_f = factor(as.character(qb_id_f), levels = qb_levels),
         target_id_f = factor(target_nfl_id, levels = target_levels),
         defender_id_f = factor(closest_defensive_player, levels = def_levels)) %>%
  select(logit_pred_c, qb_id_f, target_id_f, defender_id_f)

adjusted_pred_c_all <- predict(basic_brms_adjust, newdata = deterrence_brms_preds,
                               re_formula = ~ (1|qb_id_f) + (1|target_id_f) + (1|defender_id_f))
adjusted_pred_c_no_def <- predict(basic_brms_adjust, newdata = deterrence_brms_preds,
                                  re_formula = ~ (1|qb_id_f) + (1|target_id_f))


deterrence_all_modified <- deterrence_all %>%
  # Probabilities
  mutate(adjusted_comp_prob = adjusted_pred_c_no_def[,1],
         adjusted_comp_prob_defender = adjusted_pred_c_all[,1])%>%
  # Now new hypothetical EPA
  mutate(hypothetical_epa = adjusted_comp_prob * complete_epa + (1 - adjusted_comp_prob) * incomplete_epa,
         hypothetical_epa_with_def = adjusted_comp_prob_defender * complete_epa + (1 - adjusted_comp_prob_defender) * incomplete_epa)

# deterrence_all_modified %>% group_by(closest_defensive_player) %>% 
#   summarize(adj_comp_prob = mean(adjusted_comp_prob, na.rm = TRUE),
#             adj_comp_prob_def = mean(adjusted_comp_prob_defender, na.rm = TRUE)) %>%
#   mutate(gapped = adj_comp_prob_def - adj_comp_prob) %>%
#   arrange(gapped)

# Each player has a hypothetical epa already associated with them
# How to try assigning values
  # (observed_hyp_epa - defended_hyp_epa) * 1(Not Targeted) * P(Not Targeted) +
  # (defended_hyp_epa) * 1(Targeted) * P(Targeted)]
# Actually,
  # (observed_hyp_epa - defended_hyp_epa) * 1(Not Targeted) * P(Targeted) makes more sense
  # as we want to reward for removing the option for tageting
# How to do value? Depends on outcome
  # Not targeted? use (observed_hyp_epa - defended_hyp_epa) * 1(Not Targeted) * P(Targeted)
    # But dont assign value to less valuable options, needs defended_hyp_epa > observed_hyp_epa
  # Targeted? use (max(hyp_epa) - observed_epa) * 1(Targeted) * P(Not Targeted)
deterrence_sum <- deterrence_all %>%
  group_by(game_id, play_id) %>%
  mutate(observed_hyp_epa = max(target * hyp_epa),
         max_hyp_epa = max(hyp_epa),
         defended_hyp_epa = if_else(hyp_epa > observed_hyp_epa,
                                    hyp_epa,
                                    observed_hyp_epa),
         first_term = (observed_hyp_epa - defended_hyp_epa) * (1 - target) * (norm_no_re_estimate),
         second_term = (max_hyp_epa - observed_hyp_epa) * target * (1 - norm_no_re_estimate),
         deterrence_estimate = first_term + second_term) %>%
  group_by(closest_defensive_player) %>%
  summarize(total_det_value = sum(deterrence_estimate)) %>%
  arrange(total_det_value) %>%
  left_join(players %>%
              select(closest_defensive_player = nfl_id, defender_name = display_name))

deterrence_new_summary <- deterrence_all_modified %>%
  group_by(game_id, play_id) %>%
  mutate(observed_hyp_epa = max(target * hypothetical_epa_with_def),
         max_hyp_epa = max(hypothetical_epa_with_def),
         defended_hyp_epa = if_else(hypothetical_epa_with_def > observed_hyp_epa,
                                    hypothetical_epa_with_def,
                                    observed_hyp_epa),
         first_term = (observed_hyp_epa - defended_hyp_epa) * (1 - target) * (norm_no_re_estimate),
         second_term = (max_hyp_epa - observed_hyp_epa) * target * (1 - norm_no_re_estimate),
         deterrence_estimate = first_term + second_term) %>%
  group_by(closest_defensive_player) %>%
  summarize(total_det_value = sum(deterrence_estimate)) %>%
  arrange(total_det_value) %>%
  left_join(players %>%
              select(closest_defensive_player = nfl_id, defender_name = display_name))

deterrence_new_summary %>%
  select(closest_defensive_player, new_det_value = total_det_value) %>%
  left_join(deterrence_sum %>%
              select(closest_defensive_player, old_det_value = total_det_value, defender_name)) %>%
  mutate(gapped = new_det_value - old_det_value) %>%
  arrange(new_det_value) %>%
  View()

# norm_re_estimate, norm_no_re_estimate
saveRDS(deterrence_new_summary %>%
          filter(!is.na(closest_defensive_player)),
        "Data/deterrence/deterrence_new_value_new_probs.rds")
