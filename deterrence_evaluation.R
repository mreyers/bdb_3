# Try to apply valuation to deterrence in different ways
# Some ideas in my slack chat

# deterrence_everything old file, deterrence_unsummary new
deterrence_all <- readRDS("Data/deterrence/deterrence_unsummary.RDS")

deterrence_all %>%
  head(20) %>%
  View()

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

# norm_re_estimate, norm_no_re_estimate
saveRDS(deterrence_sum, "Data/deterrence/deterrence_new_value.rds")
