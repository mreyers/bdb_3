library(tidyverse)
library(brms)

final_train_df <- readRDS("final_train_deterrence.RDS")
hypothetical_df <- readRDS("data/first_pass_ep.rds")
deterrence_model <- readRDS("cp_det_model.RDS")
  

hyp_epa <- hypothetical_df %>% select(game_id,play_id,
                                      frame_id,nfl_id,
                                      .pred_C,.pred_I,
                                      complete_epa,incomplete_epa) %>% 
  mutate(hyp_epa = .pred_C * complete_epa + .pred_I * incomplete_epa)

combine_epa_df <- final_train_df %>% inner_join(hyp_epa) %>%
  mutate(
    logit_cp = logit_scaled(cp)
  )

## ask the gorup why its asking for new levels
## when the level already exists
target_preds_no_re <- predict(deterrence_model,newdata = combine_epa_df,
                        re_formula = ~ (1 | closest_offensive_player_f),
                        allow_new_levels = T) %>% as.data.frame()
colnames(target_preds_no_re) <- paste0(colnames(target_preds_no_re),"_no_re")

target_preds_re <- predict(deterrence_model,newdata = combine_epa_df,
                           allow_new_levels = T) %>% as.data.frame()
colnames(target_preds_re) <- paste0(colnames(target_preds_re),"_re")

target_preds <- bind_cols(target_preds_no_re,target_preds_re) 

soft_max_norm <- function(prob_vector){
 exp_vec_values = exp(prob_vector)
 norm_values = exp_vec_values/sum(exp_vec_values)
 return(norm_values)
}


combine_df <- bind_cols(combine_epa_df,target_preds) %>% 
  group_by(play_id,game_id) %>%
  mutate(
    norm_re_estimate = soft_max_norm(Estimate_re),
    norm_no_re_estimate = soft_max_norm(Estimate_no_re),
    re_diff = norm_re_estimate - norm_no_re_estimate)

## i need to drop the defender random effect
## offensive player, instead of the average player 

deterence_value <- combine_df %>% group_by(game_id, play_id) %>%
  mutate(
    observed_hyp_epa = max(target * hyp_epa),
    max_hyp_epa = max(hyp_epa),
    defended_hyp_epa = if_else(hyp_epa > observed_hyp_epa,
                               hyp_epa,
                               observed_hyp_epa),
    first_term = (observed_hyp_epa - defended_hyp_epa) * (1 - target) * (norm_no_re_estimate),
    second_term = (max_hyp_epa - observed_hyp_epa) * target * (1 - norm_no_re_estimate),
    deterrence_estimate = first_term + second_term
  ) %>%
  group_by(closest_defensive_player) %>%
  summarize(total_det_value = sum(deterrence_estimate)) %>%
  arrange(total_det_value) 
  
  
#   old method to value players
#   mutate(
#   modified = if_else(target==1,1,-1),
#   deterrence_value = modified * hyp_epa * norm_no_re_estimate,
#   deterrence_over_expected =  hyp_epa * re_diff
# ) %>% group_by(closest_defensive_player) %>% 
#   summarise(
#     total_plays = n(),
#     total_epa_value = sum(deterrence_value,na.rm=T),
#     total_epa_value_over_expected = sum(deterrence_over_expected,na.rm=T)
#   ) %>% filter(closest_defensive_player!=999999) 

saveRDS(deterence_value,"deterrence_unsummary.RDS")

## players df 
players_df <-
  read_csv("data/players.csv") %>% janitor::clean_names() %>% mutate(nfl_id = as.factor(nfl_id))

det_df <- deterence_value %>%
  rename( nfl_id=closest_defensive_player) %>%
  mutate(nfl_id = as.factor(nfl_id)) %>%
  left_join(players_df) 


saveRDS(det_df,"deterrence_summary.RDS")
