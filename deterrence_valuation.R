library(tidyverse)
library(brms)
## clean this code up and create two steps


### One Method
### 1 - Calculate P(target) w/OFf + Def RE
### 2 - Calculate xCP with average Defender + Use avg cCP as input to P(target) w/Off + AVG Def RE
### 3 - RE Diff = P(target) - P(target_avg_re) 
### 4 - Valuation = RE Diff * Hypothetical EPA

soft_max_norm <- function(prob_vector){
  exp_vec_values = exp(prob_vector)
  norm_values = exp_vec_values/sum(exp_vec_values)
  return(norm_values)
}

frame_df <- readRDS("final_train_deterrence.RDS")
frame_df2 <- frame_df %>% select(play_id,game_id,frame_id) %>% distinct()

final_train_df <- readRDS("final_train_df_lucas.RDS")
hypothetical_df <- readRDS("data/first_pass_ep.rds")
deterrence_model <- readRDS("cp_det_model_lucas.RDS")
  

hyp_epa <- hypothetical_df %>% select(game_id,play_id,
                                      frame_id,nfl_id,
                                      .pred_C,.pred_I,
                                      complete_epa,incomplete_epa) %>% 
  mutate(hyp_epa = .pred_C * complete_epa + .pred_I * incomplete_epa)

combine_epa_df <- final_train_df %>% left_join(frame_df2) %>% inner_join(hyp_epa) %>%
  mutate(
    logit_cp = logit_scaled(cp)
  )

## 
unique_offensive_players <- final_train_df %>% 
  select(closest_offensive_player_f) %>% distinct() %>%
  mutate(
    closest_defensive_player_f = 999999
  )

exp_cp_player_model <- readRDS("exp_cp_model.RDS")

average_defender <- predict(exp_cp_player_model,newdata = unique_offensive_players) %>% as.data.frame()

average_cp_df <- bind_cols(unique_offensive_players,average_defender)

re_pred_input <- combine_epa_df %>% select(logit_cp,closest_defensive_player_f,closest_offensive_player_f)

no_re_pred_input <- combine_epa_df %>% select(closest_offensive_player_f) %>%
  left_join(average_cp_df) %>%
  rename(logit_cp = Estimate)


target_preds_re <- predict(deterrence_model,newdata = re_pred_input,
                           allow_new_levels = T) %>% as.data.frame()
colnames(target_preds_re) <- paste0(colnames(target_preds_re),"_re")

target_preds_no_re <- predict(deterrence_model,newdata = no_re_pred_input,
                              allow_new_levels = T) %>% as.data.frame()
colnames(target_preds_no_re) <- paste0(colnames(target_preds_no_re),"_no_re")

target_preds <- bind_cols(target_preds_no_re,target_preds_re) 


combine_df <- bind_cols(combine_epa_df,target_preds) %>% 
  group_by(play_id,game_id) %>%
  mutate(
    norm_re_estimate = soft_max_norm(Estimate_re),
    norm_no_re_estimate = soft_max_norm(Estimate_no_re),
    re_diff = norm_re_estimate - norm_no_re_estimate)

saveRDS(combine_df,"combine_df.rds")

  
deterence_value <- combine_df %>% group_by(game_id,play_id) %>%
  mutate(
  modified = if_else(target==1,1,-1),
  deterrence_value = modified * hyp_epa * norm_no_re_estimate,
  deterrence_over_expected =  hyp_epa * re_diff
) %>% group_by(defender_id) %>%
  summarise(
    total_plays = n(),
    total_epa_value = sum(deterrence_value,na.rm=T),
    total_epa_value_over_expected = sum(deterrence_over_expected,na.rm=T)
  ) #%>% filter(closest_defensive_player!=999999)

saveRDS(deterence_value,"deterrence_unsummary.RDS")

### Second Method
### 1 - Calculate xCP with average Defender + Use avg cCP as input to P(target) w/Off + AVG Def RE 
### 2 - Option to use P(target) w/Off RE only
### Valuation
### when targeted, be penalized if the player you  are covering is NOT the most valuable target on the field by the magnitude of the difference
### when not targeted, be rewarded if your target is MORE valuable than the selected target

#hypothetical_df <- readRDS("data/first_pass_ep.rds")
#final_train_df <- readRDS("final_train_deterrence.RDS")
#deterrence_model <- readRDS("cp_det_model.RDS")

## Optional method to calculate with no RE
# target_preds_no_re <- predict(deterrence_model,newdata = combine_epa_df,
#                         re_formula = ~ (1 | closest_offensive_player_f),
#                         allow_new_levels = T) %>% as.data.frame()
# colnames(target_preds_no_re) <- paste0(colnames(target_preds_no_re),"_no_re")



deterence_value2 <- combine_df %>% group_by(game_id, play_id) %>%
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
  group_by(defender_id) %>%
  summarize(total_det_value = sum(deterrence_estimate)) %>%
  arrange(total_det_value) 


## Join data with names
players_df <-
  read_csv("data/players.csv") %>% janitor::clean_names() %>% mutate(nfl_id = as.factor(nfl_id))

det_df <- deterence_value2 %>%
  rename( nfl_id=defender_id) %>%
  mutate(nfl_id = as.factor(nfl_id)) %>%
  left_join(players_df) 


saveRDS(det_df,"deterrence_summary_update_v2.RDS")
