library(tidyverse)
library(xgboost)
source("get_receiver_ndef.R")

pbp_2018 = readRDS(url("https://github.com/guga31bb/nflfastR-data/blob/master/data/play_by_play_2018.rds?raw=true"))

yac_id = pbp_2018 %>%
  filter(old_game_id <= "2018123015") %>% 
  filter(
    complete_pass == 1, !is.na(yards_after_catch)
  ) %>% 
  transmute(id = paste0(old_game_id,play_id))

min_yard = -5
max_yard = 25

yac_data = pbp_2018 %>%
  filter(old_game_id <= "2018123015") %>% 
  filter(
    complete_pass == 1, !is.na(yards_after_catch)
  ) %>% 
  dplyr::mutate(
    distance_to_goal = yardline_100 - air_yards,
    distance_to_sticks = air_yards - ydstogo,
    down1 = if_else(down == 1, 1, 0),
    down2 = if_else(down == 2, 1, 0),
    down3 = if_else(down == 3, 1, 0),
    down4 = if_else(down == 4, 1, 0),
    home = if_else(posteam == home_team, 1, 0),
    old_game_id = as.numeric(old_game_id)
    ) %>% 
  mutate(
    yards_after_catch = 
      dplyr::case_when(
                yards_after_catch < min_yard ~ min_yard,
                yards_after_catch > max_yard ~ max_yard,
                TRUE ~ yards_after_catch
                ),
    label = yards_after_catch + 5
  )


yac_model = yac_data %>%
  dplyr::select(
    old_game_id, play_id, week,
    label, yards_after_catch,
    air_yards, yardline_100, ydstogo, distance_to_goal,
    down1, down2, down3, down4,
    half_seconds_remaining,
    qb_hit, home, distance_to_sticks,
    half_seconds_remaining
  ) %>%
  inner_join(
      ndef_data %>% 
        select(gameId, playId,
               def_x_end, def_y_end, def_s, def_a, def_dir, 
               rec_x_end, rec_y_end, rec_s, rec_a, rec_dir,
               dist_to_receiver),
      by = c( "old_game_id" = "gameId" , "play_id"= "playId")
    ) 

##################################
#### regression
##################################

nrounds <- 10
params <-
  list(
    booster = "gbtree",
    objective = "reg:squarederror",
    eval_metric = c("mae"),
    eta = .025,
    gamma = 2,
    subsample = 0.8,
    colsample_bytree = 0.8,
    max_depth = 4,
    min_child_weight = 1
  )


cv_results_regression <- map_dfr(1:17, function(x) {
  test_data <- yac_model %>%
    filter(week == x) %>% 
    select(-week)
  train_data <- yac_model %>%
    filter(week != x) %>%
    select(-week)
  
  full_train <- xgboost::xgb.DMatrix(model.matrix(~ . + 0, data = train_data %>% 
                                                    select(-label,-yards_after_catch,
                                                           -old_game_id, -play_id)), 
                                     label = train_data$yards_after_catch)
  xyac_model <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2)
  
  preds <- as.data.frame(
    matrix(predict(xyac_model, as.matrix(test_data %>% select(-label,-yards_after_catch,
                                                              -old_game_id, -play_id))))
  )
  
  cv_data <- bind_cols(test_data, preds) %>%
    mutate(week = x)
  return(cv_data)
})

### fit full model

full_train <- xgboost::xgb.DMatrix(model.matrix(~ . + 0, data = yac_model %>% 
                                                  select(-week,-label,-yards_after_catch,
                                                         -old_game_id, -play_id)), 
                                   label = yac_model$yards_after_catch)
xyac_model <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2)

# preds <- as.data.frame(
#   matrix(predict(xyac_model, as.matrix(yac_model %>% 
#                                          select(-week,-label,-yards_after_catch,
#                                                 -old_game_id, -play_id))))
#   )

importance <- xgb.importance(model = xyac_model)
head(importance,30)


## receiver xYAC ranking
cv_results_regression %>% 
  rename(pred = V1) %>% 
  left_join(yac_data %>% select(old_game_id,play_id, yards_after_catch)) %>% 
  inner_join(
    ndef_data %>% 
      select(gameId, playId,rec_name, ndef_name,def_position),   
    by = c( "old_game_id" = "gameId" , "play_id"= "playId")
  ) %>% 
  group_by(rec_name) %>% 
  summarise(n=n(),
            # total_observed_2 = sum(label),
            total_observed = sum(yards_after_catch),
            total_xYAC = sum(pred),
            avg_xYAC = total_xYAC/n,
            # YACOE_var = var(yards_after_catch - pred),
            YACOE = sum( yards_after_catch - pred)/n
            ) %>% 
  filter(n>30) %>%
  # arrange(desc(YACOE)) %>% 
  View

# cv_results_regression %>% count(old_game_id,play_id)

cv_results_regression %>% 
  rename(pred = V1) %>% 
  left_join(yac_data %>% select(old_game_id,play_id, yards_after_catch)) %>% 
  inner_join(
    ndef_data %>% 
      select(gameId, playId,rec_name, ndef_name,def_position),   
    by = c( "old_game_id" = "gameId" , "play_id"= "playId")
  ) %>% 
  count(rec_name)


## defender xYAC ranking
cv_results_regression %>% 
  rename(pred = V1) %>% 
  left_join(yac_data %>% select(old_game_id,play_id, yards_after_catch)) %>% 
  inner_join(
    ndef_data %>% 
      select(gameId, playId,rec_name, ndef_name,def_position),   
    by = c( "old_game_id" = "gameId" , "play_id"= "playId")
  ) %>% 
  group_by(ndef_name, def_position) %>% 
  summarise(n=n(),
            # total_observed_2 = sum(label),
            total_observed = sum(yards_after_catch),
            total_xYAC = sum(pred),
            avg_xYAC = total_xYAC/n,
            # YACOE_var = var(yards_after_catch - pred),
            YACOE = sum( yards_after_catch - pred)/n
  ) %>% 
  filter(n>30) %>%
  # arrange(desc(YACOE)) %>% 
  View

# check leave one week out cross validation errors
cv_results_regression %>% 
  rename(pred = V1) %>% 
  group_by(week) %>% 
  summarise(n=n(),
            tatal_error = sum(abs(pred - label)),
            mean_error = tatal_error/n) %>% 
  mutate(mae = sum(tatal_error)/sum(n))

cv_results_regression %>% 
  rename(pred_yac = V1, actual_yac = label) %>% 
  ggplot(aes(x = actual_yac, y = pred_yac)) + 
  geom_point() + 
  geom_smooth() +
  theme_bw() #+
  # facet_wrap(~week)
  


##################################
#### classification
##################################

(num_of_class = (max_yard-min_yard)+1)
yac_data$label %>% table

nrounds <- 50
params <-
  list(
    booster = "gbtree",
    objective = "multi:softprob",
    eval_metric = c("mlogloss"),
    num_class = num_of_class,
    eta = .025,
    gamma = 2,
    subsample = 0.8,
    colsample_bytree = 0.8,
    max_depth = 4,
    min_child_weight = 1
  )


cv_results_classification <- map_dfr(1:17, function(x) {
  test_data <- yac_model %>%
    filter(week == x) %>% 
    select(-week)
  train_data <- yac_model %>%
    filter(week != x) %>%
    select(-week)
  
  full_train <- xgboost::xgb.DMatrix(model.matrix(~ . + 0, data = train_data %>% 
                                                    select(-label,-yards_after_catch,
                                                           -old_game_id, -play_id)), 
                                     label = train_data$label)
  xyac_model <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2)
  
  preds <- as.data.frame(
    matrix(predict(xyac_model, as.matrix(test_data %>% 
                                           select(-label,-yards_after_catch,
                                                  -old_game_id, -play_id))),
           ncol = num_of_class, byrow = TRUE)
  )
  
  cv_data <- bind_cols(test_data, preds) %>%
    mutate(week = x)
  return(cv_data)
})

# convert_to_yards <- function(x){seq(-5, 50, 1)[x]}

yac_results = cv_results_classification %>% select(starts_with("V")) 

cv_results_classification$pred = max.col(yac_results)-6

# receiver xYAC ranking
cv_results_classification %>%
  inner_join(
    ndef_data %>% 
      select(gameId, playId,rec_name, ndef_name,def_position),   
    by = c( "old_game_id" = "gameId" , "play_id"= "playId")
  ) %>% 
  group_by(rec_name) %>% 
  summarise(n=n(),
            total_observed = sum(label),
            total_xYAC = sum(pred),
            avg_xYAC = total_xYAC/n,
            # YACOE_var = var(yards_after_catch - pred),
            YACOE = sum(label - pred)/n
  ) %>% 
  filter(n>30) %>%
  arrange(desc(YACOE)) %>% 
  View


