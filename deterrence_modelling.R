library(brms)
library(lme4)

## scale the data. 
## see how this helps
basic_fe <- glm(target ~ down + distance + air_dist,
                data = final_train_df,
                family = poisson(link="log"))


## unidentifiable model 
basic_def <- glmer(
  target ~ down + distance + air_dist  + 
    (1 |closest_defensive_player_f), 
  #(1 |qb_id_f) + 
  #(1 | closest_offensive_player_f),
  data = final_train_df,
  family = poisson(link = "log")
)

## convergence and unidentifiable error
basic_off <- glmer(
  target ~ down + distance + air_dist  + 
    #(1 |closest_defensive_player_f), 
    #(1 |qb_id_f) + 
    (1 | closest_offensive_player_f),
  data = final_train_df,
  family = poisson(link = "log")
)


basic <- glmer(
  target ~ down + distance + air_dist  + 
    (1 |closest_defensive_player_f) + 
    #(1 |qb_id_f) + 
    (1 | closest_offensive_player_f),
  data = final_train_df,
  family = poisson(link = "log")
)

## random effects 
summary(basic)

ran_eff_df <- ranef(basic_brms)

players_name <- read_csv("data/players.csv") %>% mutate(nflId=as.character(nflId))

wr <-
  ran_eff_df$closest_offensive_player_f %>% as.data.frame %>%
  tibble::rownames_to_column(var ="nflId") %>% 
  left_join(players_name) 

def <-
  ran_eff_df$closest_defensive_player_f %>% as.data.frame %>%
  tibble::rownames_to_column(var ="nflId") %>% 
  left_join(players_name) 

basic <- glmer(
  target ~ down + distance + yd_line_100 + (1 | defender_id_f) ,
  data = train_df,
  family = binomial(link="logit"),
  control=glmerControl(optimizer="bobyqa",
                       optCtrl=list(maxfun=2e5)
  ))


## Try a regular regression 

test <- glm(
  target ~ down + distance + yd_line_100,
  data = train_df,
  family = binomial(link="logit")
)


basic_brms <- brm(target ~ down + distance + air_dist + 
                    (1 |closest_defensive_player_f) +
                    (1 | closest_offensive_player_f),
                  data = final_train_df,
                  family = poisson(link="log"))




advantage_brms <-  brm(target ~ down + distance + air_dist + 
                         distance_to_goal + distance_to_sticks + 
                         (1 |closest_defensive_player_f) +
                         (1 | closest_offensive_player_f),
                       data = final_train_df,
                       family = poisson(link="log"))

saveRDS(advantage_brms,"adv_brms_det_mod.RDS")

different_model <- brm(target ~ down + distance + air_dist +
                       distance_to_goal + distance_to_sticks + 
                       rec_separation + sideline_sep + 
                         (1 |closest_defensive_player_f) +
                         (1 | closest_offensive_player_f),
                       data = final_train_df,
                       family = poisson(link="log"))

final_model <-
  brm(
    target ~ air_dist + distance_to_sticks + rec_separation + sideline_sep +
      (1 | closest_defensive_player_f) +
      (1 | closest_offensive_player_f),
    data = final_train_df,
    family = poisson(link = "log")
  )

ran_eff_df <- ranef(final_model)

players_name <- read_csv("data/players.csv") %>% mutate(nflId=as.character(nflId))

wr <-
  ran_eff_df$closest_offensive_player_f %>% as.data.frame %>%
  tibble::rownames_to_column(var ="nflId") %>% 
  left_join(players_name) 

def <-
  ran_eff_df$closest_defensive_player_f %>% as.data.frame %>%
  tibble::rownames_to_column(var ="nflId") %>% 
  left_join(players_name) 

## review model 
review_model <- function(model){
  print(glue::glue())
}



## stochastic EPA

all_df = readRDS("data/all_predictions.rds")

stoch_df = all_df %>% select(game_id,play_id,frame_id,nfl_id,ep,
                             adj_comp_ep,adj_inc_ep,adj_int_ep,initial_ep,
                             complete_epa,incomplete_epa,interception_epa)   

combine_epa_df <- final_train_df %>% left_join(stoch_df)
