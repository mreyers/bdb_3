library(brms)
library(lme4)
library(tidyverse)
library(lubridate)

final_train_df <- readRDS("final_train_deterrence.RDS") %>% 
  mutate(closest_defensive_player_f = as.factor(closest_defensive_player_f))

# From lucas assignments
def_assignments <- readRDS("data/def_assignment_df.rds")

target <<-
  read_csv("data/targetedReceiver.csv")  %>% janitor::clean_names()

players_name <- read_csv("data/players.csv") %>% mutate(nflId=as.character(nflId)) 

## calculate rough age for players,to create `is_young` variable
# end_of_18 <- lubridate::mdy(12312018)
# 
# players_name = players_name %>% mutate(nflId=as.character(nflId)) %>%
#   mutate(birth_date = as.Date(birthDate),
#          age = interval(birth_date,end_of_18)/years(1),
#          is_young = age < 22.5)
# 
# young_players <- players_name %>% select(nflId,is_young) %>% mutate(nfl_id = as.factor(nflId))

# final_train_df <- final_train_df %>% left_join(young_players,by=c("closest_defensive_player_f"="nfl_id")) %>% 
#   mutate(
#     logit_cp = logit_scaled(cp)
#   )

create_factor <- function(col) {
  cnt <- table(col)
  mid_value = quantile(cnt)[3]
  keep_ids <- names(cnt[cnt > mid_value])
  return(keep_ids)
}

test = final_train_df %>% select(play_id,game_id,nfl_id,cp) %>% distinct()

final_train_df2 <- test  %>% 
  left_join(def_assignments,by=c("game_id","play_id","nfl_id"="assigned_rec_id")) %>%
  filter(!is.na(nearest_rec_id)) %>%
  left_join(target) %>%
  mutate(
    assigned_rec_id = nfl_id,
    closest_defensive_player_f = ifelse(
      defender_id %in% create_factor(defender_id),
      defender_id,
      999999
    ),
    # set different value if player is wide open
    closest_defensive_player_f = ifelse(
      is.na(defender_id),
      999998,
      closest_defensive_player_f
    ),
    closest_offensive_player_f = as.factor(if_else(
      assigned_rec_id %in% create_factor(assigned_rec_id), assigned_rec_id, 999999
    )),
    logit_cp = logit_scaled(cp),
    target = (assigned_rec_id == target_nfl_id),
    target = if_else(is.na(target),
                   FALSE,
                   target)
  )

saveRDS(final_train_df2,"final_train_df_lucas.RDS")

target_model_cp <-
  brm(
    target ~ logit_cp +
      (1 | closest_defensive_player_f) +
      (1 | closest_offensive_player_f),
    data = final_train_df2,
    family = bernoulli(link = "logit"),
    cores = 4
  )

 saveRDS(target_model_cp,"cp_det_model_lucas.RDS")

ran_eff_df <- ranef(target_model_cp)

wr <-
  ran_eff_df$closest_offensive_player_f %>% as.data.frame %>%
  tibble::rownames_to_column(var ="nflId") %>%
  left_join(players_name)

def <-
  ran_eff_df$closest_defensive_player_f %>% as.data.frame %>%
  tibble::rownames_to_column(var ="nflId") %>%
  left_join(players_name)


## cp model by player REs
## used to calculate average defender xcp

exp_cp_player_model <- brm(
  logit_cp ~ + 
    (1 | closest_defensive_player_f) +
    (1 | closest_offensive_player_f),
  data = final_train_df2,
  family = gaussian(),
  cores = 4
)

ran_eff_df <- ranef(exp_cp_player_model)

wr <-
  ran_eff_df$closest_offensive_player_f %>% as.data.frame %>%
  tibble::rownames_to_column(var ="nflId") %>%
  left_join(players_name)

def <-
  ran_eff_df$closest_defensive_player_f %>% as.data.frame %>%
  tibble::rownames_to_column(var ="nflId") %>%
  left_join(players_name)

saveRDS(exp_cp_player_model,"exp_cp_model.RDS")

### Model Types
## scale the data. 
## see how this helps
# basic_fe <- glm(target ~ down + distance + air_dist,
#                 data = final_train_df,
#                 family = poisson(link="log"))


## unidentifiable model 
# basic_def <- glmer(
#   target ~ down + distance + air_dist  + 
#     (1 |closest_defensive_player_f), 
#   #(1 |qb_id_f) + 
#   #(1 | closest_offensive_player_f),
#   data = final_train_df,
#   family = poisson(link = "log")
# )

## convergence and unidentifiable error
# basic_off <- glmer(
#   target ~ down + distance + air_dist  + 
#     #(1 |closest_defensive_player_f), 
#     #(1 |qb_id_f) + 
#     (1 | closest_offensive_player_f),
#   data = final_train_df,
#   family = poisson(link = "log")
# )


# basic <- glmer(
#   target ~ down + distance + air_dist  + 
#     (1 |closest_defensive_player_f) + 
#     #(1 |qb_id_f) + 
#     (1 | closest_offensive_player_f),
#   data = final_train_df,
#   family = poisson(link = "log")
# )
# 
# ## random effects 
# summary(basic)

# ran_eff_df <- ranef(basic_brms)

# wr <-
#   ran_eff_df$closest_offensive_player_f %>% as.data.frame %>%
#   tibble::rownames_to_column(var ="nflId") %>% 
#   left_join(players_name) 
# 
# def <-
#   ran_eff_df$closest_defensive_player_f %>% as.data.frame %>%
#   tibble::rownames_to_column(var ="nflId") %>% 
#   left_join(players_name) 
# 
# basic <- glmer(
#   target ~ down + distance + yd_line_100 + (1 | defender_id_f) ,
#   data = train_df,
#   family = binomial(link="logit"),
#   control=glmerControl(optimizer="bobyqa",
#                        optCtrl=list(maxfun=2e5)
#   ))


## Try a regular regression 

# test <- glm(
#   target ~ down + distance + yd_line_100,
#   data = train_df,
#   family = binomial(link="logit")
# )

# 
# basic_brms <- brm(target ~ down + distance + air_dist + 
#                     (1 |closest_defensive_player_f) +
#                     (1 | closest_offensive_player_f),
#                   data = final_train_df,
#                   family = poisson(link="log"))
# 
# 
# 
# 
# advantage_brms <-  brm(target ~ down + distance + air_dist + 
#                          distance_to_goal + distance_to_sticks + 
#                          (1 |closest_defensive_player_f) +
#                          (1 | closest_offensive_player_f),
#                        data = final_train_df,
#                        family = poisson(link="log"))
# 
# saveRDS(advantage_brms,"adv_brms_det_mod.RDS")
# 
# different_model <- brm(
#   target ~ down + distance + air_dist +
#     distance_to_goal + distance_to_sticks +
#     rec_separation + sideline_sep +
#     (1 | closest_defensive_player_f) +
#     (1 | closest_offensive_player_f),
#   data = final_train_df,
#   family = bernoulli(link = "logit"),
#   cores = 4
# )
# 
# saveRDS(different_model,"general_model.RDS")
# 
# binomial_model <- 
#   brm(
#     target ~ air_dist + distance_to_sticks + rec_separation + sideline_sep +
#       (1 | closest_defensive_player_f) +
#       (1 | closest_offensive_player_f),
#     data = final_train_df,
#     family = bernoulli(link = "logit"),
#     cores = 4
#   )
# 
# saveRDS(binomial_model,"bin_model_det_1.RDS")
# 
# 
# final_binomial_model <-
#   brm(
#     target ~ air_dist + distance_to_sticks + rec_separation + sideline_sep +
#       (1 | closest_defensive_player_f) +
#       (1 | closest_offensive_player_f),
#     data = final_train_df,
#     family = bernoulli(link = "logit"),
#     cores = 4
#   )
# 
# saveRDS(final_binomial_model,"final_det_model.RDS")


# week 1 plays with coverage, look to see if preds change with coverage
# 
# cov_week1 <- read_csv("data/coverages_week1.csv") %>% janitor::clean_names()
# week_1_df <- final_train_df %>% inner_join(cov_week1)
# 
# week_1_preds <- predict(final_model_deterrencev2,newdata = week_1_df,allow_new_levels=T) 
# week_1_preds <- week_1_preds  %>% as.data.frame()
# 
# week_1 <- bind_cols(week_1_df,week_1_preds)
# 
# ## plot Target vs Coverage type preds histogram 
# 
# ggplot(data=week_1) + 
#   geom_histogram(aes(Estimate)) + 
#   facet_grid(target ~ coverage) + 
#   theme_bw(base_size = 16) + 
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
# 
#   
