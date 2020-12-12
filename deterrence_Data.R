library(tidyverse)

source("deterrence_utils.R")

play <<- read_csv("data/plays.csv") %>% janitor::clean_names()
target <<- read_csv("data/targetedReceiver.csv")  %>% janitor::clean_names()
team_info <<- read_csv("data/games.csv")  %>% janitor::clean_names()

play <- play %>% mutate(
  yd_line_100 = if_else(possession_team==yardline_side,50+yardline_number,yardline_number)
)

## check out 639 plays have NA absolute yard line
play_df <<- play %>% 
  #filter(!str_detect(personnel_o,"[0-9+] P|[0-9+] K")) %>% 
  select(game_id,play_id,possession_team,yardline_number,yardline_side,absolute_yardline_number) %>%
  rename(los_x=absolute_yardline_number) 
  
  
game_deets <<- team_info %>% select(game_id,home_team_abbr,visitor_team_abbr)

# week_by_week_df <- paste0("data/",list.files("data/",pattern = "*week[0-9]+.csv")) 
# week_by_week_df <- paste0("data/",list.files("data/",pattern = "*week[0-9]+.csv")) 
# 
# cp_week_by_week <- paste0("cp_data/",list.files("cp_data/",pattern = "*week[0-9]+.rds")) 


# deterrence_df <- data.frame()
# for(i in seq_along(week_by_week_df)){
#   print(glue::glue({"Processing Week {i} - 
#     {week_by_week_df[i]}
#     {cp_week_by_week[i]}"}))
#   raw_data <- read_csv(week_by_week_df[i])
#   cp_data <- readRDS(cp_week_by_week[i])
#   train_df <- create_train_data(raw_data,cp_data)
#   deterrence_df <- bind_rows(deterrence_df,train_df)
# }
# 
# saveRDS(deterrence_df,"data/final_deterrence.RDS")

# final_deterrence <- readRDS("data/final_deterrence.RDS")
# 
# t = final_deterrence %>% unnest()
# 
# saveRDS(t,"data/final_deterrence_nested.RDS")

t = readRDS("data/final_deterrence_nested.RDS")

create_factor <- function(col){
  cnt <- table(col)
  mid_value = quantile(cnt)[3]
  keep_ids <- names(cnt[cnt > mid_value])
  return(keep_ids)
}


t2 = t %>% select(-new_df) %>% 
  filter(!is.na(closest_offensive_player))

play_basics <- play %>% select(game_id,play_id,down,yards_to_go,yd_line_100,personnel_o) 

parse_string_2 <- function(x, pattern){
  val <- as.numeric(str_extract(x, glue::glue("(\\d)+(?= {pattern})")))
  return(val)
}

train_df <- t2 %>% left_join(play_basics) %>% left_join(target) %>%
  mutate(
    down = as.factor(down),
    distance = yards_to_go,
    closest_offensive_player_f = as.factor(
      if_else(
        closest_offensive_player %in% create_factor(closest_offensive_player),
        closest_offensive_player,
        999999
      )
    ),
    qb_id_f = as.factor(qb_id_f),
    defender_id_f = as.factor(if_else(
      nfl_id %in% create_factor(nfl_id), nfl_id, 999999
    )),
    rb_cnt = parse_string_2(personnel_o,"RB"),
    te_cnt = parse_string_2(personnel_o,"TE"),
    wr_cnt = parse_string_2(personnel_o,"WR"),
    target = as.factor(closest_offensive_player == target_nfl_id)
  ) %>% filter(!is.na(qb_id_f),!is.na(personnel_o))


train_df$total_rec_cnt <- with(train_df,rowSums(cbind(rb_cnt,wr_cnt,te_cnt),na.rm = T))

#library(brms)
library(lme4)

## scale the data. 
## see how this helps


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
