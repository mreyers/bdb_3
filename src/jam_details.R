library(tidyverse)
library(ggplot2)
library(ggmap)
library(gganimate)


df <- read_csv("data/week1.csv")
play <- read_csv("data/plays.csv")
team_info <- read_csv("data/games.csv")



nest_df <- df %>%
  group_by(nflId,gameId,playId,displayName) %>%
  tidyr::nest(tracking_data=c(x,y,s,o,a,dis,dir,time,frameId,event))

colnames(play)

game_deets <- team_info %>% select(gameId,homeTeamAbbr,visitorTeamAbbr)
play_deets <- play %>% select(gameId,playId,possessionTeam)


## need to join to get offense only
## pickup jam

## Find only offensive plays
off_tracking = nest_df %>% left_join(game_deets) %>% left_join(play_deets) %>% 
  mutate(poss_team = if_else(possessionTeam==homeTeamAbbr,"home","away")) %>%
  filter(poss_team==team)


filter_from_ball_snap <- function(df){
  ind <- which(df$event=="ball_snap")
  if(is.na(ind)){
    return(df)
  }
  filtered_df <- df %>% filter(frameId>=ind) %>%
    mutate(tot_dist_trav = cumsum(dis))
  return(filtered_df)
  
}


# ## test on one play
# one_play <- off_tracking %>% filter(playId==75)
# one_play2 <- one_play %>% 
#   mutate(
#     tracking_dt = purrr::map(tracking_data,filter_from_ball_snap),
#     max_acc = purrr::map_dbl(tracking_dt,function(dat){max(dat$a)}),
#     quant_acc = purrr::map_dbl(tracking_dt,function(dat){quantile(dat$a)[4]})
#   )

### apply this logic for all of week 1. 

week1_tracks <- off_tracking %>% filter(!is.na(route)) %>% mutate(
  tracking_dt = purrr::map(tracking_data,filter_from_ball_snap),
  max_acc = purrr::map_dbl(tracking_dt,function(dat){max(dat$a)}),
  quant_acc = purrr::map_dbl(tracking_dt,function(dat){quantile(dat$a)[4]})
  #first_five_yards_acc = purrr::map_dbl
)

tate = week1_tracks %>% filter(displayName=="Golden Tate") 

ggplot(tate,aes(max_acc)) +
  geom_histogram() + 
  facet_wrap(~route)

ggplot(tate,aes(quant_acc)) + 
  geom_histogram() + 
  facet_wrap(~route)


player_acc_deets <- week1_tracks %>% group_by(displayName,route) %>% 
  summarize(max_acc_median = median(max_acc),
            quant_acc_median = median(quant_acc))
