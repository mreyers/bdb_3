library(tidyverse)
library(ggplot2)
library(ggmap)
library(gganimate)
library(tidymodels)

source("src/jam_utils.R")



play <- read_csv("data/plays.csv")
team_info <- read_csv("data/games.csv")

## check out 639 plays have NA absolute yard line
play_df <- play %>% 
  select(gameId,playId,possessionTeam,yardlineNumber,yardlineSide,absoluteYardlineNumber) %>%
  rename(los_x=absoluteYardlineNumber)


game_deets <- team_info %>% select(gameId,homeTeamAbbr,visitorTeamAbbr)



## read week by week data
week_by_week_df <- paste0("data/",list.files("data/",pattern = "*week[0-9]+.csv")) 

## get top speed and closest defender for each WR here.
## could think about parallelizing this. So things are done fast 
rec_scores <- data.frame()

for(i in seq_along(week_by_week_df)){
  print(glue::glue({"Processing Week {i}"}))
  raw_data <- read_csv(week_by_week_df[i])
  train_df <- create_train_data(raw_data)
  rec_scores <- bind_rows(rec_scores,train_df)
}

rec_scores <- rec_scores %>% select(-tracking_data,-def_tracking,-five_yards_track)
saveRDS(rec_scores,"rec_jam_raw_data.rds")
## take that total dataframe and create z-scores
## note: create a residual defender group (anything less than x)

rec_scores <- readRDS("rec_jam_raw_data.rds")

rec_z <- create_z_scores(rec_scores)

## simple - model
defense_residuals <- rec_z %>% group_by(defender_id) %>% 
  summarize(cnt = n()) %>% 
  rename(nflId=defender_id) %>% 
  filter(cnt<=25,!is.na(nflId)) %>% 
  pull(nflId)

rec_z2 <- rec_z %>% 
  mutate(
    defender_id = if_else(defender_id %in% defense_residuals,9999999,defender_id) 
  )





library(lme4)

test_model <- lmer(top_speed_z ~ (1|route) + (1|defender_id),data=rec_z2)

randoms <-ranef(test_model)


player_names <- read_csv("data/players.csv")




def_rand_value <- randoms$defender_id %>% tibble::rownames_to_column(var = "nflId") %>% 
  mutate(nflId = as.numeric(nflId)) %>% 
  left_join(player_names) %>% 
  left_join(defense_opps)



## Testing to see what top WRs look like 

top_wrs = rec_z %>% filter(
  displayName %in% c("Julio Jones", "Davante Adams","Antonio Brown",
                     "JuJu Smith-Schuster", "Michael Thomas")
)

ggplot(top_wrs,aes(top_speed)) + 
  geom_histogram() +
  facet_grid(displayName ~ route)  +
  theme_bw(base_size=16)


