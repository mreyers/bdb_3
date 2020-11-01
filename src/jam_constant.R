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
rec_scores <- data.frame()
for(i in seq_along(week_by_week_df)){
  print(glue::glue({"Processing Week {i}"}))
  raw_data <- read_csv(week_by_week_df[i])
  train_df <- create_train_data(raw_data)
  rec_scores <- bind_rows(rec_scores,train_df)
}

## take that total dataframe and create z-scores
## note: create a residual defender group (anything less than x)





















## Testing to see what top WRs look like 

top_wrs = wr_tracking %>% filter(
  displayName %in% c("T.Y. Hilton", "Kenny Golladay",
                     "Marvin Jones", "Michael Thomas")
)

ggplot(top_wrs,aes(top_speed)) + 
  geom_histogram() +
  facet_grid(displayName ~ route)  +
  theme_bw(base_size=16)


