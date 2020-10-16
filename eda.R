# Its happening
# BDB 3
# Thank you Mike

# Main focus of this year's event: Defence
#  Identifying player, team, or strategic advantages on the
#  defensive side of the ball would be a significant breakthrough for the game.

# This is an EDA file, time to explore
# Lets do so in an efficient manner, i.e.
# dbplyr, tidyverse, and tidymodels

library(tidyverse) # General tidy code
library(tidymodels) # Tidy model structure, great for reproducibility
library(dbplyr) # Data loading efficiently
library(ggmap) # Plot tools, specifically animation
library(gganimate) # Plot animation, makes the play move
library(mvtnorm) # Need for density calculation

# I'll just load in one game for now to play
week_1 <- read_csv("Data/week1.csv")

week_1_snaked <- week_1 %>%
  janitor::clean_names(case = "snake")

View(week_1 %>% slice(1:100))

# Load the additional data files as well, assuming same structure as previously
games <- read_csv("Data/games.csv", col_types = cols()) %>%
  janitor::clean_names(case = "snake")
players <- read_csv("Data/players.csv", col_types = cols()) %>%
  janitor::clean_names(case = "snake") 
plays <- read_csv("Data/plays.csv", col_types = cols()) %>%
  janitor::clean_names(case = "snake")

# Lets see if I can just blindly gif the first play
source("src/utils/tracking_helpers.R")

sample_play <- week_1_snaked %>%
  filter(play_id == first(play_id))

animate_play(sample_play)
#anim_save("the_beginning.gif")

# The answer is a resounding yes
# I imagine much of the additional work will also port well
# Some tweaks need to happen to get influence
  # Will need to check if this framework works for interceptions
sample_play_inf_prep <- sample_play %>%
  rename(velocity = s) %>%
  group_by(play_id, team) %>%
  mutate(n_off = sum(position %in% c("QB", "WR", "TE", "RB"))) %>%
  group_by(play_id) %>%
  arrange(desc(n_off)) %>%
  mutate(poss_team = first(team)) %>%
  ungroup()

# Basic piece works, lets nest(game_id, frame_id) and try to get influence all places
sample_play_nest <- sample_play_inf_prep %>%
  nest(-c(game_id, play_id)) %>%
  mutate(data = pmap(list(data, game_id, play_id), ~quick_nest_fix(..1, ..2, ..3)),
         standard_data = map(data, ~ standardize_play(., reorient = FALSE)),
         basic_inf = map(standard_data, ~get_zone_influence(., lazy = TRUE)))

# I should be able to slice one of these and get the inf plot
holder <- sample_play_nest %>%
  slice(25) 
frame_pos <- holder %>% select(data) %>% unnest(data)  
inf_data <- holder %>% select(basic_inf) %>% unnest(basic_inf)

get_inf_plot(inf_data, frame_pos)

# Now to fix it so I can gif everything
# Something is currently very wrong
sample_play_nest %>%
  select(standard_data) %>%
  magrittr::extract2(1) %>%
  map(make_gif)
#anim_save("inf_beginning.gif")
