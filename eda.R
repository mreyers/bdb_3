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

# Meyappan reminded me we need offensive formations, all available in plays
plays %>%
  select(play_type) %>%
  table()

# Now that I have old stuff functional, lets actually play around with the point of this contest
# This means looking at defence and how it interacts with the game
# We know offensive formation and personnel on each play
# We also know what routes the pass catchers are running
# It is easy enough to grab defensive players & position
# We dont really have formations or coverage types on any given play,
  # unless those are hidden within nflfastR
# Dani thinks he will be able to get defensive alignments through some old model runs

# One question I think is particularly interesting is that of the RPO
# How do defences appropriately react to this new age approach?
# We dont have line players and only have the pass versions of the RPOs
# This means we could dig a little into plays where the QB runs an RPO
  # and try to identify changes in defender behaviour
# Realistically this should only impact the Linebackers and Safeties
# Although I guess it would also impact the Corners in a zone defence scheme

# It seems that RPO isnt available in nflfastR or in this data set
# I could create a function to check for RPO though idk the subjective cutoffs
# Perhaps something like RB moves towards the QB for multiple frames after snap
# Since we only have passing plays, most of these will be RPOs?
