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

# Changing sample play to be an RPO
sample_play <- week_1_snaked %>%
  filter(game_id == 2018090600,
         play_id == 402)

animate_play(sample_play)
#anim_save("the_rpo.gif")

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
#holder <- sample_play_nest %>%
#  slice(25) 
#frame_pos <- holder %>% select(data) %>% unnest(data)  
#inf_data <- holder %>% select(basic_inf) %>% unnest(basic_inf)

#get_inf_plot(inf_data, frame_pos)

# Now to fix it so I can gif everything
# Something is currently very wrong
sample_play_nest %>%
  select(standard_data) %>%
  magrittr::extract2(1) %>%
  map(make_gif)
#anim_save("inf_rpo.gif")

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
  # That is with reference to RBs moving towards QB because by definition
  # they wont be handoffs

# I think the general classifiers for an RPO are:
  # Snap is taken in shotgun
  # Potential runner lined up adjacent or behind QB
  # Runner on the play accelerates through QB coordinate
# The last one I am a little unsure about
# I also dont know what frame to chop for this
# I dont think there is an event for fake
# Holy shit never mind there is literally an event for play_action
week_1_snaked %>%
  select(event) %>%
  table()

# Looks like there were ~123 play action events in week 1
# and ~26 man in motion events
# Over the course of the season that is a lot, ~2000 PA and ~ 400 MIM
# We can work with that

# Lets gif a PA play
week_1_snaked %>% filter(event %in% "play_action") %>% select(game_id, play_id) %>% slice(1)

# So now I have both Play Action and Man in Motion plays
# Lets just get a quick tab on play results for these first
# To do so, I am going to quickly turn all of the weeks into one big data frame
# and append the necessary columns. Then Im going to RDS it and access
# via dbplyr()

weeks <- 1:17
all_data <- tibble()
for(i in weeks){
  all_data <- all_data %>%
    bind_rows(read_csv(glue::glue("Data/week{i}.csv")) %>%
                mutate(week = i) %>%
                janitor::clean_names(case = "snake") %>%
                rename(velocity = s) %>%
                group_by(play_id, team) %>%
                mutate(n_off = sum(position %in% c("QB", "WR", "TE", "RB"))) %>%
                group_by(play_id) %>%
                arrange(desc(n_off)) %>%
                mutate(poss_team = first(team)) %>%
                ungroup())
}

saveRDS(all_data, "Data/all_data.rds")

con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
copy_to(con, all_data, "all_weeks")
rm(all_data)

all_weeks_db <- tbl(con, "all_weeks")

# Can I just query from this now?
all_weeks_db %>%
  select(game_id) %>%
  filter(game_id == 2018090600)
