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
                ungroup() %>%
                mutate(f_last = paste0(substr(display_name, 1, 1),
                                       ".",
                                       str_extract(display_name, "[A-z\\']+$"))))
}

# Cant use nflscrapR or nflfastR for competition
# Handles Sacks appropriately
plays_parsed <- plays %>%
  mutate(likely_receiver = map2_chr(play_description, pass_result,
                                    ~ quick_str_parse(.x, .y))) %>%
  select(game_id, play_id, likely_receiver) %>%
  mutate(target = TRUE)

all_data_targets <- all_data %>%
  left_join(plays_parsed, by = c("game_id",
                               "play_id",
                               "f_last" = "likely_receiver"))

# Additionally add the nearest defenders
holder <- all_data_targets %>%
  nest(-c(game_id, play_id)) %>%
  mutate(closest_defender = map(data, calc_nearest_d_per_frame))

join_set <- holder %>%
  select(game_id, play_id, closest_defender) %>%
  unnest(closest_defender)

all_data_targets_and_distance <- all_data_targets %>%
  left_join(join_set,
            by = c("game_id", "play_id", "frame_id", "nfl_id"))

saveRDS(all_data_targets_and_distance, "Data/all_data.rds")
rm(plays_parsed)
rm(all_data)

# Set up dbplyr framework for better efficiency
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
copy_to(con, all_data_targets_and_distance, "all_weeks")
#rm(all_data_targets_and_distance)

all_weeks_db <- tbl(con, "all_weeks")

# Can I just query from this now?
# Query all Play Action and man in motion plays
  # Use collect() to execute query into R data type
pa_and_mim_plays <- all_weeks_db %>%
  group_by(game_id, play_id) %>%
  mutate(n_play_action = sum(event %in% c("play_action", "run_pass_option"), na.rm = TRUE),
         n_man_in_motion = sum(event %in% "man_in_motion", na.rm = TRUE)) %>%
  ungroup() %>%
  filter(n_play_action > 0 | n_man_in_motion > 0) %>%
  collect()

pa_and_mim_plays %>%
  ungroup() %>%
  filter(game_id == first(game_id), play_id == first(play_id)) %>%
  arrange(frame_id) %>%
  View()
# If no arrange(), output looks weird as all offence outputs come first
# Make sure to use arrange to not confuse self

# Standard plays i.e. no motion / play action
standard_plays <- all_weeks_db %>%
  group_by(game_id, play_id) %>%
  mutate(n_play_action = sum(event %in% c("play_action", "run_pass_option"), na.rm = TRUE),
         n_man_in_motion = sum(event %in% "man_in_motion", na.rm = TRUE)) %>%
  ungroup() %>%
  filter(n_play_action == 0 & n_man_in_motion == 0) %>%
  collect()

# Lets play around with outcomes quickly, as per the plays dataframe
movement_plays_id <- pa_and_mim_plays %>%
  group_by(game_id, play_id) %>%
  slice(1) %>%
  select(game_id, play_id) %>%
  ungroup() %>%
  left_join(plays, by = c("game_id", "play_id")) %>%
  mutate(play_type = "movement")

standard_plays_id <- standard_plays %>%
  group_by(game_id, play_id) %>%
  slice(1) %>%
  select(game_id, play_id) %>%
  ungroup() %>%
  left_join(plays, by = c("game_id", "play_id")) %>%
  mutate(play_type = "standard")

play_partitions <- movement_plays_id %>%
  bind_rows(standard_plays_id)

# Quick check: Are play action and standard plays more effective on average?
  # Answer: Yes, in every reasonable metric that is easy to calculate.
play_partitions %>%
  group_by(play_type) %>%
  summarize(avg_epa = mean(epa),
            avg_success = mean(epa > 0),
            prop_comp = mean(pass_result %in% "C"))

# More checking: How do the options compare over down and distance?
  # Answer avg_epa: A little sporadic in avg_epa, frequently seems better
  # but sample sizes are limited, within error of standard plays
  # Answer avg_success: Completely blankets standard plays, but again
  # is likely within error
  # Answer prop_comp: Seems more useful, especially 1st and 3rd down

play_partitions %>%
  group_by(play_type, down, yards_to_go) %>%
  summarize(avg_epa = mean(epa),
            avg_success = mean(epa > 0),
            prop_comp = mean(pass_result %in% "C"),
            n = n()) %>%
  filter(yards_to_go <= 15) %>%
  ggplot(aes(x = yards_to_go, y = prop_comp, col = play_type, size = n)) +
  geom_point() +
  geom_line(size = 1) +
  geom_vline(xintercept = 10, col = "red") +
  facet_wrap(~ down)

# With more effort, lets calculate the openness of each receiver with respect
# to their nearest defender on any given play
# Then summarize as separation for targeted receiver and average separation
# on the play in general

# Would be super cool if I could do this on any given frame, make general
sample_play <- all_weeks_db %>%
  filter(game_id == 2018090600,
         play_id == 402) %>%
  collect()

# For each offence player, find nearest defender
# Probably need a super slow for loop or to do some weird nesting
# Weird nesting it is, followed by left joining back to main data set
# This is all done by calc_nearest_d_per_frame and is in thesis_helpers
# Data is already calculated and stored in all_data
# The closest_defender column is NA because it is a relic of unnesting, dont worry

# Now to play around with this info
# I want to know if there are any marked changes before / after play action
# That means I want a plot of avg_separation before / during / after the RPO
# event separated by target / not target
rpo_frame <- pa_and_mim_plays %>%
  filter(event %in% c("play_action", "run_pass_option")) %>%
  group_by(game_id, play_id, frame_id) %>%
  summarize(n_pa = sum(
    event %in% c("play_action", "run_pass_option"), na.rm = TRUE)) %>%
  group_by(game_id, play_id) %>%
  arrange(desc(n_pa)) %>%
  slice(1) %>%
  select(game_id, play_id, rpo_frame = frame_id)

play_start <- c("ball_snap", "snap_direct")
play_pass_start <- c("pass_forward")
play_pass_arrive <- c("pass_arrived", "pass_outcome_caught",
                      "pass_outcome_incomplete", "pass_outcome_interception",
                      "pass_outcome_interception", "pass_outcome_touchdown",
                      "pass_tipped")

# This takes like 30+ minutes, improve efficiency
# Actually it works way faster now? Maybe my computer was bogged down
pa_and_mim_plays_standardized <- pa_and_mim_plays %>%
  left_join(rpo_frame, by = c("game_id", "play_id")) %>%
  mutate(new_frame_id = frame_id - rpo_frame) %>%
  group_by(game_id, play_id, frame_id) %>%
  mutate(frame_play_start = sum(event %in% play_start, na.rm = TRUE),
         frame_pass_start = sum(event %in% play_pass_start, na.rm = TRUE)) %>%
  group_by(game_id, play_id) %>%
  arrange(desc(frame_play_start)) %>%
  mutate(frame_start = first(frame_id)) %>%
  arrange(desc(frame_pass_start)) %>%
  mutate(frame_release = first(frame_id)) %>%
  ungroup() %>%
  filter(between(frame_id, frame_start, frame_release))

# Plot change in separation about time of RPO
  # Lets limit # plays for first go, dont want to crash plot
a_few_plays <- pa_and_mim_plays_standardized %>%
  select(game_id, play_id) %>%
  distinct() %>%
  slice_sample(n = 100)

pa_and_mim_plays_standardized %>%
  inner_join(a_few_plays, by = c("game_id", "play_id")) %>%
  replace_na(list(target = 0)) %>%
  filter(position %in% c("WR", "RB", "TE", "FB")) %>%
  mutate(game_play = paste0(game_id, "_", play_id, "_", nfl_id)) %>%
  ggplot(aes(x = new_frame_id, y = separation, col = target, group = target)) +#, group = game_play)) +
  #geom_point() +
  #geom_line() +
  geom_smooth(method = "loess") + 
  geom_vline(xintercept = 0) #+
  #theme(legend.position = "none")
# Separation obviously decreases for targeted receivers after a certain point due to
# defenders seeing the ball released
# I should have filtered on pass released
# I now have filtered on pass release, still a little wonky
# Though I feel like I understand the relationship between separation and target
# The real comparison I need is post snap openness of non-rpo plays with rpos
# Specifically need a marker to estimate when the RPO would have happened