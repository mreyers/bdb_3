# Defender wins scaling factor
# Follow up on the Slack idea posted earlier
# Goal is to put a scaling factor on the prevented EPA by a nearest defender
# Will require many components but should follow naturally from similar work
# done in other contexts

# I dont know exactly how to navigate the rules on EPA and WPA models but for
# sheer lazyness on a first approach Im just going to grab the results from
# nflfastR and bind them to the data.
# From there I'll work on the tracking related challenges

library(tidyverse)
library(nflfastR)

plays <- read_csv("Data/plays.csv") %>%
  janitor::clean_names()

nflfastr_2018 <- readRDS(
  url(
    glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2018.rds")
  )
)

head(plays)

# Match the plays
plays_joined <- plays %>%
  mutate(game_id = as.character(game_id)) %>%
  select(old_game_id = game_id, play_id) %>%
  left_join(nflfastr_2018, by = c("old_game_id", "play_id"))

# Add in the targeted receiver to the dataset from the bonus set, just to be safe
target <- read_csv("Data/targetedReceiver.csv",
                   col_types = cols(gameId = col_character())) %>%
  janitor::clean_names()

plays_joined_condensed <- plays_joined %>%
  select(game_id = old_game_id, play_id, epa, wpa) %>%
  left_join(target, by = c("game_id", "play_id"))


# Now rip nearest defender code from Kaggle booklet
# Or even better, just use thesis_helper code
# separation() is applied on the play level of tracking data and needs receiver ID

# Read in the tracking data
tracking <- readRDS("Data/all_data.rds") %>%
  janitor::clean_names()

pass_start <- c("pass_forward", "pass_shovel")
pass_end <- c("pass_outcome_incomplete", "pass_outcome_caught",
                "pass_outcome_interception", "pass_outcome_touchdown")

relevant_frames <- tracking %>%
  filter(event %in% c(pass_start, pass_end))

rm(tracking)

# Now to nest by play, join for target receiver, and enjoy
relevant_frames_and_target <- relevant_frames %>%
  mutate(game_id = as.character(game_id)) %>%
  nest(-c(game_id, play_id)) %>%
  left_join(plays_joined_condensed, by = c("game_id", "play_id"))

# Add separation
relevant_frames_separation <- relevant_frames_and_target %>%
  filter(!is.na(target_nfl_id)) %>%
  mutate(defense_values = map2(data, target_nfl_id,
                               ~separation(.x, .y, start_or_end = "end")))

unnested_rel_frames <- relevant_frames_separation %>%
  unnest(defense_values)

# Now that I have the nearest defender, lets do a simple version of this
# I'll quickly estimate receiver skill through small fixed effects regression
off_skill <- unnested_rel_frames %>%
  mutate(target_nfl_id = as.character(target_nfl_id)) 

simple_lm <- lm(epa ~ 0 + target_nfl_id, data = off_skill)
summary(simple_lm)
coef(simple_lm)[1:5]

# Map these coefficients to the relevant receivers
mapping <- data.frame(coef_name = names(coef(simple_lm)),
                      coef_value = coef(simple_lm)) %>%
  mutate(target_nfl_id = str_extract_all(coef_name, "[0-9]+")) %>%
  select(target_nfl_id, coef_value) %>%
  unnest(target_nfl_id)

unnested_and_off <- off_skill %>%
  left_join(mapping, by = c("target_nfl_id"))

grouped_summary <- unnested_and_off %>%
  group_by(nearest_def_to_rec_id) %>%
  summarize(tot_epa = sum(epa, na.rm = TRUE),
            tot_wpa = sum(wpa, na.rm = TRUE),
            avg_rec_skill = mean(coef_value, na.rm = TRUE),
            n_chances = n(),
            .groups = "drop")

# Super basic lm scaling factor
scaling_lm <- lm(tot_wpa ~ tot_epa + tot_epa:(0 + avg_rec_skill),
                 weights = n_chances,
                 data = grouped_summary)

scaling_coefs <- coef(scaling_lm)
intercept <- scaling_coefs[1]
slope <- scaling_coefs[2]

# scale_fn <- (intercept + slope * avg_rec_skill) / intercept
# Alternatively, 1 + slope * avg_rec_skill / intercept
scale_contributions <- grouped_summary %>%
  mutate(scale_factor = 1 + slope * avg_rec_skill / intercept)

scale_contributions %>%
  select(scale_factor) %>%
  summary()

# Basic idea is here, would probably want to refine this idea before moving forward with it
# Short and sweet, if we build any metrics that are based on reduced EPA, we should scale them
# by these factors to appropriately reflect value add
