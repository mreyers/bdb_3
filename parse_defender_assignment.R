# Trying to parse the new defender assignment data set Lucas sent

new_closest_defender <- readRDS("Data/additional_data/def_assignment_df_v2.rds")
target <- read_csv("Data/additional_data/targetedReceiver.csv") %>%
  janitor::clean_names() %>%
  mutate(target = 1)

# Condense into two structures
  # One for target receiver with just the most similar defender
most_similar <- new_closest_defender %>%
  left_join(target, by = c("game_id", "play_id", "assigned_rec_id" = "target_nfl_id")) %>%
  filter(target == 1) %>%
  group_by(game_id, play_id) %>%
  arrange(desc(prop)) %>%
  slice(1) %>% 
  ungroup() %>%
  select(game_id, play_id, defender_id, assigned_rec_id)
  
saveRDS(most_similar, "Data/additional_data/most_similar.rds")
  
  # One for target receiver with all similar defenders
all_similar <- new_closest_defender %>%
  left_join(target, by = c("game_id", "play_id", "assigned_rec_id" = "target_nfl_id")) %>%
  filter(target == 1) %>%
  select(game_id, play_id, defender_id, assigned_rec_id, prop)

saveRDS(all_similar, "Data/additional_data/all_similar.rds")
