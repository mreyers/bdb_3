# Lets make some cool summary plots
library(teamcolors)

league_pal("nfl")
nfl_teamcolors <- teamcolors %>%
  filter(league %in% "nfl") %>%
  mutate(name_abbv = c("ARI", "ATL", "BAL",
                       "BUF", "CAR", "CHI",
                       "CIN", "CLE", "DAL",
                       "DEN", "DET", "GB",
                       "HOU", "IND", "JAX",
                       "KC", "LA", "LAC",
                       "MIA", "MIN", "NE",
                       "NO", "NYG", "NYJ",
                       "OAK", "PHI", "PIT",
                       "SF", "SEA", "TB",
                       "TEN", "WAS"))

# Applied below in the player_evaluation script, saved in war_options
  # plays_map <- read_csv("Data/plays.csv", col_types = cols()) %>%
  #   janitor::clean_names(case = "snake") %>%
  #   group_by(game_id) %>%
  #   arrange(possession_team) %>%
  #   filter(!is.na(possession_team)) %>%
  #   # Mutate to get the opposite team
  #   mutate(def_team = if_else(possession_team == first(possession_team),
  #                             last(possession_team),
  #                             first(possession_team))) %>%
  #   ungroup() %>%
  #   select(game_id, play_id, def_team)

war_options <- readRDS("Data/war_options.rds")

phase_0 <- war_options %>%
  filter(!is.na(def_team)) %>%
  group_by(def_team) %>%
  summarize(tot_deterrence_war = sum(total_war_per_tenth_det, na.rm = TRUE),
            tot_complete_war = sum(total_war_per_tenth_complete, na.rm = TRUE),
            tot_int_war = sum(total_war_per_tenth_int, na.rm = TRUE),
            tot_yac_war = sum(total_war_per_tenth_yac, na.rm = TRUE)) %>%
  pivot_longer(cols = contains("war"),
               names_to = "metric",
               values_to = "value") %>%
  left_join(nfl_teamcolors, by = c("def_team" = "name_abbv")) 

phase_1 <- phase_0 %>%
  ggplot(aes(x = value, y = metric, color = name, fill = name)) +
  geom_col() +
  scale_fill_manual(values = team_vec(which = 1)) +
  scale_color_manual(values = team_vec(which = 2)) +
  ylab("") +
  facet_wrap(~ def_team) +
  theme_bw()

