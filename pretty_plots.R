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
  scale_y_discrete(breaks=c("tot_deterrence_war","tot_complete_war",
                            "tot_int_war", "tot_yac_war"),
                   labels=c("Deterrence", "Prevention", "Interception", "Pursuit")) +
  facet_wrap(~ def_team) +
  ggtitle("Comparison of WAR Generation Across Teams") +
  xlab("Total WAR") +
  theme_bw() +
  theme(legend.position = "none")

phase_1


# Next plot: Ordered total value of defensive contribution
new_setup <- war_options %>%
  filter(!is.na(def_team)) %>%
  group_by(def_team) %>%
  summarize(tot_deterrence_war = sum(total_war_per_tenth_det, na.rm = TRUE),
            tot_complete_war = sum(total_war_per_tenth_complete, na.rm = TRUE),
            tot_int_war = sum(total_war_per_tenth_int, na.rm = TRUE),
            tot_yac_war = sum(total_war_per_tenth_yac, na.rm = TRUE),
            .groups = "keep") %>%
  pivot_longer(cols = contains("war"),
               names_to = "metric",
               values_to = "value") %>%
  summarize(tot_war = sum(value)) %>%
  arrange(desc(tot_war)) %>%
  left_join(nfl_teamcolors, by = c("def_team" = "name_abbv")) 

library(ggimage)

p1 <- new_setup %>%
  mutate(def_team = fct_reorder(def_team, tot_war, .desc = TRUE)) %>%
  ggplot(aes(x = def_team, y = tot_war, color = name, fill = name)) +
  geom_col(width = 0.5, position = "dodge", alpha = 0.65) +
  geom_image(data = new_setup,
             inherit.aes = FALSE,
             aes(x = def_team, y = tot_war, image = logo), size = 0.03, asp = 16/9, nudge_y = 0.25) +
  scale_fill_manual(values = team_vec(which = 1)) +
  scale_color_manual(values = team_vec(which = 2)) +
  ggtitle("Comparison of WAR Generation Across Teams") +
  xlab("Team") +
  ylab("Total WAR") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90))

p1


team_target_control <- war_options %>%
  filter(!is.na(def_team)) %>%
  group_by(def_team) %>%
  summarize(tot_reps = sum(n_target),
            tot_deterrence_war = sum(total_war_per_tenth_det, na.rm = TRUE),
            tot_complete_war = sum(total_war_per_tenth_complete, na.rm = TRUE),
            tot_int_war = sum(total_war_per_tenth_int, na.rm = TRUE),
            tot_yac_war = sum(total_war_per_tenth_yac, na.rm = TRUE),
            .groups = "keep") %>%
  pivot_longer(cols = contains("war"),
               names_to = "metric",
               values_to = "value") %>%
  summarize(tot_war = sum(value) / first(tot_reps) * 550) %>%
  left_join(nfl_teamcolors, by = c("def_team" = "name_abbv")) 

p2 <- team_target_control %>%
  mutate(def_team = fct_reorder(def_team, tot_war, .desc = TRUE)) %>%
  ggplot(aes(x = def_team, y = tot_war, color = name, fill = name)) +
  geom_col(width = 0.5, position = "dodge", alpha = 0.65) +
  geom_image(data = team_target_control,
             inherit.aes = FALSE,
             aes(x = def_team, y = tot_war, image = logo), size = 0.03, asp = 16/9, nudge_y = 0.25) +
  scale_fill_manual(values = team_vec(which = 1)) +
  scale_color_manual(values = team_vec(which = 2)) +
  ggtitle("Comparison of WAR Generation Across Teams Controlled for Pass Attempts") +
  xlab("Team") +
  ylab("Total WAR per 550 Targets") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90))

p2
