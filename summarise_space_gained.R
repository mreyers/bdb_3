library(tidyverse)

players <- read_csv("players.csv")
SGL <- map_dfr(list.files("output/SGL", full.names = T), read_csv)


SGL %>%
  group_by(nflId) %>%
  mutate(num_plays = n()) %>%
  group_by(nflId, num_plays) %>%
  select(-SOG_mean, -SOL_mean, - team) %>%
  summarise_all(~ sum(., na.rm = T)) %>%
  left_join(players %>% select(nflId, position, displayName)) %>%
  filter(position == "WR") %>% 
  mutate(SOG_diff = SOG_sum + SOL_sum)%>%
  arrange(desc(SOG_diff))


SGG <- map_dfr(list.files("output/SGG", full.names = T), 
               ~ read_csv(., col_types = cols(
                 nflId = col_double(),
                 nflId_ip = col_double(),
                 nflId_j = col_double(),
                 SGG_sum = col_double(),
                 SGG_n = col_double()
               )))

SGG %>%
  group_by(nflId, nflId_ip) %>%
  summarise(SGG_sum = sum(SGG_sum),
            SGG_n = sum(SGG_n)) %>%
  left_join(players %>% 
              select(nflId, name =displayName)) %>%
  left_join(players %>% 
              select(nflId, name =displayName) %>%
              rename_all(~ paste0(., "_ip"))) %>%
  rename_all(~ str_replace(., "_ip", "_receiver")) %>%
  rename(name_generator = name) %>%
  select(-contains("nflId")) %>%
  arrange(desc(SGG_sum))


SGG %>%
  left_join(players %>% 
              select(nflId, pos =position, name =displayName)) %>%
  left_join(players %>% 
              select(nflId, pos =position, name =displayName) %>%
              rename_all(~ paste0(., "_ip"))) %>%
  left_join(players %>% 
              select(nflId, pos = position, name = displayName) %>%
              rename_all(~ paste0(., "_j"))) %>%
  rename_all(~ str_replace(., "_ip", "_receiver")) %>%
  rename_all(~ str_replace(., "_j", "_defender")) %>%
  rename(nflId_generator = nflId, 
         pos_generator = pos,
         name_generator = name)

