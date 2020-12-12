# Skill / Value Estimation part 2
# Deterrence model as measured by hypothetical CP throughout the frames of the play

# Deterrence we have a lot less information for but
# fortunately will be able to pull heavily from influence.

# TODO:
# 1. Formalize concept of deterrence and how it relates to defence
# a) Explore CP as a function to minimize
# b) Explore Hypothetical EPA as a function to minimize
# 2. Pull in the CP model-enhanced data set
# 3. Pull in a new CP model in which no influence data is used
# 4. Compare framewise completion probabilities for the influence based output and the influence indep output
# 5. Assess team / scheme level capabilities 


# Complete:

# use cp from nflfastR as baselin eto develop this model 


nfl_df <- readRDS(url("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2018.rds"))

colnames(nfl_df)

sub_nfl_cp <- nfl_df %>% select(old_game_id,play_id,desc,cp) %>% 
  mutate(old_game_id = as.double(old_game_id))


plays_df <- read_csv("data/plays.csv")


test = plays_df %>% left_join(sub_nfl_cp,
                              by=c("gameId"="old_game_id",
                                   "playId"="play_id"))


## create a pseudo-CP model 
## nearest defender distance, air yards, time to throw 


