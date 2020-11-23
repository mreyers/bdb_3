# Skill / Value Estimation part 1
  # CPOE Model and relative change in CPOE from pass frame to arrival frame

# This one we have a bunch of infrastructure for
# I will lay out a TO DO list and a COMPLETE list
# Move things appropriately as they are completed

# TODO:
  # 4. Apply CP model to all frames within all plays using purrr::safely()
  # 5. Return dataset appending CP values for each eligible receiver (non-QB) and their corresponding nearest defender
  # 6. Use Outcome - CP to generate CPOE across dataset
  # 7. Estimate value per frame using Hypothetical EPA
  # 8. Assign value per frame based on Hypothetical EPA, the CP * EPA hybrid from thesis
  # 9. Upweight plays corresponding to large +/-WPA moments through scale_factors.R
  


# COMPLETE:
  # 1. Generate stacked ensemble for CP using the observed covariates  
  # 2. Generate framewise covariates for the CP model from my thesis
      # a) Simple covariates
      # b) Framewise influence
      # c) Additional framewise covariates that are not related to influence 
  # 3. Framewise labelling for nearest receiver
  

# # # #
# Main
# # # #
library(futile.logger)
library(readr)
library(dplyr)
library(tidyr)
library(future)
library(furrr)
library(purrr)

source("src/utils/tracking_helpers.R")
# 1. Source in observed model from nfl_tracking
  # Code and model are complete there, next time I run just save as RDS and load here
cp_at_release_model <- readRDS("~/GitHub/nfl_tracking/src/Data_new/release/comp_prob.rds")

# 2. Source in the covariates, either by loading or creating (do by week)
  # Simple covariates (done in simple_covariates.R)
    # 'air_dist', 'rec_separation', 'sideline_sep', 'no_frame_rush_sep', 
    # 'qb_vel', 'time_to_throw', 'dist_from_pocket', 
    # 'air_yards_x',
    # 'yardline_100', 'down', 'ydstogo'


  # Ownership covariates
    # 'n_cells_at_throw',
    # 'own_intensity_at_throw', 
    # 'own_avg_intensity_at_throw',
  # Done in more_eda.R, to be moved to a better named file later
week1_inf <- readRDS("Data/additional_data/week1_influence.rds")
