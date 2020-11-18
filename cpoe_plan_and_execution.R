# Skill / Value Estimation part 1
  # CPOE Model and relative change in CPOE from pass frame to arrival frame

# This one we have a bunch of infrastructure for
# I will lay out a TO DO list and a COMPLETE list
# Move things appropriately as they are completed

# TODO:
  # 2. Generate framewise covariates for the CP model from my thesis
    # c) Additional framewise covariates that are not related to influence 
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
  # 3. Framewise labelling for nearest receiver
  
