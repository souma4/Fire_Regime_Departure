source("scripts/analysis/fire-regime-departure.R")
pkgs <- c("sampling","tidyverse", "terra", "sf", "foreach", "doParallel", "units", "data.table")
invisible(lapply(pkgs, library, character.only = T))
boundaries <- st_read("data/masks/cleaned/bps_models.gpkg") %>%
  filter(GROUPVEG == "Hardwood" | GROUPVEG == "Conifer" | GROUPVEG == "Hardwood-Conifer"|
           GROUPVEG == "Conifer-Hardwood")
boundaries_sample <- boundaries[sample(1:186,20),]
forest <- rast("data/masks/cleaned/forest_layer_both.tif")
setDTthreads(1)
set.seed(1)

bps_model_analysis <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                           "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                           "data/all_fires",
                                                           boundaries_sample,
                                                           "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                           "data/outputs/bps_model/bps_model_forested",
                                                           "BPS_MODEL",
                                                          "bps_model_forested",
                                                          forestFilter = forest,
                                                          ndvi_threshold = 0.35,
                                                           n.cores = 2,
                                                           n.iter = 100,
                                                          make_figures = F)


#full_analysis