source("scripts/analysis/Full_function.R")
pkgs <- c("sampling","tidyverse", "terra", "sf", "foreach", "doParallel", "units")
invisible(lapply(pkgs, library, character.only = T))
boundaries <- st_read("data/masks/cleaned/bps_models.shp")


bps_model_analysis <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                           "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                           "data/all_fires",
                                                           boundaries,
                                                           "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                           "data/outputs/bps_model/bps_model_all",
                                                           "DispN",
                                                           write_year_raster_out = T,
                                                           n.cores = 3,
                                                           n.iter = 100)
#full_analysis