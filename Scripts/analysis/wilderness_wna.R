source("scripts/analysis/Full_function.R")
pkgs <- c("sampling","tidyverse", "terra", "sf", "foreach", "doParallel", "units","ggpattern","transport")
invisible(lapply(pkgs, library, character.only = T))
boundaries <- st_read("data/masks/cleaned/wilderness_cleaned.shp")
crop_states <- st_read("data/masks/cleaned/wna_states.shp")
boundaries_wna <- st_intersection(boundaries,crop_states)

wilderness_analysis <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                       "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                       "data/all_fires",
                                                       boundaries_wna,
                                                       "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                       "data/outputs/wilderness",
                                                       "DispN",
                                                       write_year_raster_out = F,
                                                       n.cores = 1,
                                                       n.iter = 100)
#wilderness_analysis
