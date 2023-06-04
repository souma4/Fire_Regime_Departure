source("scripts/analysis/Full_function.R")
pkgs <- c("sampling","tidyverse", "terra", "sf", "foreach", "doParallel", "units","ggrepel")
invisible(lapply(pkgs, library, character.only = T))
boundaries <- st_read("data/masks/cleaned/wilderness_cleaned.shp") %>%
  filter(DispN == "Gila" | DispN == "Cache La Poudre")


gila_cache_demo <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                           "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                           "data/all_fires",
                                           boundaries,
                                           "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                           "Demo/Cache-Gila",
                                           "DispN",
                                           write_year_raster_out = F,
                                           n.iter = 100,
                                           n.cores = 2,
                                           alpha.lines = .1)
#gila_cache_demo
