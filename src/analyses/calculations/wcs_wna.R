source("scripts/fire-regime-departure.R")
pkgs <- c("sampling","tidyverse", "terra", "sf", "foreach", "doParallel", "units","transport")
invisible(lapply(pkgs, library, character.only = T))
crop_states <- st_read("data/masks/cleaned/wna_states.shp")
boundaries <- st_read("data/masks/cleaned/priority_landscapes/S_USA.BdyPln_BIL_LandscpInvestment.shp") %>% st_transform(crs = st_crs(crop_states))
boundaries$NAME = gsub(" ", "_", boundaries$NAME)

forest <- rast("data/masks/cleaned/forest_layer_both.tif")
set.seed(1)
wcs <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                               "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                               "data/all_fires",
                                                               boundaries,
                                                               "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                               "data/outputs/WCS",
                                                               "NAME",
                                                               "WCS",
                                                               write_year_raster_out = T,
                                                               forestFilter = forest,
                                                               n.cores = 2,
                                                               n.iter = 100,
                                                               ndvi_threshold = 0.35)

