source("scripts/analysis/fire-regime-departure.R")
pkgs <- c("sampling","tidyverse", "terra", "sf", "foreach", "doParallel", "units","ggpattern")
invisible(lapply(pkgs, library, character.only = T))
boundaries <- st_read("data/masks/cleaned/med_bps_hex.shp")
#boundaries_chunk2 <- boundaries %>%
#  mutate(hex_ID = as.numeric(hex_ID))%>%
  # filter(hex_ID < 1000)%>%
  # mutate(hex_ID = as.character(hex_ID))
forest <- rast("data/masks/cleaned/forest_layer_both.tif")
set.seed(1)
med_hex_analysis <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                        "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                        "data/all_fires",
                                                        boundaries,
                                                        "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                        "data/outputs/med_grids/med_hex_all",
                                                        "hex_ID",
                                                        "med_hex",
                                                        write_year_raster_out = T,
                                                        n.cores = 6,
                                                        n.iter = 100)
set.seed(1)
med_hex_forest_analysis <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                        "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                        "data/all_fires",
                                                        boundaries,
                                                        "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                        "data/outputs/med_grids/med_hex_forested",
                                                        "hex_ID",
                                                        "med_hex_forested",
                                                        write_year_raster_out = T,
                                                        forestFilter = forest,
                                                        ndvi_threshold = 0.35,
                                                        n.cores = 6,
                                                        n.iter = 100)

boundaries <- st_read("data/masks/cleaned/med_bps_grid.shp")
set.seed(1)
med_sq_analysis <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                        "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                        "data/all_fires",
                                                        boundaries,
                                                        "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                        "data/outputs/med_grids/med_sq_all",
                                                        "index",
                                                       "med_sq",
                                                        write_year_raster_out = T,
                                                        n.cores = 6,
                                                        n.iter = 100)
set.seed(1)
med_sq_forest_analysis <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                        "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                        "data/all_fires",
                                                        boundaries,
                                                        "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                        "data/outputs/med_grids/med_sq_forested",
                                                        "index",
                                                        "med_sq_forested",
                                                        write_year_raster_out = T,
                                                        forestFilter = forest,
                                                        ndvi_threshold = 0.35,
                                                        n.cores = 6,
                                                        n.iter = 100)
#med_hex_analysis