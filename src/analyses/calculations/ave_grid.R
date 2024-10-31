source("scripts/analysis/Full_function.R")
pkgs <- c("sampling","tidyverse", "terra", "sf", "foreach", "doParallel", "units","ggpattern")
invisible(lapply(pkgs, library, character.only = T))
boundaries <- st_read("data/masks/cleaned/ave_bps_hex.shp")
#boundaries_chunk2 <- boundaries %>%
#  mutate(hex_ID = as.numeric(hex_ID))%>%
# filter(hex_ID < 1000)%>%
# mutate(hex_ID = as.character(hex_ID))
forest <- rast("data/masks/cleaned/forest_layer_both.tif")

ave_hex_analysis <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                        "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                        "data/all_fires",
                                                        boundaries,
                                                        "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                        "data/outputs/ave_grids/ave_hex_all",
                                                        "hex_ID",
                                                        write_year_raster_out = T,
                                                        n.cores = 3,
                                                        n.iter = 100)

ave_hex_forest_analysis <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                               "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                               "data/all_fires",
                                                               boundaries,
                                                               "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                               "data/outputs/ave_grids/ave_hex_forested",
                                                               "hex_ID",
                                                               write_year_raster_out = T,
                                                               remove.nonforest = forest,
                                                               ndvi_threshold = 0.35,
                                                               n.cores = 3,
                                                               n.iter = 100)

boundaries <- st_read("data/masks/cleaned/ave_bps_grid.shp")

ave_sq_analysis <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                       "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                       "data/all_fires",
                                                       boundaries,
                                                       "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                       "data/outputs/ave_grids/ave_sq_all",
                                                       "index",
                                                       write_year_raster_out = T,
                                                       n.cores = 3,
                                                       n.iter = 100)

ave_sq_forest_analysis <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                              "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                              "data/all_fires",
                                                              boundaries,
                                                              "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                              "data/outputs/ave_grids/ave_sq_forested",
                                                              "index",
                                                              write_year_raster_out = T,
                                                              remove.nonforest = forest,
                                                              ndvi_threshold = 0.35,
                                                              n.cores = 3,
                                                              n.iter = 100)
#ave_hex_analysis