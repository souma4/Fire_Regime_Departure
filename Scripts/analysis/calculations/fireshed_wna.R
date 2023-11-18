source("scripts/analysis/Full_function.R")
pkgs <- c("sampling","tidyverse", "terra", "sf", "foreach", "doParallel", "units","ggpattern","transport")
invisible(lapply(pkgs, library, character.only = T))
fireshed <- vect("data/masks/cleaned/fs_fireshed.gpkg")
project_areas <- vect("data/masks/cleaned/fs_project_areas.gpkg")



fireshed_analysis <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                               "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                               "data/all_fires",
                                                               fireshed,
                                                               "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                               "data/outputs/firesheds/firesheds_all",
                                                               "Fireshed_ID",
                                                               write_year_raster_out = T,
                                                               n.cores = 3,
                                                               n.iter = 100)

forest <- rast("data/masks/cleaned/forest_layer_both.tif")

forest_fireshed_analysis <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                                  "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                                  "data/all_fires",
                                                                  fireshed,
                                                                  "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                                  "data/outputs/firesheds/firesheds_forested",
                                                                  "Fireshed_ID",
                                                                  write_year_raster_out = T,
                                                                  remove.nonforest = forest,
                                                                  n.cores = 2,
                                                                  n.iter = 100,
                                                                  ndvi_threshold = 0.35)



PA_analysis <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                         "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                         "data/all_fires",
                                                         fireshed,
                                                         "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                         "data/outputs/firesheds/project_areas_all",
                                                         "PA_ID",
                                                         write_year_raster_out = T,
                                                         n.cores = 3,
                                                         n.iter = 100)


forest_PA_analysis <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                                "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                                "data/all_fires",
                                                                fireshed,
                                                                "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                                "data/outputs/firesheds/project_areas_forested",
                                                                "PA_ID",
                                                                write_year_raster_out = T,
                                                                remove.nonforest = forest,
                                                                n.cores = 2,
                                                                n.iter = 100,
                                                                ndvi_threshold = 0.35)

#wilderness_analysis
