source("scripts/analysis/fire-regime-departure.R")
pkgs <- c("sampling","tidyverse", "terra", "sf", "foreach", "doParallel", "units")
invisible(lapply(pkgs, library, character.only = T))
fireshed <- vect("data/masks/cleaned/fs_fireshed.gpkg")
project_areas <- vect("data/masks/cleaned/fs_project_areas.gpkg")




set.seed(1)

fireshed_analysis <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                               "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                               "data/all_fires",
                                                               fireshed,
                                                               "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                               "data/outputs/firesheds/firesheds_all",
                                                               "Fireshed_ID",
                                                               "firesheds",
                                                               write_year_raster_out = 'memory',
                                                               n.cores = 6,
                                                               n.iter = 100,
                                                               make_figures = T)

forest <- rast("data/masks/cleaned/forest_layer_both.tif")
set.seed(1)
forest_fireshed_analysis <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                                "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                                "data/all_fires",
                                                                fireshed,
                                                                "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                                "data/outputs/firesheds/firesheds_forested",
                                                                "Fireshed_ID",
                                                                "firesheds_forested",
                                                                write_year_raster_out = 'memory',
                                                                forestFilter = forest,
                                                                ndvi_threshold = 0.35,
                                                                n.cores = 6,
                                                                n.iter = 100,
                                                                make_figures = T)



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
