source("scripts/fire-regime-departure.R")
pkgs <- c("sampling","tidyverse", "terra", "sf", "foreach", "doParallel", "units","transport")
invisible(lapply(pkgs, library, character.only = T))
crop_states <- st_read("data/masks/cleaned/wna_states.shp")
boundaries <- st_read("data/masks/cleaned/ForestUnits.gpkg") %>% st_transform(crs = st_crs(crop_states))
boundaries_wna <- st_intersection(boundaries,crop_states)
#makes "/" become "-"
boundaries_wna$DispN <- gsub("/", "-", boundaries_wna$NFSLANDU_2)
boundaries_wna$DispN <- gsub(" ", "_", boundaries_wna$DispN)

forest <- rast("data/masks/cleaned/forest_layer_both.tif")
set.seed(1)
forest_wilderness_analysis <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                               "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                               "data/all_fires",
                                                               boundaries_wna,
                                                               "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                               "data/outputs/forest/ForestUnits",
                                                               "DispN",
                                                               "ForestUnits",
                                                               write_year_raster_out = T,
                                                               forestFilter = forest,
                                                               n.cores = 3,
                                                               n.iter = 100,
                                                               ndvi_threshold = 0.35)
