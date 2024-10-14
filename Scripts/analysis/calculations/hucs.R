source("scripts/fire-regime-departure.R")
pkgs <- c("sampling","tidyverse", "terra", "sf", "foreach", "doParallel", "units")
invisible(lapply(pkgs, library, character.only = T))
western_states <- vect("data/masks/cleaned/wna_states.shp") %>% project("EPSG:3857")
huc8 <- vect("data/masks/cleaned/Hucs.gpkg", layer = "HUC8") %>%
  crop(western_states) %>%
  project("EPSG:5070")
#boundaries_chunk2 <- boundaries %>%
#  mutate(hex_ID = as.numeric(hex_ID))%>%
# filter(hex_ID < 1000)%>%
# mutate(hex_ID = as.character(hex_ID))
forest <- rast("data/masks/cleaned/forest_layer_both.tif")

set.seed(1)
#temp <- boundaries[sample(1:nrow(boundaries), 10),]
huc08_analysis <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                               "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                               "data/all_fires",
                                                               huc8,
                                                               "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                               "data/outputs/HUCs/HUC08",
                                                               "HUC8",
                                                               "HUC08",
                                                               write_year_raster_out = 'memory',
                                                               forestFilter = forest,
                                                               ndvi_threshold = 0.35,
                                                               n.cores = 3,
                                                               n.iter = 100,
                                                               make_figures = T)

####Huc 10
huc10 <- vect("data/masks/cleaned/Hucs.gpkg", layer = "HUC10") %>%
  crop(western_states) %>%
  project("EPSG:5070")

set.seed(1)
#temp <- boundaries[sample(1:nrow(boundaries), 10),]
huc10_analysis <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                      "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                      "data/all_fires",
                                                      huc10,
                                                      "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                      "data/outputs/HUCs/HUC10",
                                                      "HUC10",
                                                      "HUC10",
                                                      write_year_raster_out = 'memory',
                                                      forestFilter = forest,
                                                      ndvi_threshold = 0.35,
                                                      n.cores = 6,
                                                      n.iter = 100,
                                                      make_figures = T)
