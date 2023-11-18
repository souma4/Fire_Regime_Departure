source("scripts/analysis/fire-regime-departure.R")
pkgs <- c("sampling","tidyverse", "terra", "sf", "foreach", "doParallel", "units","ggpattern","transport")
invisible(lapply(pkgs, library, character.only = T))
boundaries <- st_read("data/masks/cleaned/wilderness_cleaned.shp")
crop_states <- st_read("data/masks/cleaned/wna_states.shp")
boundaries_wna <- st_intersection(boundaries,crop_states)
#makes "/" become "-"
boundaries_wna$DispN <- gsub("/", "-", boundaries_wna$DispN)
boundaries_wna <- boundaries_wna %>%
  mutate(DispN = ifelse((STATE == "AZ")& (DispN == "Hells Canyon"),"Hells Canyon-AZ",DispN))

set.seed(1)
all_wilderness_analysis <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                       "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                       "data/all_fires",
                                                       boundaries_wna,
                                                       "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                       "data/outputs/wilderness/wilderness_all",
                                                       "DispN",
                                                       write_year_raster_out = T,
                                                       n.cores = 2,
                                                       n.iter = 100)

forest <- rast("data/masks/cleaned/forest_layer_both.tif")
set.seed(1)
forest_wilderness_analysis <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                               "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                               "data/all_fires",
                                                               boundaries_wna,
                                                               "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                               "data/outputs/wilderness/wilderness_forested",
                                                               "DispN",
                                                               "wilderness_forested",
                                                               write_year_raster_out = T,
                                                               forestFilter = forest,
                                                               n.cores = 3,
                                                               n.iter = 100,
                                                               ndvi_threshold = 0.35)

#wilderness_analysis
# Sensitivity REMEMBER TO SET simulation_functions.R for Miller Thode before running!
miller_thode_wilderness_analysis <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                                  "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                                  "data/all_fires",
                                                                  boundaries_wna,
                                                                  "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                                  "data/outputs/wilderness/wilderness_sensitivity/Miller_Thode",
                                                                  "DispN",
                                                                  write_year_raster_out = T,
                                                                  forestFilter = forest,
                                                                  n.cores = 2,
                                                                  n.iter = 100,
                                                                  ndvi_threshold = 0.35)


# repeated runs
wilderness_analysis_1 <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                                        "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                                        "data/all_fires",
                                                                        boundaries_wna,
                                                                        "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                                        "data/outputs/wilderness/wilderness_sensitivity/one",
                                                                        "DispN",
                                                                        forestFilter = forest,
                                                                        n.cores = 2,
                                                                        n.iter = 100,
                                                                        ndvi_threshold = 0.35)
wilderness_analysis_2 <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                             "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                             "data/all_fires",
                                                             boundaries_wna,
                                                             "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                             "data/outputs/wilderness/wilderness_sensitivity/two",
                                                             "DispN",
                                                             forestFilter = forest,
                                                             n.cores = 2,
                                                             n.iter = 100,
                                                             ndvi_threshold = 0.35)
wilderness_analysis_3 <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                             "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                             "data/all_fires",
                                                             boundaries_wna,
                                                             "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                             "data/outputs/wilderness/wilderness_sensitivity/three",
                                                             "DispN",
                                                             forestFilter = forest,
                                                             n.cores = 2,
                                                             n.iter = 100,
                                                             ndvi_threshold = 0.35)
wilderness_analysis_4 <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                             "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                             "data/all_fires",
                                                             boundaries_wna,
                                                             "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                             "data/outputs/wilderness/wilderness_sensitivity/four",
                                                             "DispN",
                                                             forestFilter = forest,
                                                             n.cores = 2,
                                                             n.iter = 100,
                                                             ndvi_threshold = 0.35)
wilderness_analysis_5 <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                             "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                             "data/all_fires",
                                                             boundaries_wna,
                                                             "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                             "data/outputs/wilderness/wilderness_sensitivity/five",
                                                             "DispN",
                                                             forestFilter = forest,
                                                             n.cores = 2,
                                                             n.iter = 100,
                                                             ndvi_threshold = 0.35)
wilderness_analysis_6 <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                             "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                             "data/all_fires",
                                                             boundaries_wna,
                                                             "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                             "data/outputs/wilderness/wilderness_sensitivity/six",
                                                             "DispN",
                                                             forestFilter = forest,
                                                             n.cores = 2,
                                                             n.iter = 100,
                                                             ndvi_threshold = 0.35)
wilderness_analysis_7 <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                             "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                             "data/all_fires",
                                                             boundaries_wna,
                                                             "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                             "data/outputs/wilderness/wilderness_sensitivity/seven",
                                                             "DispN",
                                                             forestFilter = forest,
                                                             n.cores = 2,
                                                             n.iter = 100,
                                                             ndvi_threshold = 0.35)
wilderness_analysis_8 <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                             "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                             "data/all_fires",
                                                             boundaries_wna,
                                                             "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                             "data/outputs/wilderness/wilderness_sensitivity/eight",
                                                             "DispN",
                                                             forestFilter = forest,
                                                             n.cores = 2,
                                                             n.iter = 100,
                                                             ndvi_threshold = 0.35)
wilderness_analysis_9 <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                             "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                             "data/all_fires",
                                                             boundaries_wna,
                                                             "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                             "data/outputs/wilderness/wilderness_sensitivity/nine",
                                                             "DispN",
                                                             forestFilter = forest,
                                                             n.cores = 2,
                                                             n.iter = 100,
                                                             ndvi_threshold = 0.35)
wilderness_analysis_10 <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                             "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                             "data/all_fires",
                                                             boundaries_wna,
                                                             "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                             "data/outputs/wilderness/wilderness_sensitivity/ten",
                                                             "DispN",
                                                             forestFilter = forest,
                                                             n.cores = 2,
                                                             n.iter = 100,
                                                             ndvi_threshold = 0.35)


