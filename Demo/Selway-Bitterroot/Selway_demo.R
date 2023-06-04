source("scripts/analysis/Full_function.R")
pkgs <- c("sampling","tidyverse", "terra", "sf", "foreach", "doParallel", "units","ggpattern","transport")
invisible(lapply(pkgs, library, character.only = T))

sbw <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                    "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                    "data/all_fires",
                                    "data/masks/cleaned/Selway_Bitterroot.shp",
                                    "Demo/selway_fires_raw/sbw.fires.shp",
                                    "Demo/Selway-Bitterroot",
                                    "disp_name",
                                    write_year_raster_out = F,
                                    n.iter = 100)
#sbw
