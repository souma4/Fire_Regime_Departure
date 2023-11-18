source("scripts/analysis/Full_function.R")
pkgs <- c("sampling","tidyverse", "terra", "sf", "foreach", "doParallel", "units","ggpattern","transport")
invisible(lapply(pkgs, library, character.only = T))

pkgs <- c("sampling","tidyverse", "terra", "sf", "foreach", "doParallel", "units","ggrepel")
invisible(lapply(pkgs, library, character.only = T))
boundaries <- st_read("data/masks/cleaned/wilderness_cleaned.shp") %>%
  filter(DispN == "Selway-Bitterroot" | DispN == "Frank Church-River of No Return" | DispN == "Bob Marshall")


sbw_fcrnr_bm_demo <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                            "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                            "data/all_fires",
                                                            boundaries,
                                                            "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                            "Demo/SBW-FCRNR-BM_demo",
                                                            "DispN",
                                                            write_year_raster_out = T,
                                                            write_year_raster_overwrite = T,
                                                            n.iter = 100, #I run three just to get var to work
                                                            n.cores = 1,
                                                            alpha.lines = .1,
                                                            p.area = 0.01)
#sbw_fcrnr_bm_demo
