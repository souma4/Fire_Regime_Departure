source("scripts/analysis/fire-regime-departure.R")
source("scripts/productivity/productivity_source.R")
pkgs <- c("tidyverse", "terra", "sf", "foreach",
          "doParallel", "units", "data.table")
invisible(lapply(pkgs, library, character.only = T))
boundaries <- st_read("data/masks/cleaned/wilderness_cleaned.shp") %>%
  filter(DispN == "Gila" | DispN == "Cache La Poudre" | DispN == "Aldo Leopold")

forest <- rast("data/masks/cleaned/forest_layer_both.tif")
setDTthreads(1)
start <- Sys.time()
set.seed(1)
gila_cache_aldo_demo <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                           "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                           "data/all_fires",
                                           boundaries,
                                           "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                           "Demo/Cache-Gila-Aldo_demo",
                                           "DispN",
                                           vectorOutputName = "acg_demo",
                                           write_year_raster_out = T,
                                           #forestFilter = forest,
                                           n.iter = 100, #I run three just to get var to work
                                           n.cores = 2,
                                           alpha.lines = .1,
                                           p.area = 0.001,
                                           ndvi_threshold = .35,
                                           make_figures = F)
end <- Sys.time()
end - start
#gila_cache_aldo_demo
crop_states <- st_read("data/masks/cleaned/wna_states.shp") %>%
  filter(CODE %in% c("AZ","NM","CO"))
summary_stats <- production_figures_analysis(gila_cache_aldo_demo,crop_states,"Demo/Cache-Gila-Aldo_demo","DispN")
