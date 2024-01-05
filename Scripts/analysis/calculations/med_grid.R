source("scripts/analysis/fire-regime-departure.R")
pkgs <- c("sampling","tidyverse", "terra", "sf", "foreach", "doParallel", "units")
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
#temp <- boundaries[sample(1:nrow(boundaries), 10),]
med_hex_forest_analysis <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                        "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                        "data/all_fires",
                                                        boundaries,
                                                        #temp,
                                                        "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                        "data/outputs/med_grids/med_hex_forested",
                                                        "hex_ID",
                                                        "med_hex_forested",
                                                        write_year_raster_out = T,
                                                        forestFilter = forest,
                                                        ndvi_threshold = 0.35,
                                                        n.cores = 6,
                                                        n.iter = 100,
                                                        make_figures = T)

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

#med_hex
######
########
# Sensitivity REMEMBER TO SET simulation_functions.R for Miller Thode before running!
#####
miller_thode_med_hex <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                                        "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                                        "data/all_fires",
                                                                        boundaries,
                                                                        "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                                        "data/outputs/med_grids/sensitivity/miller_thode",
                                                                        "hex_ID",
                                                                        "miller_thode",
                                                                        forestFilter = forest,
                                                                        n.cores = 6,
                                                                        n.iter = 100,
                                                                        ndvi_threshold = 0.35,
                                                                        make_figures = F)


# repeated runs
runs <- c("one","two","three","four","five","six","seven","eight","nine","ten")
paths <- paste0("data/outputs/med_grids/sensitivity/",runs)
med_hex_run_sensitivity <- vector("list", length(paths))
for (i in seq_along(paths)) {
  med_hex_run_sensitivity[[i]] <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                                      "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                                      "data/all_fires",
                                                                      boundaries,
                                                                      "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                                      paths[i],
                                                                      "hex_ID",
                                                                      paste0("sensitivity_",runs[i]),
                                                                      forestFilter = forest,
                                                                      n.cores = 6,
                                                                      n.iter = 100,
                                                                      ndvi_threshold = 0.35,
                                                                      make_figures = F)
  print(paste0("Finished run ",i))
  gc()
}
saveRDS(med_hex_run_sensitivity, "data/outputs/med_grids/sensitivity/med_hex_run_sensitivity.rds")

#census
med_hex_census <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                  "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                  "data/all_fires",
                                                  boundaries,
                                                  "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                  "data/outputs/med_grids/sensitivity/census",
                                                  "hex_ID",
                                                  "census",
                                                  forestFilter = forest,
                                                  n.cores = 3,
                                                  n.iter = 3,
                                                  p.area = 1,
                                                  ndvi_threshold = 0.35,
                                                  make_figures = F)

#sequential number of iterations
n.iters <- c("10","25","50","100","200")
paths <- paste0("data/outputs/med_grids/sensitivity/",n.iters,"_iterations")
med_hex_n_iters <- vector("list", length(paths))
for (i in seq_along(paths)) {
  med_hex_n_iters[[i]] <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                              "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                              "data/all_fires",
                                                              boundaries,
                                                              "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                              paths[i],
                                                              "hex_ID",
                                                              paste0("n_iters_",n.iters[i]),
                                                              forestFilter = forest,
                                                              n.cores = 6,
                                                              n.iter = as.numeric(n.iters[i]),
                                                              ndvi_threshold = 0.35,
                                                              make_figures = F)
  print(paste0("Finished run ",i))
  gc()
}
saveRDS(med_hex_n_iters, "data/outputs/med_grids/sensitivity/med_hex_n_iters.rds")
#sequential number of sample sizes
p.areas <- c(0.001, 0.01, 0.1,0.5)
paths <- paste0("data/outputs/med_grids/sensitivity/",p.areas,"_p_areas")
med_hex_p_areas <- vector("list", length(paths))
for (i in seq_along(paths)) {
  med_hex_p_areas[[i]] <- Calculate_fire_regime_and_departure("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif",
                                                              "data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv",
                                                              "data/all_fires",
                                                              boundaries,
                                                              "data/landscape_data/mtbs_perims/mtbs_cleaned.shp",
                                                              paths[i],
                                                              "hex_ID",
                                                              paste0("p_areas_",p.areas[i]),
                                                              forestFilter = forest,
                                                              n.cores = 6,
                                                              n.iter = 100,
                                                              p.area = p.areas[i],
                                                              ndvi_threshold = 0.35,
                                                              make_figures = F)
  print(paste0("Finished run ",i))
  gc()
}
saveRDS(med_hex_p_areas, "data/outputs/med_grids/sensitivity/med_hex_p_areas.rds")
