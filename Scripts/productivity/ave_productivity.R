pkgs <- c("sf", "tidyverse", "terra","RColorBrewer", "tmap", "units", "ggrepel", "ggpubr","plotly", "reticulate")
invisible(lapply(pkgs, library, character.only = T))
source("scripts/productivity/productivity_source.R")
crop_states <- st_read("data/masks/cleaned/wna_states.shp")
ave_hex<- st_read("data/masks/cleaned/ave_bps_hex.shp")%>%
  group_by(hex_ID)%>%
  summarize(geometry = st_union(geometry))
crop_states <- crop_states %>%
  filter(!CODE %in% c("ND","SD","NE","KS","OK","TX"))
load( "data/outputs/ave_grids/ave_hex_all/!stored_data.RData")

summary_stats_all <- production_figures_analysis(stored_data,ave_hex,crop_states,"data/outputs/ave_grids/ave_hex_all","hex_ID","ave_hex_all_emd")

load( "data/outputs/ave_grids/ave_hex_forested/!stored_data.RData")

summary_stats_forested <- production_figures_analysis(stored_data,ave_hex,crop_states,"data/outputs/ave_grids/ave_hex_forested","hex_ID","ave_hex_forested_emd")

load( "data/outputs/ave_grids/ave_sq_all/!stored_data.RData")
ave_grid<- st_read("data/masks/cleaned/ave_bps_grid.shp")

summary_stats_all_grid <- production_figures_analysis(stored_data,ave_grid,crop_states,"data/outputs/ave_grids/ave_sq_all","index", "ave_sq_all_emd")

load( "data/outputs/ave_grids/ave_sq_forested/!stored_data.RData")
summary_stats_all_grid <- production_figures_analysis(stored_data,ave_grid,crop_states,"data/outputs/ave_grids/ave_sq_forested","index","ave_sq_forested_emd")
