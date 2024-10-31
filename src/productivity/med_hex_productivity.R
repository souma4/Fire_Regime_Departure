pkgs <- c("sf", "tidyverse", "terra","RColorBrewer", "tmap", "units", "ggrepel", "ggpubr","plotly", "reticulate")
invisible(lapply(pkgs, library, character.only = T))
source("scripts/productivity/productivity_source.R")
crop_states <- st_read("data/masks/cleaned/wna_states.shp")
med_hex<- st_read("data/masks/cleaned/med_bps_hex.shp")%>%
  group_by(hex_ID)%>%
  summarize(geometry = st_union(geometry))
crop_states <- crop_states %>%
  filter(!CODE %in% c("ND","SD","NE","KS","OK","TX"))
load( "data/outputs/med_hex/!stored_data1.RData")
stored_data1 <- stored_data
load( "data/outputs/med_hex/!stored_data2.RData")
stored_data2 <- stored_data
stored_data <- append(stored_data1, stored_data2)
summary_stats <- production_figures_analysis(stored_data,med_hex,crop_states,"data/outputs/med_hex","hex_ID")
