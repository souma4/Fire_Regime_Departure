pkgs <- c("sf", "tidyverse", "terra","RColorBrewer", "tmap", "units", "ggrepel", "ggpubr","plotly", "reticulate")
invisible(lapply(pkgs, library, character.only = T))
source("scripts/productivity/productivity_source.R")
crop_states <- st_read("data/masks/cleaned/wna_states.shp")
wildernesses_wna<- st_read("data/masks/cleaned/wilderness_cleaned.shp")%>%
  st_intersection(crop_states) %>%
  group_by(DispN)%>%
  summarize(geometry = st_union(geometry))
crop_states <- crop_states %>%
  filter(!CODE %in% c("ND","SD","NE","KS","OK","TX"))
load("data/outputs/wilderness/stored_data.RData")

summary_stats <- production_figures_analysis(stored_data,wildernesses_wna,crop_states,"data/outputs/wilderness","DispN")
