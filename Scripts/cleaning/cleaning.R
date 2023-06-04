pkgs <- c("sf", "tidyverse", "terra","RColorBrewer", "tmap", "units")
invisible(lapply(pkgs, library, character.only = T))

wildernesses <- st_read("data/masks/raw/wilderness.shp")
wildernesses$DispN <- gsub(" Wilderness","", wildernesses$NAME1)

st_write(wildernesses, "data/masks/cleaned/wilderness_cleaned.shp", append = F)

states <- st_read("data/masks/raw/na_states_aea.shp")

ecoregions <- st_read("data/masks/cleaned/tnc_ecoregions.shp")
ecoregions_cleaned <- ecoregions %>% mutate(r_name = paste0("r",ECO_NUM))
st_write(ecoregions_cleaned, "data/masks/cleaned/ecoregions_cleaned.shp", append = F)

us_states <- states[45:dim(states)[1],]
valid_states <- lengths(st_intersects(us_states,ecoregions)) > 0
states_subset <- us_states[valid_states,]
plot(vect(states_subset))
st_write(states_subset, "data/masks/cleaned/wna_states.shp")

mtbs <- st_read("data/landscape_data/mtbs_perims/mtbs_perims_DD.shp")
fire_str <- strsplit(t$Event_ID,'')
fire_year <- foreach(j = 1:length(fire_str), .errorhandling = "pass", .combine = "c") %do%{
  year <- paste(fire_str[[j]][14:17],collapse = "")
  return(year)
}
mtbs$Fire_Year <- as.numeric(fire_year)
st_write(mtbs,"data/landscape_data/mtbs_perims/mtbs_cleaned.shp", append = F)

#create BPS polygon

bps <- rast("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif")
states_wna <- vect("data/masks/cleaned/wna_states.shp")
bps_coltab <- coltab(bps)
bps_wna <- crop(bps, states_wna)
bps_wna_1km <- aggregate(bps_wna, 33, fun = "modal")
rm(bps, bps_wna)

activeCat(bps_wna_1km) <- 1
bps_polygon_code <- as.polygons(bps_wna_1km,dissolve =T)
bps_polygon_code_sv <- aggregate(bps_polygon_code, by = "BPS_CODE")%>%
  subset(BPS_CODE != -9999 & BPS_CODE != -1111 & BPS_CODE != 11
         & BPS_CODE != 12  & BPS_CODE != 31, NSE = T) 
names(bps_polygon_code_sv)[1] <- "DipsN"

bps_polygon_code_sf <- st_as_sf(bps_polygon_code)%>%
  group_by(BPS_CODE)%>%
  summarize(geometry = st_union(geometry))%>%
  drop_na()%>%
  filter(BPS_CODE != -9999 & BPS_CODE != -1111 & BPS_CODE != 11
         & BPS_CODE != 12  & BPS_CODE != 31) %>%
  mutate(DispN = as.character(BPS_CODE), BPS_CODE = NULL)
st_write(bps_polygon_code_sf,"data/masks/cleaned/bps_code.shp")


activeCat(bps_wna_1km) <- 2
bps_polygon_zone <- as.polygons(bps_wna_1km,dissolve =T,na.rm = T,trunc = F)
bps_polygon_zone_sv <- aggregate(bps_polygon_zone, by = "ZONE")%>%
  subset(ZONE = "na", NSE = T)
names(bps_polygon_zone_sv)[1] <- "DispN"
bps_polygon_zone_sf <- st_as_sf(bps_polygon_zone)%>%
  group_by(ZONE)%>%
  summarize(geometry = st_union(geometry))%>%
  drop_na()%>%
  filter(ZONE != "na") %>%
  mutate(DispN = paste0("z_",as.character(ZONE)), ZONE = NULL)
st_write(bps_polygon_zone_sf,"data/masks/cleaned/bps_zones.shp")


activeCat(bps_wna_1km) <- 3
bps_polygon_models <- as.polygons(bps_wna_1km,dissolve =T)
bps_polygon_models_sv <- aggregate(bps_polygon_models, by = "ZONE")%>%
  subset(BPS_MODEL != "na", NSE = T)
names(bps_polygon_models_sv)[1] <- "DispN"

bps_polygon_models_sf <- st_as_sf(bps_polygon_models)%>%
  group_by(BPS_MODEL)%>%
  summarize(geometry = st_union(geometry))%>%
  drop_na()%>%
  filter(BPS_MODEL != "na") %>%
  mutate(DispN = as.character(BPS_MODEL), BPS_MODEL = NULL)
st_write(bps_polygon_models_sf,"data/masks/cleaned/bps_models.shp")


#hexes
##parks et al 2015

##average bps_model
bps_polygon_models_sv <- vect("data/masks/cleaned/bps_models.shp")
bps_polygon_areas <- expanse(bps_polygon_models_sv, unit = "km", transform = T)
ave_model_area <- set_units(mean(bps_polygon_areas),"km^2")
med_model_area <- set_units(median(bps_polygon_areas), "km^2")
states_wna <- st_read("data/masks/cleaned/wna_states.shp")
valid_types <- c("POLYGON", "MULTIPOLYGON")
ave_hex <- st_make_grid(states_wna, cellsize = ave_model_area, what = "polygons",square = F)%>%
  st_as_sf()%>%
  mutate(hex_ID = as.character(row_number()))%>%
  st_intersection(states_wna)%>%
  dplyr::select(hex_ID) 
ave_hex_types <- st_geometry_type(ave_hex)
ave_hex_valid_index <- which(ave_hex_types %in% valid_types)
ave_hex_valid <- ave_hex[ave_hex_valid_index,]
med_hex <- st_make_grid(states_wna, cellsize = med_model_area, what = "polygons",square = F)%>%
  st_as_sf() %>%
  mutate(hex_ID = as.character(row_number()))%>%
  st_intersection(states_wna)%>%
  dplyr::select(hex_ID) 
med_hex_types <- st_geometry_type(med_hex)
med_hex_valid_index <- which(med_hex_types %in% valid_types)
med_hex_valid <- med_hex[med_hex_valid_index,]

st_write(ave_hex_valid,"data/masks/cleaned/ave_bps_hex.shp", append = F)
st_write(med_hex_valid,"data/masks/cleaned/med_bps_hex.shp", append = F)



####firesheds
fireshed <- st_read("data/masks/raw/firesheds/Data/Firesheds_CONUS.gdb", layer = "Firesheds")%>%
  st_transform(st_crs(states_wna))%>%
  st_intersection(states_wna)
st_write(fireshed, "data/masks/cleaned/firesheds.gpkg")
project_areas <- st_read('data/masks/raw/firesheds/Data/Firesheds_CONUS.gdb', layer = "ProjectAreas")%>%
  st_transform(st_crs(states_wna))%>%
  st_intersection(states_wna)
st_write(project_areas, "data/masks/cleaned/fs_project_areas.gpkg", delete_layer = T)

