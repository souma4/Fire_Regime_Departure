pkgs <- c("sf", "tidyverse", "terra","RColorBrewer", "tmap", "units")
invisible(lapply(pkgs, library, character.only = T))


states <- st_read("data/masks/raw/na_states_aea.shp")

us_states <- states[45:dim(states)[1],]
valid_codes <- c( "AZ", "CA", "CO",  "ID", "MT", "NM", "NV", "OR", "UT", "WA", "WY")
# valid_states <- us_states$STATE_ABBR %in% valid_codes
states_subset <- us_states %>% filter(CODE %in% valid_codes)
plot(vect(states_subset))
st_write(states_subset, "data/masks/cleaned/wna_states.shp")

mtbs <- st_read("data/landscape_data/mtbs_perims/mtbs_perims_DD.shp")
fire_str <- strsplit(mtbs$Event_ID,'')
fire_year <- foreach(j = 1:length(fire_str), .errorhandling = "pass", .combine = "c") %do%{
  year <- paste(fire_str[[j]][14:17],collapse = "")
  return(year)
}
mtbs$Fire_Year <- as.numeric(fire_year)
st_write(mtbs,"data/landscape_data/mtbs_perims/mtbs_cleaned.shp", append = F)
# wilderness
wildernesses <- st_read("data/masks/raw/wilderness.shp") %>%
  mutate(NAME1 = ifelse((STATE == "AZ")& (NAME1 == "Hells Canyon Wilderness"),"Hells Canyon Wilderness-AZ",NAME1),
         NAME1 = ifelse(str_detect(NAME1,"Bosque"),"Bosque del Apache",NAME1)) %>%
  select(NAME1) %>%
  group_by(NAME1) %>%
  summarize()
wildernesses$DispN <- gsub(" Wilderness","", wildernesses$NAME1)


st_write(wildernesses, "data/masks/cleaned/wilderness_cleaned.shp", append = F)

# create protected areas

NPS <- st_read("data/masks/raw/NPS_-_Land_Resources_Division_Boundary_and_Tract_Data_Service.shp") %>%
  rename( NAME1 = "UNIT_NAME") %>%
  select(all_of(c( "NAME1")) ) %>%
  st_transform(crs = st_crs(wildernesses)) %>%
  filter(str_detect(NAME1, "National Park")) %>%
  group_by(NAME1) %>%
  summarize()
NPS$DispN <- gsub(" National.*","", NPS$NAME1) #%>%
  # gsub(" Memorial.*","", .) %>%
  # gsub(" Mountain Park","", .) %>%
  # gsub(" Ecological and Historic Preserve","", .) %>%
  # gsub(" Scenic and Recreational River","", .)
st_write(NPS, "data/masks/cleaned/NPS_cleaned.shp", append = F)

protected_areas <- bind_rows(wildernesses, NPS) %>%
  group_by(DispN) %>% 
  summarize()
st_write(protected_areas, "data/masks/cleaned/protected_areas.shp", append = F)

#NPS and wilderness mask
protected_areas <- vect("data/masks/cleaned/protected_areas.shp") %>%
  project("EPSG:5070")
wna_states <- vect("data/masks/cleaned/wna_states.shp") %>%
  project(crs(protected_areas))
protected_areas_crop_wna <- crop(protected_areas, states_subset)
protected_areas_crop_wna$protected <- 1

protected_areas_mask <- rast(protected_areas_crop_wna, vals = 0, res = 30) %>%
  terra::rasterize(protected_areas_crop_wna, ., field = "protected", fun = "min", background = 0)
writeRaster(protected_areas_mask, "data/masks/cleaned/protected_areas_mask.tif", overwrite = T)



#create BPS polygon

bps <- rast("data/landscape_data/LF2020_BPS_220_CONUS/tif/LC20_BPS_220.tif")
bps_csv <- read_csv("data/landscape_data/LF2020_BPS_220_CONUS/CSV_data/LF20_BPS_220.csv")
states_wna <- vect("data/masks/cleaned/wna_states.shp")
bps_coltab <- coltab(bps)
bps_wna <- crop(bps, states_wna)
bps_wna_1km <- aggregate(bps_wna, 33, fun = "modal")
rm(bps, bps_wna)

activeCat(bps_wna_1km) <- 1
bps_polygon_code <- as.polygons(bps_wna_1km,dissolve =T)
bps_polygon_code_sv <- aggregate(bps_polygon_code, by = "BPS_CODE") %>%
  as.data.frame(geom = "WKT") %>%
  left_join(bps_csv, by = join_by("BPS_CODE" == "BPS_CODE")) %>%
  select(BPS_CODE, GROUPVEG, FRG_NEW, geometry)%>%
  distinct() %>%
  filter(BPS_CODE != -9999 & BPS_CODE != -1111 & BPS_CODE != 11
         & BPS_CODE != 12  & BPS_CODE != 31) %>%
  vect(., "geometry", crs = crs(bps_polygon_models))
writeVector(bps_polygon_code_sv, "data/masks/cleaned/bps_codes.gpkg", overwrite = T)





bps_polygon_code_sf <- st_as_sf(bps_polygon_code)%>%
  group_by(BPS_CODE)%>%
  summarize(geometry = st_union(geometry))%>%
  drop_na()%>%
  filter(BPS_CODE != -9999 & BPS_CODE != -1111 & BPS_CODE != 11
         & BPS_CODE != 12  & BPS_CODE != 31) %>%
  mutate(DispN = as.character(BPS_CODE), BPS_CODE = NULL) %>%
  right_join(.,bps_csv_group_veg, by = join_by("DispN" == "BPS_MODEL"))
st_write(bps_polygon_code_sf,"data/masks/cleaned/bps_code.shp")


activeCat(bps_wna_1km) <- 2
bps_polygon_zone <- as.polygons(bps_wna_1km,dissolve =T,na.rm = T,trunc = F)
bps_polygon_zone_sv <- aggregate(bps_polygon_zone, by = "ZONE") %>%
  as.data.frame(geom = "WKT") %>%
  left_join(bps_csv, by = join_by("ZONE" == "ZONE")) %>%
  select(ZONE, GROUPVEG, FRG_NEW, geometry)%>%
  distinct() %>%
  vect(., "geometry", crs = crs(bps_polygon_zone))
writeVector(bps_polygon_zone_sv, "data/masks/cleaned/bps_zones.gpkg", overwrite = T)

bps_polygon_zone_sf <- st_as_sf(bps_polygon_zone)%>%
  group_by(ZONE)%>%
  summarize(geometry = st_union(geometry))%>%
  drop_na()%>%
  filter(ZONE != "na") %>%
  mutate(DispN = paste0("z_",as.character(ZONE)), ZONE = NULL)
st_write(bps_polygon_zone_sf,"data/masks/cleaned/bps_zones.shp")


activeCat(bps_wna_1km) <- 3
bps_polygon_models <- as.polygons(bps_wna_1km,dissolve =T)
bps_polygon_models_sv <- aggregate(bps_polygon_models, by = "BPS_MODEL") %>%
  as.data.frame(geom = "WKT") %>%
  left_join(bps_csv, by = join_by("BPS_MODEL" == "BPS_MODEL")) %>%
  select(BPS_MODEL, GROUPVEG, FRG_NEW, geometry)%>%
  distinct() %>%
  filter(BPS_MODEL != "na") %>%
  vect(., "geometry", crs = crs(bps_polygon_models))
writeVector(bps_polygon_models_sv, "data/masks/cleaned/bps_models.gpkg", overwrite = T)


bps_polygon_models_sf <- st_as_sf(bps_polygon_models)%>%
  group_by(BPS_MODEL)%>%
  summarize(geometry = st_union(geometry))%>%
  drop_na()%>%
  filter(BPS_MODEL != "na") %>%
  mutate(DispN = as.character(BPS_MODEL), BPS_MODEL = NULL)
st_write(bps_polygon_models_sf,"data/masks/cleaned/bps_models.shp")


#hexes
##parks et al 2015

##average bps_model hex
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

####average bps_model grid
bps_polygon_models_sv <- vect("data/masks/cleaned/bps_models.shp")
bps_polygon_areas <- expanse(bps_polygon_models_sv, unit = "m", transform = T)
ave_model_area <- set_units(mean(bps_polygon_areas),"m^2")
med_model_area <- set_units(median(bps_polygon_areas), "m^2")
states_wna <- vect("data/masks/cleaned/wna_states.shp")%>%
  project(crs(bps_polygon_models_sv))


states_wna_ext <- ext(states_wna)


resolute <- floor(sqrt(med_model_area))
states_wna_ext[1]<- states_wna_ext[1]-as.numeric(resolute)-(abs(states_wna_ext[2]-states_wna_ext[1]) %%as.numeric(resolute))
states_wna_ext[4]<- states_wna_ext[4]+as.numeric(resolute)-(abs(states_wna_ext[4]-states_wna_ext[3]) %%as.numeric(resolute))


med_hex_grid_r <- rast(states_wna, crs = crs(states_wna), ext = states_wna_ext, res = resolute, vals = T)
med_hex_grid_r_index <- as.data.frame(med_hex_grid_r, xy = T, cells = T) %>%
                        dplyr::select(x,y,cell)%>%
                        rename(index = cell)%>%
                        rast(.,type = "xyz", crs = crs(states_wna), ext = states_wna_ext)
med_hex_grid_poly <- as.polygons(med_hex_grid_r_index)


states_wna_ext <- ext(states_wna)
resolute <- floor(sqrt(ave_model_area))
states_wna_ext[1]<- states_wna_ext[1]-as.numeric(resolute)-(abs(states_wna_ext[2]-states_wna_ext[1]) %%as.numeric(resolute))
states_wna_ext[4]<- states_wna_ext[4]+as.numeric(resolute)-(abs(states_wna_ext[4]-states_wna_ext[3]) %%as.numeric(resolute))

ave_hex_grid_r <- rast(states_wna, crs = crs(states_wna), ext = states_wna_ext, res = resolute, vals = T)
ave_hex_grid_r_index <- as.data.frame(ave_hex_grid_r, xy = T, cells = T) %>%
  dplyr::select(x,y,cell)%>%
  rename(index = cell)%>%
  rast(.,type = "xyz", crs = crs(states_wna), ext = states_wna_ext)
ave_hex_grid_poly <- as.polygons(ave_hex_grid_r_index)



writeVector(ave_hex_grid_poly,"data/masks/cleaned/ave_bps_grid.shp", overwrite=T)
writeVector(med_hex_grid_poly,"data/masks/cleaned/med_bps_grid.shp", overwrite=T)

####firesheds
fireshed <- st_read("data/masks/raw/firesheds/Data/Firesheds_CONUS.gdb", layer = "Firesheds") %>%
  vect() %>%
  project(crs(states_wna))%>%
  crop(states_wna)
st_write(fireshed, "data/masks/cleaned/firesheds.gpkg")
project_areas <- st_read("data/masks/raw/firesheds/Data/Firesheds_CONUS.gdb", layer = "ProjectAreas") %>%
  vect() %>%
  project(crs(states_wna))%>%
  crop(states_wna)


writeVector(fireshed, "data/masks/cleaned/fs_fireshed.gpkg", overwrite = T)
writeVector(project_areas, "data/masks/cleaned/fs_project_areas.gpkg", overwrite = T)

