#Regions to years
library(terra)
library(sf)
library(tidyverse)
library(foreach)

valid_regions <- c(2,3,4,7,10,11,12,13,15,18,19,21,22,23,24,27,28)
mtbs <- st_read("C:\\Users\\JChandler\\Desktop\\Work\\Masters\\data\\landscape_data\\mtbs_perims\\mtbs_cleaned.shp")
  filter(Region_ID %in% valid_regions)
### region_folders
for(i in 1:dim(ecoregions)[1]){
  id <- ecoregions$Region_ID[i]
  region_folder <- paste0("E:\\severity.1985.2000\\r",id)
  fires <- unlist(list.files(region_folder, pattern = "_CBI_bc.tif"))
  fire_str <- strsplit(fires,'')
  foreach(j = 1:length(fire_str), .errorhandling = "pass", .combine = "c") %do%{
    year <- paste(fire_str[[j]][14:17],collapse = "")
    fire_name <- fires[j]
    fire <- rast(paste0(region_folder,"/",fire_name))
    out_folder <- paste0("data/all_fires/",year,"/")
    writeRaster(fire,paste0(out_folder,fire_name),overwrite=T)
    return(j)
  }
  
}

  ####additional folders
    region_folder <- "E:\\severity.1985.2000\\additional_fires"
    fires <- unlist(list.files(region_folder, pattern = "_CBI_bc.tif"))
    fire_str <- strsplit(fires,'')
    foreach(j = 1:length(fire_str), .errorhandling = "pass", .combine = "c") %do%{
      year <- paste(fire_str[[j]][14:17],collapse = "")
      fire_name <- fires[j]
      fire <- rast(paste0(region_folder,"/",fire_name))
      out_folder <- paste0("data/all_fires/",year,"/")
      writeRaster(fire,paste0(out_folder,fire_name),overwrite=T)
      return(j)
    }
    
  
  
  
folder <- "E:\\severity.1985.2000"
all_files <- list.files(folder, pattern = "_CBI_bc.tif", recursive = T)
all_files_split <- unlist(strsplit(all_files,"/"))
file_names <- all_files_split[c(F,T)]
file_names_trunc <- unlist(strsplit(file_names, "_CBI_bc.tif"))
fire_years <- vector(length = length(file_names_trunc))
for(i in 1:length(file_names_trunc)){
  split <- unlist(strsplit(file_names_trunc[[i]], ""))
  year <- paste(split[14:17], collapse = "")
  fire_years[i] <- year
}

#Verifying which fires are missing
  fire_years <- as.integer(fire_years) 
  valid_fires <- data.frame(Event_ID = file_names_trunc, Fire_Year = fire_years)
  valid_fires$present <- 1
  
  join <- left_join(mtbs,valid_fires)
    join$present[is.na(join$present)] <- 0
    join_missing <- join %>% filter(present == 0 & Fire_Year != 1984) 
  join_missing_wna <- st_transform(join_missing, st_crs(ecoregions))%>%
               st_crop( ecoregions)
  missing_fire_map <- ggplot()+
    geom_sf(data = ecoregions, fill = "grey90")+
    geom_sf(data=join_missing_wna,fill= "red")+
    theme_classic()
  missing_fire_map
  states_wna <- st_read("data\\masks\\cleaned\\wna_states.shp") %>%
    st_transform
  missing_fire_map + geom_sf(data = states_wna, alpha = .1)
states_start_end_south <-states_wna %>%
  filter(CODE  %in% c("TX","NM","AZ")) %>%
  mutate(Start_Day = 91, End_Day = 181)
states_start_end_wna <- states_wna %>%
  filter(CODE %in% c("CA","ID","MT","OR","SD","WA","WY","UT", "NV", "CO")) %>%
  mutate(Start_Day = 152, End_Day = 212)
  
states_start_end <- rbind(states_start_end_south, states_start_end_wna)

missing_fires_full_info <- st_join(join_missing_wna,states_start_end)%>%
  drop_na(Start_Day) %>%
  select(all_of(c("Incid_Name", "Event_ID", "Fire_Year", "Start_Day", "End_Day")))%>%
  rename(Fire_ID = Event_ID, NAME = Incid_Name) %>%
  st_transform("EPSG:4326") 
missing_fires_full_info$Fire_Year <-as.character(missing_fires_full_info$Fire_Year)

for(i in 1:24){
  start <- (i-1)*146+1
  end <-i*146 
st_write(missing_fires_full_info[start:end,],paste0("data/outputs/missing_fires/missing_fires_",i,".shp"), append = F)
}
