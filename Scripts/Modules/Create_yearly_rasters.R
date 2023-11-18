#This script creates yearly rasters of all fires
pkgs <- c("terra","sf","tidyverse")
invisible(lapply(pkgs, library, character.only = T))

create_yearly_rasters <- function(list_names, #data frame of fire IDs, fire_rast_paths, and fire years created in fire-regime-departure.R.
                            valid_years, #sequence of valid years of study
                            location_name, #name of location
                            buffer_sv, #spatVect of buffered fire perimeters we want to use, units of the crs
                            bps_mask, #reference spatRast, BPS masked to our location is best prospect
                            fire_path, #path to a folder containing folders for each valid_year, which contain individual fires for that year given in list_names
                            folder_out, #path to the folder you want to write new yearly rasters to
                            ndvi_threshold = NA #threshold for pre-fire NDVI. Fire IDs MUST match those given in list_names, and end in "_pre_ndvi.tif"
                            ){
  #loop through each valid year
for(j in 1:length(valid_years)) {
  year <- valid_years[j] #grab year
  year_ids <- list_names %>% #grab the fires of that year
    dplyr::filter(Fire_Year == year)
  
  #raster of buffered fire perimeters
  buffer_rast <- buffer_sv %>%
    terra::subset(buffer_sv$Fire_Year == year) %>%
    terra::rasterize(bps_mask)
  #merges all fires of the given year
  year_rast <- sapply(paste0(fire_path,'/', year,'/', year_ids$Tif), rast)
  mos <- sprc(year_rast) %>%
    merge(., first = T, na.rm = T) %>%
   classify(matrix(c(-100,0,NA),ncol = 3), right = T)
  
  #creates and NDVI threshold filter, merges, then filters out valid fires by that filter
  if(!is.na(ndvi_threshold)){
    ndvi_ids <- gsub("CBI_bc", "pre_ndvi", year_ids$Tif)
  ndvi_binary_reclass <- matrix(c(0,ndvi_threshold,NA,
                                  ndvi_threshold,Inf,1), ncol = 3, byrow  = T)
  ndvi_rast <- sapply(paste0(fire_path,'/', year,'/', ndvi_ids), rast)
  mos_ndvi <- sprc(ndvi_rast) %>%
    merge(., first = T, na.rm = T) %>%
    classify(ndvi_binary_reclass)%>%
    project(crs(mos))
  mos <-   mos * mos_ndvi
  }
  #project to our reference mask
  mos_reproj <- project(mos, bps_mask)
  #filter for what is within our perimeter buffer
 mos_reproj <- mos_reproj * buffer_rast
#create variables of each raster
  assign(paste0("mos_",year), mos)
 assign(paste0("mos_",year, "_30m"), mos_reproj)

 #write each raster to our output folder
writeRaster(get(paste0("mos_",year)), 
         filename = paste0(folder_out,"/",location_name,"_",year,".tif" ),
        overwrite = T)
writeRaster(get(paste0("mos_",year, "_30m")), 
           filename = paste0(folder_out,"/",location_name,"_",year,"_30m.tif" ),
          overwrite = T)
  rm(list = paste0("mos_",year))
  rm(list = paste0("mos_",year,"_30m"))
 rm(year_rast, mos, mos_reproj)
}
}
