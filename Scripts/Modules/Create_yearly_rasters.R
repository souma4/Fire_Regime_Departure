
pkgs <- c("terra","sf","tidyverse")
invisible(lapply(pkgs, library, character.only = T))

create_yearly_rasters <- function(list_names,
                            valid_years,
                            location_name,
                            buff,
                            bps_mask,
                            fire_path,
                            folder_out){
for(j in 1:length(valid_years)) {
  year = valid_years[j]
  year_ids <- list_names %>%
    filter(Fire_Year == year)
  buffer_rast <- buff %>%
    filter(Fire_Year == year) %>%
    rasterize(.,bps_mask)
  year_rast <- sapply(paste0(fire_path,'/', year,'/', year_ids$Tif), rast)
  mos <- sprc(year_rast) %>%
    mosaic() %>%
   classify(cbind(c(0),c(NA)))
  mos_reproj <- project(mos, bps_mask)
 mos_reproj <- mos_reproj *buffer_rast

  assign(paste0("mos_",year), mos)
 assign(paste0("mos_",year, "_30m"), mos_reproj)

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
