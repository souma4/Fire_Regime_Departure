#THIS SCRIPT FILTERS OUR DATA FOR ALL ITERATIONS AND PULLS THE RELEVANT STATISTICS OUT. PRIMARY PLACE TO FURTHER OPTIMIZE
#load in packages
pkgs <- c("terra","sf","tidyverse", "data.table") 
invisible(lapply(pkgs, library, character.only = T))
#create the sampling scheme
Sampling_scheme <- function(bps_mask, #a spatRaster masked by your location of interest, in this case BPS masked to a landscape unit
                            mask_polygon, #a single spatVect object that you want to analyze
                            perims, #spatVect perimeters of fires for your mask
                            raster_stack, #a spatRast stack of all fires within the mask
                            bps_csv, #the CSV for BPS to join as a data.frame/tibble
                            proportion = 0.001, #proportion of area you want to sample
                            n.iter = 100, #how many iterations you want to run
                            forestFilter = NULL # whether to remove nonforest. Place a spatRast of forested areas here if you want to remove nonforest
                            ){
#START OF FILTERING
##### creation of 30m template
template <- rast(extent = ext( bps_mask),
                 res = 30, crs = crs(bps_mask), vals = T)



#template_DF <- template_DF[,c( "x", "y","cell")]


##### creation of frame
frame <- rast(crs = crs(bps_mask), ext = ext(bps_mask), vals = 1, resolution = 30) %>%
  rasterize(mask_polygon,.)



##### creating valid pixels to draw from. This filters out invalid BPS pixels

activeCat(bps_mask) <- 9
BPS_reclass <- bps_mask |>
                as.data.frame(xy = T) |>
                as.data.table()
BPS_reclass <- BPS_reclass[FRI_ALLFIR != "NA"] %>%
                as.data.frame() %>%
                rast(type = "xyz", crs = crs(bps_mask), ext = ext(bps_mask))
ext.2 <-  BPS_reclass* frame 



if(!is.null(forestFilter)){
ext.2 <- ext.2 * forestFilter
ext.2 <- classify(ext.2, matrix(c(0,NA),ncol= 2), others = T)

}
#convert to data Frame
frame_domain <- as.data.frame(ext.2, cells = T, xy = T, na.rm = T) %>%
  as.data.table()
frame_domain[,.(cell, x, y)]


#generate frequency map as global spatRast
freq_map <- sum(raster_stack > 0, na.rm = T) %>%
  classify(matrix(c(NA,0),ncol = 2))
#freq_map <- rasterize(perims, ext.2, fun = "sum") #generate frequency map as spatVect *NOTE: does not remove pixels that we filtered out in prior steps!! may overestimate fire frequency

#generate global sample size
sample.size <- trunc(dim(frame_domain)[1] * proportion)
rm(ext.2)

#draw random locations based on number of iterations and sample size
srs <- replicate(n.iter, sample(frame_domain$cell, sample.size))
#END FILTERING
#START OF CONTEMPORARY SAMPLING
#optimization step. find unique cells, then only extract and join once. Extracts are really slow and it's better to perform one large extract than many smaller ones
unique.cells <- sort(unique(as.vector(srs)))

frame_domain_subset <- frame_domain[cell %in% unique.cells]
rm(frame_domain)

contemporary_subset_data_sev <- as.data.table(terra::extract(raster_stack,vect(frame_domain_subset, geom = c("x","y")), xy = T))[,ID := frame_domain_subset$cell,]




contemporary_subset_data_freq <- as.data.table(terra::extract(freq_map, vect(frame_domain_subset, geom = c("x","y")),xy = T))[,ID := frame_domain_subset$cell,
                                                                                                                               ][,`:=`(freq  = sum, sum = NULL)]


# END CONTEMPORARY SAMPLING


#START HISTORICAL SAMPLING
# activeCat(bps_mask) <- 1
# codes <- terra::extract(bps_mask,vect(frame_domain_subset, geom = c("x","y")), xy =T)
# codes$BPS_CODE <- as.numeric(as.character(codes$BPS_CODE))
activeCat(bps_mask) <- 2
zones <-as.data.table(terra::extract(bps_mask,vect(frame_domain_subset, geom = c("x","y"))))
setkey(zones, ON = ID)
activeCat(bps_mask) <- 3
models <-as.data.table(terra::extract(bps_mask,vect(frame_domain_subset, geom = c("x","y"))))
setkey(models, ON = ID)
rm(bps_mask)

#join the relevant codes
# hist_dat_full <- full_join(codes, zones, by = "ID") %>%             
#   full_join(., models, by = "ID") %>%
#   mutate(ID = frame_domain_subset$cell)
hist_dat_full <- merge(zones, models, all = TRUE)[, ID := frame_domain_subset$cell,]
#hist_dat_full <- full_join(zones, models, by = "ID") %>%
#  mutate(ID = frame_domain_subset$cell)
#rm(codes, zones, models)
rm( zones, models)

# historical_subset_data <- left_join(hist_dat_full, bps_csv, by =c("BPS_CODE", "ZONE", "BPS_MODEL"),
#                                      copy = TRUE)
historical_subset_data <- bps_csv[hist_dat_full, on = c("ZONE", "BPS_MODEL")]
# historical_subset_data <- left_join(hist_dat_full, bps_csv, by =c( "ZONE", "BPS_MODEL"),
#                                                                            copy = TRUE)

#END HISTORICAL SAMPLING
output <- list(freq_map = freq_map,
               sample.size = sample.size,
               srs = srs,
               contemporary_subset_data_sev = contemporary_subset_data_sev,
               contemporary_subset_data_freq = contemporary_subset_data_freq,
               historical_subset_data = historical_subset_data)
return(output)

}
