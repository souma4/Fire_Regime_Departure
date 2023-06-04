pkgs <- c("terra","sf","tidyverse")
invisible(lapply(pkgs, library, character.only = T))
Sampling_scheme <- function(bps_mask,
                            mask_polygon,
                            perims,
                            raster_stack,
                            bps_csv,
                            proportion = 0.001,
                            n.iter = 100){

##### creation of 30m template
template <- rast(extent = ext( bps_mask),
                 res = 30, crs = crs(bps_mask), vals = T)
template_DF <- as.data.frame(template, cells = T, xy = T) %>%
  dplyr::select(cell, x, y)


template_DF <- template_DF[,c( "x", "y","cell")]


##### creation of frame
frame <- rast(crs = crs(bps_mask), ext = ext(bps_mask), vals = 1, resolution = 30) %>%
  rasterize(vect(mask_polygon),.)



##### creating extracted points

activeCat(bps_mask) <- 9
ext.1 <- terra::extract(frame, 
                        vect(template_DF, geom = c("x", "y")), xy = T) %>%
  drop_na()
ext.2 <-  terra::extract(bps_mask, vect(ext.1, geom = c("x","y")), xy = T, layer = "FRI_ALLFIR") %>%
  drop_na(FRI_ALLFIR) %>%
  filter(FRI_ALLFIR != "NA")%>%
  dplyr::select(-one_of("ID", "FRI_ALLFIR"))
#ext.2 <- terra::extract(forest_cover_mask, vect(ext.2, geom = c("x","y")),xy = T)%>%
#  drop_na() %>%
#dplyr::select(-one_of("ID", "1984"))
#, "FRI_ALLFIR"

frame_domain <<- template_DF %>%
  right_join(ext.2, by = c("x","y"))
rm( ext.1, ext.2)

frame_domain_rast <-  frame_domain %>% dplyr::select(x,y)%>% mutate(val = T)%>%
  rast(., type = "xyz", crs = crs(bps_mask))
freq_map <<- rasterize(vect(perims), frame_domain_rast, fun = "sum",background = 0)

sample.size <<- trunc(dim(frame_domain)[1] * proportion)


srs <<- replicate(n.iter, sample(frame_domain$cell, sample.size))
unique.cells <- sort(unique(as.vector(srs)))

frame_domain_subset <- frame_domain %>% filter(cell %in% unique.cells)
rm(frame_domain, frame_domain_rast)

contemporary_subset_data_sev <<- terra::extract(raster_stack,vect(frame_domain_subset, geom = c("x","y")), xy = T) %>%
  mutate(ID = frame_domain_subset$cell)



contemporary_subset_data_freq <<- terra::extract(freq_map, vect(frame_domain_subset, geom = c("x","y")),xy = T) %>%
  mutate(ID = frame_domain_subset$cell)

names(contemporary_subset_data_freq)[2] <<- "freq"



activeCat(bps_mask) <- 1
codes <- terra::extract(bps_mask,vect(frame_domain_subset, geom = c("x","y")), xy =T)
codes$BPS_CODE <- as.numeric(as.character(codes$BPS_CODE))
activeCat(bps_mask) <- 2
zones <-terra::extract(bps_mask,vect(frame_domain_subset, geom = c("x","y")))
activeCat(bps_mask) <- 3
models <-terra::extract(bps_mask,vect(frame_domain_subset, geom = c("x","y")))

rm(bps_mask)

hist_dat_full <- full_join(codes, zones, by = "ID") %>%             
  full_join(., models, by = "ID") %>%
  mutate(ID = frame_domain_subset$cell)
rm(codes, zones, models)

historical_subset_data <<- left_join(hist_dat_full, bps_csv, by =c("BPS_CODE", "ZONE", "BPS_MODEL"))




}
