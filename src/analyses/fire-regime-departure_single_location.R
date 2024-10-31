pkgs <- c("foreach", "doParallel","sampling","tidyverse", 
          "terra", "sf",  "units", "RColorBrewer","ggrepel",
          "data.table")
invisible(lapply(pkgs, library, character.only = T))
source("scripts/analysis/fire-regime-departure_helpers.R")
####LINKS TO PACKAGE DOCUMENTATION
#https://cran.r-project.org/web/packages/foreach/vignettes/foreach.html   #foreach vignette
#ttps://cran.r-project.org/web/packages/foreach/foreach.pdf               #foreach
#https://cran.r-project.org/web/packages/doParallel/index.html            #doParallel
#https://cran.r-project.org/web/packages/sampling/sampling.pdf            #sampling
#https://cran.r-project.org/web/packages/tidyverse/tidyverse.pdf          #tidyverse
#https://cran.r-project.org/web/packages/terra/terra.pdf                  #terra
#https://rspatial.org/pkg/                                                #terra vignettes
#https://cran.r-project.org/web/packages/sf/sf.pdf                        #sf
#https://cran.r-project.org/web/packages/units/units.pdf                  #units
#https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf    #RColorBrewer
#https://cran.r-project.org/web/packages/ggrepel/ggrepel.pdf              #ggrepel
#https://cran.r-project.org/web/packages/data.table/data.table.pdf        #data.table
#https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html #data.table vignette



Calculate_fire_regime_and_departure_single_area <- function(bps_rast_path, #Path to your BPS raster  including the file name
                                                bps_csv_path, #Path to your BPS csv including the file name
                                                fire_path_folder, #Path to a folder of all fires, each within a folder for the year they occured
                                                mask_polygon_path, # Path to your vector (shapefile), including file name, of every area you want to analyze. You can also put an SF or spatVect object because vectors usually require some cleaning before analysis
                                                fire_polygon_path, #Path to your vector of fire perimeters including file name. Fire perimeters MUST include Event_ID and Fire_Year
                                                output_path, #The folder you want outputs to be
                                                display_name, #The column in your mask_polygon vector that you want to use for naming each area
                                                vectorOutputName,
                                                write_year_raster_out = 'memory', # if true, writes yearly fires out to a created folder (output_path/mask_name/fires/...) IF no other fires exist or write_year_raster_overwrite = T if FALSE, doesn't write rasters
                                                #if 'overwrite' is TRUE, will overwrite any existing rasters
                                                #if 'memory' is TRUE, will create rasters in memory instead of disk
                                                
                                                forestFilter = NULL, #a filter for forested areas. if you want to filter to forested areas, put a binary nonforest/forest spatRast here
                                                normalize_plots = F, #If you want plots that are normalized by historical mean and variance
                                                n.chunks.x = 1, #number of chunks in the x
                                                n.chunks.y = 1, #number of chunks in the y
                                                p.area = 0.001, #proportion of each landscape you want to sample
                                                n.iter = 100, #number of simulations to perform for each landscape
                                                #contemporary4historical = F, #niche use. Uses contemporary fires to estimate historical fire severity. IE "If it burned today, what would it have burned like in the past". use with caution
                                                ndvi_threshold = NA, #positive number to indicate which prefire NDVI you want to use to filter for pixels that burned in forest. Make sure your fire path folders contain pre_ndvi with the CBI_bc.tif files
                                                buffer_tune = -150, #number to indicate a buffer around fire perims. We default to -150 to remove the outher 150 meters from fire perimeters (to asses the "core" of fires). Units are in meters
                                                colors2 = RColorBrewer::brewer.pal(5, "RdYlBu")[c(5,2)], # colors for historical and contemporary comparative plots. Blue and Orange by default
                                                n.lines = 30, #number of simulations to show in comparative plots. VERY RAM INTENSIVE
                                                alpha.lines = .15, #Transparency of the lines from n.lines. I recommend something that allows rare events to look faded, but common event to look bold
                                                write_individual_gz = FALSE, #DONT MAKE THIS TRUE RIGHT NOW. CURRENT IMPLEMENTATION WRITES MASSIVE COMPRESSED RDATA (GZ) FILES AND WILL FILL YOUR HARDDRIVE WITH CLUTTER. Intent is to write the data indivudally for areas so you can load in only that landscapes data.
                                                make_figures = TRUE #if you want to make figures
){
  #load in paths
  bps <- terra::rast(bps_rast_path)
  bps_csv <- data.table::fread(bps_csv_path)
  #Convert polygons to spatVectors of the correct projection
  if(class(mask_polygon_path)[1] == "SpatVector"){
    mask <- mask_polygon_path %>%
      terra::project(crs(bps))
    #grabs the landscape Vector, then aggregates all sublayers
    
  }else if(class(mask_polygon_path)[1] == "sf"){
    mask <- mask_polygon_path %>%
      terra::vect() %>%
      terra::project(crs(bps))
  }else{
    mask <- terra::vect(mask_polygon_path) %>%
      terra::project(crs(bps))
  }
  #converts forest filter to correct projection
  if(!is.null(forestFilter)){
    forestFilter <- terra::project(forestFilter, crs(bps), threads = TRUE)
  }
  #grabs landscape names by your display name
  name <- c(mask[,display_name, drop = T]) %>%
    unname() %>%
    unlist() %>%
    unique() 
  mask <- mask %>%
    terra::aggregate(name)
  if(length(name) >1){
    stop("You have multiple areas. Please make sure there is one single name and one polygon object")
  }
  
  #load fire perimeters
  fire_perim_all <- vect(fire_polygon_path) %>%
    terra::project(crs(bps))
  #wrap for parrallelization
  bps_mask <- bps %>%
    terra::crop(mask, mask=T) %>%
    terra::wrap()
  fire_perim_all <- terra::wrap(fire_perim_all)
  mask_all <- terra::wrap(mask)
  if(!is.null(forestFilter)){
    forestFilter <- terra::wrap(forestFilter)
  }
  
  template_raster <- matrix(1, nrow = n.chunks.y, ncol = n.chunks.x) %>%
    terra::rast(crs = crs(bps_mask), ext = ext(bps_mask))
  tiles <- terra::makeTiles(bps_mask, template_raster)
  
 
  #start the loop through all mask_units
  stored_data <- foreach(i = seq_along(n.chunks.x*n.chunks.y), .export = c("mask","fire_path_folder",
                                                               "bps_mask", "n.iter", "bps_csv", 
                                                               "name", "fire_perim_all",
                                                               "forestFilter", "n.chunks.x",
                                                               "n.chunks.y",
                                                               "tiles"),
                         .packages = pkgs[c(-1,-2)]) %do%{
                           #source all of the relevant functions. MUST DO WITHIN LOOP
                           module_path <- "scripts/modules/"
                           module_names <- list.files(module_path)
                           source_paths = paste0(module_path,module_names)
                           
                           invisible(lapply(source_paths,source))
                           
                           #unwrap relevant layers        
                           bps_mask <- terra::unwrap(bps_mask)
                           fire_perim_all <- terra::unwrap(fire_perim_all)
                           mask <- terra::unwrap(mask)
                           
                           #zoom into the tile of interest
                           bps_tile <- terra::crop(bps_mask, terra::vrt(tiles[i]))
                           fire_perim_tile <- terra::crop(fire_perim_all, terra::vrt(tiles[i]))
                           mask_tile <- terra::crop(mask, terra::vrt(tiles[i]))
                           #grab the landscape unit we are iterating on, converts spaces to underscores to create folder for that landscape
                           
                           dir_name <- gsub(" ", "_", name)
                           output_name <- paste0(output_path,"/",dir_name)
                           if(write_year_raster_out != "memory" & make_figures == FALSE){
                             dir.create(output_name, showWarnings = F)
                           }
                           
                          
                           
                           
                           
                           #Grabs perimeters that fall within the mask. If it errors, you have NO FIRES AND CANNOT ANALYZE. WRITES SUCH THEN BREAKS OUT OF ITERATION
                           perims <- tryCatch(
                             expr = {
                               perims <- terra::crop(fire_perim_tile, mask_tile)%>%
                                 terra::subset(Fire_Year >=year_caps[1] & Fire_Year <= year_caps[2], NSE = T)
                               
                             },
                             error =  function(e){
                               e
                             }
                           )
                           
                           
                           #create yearly rasters and figures folder if contemporary fires exist
                           yearly_rasters_folder <- paste0(output_name,"/fires")
                           
                           figure_folder <- paste0(output_name,"/figures")
                           if(write_year_raster_out != "memory"){
                             dir.create(yearly_rasters_folder, showWarnings = F)
                           }
                           if(make_figures == TRUE){
                             dir.create(figure_folder, showWarnings = F)
                           }
                           
                           #create buffer of fire perims
                           buffer_sv <- terra::buffer(perims, buffer_tune)
                           #data frame of fire year and event_ID
                           list_names <- perims %>%
                             as.data.frame() %>%
                             dplyr::select(Fire_Year, Event_ID)
                           
                           #creates cbi file name using Parks, et al(2019)
                           list_names$Tif <- paste0(list_names$Event_ID, "_CBI_bc.tif")
                           
                           #all years that had a fire occur
                           all_years <- unique(perims$Fire_Year)
                           
                           #generates the year_ids
                           year_ids <- list(year = all_years, IDs = NULL)
                           #creates the naming convention pattern for valid yearly rasters
                           year_pattern <- paste0(all_years, "_30m.tif")
                           #finds all files within the yearly raster folder.  VERIFIES WHETHER RASTERS HAVE ALREADY BEEN CREATED. IF TRUE AND OVERWRITE == FALSE THEN YOU WILL SKIP CREATING YEARLY RASTERS
                           year_files <- list.files(yearly_rasters_folder, paste0(year_pattern,collapse = "|"))
                           
                           if((write_year_raster_out == TRUE & length(all_years) == length(year_files))| (write_year_raster_out == "memory")){
                             check_fires <- TRUE
                           }else{
                             check_fires <- FALSE
                           }
                           
                           #IF we pass the logic checks, create yearly rasters
                           if(check_fires == FALSE ){
                             check <- tryCatch(
                               {
                                 create_yearly_rasters(list_names, #names with Event_ID, Fire_Year, and Tif
                                                       all_years, #all the years to loop through
                                                       dir_name, #name of our output folder for the landscape
                                                       buffer_sv, #spatVect of buffered perims
                                                       bps_tile, # reference mask raster
                                                       fire_path_folder, #path to fire folder
                                                       yearly_rasters_folder, #path to output yearly raster folder
                                                       ndvi_threshold #ndvi threshold. if below, remove
                                 )   
                               },
                               #If an error occurs here it means YOU ARE MISSING THE CBI MAP FROM PARKS ET AL 2019
                               error = function(e){
                                 m <- message(paste0(e," You need to add the named file to the yearly fires folder using Parks et al 2019"))
                                 return(m)
                               })
                             if(inherits(check, "error")){
                               return(check)
                             } 
                             
                           }
                           if(write_year_raster_out == FALSE | write_year_raster_out == "overwrite" | write_year_raster_out == TRUE){
                             mosaic_stack_30m <- terra::rast(paste0(yearly_rasters_folder,"/",dir_name,"_",all_years,"_30m.tif"))
                           } else if(write_year_raster_out == 'memory'){
                             mosaic_stack_30m <- create_rasters_in_memory(list_names, #names with Event_ID, Fire_Year, and Tif
                                                                          all_years, #all the years to loop through
                                                                          buffer_sv, #spatVect of buffered perims
                                                                          bps_tile, # reference mask raster
                                                                          fire_path_folder, #path to fire folder
                                                                          ndvi_threshold #ndvi threshold. if below, remove
                             )
                           } else{
                             errorCondition("write_year_raster_out must be either TRUE, FALSE, overwrite, or memory")
                           }
                           names(mosaic_stack_30m) <- all_years
                           rm(list_names)
                           #create raster stack
                           #mosaic_stack_30m <- terra::rast(paste0(yearly_rasters_folder,"/",dir_name,"_",all_years,"_30m.tif"))
                           #
                          
                           #then we need forestFilter for sampling
                           if(!is.null(forestFilter)){
                             forestFilter_mask <- terra::unwrap(forestFilter)%>%
                               terra::crop(bps_mask, mask = T)
                           }else{
                             forestFilter_mask <- NULL
                           }  
                           #filters pixels
                           filter_data <- Sampling_scheme(bps_tile, #landscape bps raster
                                                          tile, #spatVect of landscape
                                                          perims, #perims for landscape
                                                          mosaic_stack_30m, #raster stack of yearly fire severities
                                                          bps_csv, #bps csv
                                                          proportion = p.area, #proportion of area to sample
                                                          n.iter, #number of simulations to run for each landscape
                                                          forestFilter = forestFilter_mask #whether to filter forests
                           )
                           #create and write fire freq map
                           
                           rm(bps_mask, mosaic_stack_30m)
                           
                           #prepare vectors for simulations
                           emd_freq_norm <- vector(length = n.iter)
                           emd_sev_norm <- vector(length = n.iter)
                           emd_combined_norm <- vector(length = n.iter)
                           
                           
                           emd_freq <- vector(length = n.iter)
                           emd_sev <- vector(length = n.iter)
                           emd_combined <- vector(length = n.iter)
                           
                           fri_contemporary <- vector(length = n.iter)
                           fri_historical <- vector(length = n.iter)
                           fri_contemporary_var <- vector(length = n.iter)
                           fri_historical_var <- vector(length = n.iter)
                           
                           pbhs_contemporary <- vector(length = n.iter)
                           pbhs_historical <- vector(length = n.iter)
                           pbhs_contemporary_var <- vector(length = n.iter)
                           pbhs_historical_var <- vector(length = n.iter)
                           
                           proportion_FRGI <- vector(length = n.iter)
                           
                           historical_area_burned <- vector(length = n.iter)
                           severity_average_contemporary_lst <- vector(length = n.iter)
                           severity_average_historical_lst <- vector(length = n.iter)
                           
                           #loop through each iteration
                           for(k in 1:n.iter){
                             #checks if first loop, if so, initialize plots
                            
                             
                             
                             #sample based on iteration
                             sampled <- sample_pixels( k,
                                                       filter_data$contemporary_subset_data_sev,
                                                       filter_data$contemporary_subset_data_freq,
                                                       filter_data$historical_subset_data,
                                                       filter_data$srs)
                             
                             
                             proportion_FRGI[k] <-length(frg1s) / dim(sampled$historical_sample)[1]
                             rm(frgs, frg1s)
                             
                             #Create frequency and severity distributions. estimate historical area burned
                             dfrequency <- create_frequency_distributions(sampled$contemporary_sample_freq,
                                                                          sampled$historical_sample)
                             
                             historical_area_burned[k] <- ((sum(dfrequency$historical_freq$freq))*(30*30)/10000)/p.area
                             
                             severity <- create_severity_distributions(sampled$historical_sample,
                                                                       sampled$contemporary_sample_sev,
                                                                       #contemporary4historical = F,
                                                                       k
                                                                       #historical_sample,
                             )
                             
                             
                             
                             
                             
                             #checks that you have both severities [check]
                             if(length(severity$historical_sev$sev)!=0 & length(severity$contemporary_sev$sev)!= 0){
                               freq_distrib_tile <- emd_Calculation(dfrequency$contemporary_freq,dfrequency$historical_freq, freq = T )
                               sev_distrib_tile <- emd_Calculation(severity$contemporary_sev,severity$historical_sev, freq = F )
                               
                               
                             
                               
                               
                               
                               
                               
                               
                               #if we got normalized of both frequency and severity, then run otherwise, set to NA
                               if(check_freq_norm == "Passed" & check_sev_norm == "Passed"){
                                 freq_distrib_tile <- emd_Calculation(dfrequency$contemporary_freq,dfrequency$historical_freq, freq = T )
                                 sev_distrib_tile <- emd_Calculation(severity$contemporary_sev,severity$historical_sev, freq = F )
                                 
                                 emd_combined_norm[k] <- sqrt(emd_freq_norm[k]^2+emd_sev_norm[k]^2)
                                 
                                
                                 
                                 
                                 
                                 
                                 
                               }else{
                                 emd_freq_norm[k] <- NA
                                 emd_sev_norm[k] <- NA
                                 emd_combined_norm[k] <- NA
                               }
                               
                               #if we are at iterations below n.lines, then add our data to the initialized plots
                               
                              
                               gc()
                             } else{ #if we fail the check for contemporary severity data only do emd on frequency
                               emd_freq[k] <- emd_Calculation(dfrequency$contemporary_freq, dfrequency$historical_freq, freq = T )
                               emd_sev[k] <- NA
                               emd_combined[k] <- NA
                               
                               
                               
                               fri_contemporary[k] <- year_range/mean(dfrequency$contemporary_freq$freq)
                               fri_historical[k] <- year_range/mean(dfrequency$historical_freq$freq)
                               pbhs_contemporary[k] <-NA
                               pbhs_historical[k] <- NA
                               emd_sev_norm[k] <-  NA
                               emd_combined_norm[k] <- NA
                               
                               check_freq_norm <- normalize_frequency(dfrequency$historical_freq,
                                                                      dfrequency$contemporary_freq,
                                                                      freq_dat_norm = normalize_plots)
                               
                               
                               if(check_freq_norm == "Passed"){
                                 
                                 
                                 emd_freq_norm[k] <- emd_Calculation(contemporary_freq_norm,historical_freq_norm, freq = T )
                                 
                               }else{
                                 
                                 emd_freq_norm[k] <- NA
                                 
                               }
                               
                             }
                             
                           }
                           #END SIMULATION
                           #calculate the median, mins, maxs, and variances of each simulated result
                           emd_freq_med <- quantile(emd_freq,.5, names = F, na.rm = T)
                           emd_sev_med <- quantile(emd_sev,.5, names = F, na.rm = T)
                           emd_combined_med <- quantile(emd_combined,.5, names = F, na.rm = T)
                           emd_freq_norm_med <- quantile(emd_freq_norm,.5, names = F, na.rm = T)
                           emd_sev_norm_med <- quantile(emd_sev_norm,.5, names = F, na.rm = T)
                           emd_combined_norm_med <- quantile(emd_combined_norm,.5, names = F, na.rm = T)
                           
                           emd_freq_var <- var(emd_freq, na.rm = T)
                           emd_sev_var <- var(emd_sev, na.rm = T)
                           emd_combined_var <- var(emd_combined, na.rm = T)
                           emd_freq_norm_var <- var(emd_freq_norm, na.rm = T)
                           emd_sev_norm_var <- var(emd_sev_norm, na.rm = T)
                           emd_combined_norm_var <- var(emd_combined_norm, na.rm = T)
                           
                           proportion_FRGI_med <- quantile(proportion_FRGI, .5, names = F, na.rm = T)
                           
                           fri_contemporary_med <- quantile(fri_contemporary, .5, names = F, na.rm = T)
                           fri_historical_med <- quantile(fri_historical, .5, names = F, na.rm = T)
                           
                           
                           
                           fri_contemporary_var <- var(fri_contemporary, na.rm = T)
                           fri_historical_var <- var(fri_historical, na.rm = T)
                           
                           fri_contemporary_min <- min(fri_contemporary, na.rm = T)
                           fri_historical_min <- min(fri_historical, na.rm = T)
                           
                           fri_contemporary_max <- max(fri_contemporary, na.rm = T)
                           fri_historical_max <- max(fri_historical, na.rm = T)
                           
                           pfrid <-calculate_pfrid(fri_contemporary_med,fri_historical_med)
                           
                           
                           pbhs_contemporary_med <- quantile(pbhs_contemporary, .5, names = F, na.rm = T)
                           pbhs_historical_med <- quantile(pbhs_historical, .5, names = F, na.rm = T)
                           
                           
                           pbhs_contemporary_var <- var(pbhs_contemporary, na.rm = T)
                           pbhs_historical_var <- var(pbhs_historical, na.rm = T)
                           
                           pbhs_contemporary_min <- min(pbhs_contemporary, na.rm = T)
                           pbhs_historical_min <- min(pbhs_historical, na.rm = T)
                           
                           pbhs_contemporary_max <- max(pbhs_contemporary, na.rm = T)
                           pbhs_historical_max <- max(pbhs_historical, na.rm = T)
                           
                           
                           historical_area_burned_ha <- quantile(historical_area_burned, .5,names = F,na.rm = T)
                           
                           severity_median_contemporary <- quantile(severity_average_contemporary_lst, .5, names = F, na.rm = T)
                           severity_median_historical <- quantile(severity_average_historical_lst, .5, names = F, na.rm = T)
                           
                           #calculate area, area burned, proportion burned
                           focal_area_ha <- terra::expanse(mask, unit = "ha") %>%
                             as.numeric()
                           fire_area_ha <- sum(terra::expanse(perims, "ha")) %>%
                             as.numeric
                           proportion_burned <- fire_area_ha/focal_area_ha
                           
                           
                           #calculated median severity_ratio_departure
                           percent_severity_departure <- calculate_percent_severity_departure(pbhs_contemporary_med, pbhs_historical_med)
                           
                           #calculate median FRCC departures
                           frequency_departure_frcc <- (1-(pmin(fri_contemporary_med,fri_historical_med)/pmax(fri_contemporary_med,fri_historical_med)))*100
                           severity_departure_frcc <- (1-(pmin(pbhs_contemporary_med,pbhs_historical_med)/pmax(pbhs_contemporary_med,pbhs_historical_med)))*100
                           regime_departure_frcc <- (severity_departure_frcc+frequency_departure_frcc)/2
                           
                           #statistic names
                           stat_names <-c("name",
                                          "sample size",
                                          "study area ha",
                                          "area burned ha",
                                          "prop area burned",
                                          "prop FRG I",
                                          "contemp fri",
                                          "hist fri",
                                          
                                          "contemp fri var",
                                          "hist fri var",
                                          
                                          "contemp fri min",
                                          "hist fri min",
                                          
                                          "contemp fri max",
                                          "hist fri max",
                                          
                                          "pfrid",
                                          "contemp pbhs",
                                          "hist pbhs",
                                          
                                          "contemp pbhs var",
                                          "hist pbhs var",
                                          
                                          "contemp pbhs min",
                                          "hist pbhs min",
                                          
                                          "contemp pbhs max",
                                          "hist pbhs max",
                                          
                                          "percent severity departure",
                                          
                                          "emd frequency",
                                          "emd severity",
                                          "emd both",
                                          "emd frequency normalized",
                                          "emd severity normalized",
                                          "emd both normalized",
                                          
                                          "emd frequency var",
                                          "emd severity var",
                                          "emd both var",
                                          "emd frequency normalized var",
                                          "emd severity normalized var",
                                          "emd both normalized var",
                                          
                                          "FRCC freq dep",
                                          "FRCC sev dep",
                                          "FRCC reg dep",
                                          "historical area burned ha",
                                          
                                          "median historical severity",
                                          "median contemporary severity")
                           #statistics
                           stats <- c(
                             name_unit,
                             filter_data$sample.size,
                             focal_area_ha,
                             fire_area_ha, 
                             proportion_burned, 
                             proportion_FRGI_med,
                             
                             fri_contemporary_med, 
                             fri_historical_med,
                             
                             fri_contemporary_var, 
                             fri_historical_var,
                             
                             fri_contemporary_min, 
                             fri_historical_min,
                             
                             fri_contemporary_max, 
                             fri_historical_max,
                             
                             pfrid, 
                             
                             pbhs_contemporary_med, 
                             pbhs_historical_med,
                             
                             
                             pbhs_contemporary_var, 
                             pbhs_historical_var,
                             
                             pbhs_contemporary_min, 
                             pbhs_historical_min,
                             
                             pbhs_contemporary_max, 
                             pbhs_historical_max,
                             
                             percent_severity_departure,
                             
                             emd_freq_med, 
                             emd_sev_med, 
                             emd_combined_med,
                             emd_freq_norm_med, 
                             emd_sev_norm_med ,
                             emd_combined_norm_med,
                             
                             emd_freq_var, 
                             emd_sev_var, 
                             emd_combined_var,
                             emd_freq_norm_var, 
                             emd_sev_norm_var ,
                             emd_combined_norm_var,
                             
                             
                             
                             frequency_departure_frcc,
                             severity_departure_frcc,
                             regime_departure_frcc,
                             historical_area_burned_ha,
                             
                             severity_median_historical,
                             severity_median_contemporary
                           )
                           #data frame of all statistics with name
                           stat_df <- data.frame(stat = stat_names, value = stats) 
                           
                           
                           #finish figures with mean values, then save them
                           
                           
                       
                           #stats for an individual landscape
                           #individual_stats <- list(stat_df,bps_breakdown,freq_compare_bar,sev_density,freq_sev_each,freq_sev_lm2h)
                           
                           stat_df <- list(stats = stat_df)
                           
                           #mark of completion
                           current_date <- Sys.Date()
                           if(dir.exists(output_name)){
                             unlink(paste0(output_name,"/",list.files(output_name, ".txt")))
                             write.table(matrix(paste0("Completed ",current_date)), file = paste0(output_name,"/Completed_,",current_date,".txt"), append = F)
                           }
                           #should write a GZ for each landscape but is super heavy and overflows storage
                           # if(write_individual_gz == TRUE){
                           # save(individual_stats, file = paste0(output_name,"/",name_unit,"_stats.gz"))
                           # }
                           gc()
                           return(c(stat_df)) #END OF LOOP
                         }
  stopImplicitCluster() #CLOSE PARALLEL WORKERS
  
  
  #### take stored data and create a shapefile, joining valid data to our initial 
  #masks
  
  out_file <- paste0(output_path,"/!summaries")
  dir.create(out_file, showWarnings = F)
  mask_all <- terra::unwrap(mask_all) %>%
    st_as_sf()
  #convert join by name to "name"
  mask_df <- as.data.frame(mask_all)
  name_index <- which(names(mask_df) == display_name)
  names(mask_df)[name_index] <- "name"
  mask_sf <- st_as_sf(mask_df)
  
  mask_sf <- mask_sf %>%
    dplyr::group_by(name) %>%
    dplyr::summarize(geometry = st_union(geometry))
  
  
  
  #find valid data, then transform into dataframe
  valid_data <- check_data(stored_data)
  valid_df <- production_pulling(valid_data) %>%
    tidyr::drop_na()%>%
    dplyr::distinct(name, .keep_all = T)
  
  
  #join valid data to our mask layer, create post hoc statistics
  valid_sf <- st_as_sf(left_join(valid_df, mask_sf, by = "name")) %>%
    dplyr::mutate(emd_freq_cat = case_when(emd_frequency <= quantile(valid_df$emd_frequency, 0.33) ~ 'A',
                                           emd_frequency > quantile(valid_df$emd_frequency, 0.33) & 
                                             emd_frequency <= quantile(valid_df$emd_frequency, 0.66) ~ 'B',
                                           emd_frequency > quantile(valid_df$emd_frequency, 0.66) ~ 'C'),
                  emd_sev_cat = case_when(emd_severity <= quantile(valid_df$emd_severity, 0.33) ~ '3',
                                          emd_severity > quantile(valid_df$emd_severity, 0.33) & 
                                            emd_severity <= quantile(valid_df$emd_severity, 0.66) ~ '2',
                                          emd_severity > quantile(valid_df$emd_severity, 0.66) ~ '1'),
                  
                  emd_freq_norm_cat = case_when(emd_frequency_normalized <= quantile(valid_df$emd_frequency_normalized, 0.33) ~ 'A',
                                                emd_frequency_normalized > quantile(valid_df$emd_frequency_normalized, 0.33) & 
                                                  emd_frequency_normalized <= quantile(valid_df$emd_frequency_normalized, 0.66) ~ 'B',
                                                emd_frequency_normalized > quantile(valid_df$emd_frequency_normalized, 0.66) ~ 'C'),
                  emd_sev_norm_cat = case_when(emd_severity_normalized <= quantile(valid_df$emd_severity_normalized, 0.33) ~ '3',
                                               emd_severity_normalized > quantile(valid_df$emd_severity_normalized, 0.33) & 
                                                 emd_severity_normalized <= quantile(valid_df$emd_severity_normalized, 0.66) ~ '2',
                                               emd_severity_normalized > quantile(valid_df$emd_severity_normalized, 0.66) ~ '1'),
                  
                  
                  #
                  #
                  
                  
                  signed_emd_frequency_normalized = sign(pfrid)*emd_frequency_normalized,
                  signed_emd_frequency = sign(pfrid)*emd_frequency,
                  
                  signed_emd_severity_normalized = sign(percent_severity_departure)*emd_severity_normalized,
                  signed_emd_severity= sign(percent_severity_departure)*emd_severity,
                  
                  
                  
                  
                  
    ) %>%
    tidyr::unite('bicat_median', emd_freq_cat, emd_sev_cat) %>%
    tidyr::unite('bicat_norm',emd_freq_norm_cat,emd_sev_norm_cat)%>%
    dplyr::mutate('numcat_median' = as.numeric(factor(bicat_median)), 'numcat_norm' = as.numeric(factor(bicat_norm)))
  
  terra::writeVector(terra::vect(valid_sf), filename = paste0(out_file,"/",vectorOutputName,".gpkg"), overwrite = T)
  
  
  return(valid_sf) #SAVE STORED_DATA (all landscapes looped through) 
}
