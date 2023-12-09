pkgs <- c("foreach", "doParallel","sampling","tidyverse", "terra", "sf",  "units", "RColorBrewer","ggrepel")
invisible(lapply(pkgs, library, character.only = T))
source("scripts/analysis/fire-regime-departure_helpers.R")



Calculate_fire_regime_and_departure <- function(bps_rast_path, #Path to your BPS raster  including the file name
                                                bps_csv_path, #Path to your BPS csv including the file name
                                                fire_path_folder, #Path to a folder of all fires, each within a folder for the year they occured
                                                mask_polygon_path, # Path to your vector (shapefile), including file name, of every area you want to analyze. You can also put an SF or spatVect object because vectors usually require some cleaning before analysis
                                                fire_polygon_path, #Path to your vector of fire perimeters including file name. Fire perimeters MUST include Event_ID and Fire_Year
                                                output_path, #The folder you want outputs to be
                                                display_name, #The column in your mask_polygon vector that you want to use for naming each area
                                                vectorOutputName,
                                                write_year_raster_out = F, # if true, writes yearly fires out to a created folder (output_path/mask_name/fires/...) IF no other fires exist or write_year_raster_overwrite = T if FALSE, doesn't write rasters
                                                write_year_raster_overwrite = F, #if true forces the writing of yearly fire rasters for each landscape
                                                forestFilter = NULL, #a filter for forested areas. if you want to filter to forested areas, put a binary nonforest/forest spatRast here
                                                normalize_plots = F, #If you want plots that are normalized by historical mean and variance
                                                n.cores = 1, #number of cores you want to use for parallel computing. Defaults to non-parallel. This should NEVER be more than half of the logical processors on your device
                                                p.area = 0.001, #proportion of each landscape you want to sample
                                                n.iter = 100, #number of simulations to perform for each landscape
                                                contemporary4historical = F, #niche use. Uses contemporary fires to estimate historical fire severity. IE "If it burned today, what would it have burned like in the past". use with caution
                                                ndvi_threshold = NA, #positive number to indicate which prefire NDVI you want to use to filter for pixels that burned in forest. Make sure your fire path folders contain pre_ndvi with the CBI_bc.tif files
                                                buffer_tune = -150, #number to indicate a buffer around fire perims. We default to -150 to remove the outher 150 meters from fire perimeters (to asses the "core" of fires). Units are in meters
                                                colors2 = RColorBrewer::brewer.pal(5, "RdYlBu")[c(5,2)], # colors for historical and contemporary comparative plots. Blue and Orange by default
                                                n.lines = 30, #number of simulations to show in comparative plots. VERY RAM INTENSIVE
                                                alpha.lines = .15, #Transparency of the lines from n.lines. I recommend something that allows rare events to look faded, but common event to look bold
                                                write_individual_gz = FALSE #DONT MAKE THIS TRUE RIGHT NOW. CURRENT IMPLEMENTATION WRITES MASSIVE COMPRESSED RDATA (GZ) FILES AND WILL FILL YOUR HARDDRIVE WITH CLUTTER. Intent is to write the data indivudally for areas so you can load in only that landscapes data.
                                                ){
  #load in paths
 bps <- terra::rast(bps_rast_path)
 bps_csv <- readr::read_csv(bps_csv_path)
 #Convert polygons to spatVectors of the correct projection
 if(class(mask_polygon_path)[1] == "SpatVector"){
   mask_all <- mask_polygon_path %>%
     terra::project(crs(bps))
 }else if(class(mask_polygon_path)[1] == "sf"){
   mask_all <- mask_polygon_path %>%
     terra::vect() %>%
     terra::project(crs(bps))
 }else{
     mask_all <- terra::vect(mask_polygon_path) %>%
        terra::project(crs(bps))
 }
 #converts forest filter to correct projection
 if(!is.null(forestFilter)){
   forestFilter <- terra::project(forestFilter, crs(bps), threads = TRUE)
 }
 #grabs landscape names by your display name
 all_names <- c(mask_all[,display_name, drop = T]) %>%
   unname() %>%
   unlist()
 mask_units <- all_names %>% 
    unique() %>%
   unlist()
  
 #load fire perimeters
 fire_perim_all <- vect(fire_polygon_path) %>%
   terra::project(crs(bps))
 #wrap for parrallelization
bps <- terra::wrap(bps)
fire_perim_all <- terra::wrap(fire_perim_all)
mask_all <- terra::wrap(mask_all)
if(!is.null(forestFilter)){
  forestFilter <- terra::wrap(forestFilter)
}


cores <- n.cores
registerDoParallel(cores) #register parralel workspaces


#start the loop through all mask_units
stored_data <- foreach(i = 1:length(mask_units), .export = c("mask_all","fire_path_folder",
                                                             "bps", "n.iter", "bps_csv", 
                                                             "mask_units", "fire_perim_all",
                                                             "forestFilter"),
                       .packages = pkgs[c(-1,-2)],
                       .inorder = FALSE, .errorhandling = "pass") %dopar%{
 #source all of the relevant functions. MUST DO WITHIN LOOP
 module_path <- "scripts/modules/"
 module_names <- list.files(module_path)
 source_paths = paste0(module_path,module_names)
                         
 invisible(lapply(source_paths,source))
        
 #unwrap relevant layers        
      fire_perim_all <- terra::unwrap(fire_perim_all)
    mask_all <- terra::unwrap(mask_all)
                  
    #grab the landscape unit we are iterating on, converts spaces to underscores to create folder for that landscape
    name_unit <- mask_units[i]
    dir_name <- gsub(" ", "_", name_unit)
      output_name <- paste0(output_path,"/",dir_name)
    dir.create(output_name, showWarnings = F)
    

    
    #grabs the landscape Vector, then aggregates all sublayers
    mask <- mask_all[which(all_names == name_unit),] %>%
      terra::aggregate()
    #Grabs perimeters that fall within the mask. If it errors, you have NO FIRES AND CANNOT ANALYZE. WRITES SUCH THEN BREAKS OUT OF ITERATION
    perims <- tryCatch(
      expr = {
      perims <- terra::crop(fire_perim_all, mask)%>%
      terra::subset(Fire_Year >=year_caps[1] & Fire_Year <= year_caps[2], NSE = T)
      
      },
      error =  function(e){
        e
      }
    )
    if(inherits(perims, "error")){
      individual_stats <- list("No contemporary fire, cannot analyze")
      save(individual_stats, file = paste0(output_name,"/",name_unit,"_stats.RData"))
      write.table(matrix("Cannot Analyze"), file = paste0(output_name,"/CANT_ANALYZE.txt"), append = F)
      return(individual_stats)

    }
    
      #create yearly rasters and figures folder if contemporary fires exist
    yearly_rasters_folder <- paste0(output_name,"/fires")
    
    figure_folder <- paste0(output_name,"/figures")
    dir.create(yearly_rasters_folder, showWarnings = F)
    dir.create(figure_folder, showWarnings = F)
    
    #unwrap and crop to our landscape of interest
    bps <- terra::unwrap(bps)
   bps_mask <- terra::crop(bps, mask, mask = T)
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
    
    if(write_year_raster_out == TRUE & length(all_years) == length(year_files)){
      check_fires <- TRUE
    }else{
      check_fires <- FALSE
    }
    
    if(write_year_raster_overwrite == T){
      check_fires <- FALSE
    }
    
    #IF we pass the logic checks, create yearly rasters
    if(write_year_raster_out == TRUE & check_fires == FALSE ){
      check <- tryCatch(
        {
          create_yearly_rasters(list_names, #names with Event_ID, Fire_Year, and Tif
                            all_years, #all the years to loop through
                            dir_name, #name of our output folder for the landscape
                            buffer_sv, #spatVect of buffered perims
                            bps_mask, # reference mask raster
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
    rm(list_names)
    #create raster stack
    mosaic_stack_30m <- terra::rast(paste0(yearly_rasters_folder,"/",dir_name,"_",all_years,"_30m.tif"))
    #
    # create and write average severity map
    #
    ave_sev <- terra::app(mosaic_stack_30m, mean, na.rm = T) %>%
      terra::crop(mask, mask = T)
    ave_sev_reclass <- matrix(c(0,low.thresh_median, 1,
                                low.thresh_median, high.thresh_median , 2,
                                high.thresh_median, 3 , 3), byrow = T, ncol = 3)
    ave_sev <- terra::classify(ave_sev,ave_sev_reclass, others = 0)%>%
      terra::subst(NA,0)%>%
      as.factor()
    levels(ave_sev) <- data.frame(value = c(0,1,2,3),label = c("Unburned","Low","Mixed","High"))
    sev_map_colorPalette <- c("#006600","#00ffff", "#ffff00", "#ff0000")
    coltab(ave_sev) <- sev_map_colorPalette
    
    writeRaster(ave_sev, paste0(figure_folder,"/average_severity_map_",dir_name,".tiff"), filetype = "GTiff", overwrite = T)
    
    rm(ave_sev, sev_map_colorPalette)
    #
    #
    #
    
    names(mosaic_stack_30m) <- all_years
    #then we need forestFilter for sampling
    if(!is.null(forestFilter)){
      forestFilter_mask <- terra::unwrap(forestFilter)%>%
        terra::crop(bps_mask, mask = T)
    }else{
      forestFilter_mask <- NULL
    }  
    #samples pixels
    Sampling_scheme(bps_mask, #landscape bps raster
                    mask, #spatVect of landscape
                    perims, #perims for landscape
                    mosaic_stack_30m, #raster stack of yearly fire severities
                    bps_csv, #bps csv
                    proportion = p.area, #proportion of area to sample
                    n.iter, #number of simulations to run for each landscape
                    forestFilter = forestFilter_mask #whether to filter forests
                    )
     #create and write fire freq map
    freq_map_colorPalette <- rev(RColorBrewer::brewer.pal(9, "YlOrRd"))
    freq_map_length <- terra::minmax(freq_map)[2]-terra::minmax(freq_map)[1]
    
    freq_map_colorPalette <- c("#787882",freq_map_colorPalette[1:freq_map_length])
    coltab(freq_map)<-freq_map_colorPalette
    writeRaster(freq_map, paste0(figure_folder,"/fire_frequency_map_",dir_name,".tiff"), filetype = "GTiff", overwrite = T)
    
    # 
    # pdf(file = paste0(figure_folder,"/frequency_map_",dir_name,".pdf"), width = 1920, height = 1080, pointsize = 20)
    # freq_map <- crop(freq_map, mask, mask = T)
    # plot(freq_map, col = freq_map_colorPalette, type = "classes",
    #      plg = list(title = "# of Fires"))
    # plot(mask, lwd = 2, add = T)
    # dev.off()
    # 
        #create and write bps_map
    pdf(file = paste0(figure_folder,"/bps_",dir_name,".pdf"), width = 1920, height = 1080, pointsize = 20)
    activeCat(bps_mask) <- 4
    #writeRaster(bps_mask, paste0(figure_folder,"/bps_map_",dir_name,".tiff"), filetype = "GTiff", overwrite = T)
    
    plot(bps_mask, mar = c(3.1, 3.1,3.1,1800),
         plg=list( # parameters for drawing legend
           title = "BioPhysical Setting",
           title.cex = 3, # Legend title size
           cex = 70 # Legend text size
         ))
    plot(mask, lwd = 2, add = T)
    dev.off()
    
   rm(bps_mask, freq_map,mosaic_stack_30m)
    
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
      if(k == 1){
        completed.freq <- 1
        completed.sev <- 1
        if(normalize_plots == T){
          freq_compare_bar <- ggplot()+
            ggtitle(paste0(name_unit, " Frequency Bar Plot")) +
            xlab("# Fires in 35 Years")+
            ylab("Proportion of Landscape")+
            ylim(c(0,1))+
            scale_x_continuous(breaks = seq(-2,10,1))+
            scale_color_manual(values = colors2) +
            #scale_pattern_manual(values = c("stripe","none"))+
            guides(color = guide_legend(title ="",
                                        override.aes =list(alpha = 1))
                   #pattern = guide_legend(title = "",
                   # override.aes = list(pattern = c("stripe","none")))
            )+
            theme(axis.text = element_text(size = 6),
                  legend.text = element_text(size = 8),
                  axis.title = element_text(size = 12),
                  plot.title = element_text(size = 14),
                  legend.title = element_blank)  + 
            theme_classic()
          
          sev_density <- ggplot() +
            ggtitle(paste0(name_unit, " Severity Density from \n Historic Burning")) +
            xlab("Severity(CBI)")+
            ylab("Density")+
            scale_color_manual(values = colors2) +
            #scale_pattern_manual(values = c("stripe","none"))+
            guides(color = guide_legend(title ="",
                                        override.aes = list(alpha = 1))
                   #pattern = guide_legend(title = "",
                   #override.aes = list(pattern = c("stripe","none")))
            )+
            theme(axis.text = element_text(size = 6),
                  legend.text = element_text(size = 12),
                  axis.title = element_text(size = 14),
                  plot.title = element_text(size = 16),
                  legend.title = element_blank)  +
            theme_classic()
          
        }else{
          freq_compare_bar <- ggplot()+
            ggtitle(paste0(name_unit, " Frequency Bar Plot")) +
            xlab("# Fires in 35 Years")+
            ylab("Proportion of Landscape")+
            ylim(c(0,1))+
            scale_x_continuous(breaks = seq(0,15,1))+
            scale_color_manual(values = colors2) +
            #scale_pattern_manual(values = c("stripe","none"))+
            guides(color = guide_legend(title ="",
                                        override.aes =list(alpha = 1))
                   #pattern = guide_legend(title = "",
                   # override.aes = list(pattern = c("stripe","none")))
            )+
            theme(axis.text = element_text(size = 6),
                  legend.text = element_text(size = 8),
                  axis.title = element_text(size = 12),
                  plot.title = element_text(size = 14),
                  legend.title = element_blank)  + 
            theme_classic()
          
          sev_density <- ggplot() +
            ggtitle(paste0(name_unit, " Severity Density from \n Historic Burning")) +
            xlab("Severity(CBI)")+
            ylab("Density")+
            scale_color_manual(values = colors2) +
            #scale_pattern_manual(values = c("stripe","none"))+
            guides(color = guide_legend(title ="",
                                        override.aes = list(alpha = 1))
                   #pattern = guide_legend(title = "",
                   #override.aes = list(pattern = c("stripe","none")))
            )+
            theme(axis.text = element_text(size = 6),
                  legend.text = element_text(size = 12),
                  axis.title = element_text(size = 14),
                  plot.title = element_text(size = 16),
                  legend.title = element_blank)  +
            theme_classic()
          
          
        }
        freq_sev_each <- ggplot()+
          
          ggtitle(paste0(name_unit, " Severity Class Bar Plot"))+
          xlab("Severity Class")+
          ylab("Proportion of Fires")+
          scale_color_manual(values = colors2) +
          scale_x_continuous(breaks = c(1,2,3),labels = c("Low","Mixed","High"), limits = c(0.5, 3.5))+
          ylim(c(0,1))+
          guides(color = guide_legend(title ="",
                                      override.aes = list(alpha = 1)))+
          theme(axis.text = element_text(size = 6),
                legend.text = element_text(size = 12),
                axis.title = element_text(size = 14),
                plot.title = element_text(size = 16),
                legend.title = element_blank) +
          theme_classic()
        
        freq_sev_lm2h <- 
          ggplot() +
          ggtitle(paste0(name_unit, " Severity Class \n Low+Mixed / High Bar Plot"))+
          xlab("Severity Class")+
          ylab("Proportion of Fires")+
          scale_color_manual(values = colors2) +
          scale_x_continuous(breaks = c(1,2),labels = c("Low/Mixed","High"), limits = c(0.5,2.5))+
          guides(color = guide_legend(title ="",
                                      override.aes = list(alpha = 1)),
          )+
          ylim(c(0,1))+
          theme(axis.text = element_text(size = 6),
                legend.text = element_text(size = 12),
                axis.title = element_text(size = 14),
                plot.title = element_text(size = 16),
                legend.title = element_blank)  +
          theme_classic()
      }
      
      
      #sample based on iteration
       sample_pixels( k,
                      contemporary_subset_data_sev,
                      contemporary_subset_data_freq,
                      historical_subset_data,
                      srs)
      #calculate proportion of FRG 1
      frgs <- unlist(strsplit(historical_subset_data$FRG_NEW,"-"))
      frg1s <- frgs[which(frgs == "I")]
      
      
      proportion_FRGI[k] <-length(frg1s) / dim(historical_subset_data)[1]
      rm(frgs, frg1s)
      
      #Create frequency and severity distributions. estimate historical area burned
      create_frequency_distributions (contemporary_sample_freq,
                                      historical_sample)
      historical_area_burned[k] <- ((sum(historical_freq$freq))*(30*30)/10000)/p.area
      create_severity_distributions(historical_sample,
                                    historical_sample_freq,
                                    contemporary_sample_sev,
                                    contemporary4historical = F,
                                    k)
     
        
      
      
      
      #checks that you have both severities [check]
      if(length(historical_sev$sev)!=0 & length(contemporary_sev$sev)!= 0){
          
        
      emd_freq[k] <- emd_Calculation(contemporary_freq,historical_freq, freq = T )
      emd_sev[k] <-  emd_Calculation(contemporary_sev,historical_sev, freq = F )
      emd_combined[k] <- sqrt(emd_freq[k]^2+emd_sev[k]^2)
      
      
      #build multivariate distribs
      # historical_0pad <- length(which(historical_freq$freq == 0)) %>%
      #   replicate(c("Historical","0","0")) %>%
      #   t() %>%
      #   as.data.frame() %>%
      #   rename_with( ~ c("time","freq","severity")) %>%
      #   mutate(across(c(freq,severity), ~ as.numeric(.)))
      # 
      # historical_freq_sev <- historical_freq %>%
      #   uncount(freq, .remove = F) %>%
      #   cbind(historical_sev$sev) %>%
      #   rename("severity" = "historical_sev$sev" ) %>%
      #   select(all_of(c("time", "freq", "severity"))) %>%
      #   add_row(historical_0pad)
      # 
      # 
      # contemp_0pad <- length(which(contemporary_freq$freq == 0)) %>%
      #   replicate(c("Contemporary","0","0")) %>%
      #   t() %>%
      #   as.data.frame() %>%
      #   rename_with( ~ c("time","freq","severity")) %>%
      #   mutate(across(c(freq,severity), ~ as.numeric(.)))
      # contemporary_freq_sev <- contemporary_freq %>%
      #   uncount(freq, .remove = F) %>%
      #   cbind(contemporary_sev$sev) %>%
      #   rename("severity" = "contemporary_sev$sev" ) %>%
      #   select(all_of(c("time", "freq", "severity")))%>%
      #   add_row(contemp_0pad)
      # 
      # freq_bins <- seq(min(c(historical_freq_sev$freq,contemporary_freq_sev$freq)), max(c(historical_freq_sev$freq, contemporary_freq_sev$freq)),1)
      # sev_bins <- seq(0,3,length = 16)
      # sev_bins[1] <- -0.01
      # 
      # 
      # bin_df <- data.frame(bin = sev_bins)%>%
      #   mutate( index = row_number())
      # dx <- sev_bins[2]-sev_bins[1]
      # mi <- historical_freq_sev %>%
      #   mutate(bin = cut(severity, breaks = sev_bins, labels = F)) %>%
      #   group_by(freq, bin) %>%
      #   summarise(n = n()) %>%
      #   ungroup() %>%
      #   mutate( rel = n/sum(n)) %>%
      #   right_join(bin_df, by = join_by("bin" == "index")) %>%
      #   dplyr::select(freq, bin.y, rel) %>%
      #   rename(sev = bin.y)%>%
      #   mutate(rel = ifelse(is.na(rel),0,rel), sev = ifelse(sev == -.01, 0, sev),
      #          rel = ifelse(freq == 0, rel, rel/freq), rel = rel/sum(rel,na.rm = T)) %>%
      #   drop_na(rel) %>%
      #   select(rel, freq, sev)
      # 
      # mi_mat <- matrix(0, nrow = length(sev_bins), ncol = length(freq_bins))
      # for(a in 1:nrow(mi_mat)){
      #   sev_a <- sev_bins[a]
      #   if(sev_a == -.01){
      #     sev_a <- 0
      #   }
      #   for(b in 1:nrow(mi_mat)){
      #     freq_b <- freq_bins[b]
      #     skip_to_next <- FALSE
      #     tryCatch({
      #       mi_mat[(nrow(mi_mat)-a),b] <- mi %>%
      #       filter(freq == freq_b & sev == sev_a) %>% pull(rel)
      #       },
      #     error = function(e){skip_to_next <- TRUE})
      #     if(skip_to_next){next}
      # 
      #   }
      # }
      # 
      # fi <- contemporary_freq_sev %>%
      #   mutate(bin = cut(severity, breaks = sev_bins, labels = F)) %>%
      #   group_by(freq, bin) %>%
      #   summarise(n = n()) %>%
      #   ungroup() %>%
      #   mutate( rel = n/sum(n)) %>%
      #   right_join(bin_df, by = join_by("bin" == "index")) %>%
      #   dplyr::select(freq, bin.y, rel) %>%
      #   rename(sev = bin.y)%>%
      #   mutate(rel = ifelse(is.na(rel),0,rel), sev = ifelse(sev == -.01, 0, sev),
      #          rel = ifelse(freq == 0, rel, rel/freq), rel = rel/sum(rel,na.rm = T)) %>%
      #   drop_na(rel) %>%
      #   select(rel, freq, sev)
      # 
      # fi_mat <- matrix(0, nrow = length(sev_bins), ncol = length(freq_bins))
      # for(a in 1:nrow(mi_mat)){
      #   sev_a <- sev_bins[a]
      #   if(sev_a == -.01){
      #     sev_a <- 0
      #   }
      #   for(b in 1:nrow(mi_mat)){
      #     freq_b <- freq_bins[b]
      #     skip_to_next <- FALSE
      #     tryCatch({
      #       fi_mat[(nrow(mi_mat)-a),b] <- fi %>%
      #         filter(freq == freq_b & sev == sev_a) %>% pull(rel)
      #     },
      #     error = function(e){skip_to_next <- TRUE})
      #     if(skip_to_next){next}
      # 
      #   }
      # }
      # gridtriple_rules <- rbind(c(0, 3, .2), c(min(c(historical_freq_sev$freq,contemporary_freq_sev$freq)),
      #                                          max(c(historical_freq_sev$freq, contemporary_freq_sev$freq)),
      #                                          1)
      # )
      # mi_grid <- pgrid(mi_mat, boundary = c(0,max(freq_bins),0,3))
      # fi_grid <- pgrid(fi_mat, boundary = c(0,max(freq_bins),0,3))
      # twoD_transport <- transport(mi_grid, fi_grid, p = 1)
      # twoD_wass <- wasserstein(mi_grid, fi_grid, 1, tplan = twoD_transport) #Gila is 22/2.65 Cache is 1.6/1.3
      # plot(mi_grid,fi_grid, twoD_transport)
      # row.names(mi_mat) <- rev(sev_bins)
      # colnames(mi_mat) <- freq_bins
      # 
      # row.names(fi_mat) <- rev(sev_bins)
      # colnames(fi_mat) <- freq_bins
      # plot(log(mi_mat), main = "logDensity of Historical", xlab = "Frequency", ylab = "Severity")
      # plot(log(fi_mat), main = "logDensity of Contemporary", xlab = "Frequency", ylab = "Severity")
      # 

      
      check_freq_norm <- normalize_frequency(historical_freq,
                                      contemporary_freq,
                           freq_dat_norm = normalize_plots)
      check_sev_norm <- normalize_severity(historical_sev,
                                    contemporary_sev,
                         combined_sev_norm = normalize_plots)
      
      
      severity_average_contemporary_lst[k] <- mean(contemporary_sev$sev, na.rm = T)
      severity_average_historical_lst[k] <- mean(historical_sev$sev, na.rm = T)
      
      
      
      
      
      contemporary_relative_sev <- contemporary_sev_class[,.(n := .N, freq = n/sum(n)), by = .(sev)]
      historical_relative_sev <- historical_sev_class[,.(n := .N, freq = n/sum(n)), by = .(sev)]
      
      
      
      contemporary_relative_sev <- contemporary_sev_class %>%
        group_by(sev) %>%
        summarise(n = n())%>%
        mutate(freq = n/sum(n))
      historical_relative_sev <- historical_sev_class %>%
        group_by(sev) %>%
        summarise(n = n()) %>%
        mutate(freq = n/sum(n))
      
      

      
      
      fri_contemporary[k] <- year_range/mean(contemporary_freq$freq)
      fri_historical[k] <- year_range/mean(historical_freq$freq)
     
      
      pbhs_contemporary[k] <-as.numeric(contemporary_relative_sev %>%
                                          filter(sev == "High")%>%
                                          dplyr::select(freq))
      pbhs_historical[k] <- as.numeric(historical_relative_sev %>%
                                         filter(sev == "High")%>%
                                         dplyr::select(freq))
      #if we got normalized of both frequency and severity, then run otherwise, set to NA
     if(check_freq_norm == "Passed" & check_sev_norm == "Passed"){
      emd_freq_norm[k] <- emd_Calculation(contemporary_freq_norm,historical_freq_norm, freq = T )
      emd_sev_norm[k] <-  emd_Calculation(contemporary_sev_norm,historical_sev_norm, freq = F )
      emd_combined_norm[k] <- sqrt(emd_freq_norm[k]^2+emd_sev_norm[k]^2)
      
      
      #build multivariate distribs
      # hist_freq_mean <- mean(historical_freq$freq)
      # hist_freq_sd <- sd(historical_freq$freq)
      # hist_sev_mean <- mean(historical_sev$sev)
      # hist_sev_sd <- sd(historical_sev$sev)
      # 
      # o_check <- ((0-hist_freq_mean)/hist_freq_sd)
      # historical_0pad <- length(which(historical_freq_norm$freq == o_check)) %>%
      #   replicate(c("Historical",rep(as.character(o_check),2))) %>%
      #   t() %>%
      #   as.data.frame() %>%
      #   rename_with( ~ c("time","freq","severity")) %>%
      #   mutate(across(c(freq,severity), ~ as.numeric(.)))
      # 
      # historical_freq_sev <- historical_freq %>%
      #   uncount(freq, .remove = F) %>%
      #   cbind(historical_sev_norm$sev) %>%
      #   rename("severity" = "historical_sev_norm$sev" ) %>%
      #   select(all_of(c("time", "freq", "severity"))) %>%
      #   mutate(freq = (freq-hist_freq_mean)/hist_freq_sd) %>%
      #   add_row(historical_0pad)
      # 
      # 
      # contemporary_0pad <- length(which(contemporary_freq$freq == 0)) %>%
      #   replicate(c("Historical",rep(as.character(o_check),2))) %>%
      #   t() %>%
      #   as.data.frame() %>%
      #   rename_with( ~ c("time","freq","severity")) %>%
      #   mutate(across(c(freq,severity), ~ as.numeric(.)))
      # contemporary_freq_sev <- contemporary_freq %>%
      #   uncount(freq, .remove = F) %>%
      #   cbind(contemporary_sev_norm$sev) %>%
      #   rename("severity" = "contemporary_sev_norm$sev" ) %>%
      #   select(all_of(c("time", "freq", "severity"))) %>%
      #   mutate(freq = (freq-hist_freq_mean)/hist_freq_sd) %>%
      #   add_row(contemporary_0pad)
      # 
      # freq_bins <- unique(c(unique(contemporary_freq_sev$freq),unique(historical_freq_sev$freq)))
      # sev_bins <- seq(((0-hist_sev_mean)/hist_sev_sd),((3-hist_sev_mean)/hist_sev_sd),length = 16)
      # sev_bins[1] <- sev_bins[1]-0.01
      # gridtriple_rules <- rbind(c(0, 3, .2), c(min(c(historical_freq_sev$freq,contemporary_freq_sev$freq)),
      #                                          max(c(historical_freq_sev$freq, contemporary_freq_sev$freq)),
      #                                          1)
      # )
      # 
      # bin_df <- data.frame(bin = sev_bins)%>%
      #   mutate( index = row_number())
      # dx <- sev_bins[2]-sev_bins[1]
      # mi <- historical_freq_sev %>%
      #   mutate(bin = cut(severity, breaks = sev_bins, labels = F)) %>%
      #   group_by(freq, bin) %>%
      #   summarise(n = n()) %>%
      #   ungroup() %>%
      #   mutate( rel = n/sum(n)) %>%
      #   right_join(bin_df, by = join_by("bin" == "index")) %>%
      #   dplyr::select(freq, bin.y, rel) %>%
      #   rename(sev = bin.y)%>%
      #   mutate(rel = ifelse(is.na(rel),0,rel), sev = ifelse(sev == sev_bins[1], 0, sev),
      #          rel = ifelse(freq == sev_bins[1], rel, rel/freq), rel = rel/sum(rel,na.rm = T)) %>%
      #   drop_na(rel) %>%
      #   select(rel, freq, sev)
      # 
      # mi_mat <- matrix(0, nrow = length(sev_bins), ncol = length(freq_bins))
      # for(a in 1:nrow(mi_mat)){
      #   sev_a <- sev_bins[a]
      #   if(sev_a == sev_bins[1]){
      #     sev_a <- 0
      #   }
      #   for(b in 1:nrow(mi_mat)){
      #     freq_b <- freq_bins[b]
      #     skip_to_next <- FALSE
      #     tryCatch({
      #       mi_mat[(nrow(mi_mat)-a),b] <- mi %>%
      #         filter(freq == freq_b & sev == sev_a) %>% pull(rel)
      #     },
      #     error = function(e){skip_to_next <- TRUE})
      #     if(skip_to_next){next}
      # 
      #   }
      # }
      # 
      # fi <- contemporary_freq_sev %>%
      #   mutate(bin = cut(severity, breaks = sev_bins, labels = F)) %>%
      #   group_by(freq, bin) %>%
      #   summarise(n = n()) %>%
      #   ungroup() %>%
      #   mutate( rel = n/sum(n)) %>%
      #   right_join(bin_df, by = join_by("bin" == "index")) %>%
      #   dplyr::select(freq, bin.y, rel) %>%
      #   rename(sev = bin.y)%>%
      #   mutate(rel = ifelse(is.na(rel),0,rel), sev = ifelse(sev == sev_bins[1], 0, sev),
      #          rel = ifelse(freq == sev_bins[1], rel, rel/freq), rel = rel/sum(rel,na.rm = T)) %>%
      #   drop_na(rel) %>%
      #   select(rel, freq, sev)
      # 
      # fi_mat <- matrix(0, nrow = length(sev_bins), ncol = length(freq_bins))
      # for(a in 1:nrow(mi_mat)){
      #   sev_a <- sev_bins[a]
      #   if(sev_a == sev_bins[1]){
      #     sev_a <- 0
      #   }
      #   for(b in 1:nrow(mi_mat)){
      #     freq_b <- freq_bins[b]
      #     skip_to_next <- FALSE
      #     tryCatch({
      #       fi_mat[(nrow(mi_mat)-a),b] <- fi %>%
      #         filter(freq == freq_b & sev == sev_a) %>% pull(rel)
      #     },
      #     error = function(e){skip_to_next <- TRUE})
      #     if(skip_to_next){next}
      # 
      #   }
      # }
      # mi_grid <- pgrid(mi_mat, boundary = c(min(freq_bins),max(freq_bins),min(sev_bins),max(sev_bins)))
      # fi_grid <- pgrid(fi_mat, boundary = c(min(freq_bins),max(freq_bins),min(sev_bins),max(sev_bins)))
      # twoD_transport <- transport(mi_grid, fi_grid, p = 1)
      # twoD_wass <- wasserstein(mi_grid, fi_grid, 1, tplan = twoD_transport) #Gila was 14/1.27 Cache was 1.47/1.33
      # plot(mi_grid,fi_grid, twoD_transport)
      # row.names(mi_mat) <- rev(sev_bins)
      # colnames(mi_mat) <- freq_bins
      # 
      # row.names(fi_mat) <- rev(sev_bins)
      # colnames(fi_mat) <- freq_bins
      # plot(log(mi_mat), main = "logDensity of normalized Historical", xlab = "Frequency", ylab = "Severity")
      # plot(log(fi_mat), main = "logDensity of normalized Contemporary", xlab = "Frequency", ylab = "Severity")


      
      
      
      
      
      }else{
        emd_freq_norm[k] <- NA
        emd_sev_norm[k] <- NA
        emd_combined_norm[k] <- NA
      }
      
      #if we are at iterations below n.lines, then add our data to the initialized plots
      #TODO
      if(completed.freq <=n.lines){
        freq_dat_4plot <- freq_dat %>%
          mutate( 
            freq_start = ifelse(time == "Historical", freq-((freq[2]-freq[1])/2), freq),
            freq_end = ifelse(time == "Contemporary",freq+((freq[2]-freq[1])/2),freq)
          )
        completed.freq <- completed.freq + 1
        freq_compare_bar <- freq_compare_bar +
          geom_segment( aes(x = freq_start,y = relfreq, xend = freq_end,yend = relfreq,
                            color = time),data = freq_dat_4plot,
                        alpha = alpha.lines, linewidth = .7
          ) 
        rm(freq_dat_4plot)
      }
        

       if(completed.sev <=n.lines){
         completed.sev <- completed.sev + 1
        sev_density <- sev_density + 
          stat_density(aes(x = sev,  color = time),combined_sev, position = "identity",  geom = "line",alpha =alpha.lines)
        
        freq_sev_each_4plot <-  combined_sev_class %>%
          mutate(sev = as.numeric(sev), 
                 sev_start = ifelse(time == "Historical", sev-.5,sev),
                 sev_end = ifelse(time == "Contemporary",sev+.5,sev))
        
        freq_sev_each <- freq_sev_each+
          geom_segment(aes(x = sev_start, xend = sev_end, y = relfreq, yend = relfreq, color = time),
                       freq_sev_each_4plot,
                       alpha = alpha.lines, linewidth = .7)
        
        
        
        
        freq_sev_lm2h_4plot <-  combined_sev_class %>%
          mutate(sev = recode(sev, Low = "Low/Mixed", Mixed = "Low/Mixed"))%>%
          group_by(time, sev)%>%
          summarize(n = sum(n), relfreq = sum(relfreq))%>%
          mutate(sev = as.numeric(sev), 
                 sev_start = ifelse(time == "Historical", sev-.5,sev),
                 sev_end = ifelse(time == "Contemporary",sev+.5,sev))
        
        freq_sev_lm2h <- freq_sev_lm2h+
          geom_segment(aes(x = sev_start, xend = sev_end, y = relfreq, yend = relfreq, color = time),
                       freq_sev_lm2h_4plot,
                       alpha = alpha.lines, linewidth = .7)
        
        rm(freq_sev_lm2h_4plot)
        gc()
       }
      
      
      rm(contemporary_sample_freq,historical_sample, historical_sample_freq,
         contemporary_sample_sev, contemporary_freq,historical_freq,
         contemporary_sev, historical_sev)
      gc()
      } else{ #if we fail the check for contemporary severity data only do emd on frequency
        emd_freq[k] <- emd_Calculation(contemporary_freq,historical_freq, freq = T )
        emd_sev[k] <- NA
        emd_combined[k] <- NA
        
        
        
        fri_contemporary[k] <- year_range/mean(contemporary_freq$freq)
        fri_historical[k] <- year_range/mean(historical_freq$freq)
        pbhs_contemporary[k] <-NA
        pbhs_historical[k] <- NA
        emd_sev_norm[k] <-  NA
        emd_combined_norm[k] <- NA
        
        check_freq_norm <- normalize_frequency(historical_freq,
                                          contemporary_freq,
                                          freq_dat_norm = normalize_plots)
        
        
        if(check_freq_norm == "Passed"){
          
          
          emd_freq_norm[k] <- emd_Calculation(contemporary_freq_norm,historical_freq_norm, freq = T )
          
        }else{
        
        emd_freq_norm[k] <- NA
        
        }
        if(completed.freq <=n.lines){
            freq_dat_4plot <- freq_dat %>%
              mutate( 
                freq_start = ifelse(time == "Historical", freq-((freq[2]-freq[1])/2), freq),
                freq_end = ifelse(time == "Contemporary",freq+((freq[2]-freq[1])/2),freq)
              )
            completed.freq <- completed.freq + 1
            freq_compare_bar <- freq_compare_bar +
              geom_segment( aes(x = freq_start,y = relfreq, xend = freq_end,yend = relfreq,
                                color = time),data = freq_dat_4plot,
                            alpha = alpha.lines, linewidth = .7
              ) 
            
            gc()
            
           
            
            
            
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
    
    emd_freq_min <- min(emd_freq, na.rm = T)
    emd_sev_min <- min(emd_sev, na.rm = T)
    emd_combined_min <- min(emd_combined, na.rm = T)
    emd_freq_norm_min <- min(emd_freq_norm, na.rm = T)
    emd_sev_norm_min <- min(emd_sev_norm, na.rm = T)
    emd_combined_norm_min <- min(emd_combined_norm, na.rm = T)
    
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
                   
                   "emd median frequency",
                   "emd median severity",
                   "emd median both",
                   "emd median frequency normalized",
                   "emd median severity normalized",
                   "emd median both normalized",
                   
                   "emd minimum frequency",
                   "emd minimum severity",
                   "emd minimum both",
                   "emd minimum frequency normalized",
                   "emd minimum severity normalized",
                   "emd minimum both normalized",
                   
                   "FRCC freq dep",
                   "FRCC sev dep",
                   "FRCC reg dep",
                   "historical area burned ha",
                   
                   "median historical severity",
                   "median contemporary severity")
    #statistics
    stats <- c(
      name_unit,
      sample.size,
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
      
      emd_freq_min, 
      emd_sev_min, 
      emd_combined_min,
      emd_freq_norm_min, 
      emd_sev_norm_min ,
      emd_combined_norm_min,
      
      
      
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
    mean_freq <- year_range/c(fri_historical_med,fri_contemporary_med)
   
    freq_compare_bar <- freq_compare_bar+
    geom_vline(xintercept = mean_freq, linetype = "dotted", linewidth = .6, color = colors2)
    
  
    ggsave(filename = paste0(figure_folder,"/frequency_bar_",dir_name,".pdf"),freq_compare_bar, width = 1280, height = 720, units = "px")
    
    
    
    ggsave(filename = paste0(figure_folder,"/Severity_Density_",dir_name,".pdf"), sev_density, width = 1280, height = 720, units = "px")
    
    
     
     ggsave(filename = paste0(figure_folder,"/Severity_class_all_",dir_name,".pdf"),freq_sev_each, width = 1280, height = 720, units = "px")
     
    
  
    ggsave(filename = paste0(figure_folder,"/Severity_class_lm2h_",dir_name,".pdf"),freq_sev_lm2h, width = 1280, height = 720, units = "px")
    
    
    #calulcate the relative frequencies of BPS models from our sample
    bps_breakdown <- historical_subset_data[,.(n := .N, relfreq := n/sum(n)), by = .(BPS_MODEL)
                                            ][,order(-relfreq)
                                              ][, .I[1:10]] |>
                                                merge(_,historical_subset_data, on = .(BPS_MODEL), all.x = T) |>
                                                unique(_, by = "BPS_MODEL")
    
   
    
    bps_breakdown <- historical_subset_data %>%
      dplyr::group_by(BPS_MODEL) %>%
      dplyr::summarize(n = n()) %>%
      dplyr::mutate(relfreq = n/sum(n)) %>%
      dplyr::arrange(desc(relfreq))%>%
      head(.,10) %>%
      dplyr::left_join(x = .,y = historical_subset_data , by = "BPS_MODEL")%>%
      dplyr::distinct(BPS_MODEL, .keep_all = T) 
      
    #stats for an individual landscape
    individual_stats <- list(stat_df,bps_breakdown,freq_compare_bar,sev_density,freq_sev_each,freq_sev_lm2h)
    rm(freq_compare_bar, sev_density, freq_sev_each,freq_sev_lm2h,historical_subset_data,
       contemporary_subset_data_freq, contemporary_subset_data_sev,srs,
       contemporary_sample_sev, historical_sample,contemporary_sample_freq)
    
    stat_df <- list(stats = stat_df, top_bps_models = bps_breakdown)
    
    #mark of completion
    current_date <- Sys.Date()
    
    unlink(paste0(output_name,"/",list.files(output_name, ".txt")))
    write.table(matrix(paste0("Completed ",current_date)), file = paste0(output_name,"/Completed_,",current_date,".txt"), append = F)
    
    #should write a GZ for each landscape but is super heavy and overflows storage
    if(write_individual_gz == TRUE){
    save(individual_stats, file = paste0(output_name,"/",name_unit,"_stats.gz"))
    }
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
  dplyr::mutate(emd_freq_median_cat = case_when(emd_median_frequency <= quantile(valid_df$emd_median_frequency, 0.33) ~ 'A',
                                         emd_median_frequency > quantile(valid_df$emd_median_frequency, 0.33) & 
                                           emd_median_frequency <= quantile(valid_df$emd_median_frequency, 0.66) ~ 'B',
                                         emd_median_frequency > quantile(valid_df$emd_median_frequency, 0.66) ~ 'C'),
         emd_sev_median_cat = case_when(emd_median_severity <= quantile(valid_df$emd_median_severity, 0.33) ~ '3',
                                        emd_median_severity > quantile(valid_df$emd_median_severity, 0.33) & 
                                          emd_median_severity <= quantile(valid_df$emd_median_severity, 0.66) ~ '2',
                                        emd_median_severity > quantile(valid_df$emd_median_severity, 0.66) ~ '1'),
         
         emd_freq_median_norm_cat = case_when(emd_median_frequency_normalized <= quantile(valid_df$emd_median_frequency_normalized, 0.33) ~ 'A',
                                              emd_median_frequency_normalized > quantile(valid_df$emd_median_frequency_normalized, 0.33) & 
                                                emd_median_frequency_normalized <= quantile(valid_df$emd_median_frequency_normalized, 0.66) ~ 'B',
                                              emd_median_frequency_normalized > quantile(valid_df$emd_median_frequency_normalized, 0.66) ~ 'C'),
         emd_sev_median_norm_cat = case_when(emd_median_severity_normalized <= quantile(valid_df$emd_median_severity_normalized, 0.33) ~ '3',
                                             emd_median_severity_normalized > quantile(valid_df$emd_median_severity_normalized, 0.33) & 
                                               emd_median_severity_normalized <= quantile(valid_df$emd_median_severity_normalized, 0.66) ~ '2',
                                             emd_median_severity_normalized > quantile(valid_df$emd_median_severity_normalized, 0.66) ~ '1'),
         
        
         #
         #
         emd_freq_minimum_cat = case_when(emd_minimum_frequency <= quantile(valid_df$emd_minimum_frequency, 0.33) ~ 'A',
                                          emd_minimum_frequency > quantile(valid_df$emd_minimum_frequency, 0.33) & 
                                            emd_minimum_frequency <= quantile(valid_df$emd_minimum_frequency, 0.66) ~ 'B',
                                          emd_minimum_frequency > quantile(valid_df$emd_minimum_frequency, 0.66) ~ 'C'),
         emd_sev_minimum_cat = case_when(emd_minimum_severity <= quantile(valid_df$emd_minimum_severity, 0.33) ~ '3',
                                         emd_minimum_severity > quantile(valid_df$emd_minimum_severity, 0.33) & 
                                           emd_minimum_severity <= quantile(valid_df$emd_minimum_severity, 0.66) ~ '2',
                                         emd_minimum_severity > quantile(valid_df$emd_minimum_severity, 0.66) ~ '1'),
         
         emd_freq_minimum_norm_cat = case_when(emd_minimum_frequency_normalized <= quantile(valid_df$emd_minimum_frequency_normalized, 0.33) ~ 'A',
                                               emd_minimum_frequency_normalized > quantile(valid_df$emd_minimum_frequency_normalized, 0.33) & 
                                                 emd_minimum_frequency_normalized <= quantile(valid_df$emd_minimum_frequency_normalized, 0.66) ~ 'B',
                                               emd_minimum_frequency_normalized > quantile(valid_df$emd_minimum_frequency_normalized, 0.66) ~ 'C'),
         emd_sev_minimum_norm_cat = case_when(emd_minimum_severity_normalized <= quantile(valid_df$emd_minimum_severity_normalized, 0.33) ~ '3',
                                              emd_minimum_severity_normalized > quantile(valid_df$emd_minimum_severity_normalized, 0.33) & 
                                                emd_minimum_severity_normalized <= quantile(valid_df$emd_minimum_severity_normalized, 0.66) ~ '2',
                                              emd_minimum_severity_normalized > quantile(valid_df$emd_minimum_severity_normalized, 0.66) ~ '1'),
         
        
         signed_emd_median_frequency_normalized = sign(pfrid)*emd_median_frequency_normalized,
         signed_emd_median_frequency = sign(pfrid)*emd_median_frequency,
         
         signed_emd_median_severity_normalized = sign(percent_severity_departure)*emd_median_severity_normalized,
         signed_emd_median_severity= sign(percent_severity_departure)*emd_median_severity,
         
         signed_emd_minimum_frequency_normalized = sign(pfrid)*emd_minimum_frequency_normalized,
         signed_emd_minimum_frequency = sign(pfrid)*emd_minimum_frequency,
         
         signed_emd_minimum_severity_normalized = sign(percent_severity_departure)*emd_minimum_severity_normalized,
         signed_emd_minimum_severity= sign(percent_severity_departure)*emd_minimum_severity
         
         
         
  ) %>%
  tidyr::unite('bicat_median', emd_freq_median_cat, emd_sev_median_cat) %>%
  tidyr::unite('bicat_median_norm',emd_freq_median_norm_cat,emd_sev_median_norm_cat)%>%
  dplyr::mutate('numcat_median' = as.numeric(factor(bicat_median)), 'numcat_median_norm' = as.numeric(factor(bicat_median_norm)))%>%
  tidyr::unite('bicat_minimum', emd_freq_minimum_cat, emd_sev_minimum_cat) %>%
  tidyr::unite('bicat_minimum_norm',emd_freq_minimum_norm_cat,emd_sev_minimum_norm_cat)%>%
  dplyr::mutate('numcat_minimum' = as.numeric(factor(bicat_minimum)), 'numcat_minimum_norm' = as.numeric(factor(bicat_minimum_norm)))

terra::writeVector(terra::vect(valid_sf), filename = paste0(out_file,"/",vectorOutputName,".gpkg"), overwrite = T)


  return(valid_sf) #SAVE STORED_DATA (all landscapes looped through) 
}
