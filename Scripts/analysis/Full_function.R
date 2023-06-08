pkgs <- c("foreach", "doParallel","sampling","tidyverse", "terra", "sf",  "units", "RColorBrewer","ggrepel")
invisible(lapply(pkgs, library, character.only = T))

Calculate_fire_regime_and_departure <- function(bps_rast_path,
                                                bps_csv_path,
                                                fire_path_folder,
                                                mask_polygon_path,
                                                fire_polygon_path,
                                                output_path,
                                                display_name,
                                                write_year_raster_out = F,
                                                n.cores = 1,
                                                p.area = 0.001,
                                                n.iter = 100,
                                                contemporary4historical = F,
                                                buffer_tune = -150,
                                                colors2 = RColorBrewer::brewer.pal(5, "RdYlBu")[c(5,2)],
                                                n.lines = 30,
                                                alpha.lines = .1,
                                                write_individual_gz = FALSE){
 bps <- rast(bps_rast_path)
 bps_csv <- read_csv(bps_csv_path)
if(class(mask_polygon_path)[1] == "sf"){
   mask_all <- mask_polygon_path %>%
     st_transform(st_crs(bps))
 }else{
     mask_all <- st_read(mask_polygon_path) %>%
        st_transform(st_crs(bps))
 }
 all_names <- c(mask_all[,display_name, drop = T]) 
 mask_units <- all_names %>% 
    unique()
  
 fire_perim_all <- st_read(fire_polygon_path) %>%
   st_transform(st_crs(bps))
 
cores <- n.cores
registerDoParallel(cores)
#bps <- crop(bps, vect(mask_all), mask = T)
bps <- wrap(bps)


stored_data <- foreach(i = 1:length(mask_units), .export = c("mask_all","fire_path_folder",
                                                             "bps", "n.iter", "bps_csv", 
                                                             "mask_units", "fire_perim_all"),
                       .packages = pkgs[c(-1,-2)],
                       .inorder = FALSE, .errorhandling = "pass") %dopar%{
 module_path <- "scripts/modules/"
 module_names <- list.files(module_path)
 source_paths = paste0(module_path,module_names)
                         
 invisible(lapply(source_paths,source))
                         
                         
    
    name_unit <- mask_units[i]
    dir_name <- gsub(" ", "_", name_unit)
      output_name <- paste0(output_path,"/",dir_name)
    dir.create(output_name, showWarnings = F)
    
    
    
    mask <- mask_all[which(all_names == name_unit),] %>%
      st_union()
    perims <- st_intersection(fire_perim_all, mask)%>%
      arrange(Fire_Year) %>%
      filter(Fire_Year >=year_caps[1] & Fire_Year <= year_caps[2])
    
    if(nrow(perims) == 0){
      individual_stats <- list("No contemporary fire, cannot analyze")
      save(individual_stats, file = paste0(output_name,"/",name_unit,"_stats.RData"))
      write.table(matrix("Cannot Analyze"), file = paste0(output_name,"/CANT_ANALYZE.txt"), append = F)
      return(individual_stats)
      
    }
    
      
    yearly_rasters_folder <- paste0(output_name,"/fires")
    
    figure_folder <- paste0(output_name,"/figures")
    dir.create(yearly_rasters_folder, showWarnings = F)
    dir.create(figure_folder, showWarnings = F)
    
    bps <- unwrap(bps)
   bps_mask <- crop(bps, vect(mask), mask = T)
    buff <- st_buffer(perims, buffer_tune)
    
    list_names <- perims %>%
      st_drop_geometry %>%
      dplyr::select(Fire_Year, Event_ID)
    
    list_names$Tif <- paste0(list_names$Event_ID, "_CBI_bc.tif")
    
    all_years <- unique(perims$Fire_Year)
    
    year_ids <- list(year = all_years, IDs = NULL)
    
    year_pattern <- paste0(all_years, "_30m.tif")
    year_files <- list.files(yearly_rasters_folder, paste0(year_pattern,collapse = "|"))
    if(write_year_raster_out == TRUE & length(all_years) == length(year_files)){
      check_fires <- TRUE
    }else{
      check_fires <- FALSE
    }
    
    if(write_year_raster_out == TRUE & check_fires ==FALSE ){
      create_yearly_rasters(list_names,
                            all_years,
                            dir_name,
                            buff,
                            bps_mask,
                            fire_path_folder,
                            yearly_rasters_folder)                  
    }
    rm(list_names)
    mosaic_stack_30m <- rast(paste0(yearly_rasters_folder,"/",dir_name,"_",all_years,"_30m.tif"))
    names(mosaic_stack_30m) <- all_years
    
    Sampling_scheme(bps_mask,
                    mask,
                    perims,
                    mosaic_stack_30m,
                    bps_csv,
                    proportion = p.area,
                    n.iter)
     #fire freq map
    freq_map_colorPalette <- rev(RColorBrewer::brewer.pal(9, "YlOrRd"))
    freq_map_length <- minmax(freq_map)[2]-minmax(freq_map)[1]
    
    freq_map_colorPalette <- c("#1F1F46",freq_map_colorPalette[1:freq_map_length])
    jpeg(file = paste0(figure_folder,"/frequency_map_",dir_name,".jpg"), width = 1920, height = 1080, pointsize = 20)
    freq_map <- crop(freq_map, vect(mask), mask = T)
    plot(freq_map, col = freq_map_colorPalette, type = "classes")
    plot(vect(mask), lwd = 2, add = T)
    dev.off()
    
        #bps_map
    jpeg(file = paste0(figure_folder,"/bps_",dir_name,".jpg"), width = 1920, height = 1080, pointsize = 20)
    activeCat(bps_mask) <- 4
    plot(bps_mask, mar = c(3.1, 3.1,3.1,35))
    plot(vect(mask), lwd = 2, add = T)
    dev.off()
    
   rm(bps_mask, freq_map,mosaic_stack_30m)
    
   
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
    
    
    for(k in 1:n.iter){
       sample_pixels( k,
                      contemporary_subset_data_sev,
                      contemporary_subset_data_freq,
                      historical_subset_data)
      
      frgs <- unlist(strsplit(historical_subset_data$FRG_NEW,"-"))
      frg1s <- frgs[which(frgs == "I")]
      
      proportion_FRGI[k] <-length(frg1s) / dim(historical_subset_data)[1]
      rm(frgs, frg1s)
      
      create_frequency_distributions (contemporary_sample_freq,
                                      historical_sample)
      create_severity_distributions(historical_sample,
                                    historical_sample_freq,
                                    contemporary_sample_sev,
                                    contemporary4historical = contemporary4historical,
                                    k)
      emd_freq[k] <- create_emd_hungarian_algo(contemporary_freq,historical_freq, freq = T ,bin_vec = year_range)
      emd_sev[k] <-  create_emd_hungarian_algo(contemporary_sev,historical_sev, freq = F )
      
      contemporary_relative_sev <- contemporary_sev_class %>%
        group_by(sev) %>%
        summarise(n = n())%>%
        mutate(freq = n/sum(n))
      historical_relative_sev <- historical_sev_class %>%
        group_by(sev) %>%
        summarise(n = n()) %>%
        mutate(freq = n/sum(n))
      
      

      emd_combined <- sqrt(emd_freq[k]^2+emd_sev[k]^2)
      
      fri_contemporary[k] <- year_range/mean(contemporary_freq$freq)
      fri_historical[k] <- year_range/mean(historical_freq$freq)
     
      
      pbhs_contemporary[k] <-as.numeric(contemporary_relative_sev %>%
                                          filter(sev == "High")%>%
                                          dplyr::select(freq))
      pbhs_historical[k] <- as.numeric(historical_relative_sev %>%
                                         filter(sev == "High")%>%
                                         dplyr::select(freq))
      if(k == 1){
      freq_compare_bar <- ggplot()+
        ggtitle(paste0(name_unit, " Frequency Bar Plot")) +
        xlab("Fire Frequency")+
        ylab("Relative Frequency")+
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
      
      
      
      
      
      
      freq_sev_each <- ggplot()+
        
        ggtitle(paste0(name_unit, " Severity Class Bar Plot"))+
        xlab("Severity Class")+
        ylab("Relative Frequency")+
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
        ylab("Relative Frequency")+
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
      if(k <=n.lines){
      freq_dat_4plot <- freq_dat %>%
        mutate( 
               freq_start = ifelse(time == "Historical", freq-.5, freq),
               freq_end = ifelse(time == "Contemporary",freq+.5,freq)
               )
      freq_compare_bar <- freq_compare_bar +
        geom_segment( aes(x = freq_start,y = relfreq, xend = freq_end,yend = relfreq,
                         color = time),data = freq_dat_4plot,
                         alpha = alpha.lines
                         ) 
      rm(freq_dat_4plot)
      sev_density <- sev_density + 
        stat_density(aes(x = sev,  color = time),combined_sev, position = "identity",  geom = "line",alpha =alpha.lines)
      
      freq_sev_each_4plot <-  combined_sev_class %>%
        mutate(sev = as.numeric(sev), 
               sev_start = ifelse(time == "Historical", sev-.5,sev),
               sev_end = ifelse(time == "Contemporary",sev+.5,sev))
      
      freq_sev_each <- freq_sev_each+
        geom_segment(aes(x = sev_start, xend = sev_end, y = relfreq, yend = relfreq, color = time),
                     freq_sev_each_4plot,
                     alpha = alpha.lines)
      
      
      
      
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
                     alpha = alpha.lines)
      
      rm(freq_sev_lm2h_4plot)
      gc()
      }
      
      rm(contemporary_sample_freq,historical_sample, historical_sample_freq,
         contemporary_sample_sev, contemporary_freq,historical_freq,
         contemporary_sev, historical_sev)
      gc()
      
    }
    emd_freq_med <- quantile(emd_freq,.5, names = F, na.rm = T)
    emd_sev_med <- quantile(emd_sev,.5, names = F, na.rm = T)
    emd_combined_med <- quantile(emd_combined,.5, names = F, na.rm = T)
    
   
    focal_area_ha <- st_area(mask) %>%
      set_units(ha) %>%
      as.numeric()
    fire_area_ha <- sum(st_area(perims) %>%
      set_units(ha)) %>%
      as.numeric
    proportion_burned <- fire_area_ha/focal_area_ha
    
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
    
    sev_ratio_departure <- pbhs_contemporary_med/(1-pbhs_contemporary_med)-pbhs_historical_med/(1-pbhs_historical_med)
    
    frequency_departure_frcc <- (1-(pmin(fri_contemporary_med,fri_historical_med)/pmax(fri_contemporary_med,fri_historical_med)))*100
    severity_departure_frcc <- (1-(pmin(pbhs_contemporary_med,pbhs_historical_med)/pmax(pbhs_contemporary_med,pbhs_historical_med)))*100
    regime_departure_frcc <- (severity_departure_frcc+frequency_departure_frcc)/2
    stat_names <-c("name",
                   "sample size",
                   "study area (ha)",
                   "area burned (ha)",
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
                   
                   "sev_ratio_departure",
                   
                   "emd frequency",
                   "emd severity",
                   "emd both",
                   
                   "FRCC freq dep",
                   "FRCC sev dep",
                   "FRCC reg dep")
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
      
      sev_ratio_departure,
      
      emd_freq_med, 
      emd_sev_med, 
      emd_combined_med,
      
      frequency_departure_frcc,
      severity_departure_frcc,
      regime_departure_frcc
    )
    stat_df <- data.frame(stat = stat_names, value = stats) 
    
    
    
    mean_freq <- year_range/c(fri_historical_med,fri_contemporary_med)
   
    freq_compare_bar <- freq_compare_bar+
    geom_vline(xintercept = mean_freq, linetype = "dashed", linewidth = 1, color = colors2)
    
  
    ggsave(filename = paste0(figure_folder,"/frequency_bar_",dir_name,".jpg"),freq_compare_bar, width = 1280, height = 720, units = "px")
    
    
    
    ggsave(filename = paste0(figure_folder,"/Severity_Density_",dir_name,".jpg"), sev_density, width = 1280, height = 720, units = "px")
    
    
     
     ggsave(filename = paste0(figure_folder,"/Severity_class_all_",dir_name,".jpg"),freq_sev_each, width = 1280, height = 720, units = "px")
     
    
  
    ggsave(filename = paste0(figure_folder,"/Severity_class_lm2h_",dir_name,".jpg"),freq_sev_lm2h, width = 1280, height = 720, units = "px")
    
    
    bps_breakdown <- historical_subset_data %>%
      group_by(BPS_MODEL) %>%
      summarize(n = n()) %>%
      mutate(relfreq = n/sum(n)) %>%
      dplyr::arrange(desc(relfreq))%>%
      head(.,10) %>%
      left_join(x = .,y = historical_subset_data , by = "BPS_MODEL")%>%
      dplyr::distinct(BPS_MODEL, .keep_all = T) 
      
    individual_stats <- list(stat_df,bps_breakdown,freq_compare_bar,sev_density,freq_sev_each,freq_sev_lm2h)
    rm(freq_compare_bar, sev_density, freq_sev_each,freq_sev_lm2h,historical_subset_data,
       contemporary_subset_data_freq, contemporary_subset_data_sev,srs,
       contemporary_sample_sev, historical_sample,contemporary_sample_freq)
    
    stat_df <- list(stats = stat_df, top_bps_models = bps_breakdown)
    
    
    current_date <- Sys.Date()
    
    unlink(paste0(output_name,"/",list.files(output_name, ".txt")))
    write.table(matrix(paste0("Completed ",current_date)), file = paste0(output_name,"/Completed_,",current_date,".txt"), append = F)
    
    
    if(write_individual_gz == TRUE){
    saveRDS(individual_stats, file = paste0(output_name,"/",name_unit,"_stats.gz"))
    }
      gc()
      return(c(stat_df))
        }
stopImplicitCluster()

save(stored_data, file = paste0(output_path,"/!stored_data.RData"))
  return(stored_data)
}
