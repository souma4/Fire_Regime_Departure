pkgs <- c("sf", "tidyverse", "terra","RColorBrewer", "tmap", "units", "ggrepel","ggrepel","plotly","pdftools")
invisible(lapply(pkgs, library, character.only = T))

check_data <- function(data){
  out <- vector(length = length(data))
  for(i in 1:length(data)){
    check <- try(data[[i]][[1]][[1,2]], silent = T)
    
    
    out[i] <- ifelse(inherits(check, "try-error"), "invalid", check)
  }
  valid_indices <- which(out != "invalid")
  
  
  stored_data_valid <- data[valid_indices]
  return(stored_data_valid)
}
production_pulling <- function(stored_data_valid){
  
  len <- length(stored_data_valid)
  new_cols <- stored_data_valid[[1]][[1]][,1]
  new_cols <- gsub(" ","_", new_cols)
  full_df <- data.frame(index = 1:len)
  full_df[,new_cols] <- NA
  for(i in 1:len){
    interdat <- stored_data_valid[[i]][[1]][,2]
    full_df[i, 2] <- interdat[1]
    full_df[i,3:(length(new_cols)+1)] <- as.numeric(interdat[-1])
  }
  return(full_df)
  
}
density_template <- function(name, data, log = F){
  if(log == T){
    plot <- ggplot(data.frame(name = data),aes(x = name))+
      geom_density()+
      labs(x = "Value",
           y = "Density",
           title = paste0(name, " LogDensity"))+
      scale_x_continuous(trans='log10')+
      theme_classic()
  }else{plot <- ggplot(data.frame(name = data),aes(x = name))+
    geom_density()+
    labs(x = "Value",
         y = "Density",
         title = paste0(name, " Density"))+
    theme_classic()
  }
  return(plot)
}

production_figures_analysis <- function(data_list,masks,context_sf, output_path, mask_join_name){
  
  out_file <- paste0(output_path,"/!summaries")
  dir.create(out_file, showWarnings = F)
  
  masks <- as.data.frame(masks)
  name_index <- which(names(masks) == mask_join_name)
  names(masks)[name_index] <- "name"
  masks <- st_as_sf(masks)
  valid_data <- check_data(data_list)
  
  
  valid_df <- production_pulling(valid_data) %>%
    drop_na() %>%
    distinct(name, .keep_all = T)
  valid_sf <- st_as_sf(left_join(valid_df, masks, by = "name"))
  #############
  ##############
  #############   summary stats
  #############
  ###########
  
  means <- base::colMeans(valid_df[,-2], na.rm = T)
  vars <- base::apply(valid_df[,-2], 2, var, na.rm = T)
  meds <- base::apply(valid_df[,-2],2, median, na.rm = T)
  mins <- apply(valid_df[,-2],2,min,na.rm=T)
  maxs <- apply(valid_df[,-2],2,max,na.rm=T)
  summary_stats <- as.data.frame(rbind(means,vars,mins,meds,maxs))
  
  output <- list( stats = summary_stats)
  saveRDS(output, file = paste0(out_file,"/!Summary_stats.rds"))
  #############
  ##############
  #############   PLOTS
  #############
  ###########
  
  name <- names(valid_df)
  name_var <- name
  name <- gsub("_", " ", name)
  for(i in 3:dim(valid_df)[2]){
    namei <- name[i]
    name_vari <- name_var[i]
    
    datai <- valid_df[,i]
    
    plot_obj <- density_template(namei, datai)
    assign(paste0(name_vari,"_density"),plot_obj)
    
  }
  density_plots <- ggarrange(                                     get(paste0(name_var[3],"_density")),
                                                                  get(paste0(name_var[4],"_density")), get(paste0(name_var[5],"_density")),
                                                                  get(paste0(name_var[6],"_density")), get(paste0(name_var[7],"_density")),
                                                                  get(paste0(name_var[8],"_density")), get(paste0(name_var[9],"_density")),
                                                                  get(paste0(name_var[10],"_density")), get(paste0(name_var[11],"_density")),
                                                                  get(paste0(name_var[12],"_density")), get(paste0(name_var[13],"_density")),
                                                                  get(paste0(name_var[14],"_density")), get(paste0(name_var[15],"_density")),
                                                                  get(paste0(name_var[16],"_density")), get(paste0(name_var[17],"_density")),
                                                                  get(paste0(name_var[18],"_density")), get(paste0(name_var[19],"_density")),
                                                                  get(paste0(name_var[20],"_density")), get(paste0(name_var[21],"_density")),
                                                                  get(paste0(name_var[22],"_density")), get(paste0(name_var[23],"_density")),
                                                                  get(paste0(name_var[24],"_density")), get(paste0(name_var[25],"_density")),
                                                                  get(paste0(name_var[26],"_density")), get(paste0(name_var[27],"_density")),                                                                                  
                                                                  get(paste0(name_var[28],"_density")), 
                                                                  nrow = 6,ncol = 5)
  ggsave("density_plots.pdf",density_plots,"pdf", out_file,2, 1920,1080,"px", 150)
  
  for(i in 3:dim(valid_df)[2]){
    namei <- name[i]
    name_vari <- name_var[i]
    
    datai <- valid_df[,i]
    
    plot_obj <- density_template(namei, datai, log = T)
    assign(paste0(name_vari,"_logdensity"),plot_obj)
    
  }
  logdensity_plots <- ggarrange(                                     get(paste0(name_var[3],"_logdensity")),
                                                                     get(paste0(name_var[4],"_logdensity")), get(paste0(name_var[5],"_logdensity")),
                                                                     get(paste0(name_var[6],"_logdensity")), get(paste0(name_var[7],"_logdensity")),
                                                                     get(paste0(name_var[8],"_logdensity")), get(paste0(name_var[9],"_logdensity")),
                                                                     get(paste0(name_var[10],"_logdensity")), get(paste0(name_var[11],"_logdensity")),
                                                                     get(paste0(name_var[12],"_logdensity")), get(paste0(name_var[13],"_logdensity")),
                                                                     get(paste0(name_var[14],"_logdensity")), get(paste0(name_var[15],"_logdensity")),
                                                                     get(paste0(name_var[16],"_logdensity")), get(paste0(name_var[17],"_logdensity")),
                                                                     get(paste0(name_var[18],"_logdensity")), get(paste0(name_var[19],"_logdensity")),
                                                                     get(paste0(name_var[20],"_logdensity")), get(paste0(name_var[21],"_logdensity")),
                                                                     get(paste0(name_var[22],"_logdensity")), get(paste0(name_var[23],"_logdensity")),
                                                                     get(paste0(name_var[24],"_logdensity")), get(paste0(name_var[25],"_logdensity")),
                                                                     get(paste0(name_var[26],"_logdensity")), get(paste0(name_var[27],"_logdensity")),                                                                                  
                                                                     get(paste0(name_var[28],"_logdensity")), 
                                                                     nrow = 6,ncol = 5)
  ggsave("densityLog_plots.pdf", logdensity_plots,"pdf",out_file,2,1920,1080,"px", 150)
  ###########################
  ###########################
  ###########################
  ###########################
  pfrid_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = (pfrid)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "PFRID")+
    theme(
      panel.background = element_rect(fill = "grey20",
                                      colour = "grey20",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("pfrid_map.pdf",pfrid_sfplot,"pdf",out_file,1,2560,1440,"px",150)
  
  
  logpfrid_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = log10(pfrid)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = bquote(paste("Log"["10"]*"(PFRID)")))+
    theme(
      panel.background = element_rect(fill = "grey20",
                                      colour = "grey20",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("logPfrid_map.pdf", logpfrid_sfplot, "pdf", out_file,1,2560,1440,"px", 200)
  
  sevrat_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = (sev_ratio_departure)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Severity Ratio Departure")+
    theme(
      panel.background = element_rect(fill = "grey20",
                                      colour = "grey20",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("sevRatioDepart_map.pdf",sevrat_sfplot,"pdf",out_file,1,2560,1440,"px",200)
  
  
  sevratabslog_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = log10(abs(sev_ratio_departure))))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = bquote(paste("Log"["10"]*"(|Severity Ratio Departure|)")))+
    theme(
      panel.background = element_rect(fill = "grey20",
                                      colour = "grey20",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("sevRatioDepart_absLog_map.pdf",sevratabslog_sfplot,"pdf",out_file,1,2560,1440,"px",200)
  
  sevratabsrank_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = rank(abs(sev_ratio_departure))))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Rank(|Severity Ratio Departure|)")+
    theme(
      panel.background = element_rect(fill = "grey20",
                                      colour = "grey20",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("sevRatioDepart_absRank_map.pdf",sevratabsrank_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  ########################
  #########################
  #########################
  ##########################
  emd_freqBySev_plot <- ggplot(valid_df,aes(x=emd_frequency, y = emd_severity, color = (emd_both)) )+
    geom_point()+
    scale_color_viridis_c(option = "B")+
    scale_x_continuous(minor_breaks = 0.001)+
    scale_y_continuous(breaks = c(0,0.5,1,1.5,2),
                       minor_breaks = 0.001)+
    geom_hline(yintercept=0, color = "grey70", linewidth = .1)+
    labs(color = "Total EMD",#bquote(paste("Log"["10"]*"(Total EMD)")
         x = "Frequency EMD",
         y = "Severity EMD"
    )+
    theme(
      panel.background = element_rect(fill = "grey20",
                                      colour = "grey20",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_line(color = "grey70",
                                      linewidth = .1)
    )
  ggsave("emd_freq_by_sev_plot.pdf", emd_freqBySev_plot,"pdf",out_file,1,1920,1080,"px", 150)
  
  
  p_scatter <- plot_ly(data = valid_df, x= ~emd_frequency, y= ~emd_severity, z= ~emd_both, color =  ~emd_both) %>%
    # the scatter plot of the data points 
    add_markers()%>%
    layout(scene = list(xaxis = list(title = 'EMD Frequency'),
                        yaxis = list(title = 'EMD Severity'),
                        zaxis = list(title = 'EMD Total')))
  p_scatter
  p_mesh <-plot_ly(data = valid_df, x= ~emd_frequency, y= ~emd_severity, z= ~emd_both, color =  ~emd_both) %>%
    # the scatter plot of the data points 
    add_mesh()%>%
    layout(scene = list(xaxis = list(title = 'EMD Frequency'),
                        yaxis = list(title = 'EMD Severity'),
                        zaxis = list(title = 'EMD Total')))
  p_mesh
  
  emd_freq_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = emd_frequency))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Frequency EMD")+
    theme(
      panel.background = element_rect(fill = "grey20",
                                      colour = "grey20",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_freq_map.pdf",emd_freq_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  emd_sev_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = emd_severity))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Severity EMD")+
    theme(
      panel.background = element_rect(fill = "grey20",
                                      colour = "grey20",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_sev_map.pdf",emd_sev_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  
  emd_total_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = emd_both))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Total EMD")+
    theme(
      panel.background = element_rect(fill = "grey20",
                                      colour = "grey20",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_total_map.pdf",emd_total_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  
  ######################
  ###################
  ##################
  
  emd_logfreq_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = log10(emd_frequency)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = bquote(paste("Log"["10"]*"(Frequency EMD)")))+
    theme(
      panel.background = element_rect(fill = "grey20",
                                      colour = "grey20",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_freqLog_map.pdf", emd_logfreq_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  emd_logsev_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = log(emd_severity)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = bquote(paste("Log"["10"]*"(Severity EMD)")))+
    theme(
      panel.background = element_rect(fill = "grey20",
                                      colour = "grey20",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_sevLog_map.pdf",emd_logsev_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  
  emd_logtotal_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = log10(emd_both)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = bquote(paste("Log"["10"]*"(Total EMD)")))+
    theme(
      panel.background = element_rect(fill = "grey20",
                                      colour = "grey20",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_totalLog_map.pdf",emd_logtotal_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  ######################
  ###################
  ##################
  
  emd_rankfreq_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = rank(emd_frequency)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Rank Frequency EMD")+
    theme(
      panel.background = element_rect(fill = "grey20",
                                      colour = "grey20",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_freqRank_map.pdf",emd_rankfreq_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  emd_ranksev_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = rank(emd_severity)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Rank Severity EMD")+
    theme(
      panel.background = element_rect(fill = "grey20",
                                      colour = "grey20",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_sevRank_map.pdf",emd_ranksev_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  
  emd_ranktotal_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = rank(emd_both)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Rank Total EMD")+
    theme(
      panel.background = element_rect(fill = "grey20",
                                      colour = "grey20",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_totalRank_map.pdf",emd_ranktotal_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  #################
  #################
  #################
  
  fri_abs <-valid_df %>%
    rename("Historic" = "hist_fri", "Contemporary" = "contemp_fri")%>%
    select(all_of(c("name", "Historic", "Contemporary")))%>%
    pivot_longer(c(Historic, Contemporary), names_to = "time", values_to = "fri")%>%
    mutate(time = as_factor(time))
  
  pbhs_abs <- valid_df %>%
    rename("Historic" = "hist_pbhs", "Contemporary" = "contemp_pbhs")%>%
    select(all_of(c("name", "Historic", "Contemporary")))%>%
    pivot_longer(c(Historic, Contemporary), names_to = "time", values_to = "pbhs") %>%
    mutate(time = as_factor(time))
  fri_contemp <-data.frame(name = valid_df$name,time = "Contemporary", fri = valid_df$contemp_fri-valid_df$hist_fri)
  fri_ref <-data.frame(name = valid_df$name, time = "Historic", fri = rep(0,length(valid_df$contemp_fri)))
  fri_departure <-rbind(fri_ref, fri_contemp)%>%
    mutate(time = as_factor(time))
  pbhs_contemp <- data.frame(name = valid_df$name, time = "Contemporary", pbhs = valid_df$contemp_pbhs-valid_df$hist_pbhs)
  pbhs_ref <-data.frame(name = valid_df$name, time = "Historic", pbhs = rep(0,length(valid_df$contemp_pbhs)))
  pbhs_departure <-rbind(pbhs_ref, pbhs_contemp)%>%
    mutate(time = as_factor(time))
  
  cpal_abs <- c("#999999", "red")
  abs_med <- data.frame(time = c("Historic", "Contemporary"),fri = c(meds[8],meds[7]), pbhs =c(meds[17],meds[16]), emd_both = c(0,meds[27]), emd_frequency = c(0,meds[25]), emd_severity = c(0,meds[26]) )
  abs_diff_plot <- full_join(fri_abs,pbhs_abs, by = c("name", "time"))%>%
    ggplot(aes(x=fri, y = pbhs, color = time))+
    geom_point()+
    geom_point(aes(x = fri, y =pbhs, color = time), data = abs_med, size = 6)+
    geom_text(aes(x = fri, y =pbhs-.04),label = "Median", data = abs_med,  color = "black", size = 6)+
    
    labs(x = "FRI",
         y = "PBHS")+
    scale_x_continuous(trans = "log10")+
    scale_color_manual(values = cpal_abs)+
    theme_bw()+
    guides(color = guide_legend(override.aes = list(size = 1)))+
    theme(legend.title = element_blank())
  ggsave("freqPbhs_abs_plot.pdf",abs_diff_plot,"pdf",out_file, 1,1920,1080,"px", 150)
  
  dep_med <- abs_med
  dep_med[1,-1] <- 0
  dep_med[2,-1] <- abs_med[2,-1]-abs_med[1,-1]
  dep_med$fri <- sign(dep_med$fri)*abs(dep_med$fri)^(1/3)
  dep_diff_plot <- full_join(fri_departure, pbhs_departure, by = c("name", "time")) %>%
    mutate(cubeRoot = sign(fri)*abs(fri)^(1/3))%>%
    #mutate(logAbsFri  =log10(abs(fri)), r = 0, g = floor(256+(logAbsFri-max(logAbsFri, na.rm = T))), b = floor(256*(abs(pbhs)/max(abs(pbhs),na.rm = T))))%>%
    #  drop_na()%>%
    #  mutate(hexcolor =  rgb(r,g,b, maxColorValue = 255)) %>%
    ggplot(aes(x=cubeRoot,y = pbhs, color = time, size = time))+
    geom_point()+
    geom_point(aes(x=fri, y = pbhs), data = dep_med, size = 6)+
    geom_text(x= dep_med$fri[2],y = (dep_med$pbhs[2]-.05), label = "Median", color = "black", show.legend = F)+
    scale_size_manual(values = c(6, 2))+
    scale_color_manual(values = cpal_abs)+
    scale_x_continuous(breaks = c(-5,0,5,10,15,20),
                       minor_breaks = 0.001)+
    scale_y_continuous(minor_breaks = 0.001)+
    labs(x = expression("FRI"^(1/3)," Departure"),
         y = "PBHS Departure")+
    theme_bw()+
    theme(legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_line(color = "grey70",
                                          linewidth = .1))
  ggsave("freqPbhs_dep_plot.pdf",dep_diff_plot,"pdf",out_file, 1,1920,1080,units ="px", 150)
  
  
  
  
  filler <- data.frame(name = valid_df$name, time = "Historic", emd_frequency = 0,
                       emd_severity = 0, emd_both = 0)
  emd_4plot <- data.frame(name = valid_df$name, emd_frequency = valid_df$emd_frequency,
                          emd_severity = valid_df$emd_severity,emd_both = valid_df$emd_both,
                          time = "Contemporary") %>%
    rbind(filler)%>%
    mutate(time = as_factor(time))
  
  dep_diff_emd_plot <-full_join(fri_departure, pbhs_departure, by = c("name", "time")) %>%
    mutate(cubeRoot = sign(fri)*abs(fri)^(1/3))%>%
    left_join(emd_4plot, by = c("name","time")) %>%
    
    ggplot(aes(x=cubeRoot,y = pbhs, color = emd_both, size = time))+
    geom_point()+
    geom_point(aes(x=fri, y = pbhs, color = emd_both), data = dep_med, size = 6)+
    geom_text(x= dep_med$fri[2],y = (dep_med$pbhs[2]-.05), label = "Median", color = "white", show.legend = F)+
    
    scale_size_manual(values = c(6, 2))+
    scale_color_viridis_c(option = "B")+
    scale_x_continuous(breaks = c(-5,0,5,10,15,20),
                       minor_breaks = c(0.001))+
    scale_y_continuous(minor_breaks = 0.001)+
    labs(color = "Total EMD",
         x = expression("FRI"^(1/3)),
         y = "PBHS"
    )+
    theme(
      panel.background = element_rect(fill = "grey20",
                                      colour = "grey20",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank() ,
      panel.grid.minor = element_line(color = "grey70",
                                      linewidth = .1)
    )
  ggsave("freqPbhs_depEmd_plot.pdf",dep_diff_emd_plot,"pdf",out_file, 1,1920,1080,units = "px", 150)
  
  

  freqEmd_dep_plot <-full_join(fri_departure, pbhs_departure, by = c("name", "time")) %>%
    mutate(cubeRoot = sign(fri)*abs(fri)^(1/3))%>%
    left_join(emd_4plot, by = c("name","time")) %>%
    
    ggplot(aes(x=cubeRoot,y = emd_frequency,color = time, size = time))+
    geom_point()+
    geom_point(aes(x=fri, y = emd_frequency), data = dep_med, size = 6)+
    geom_text(x= dep_med$fri[2],y = (dep_med$emd_frequency[2]-.05), label = "Median", color = "black", show.legend = F)+
    
    scale_size_manual(values = c(6, 2))+
    scale_color_manual(values = cpal_abs)+
    scale_x_continuous(breaks = c(-5,0,5,10,15,20),
                       minor_breaks = c(0.001))+
    scale_y_continuous(minor_breaks = 0.001)+
    labs(
         x = expression("FRI"^(1/3)),
         y = "EMD"
    )+
    theme_bw()+
    theme(legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_line(color = "grey70",
                                          linewidth = .1))
  ggsave("freqEmd_dep_plot.pdf",freqEmd_dep_plot,"pdf",out_file, 1,1920,1080,units = "px", 150)
  
  
  
  
  
  
  
  
   
  pfrid_sev_ratio_plot <- ggplot(valid_df, aes(x= pfrid, y = sev_ratio_departure))+
    geom_point()+
    labs(x = "PFRID",
        y = "Sev Ratio Departure")+
    scale_color_manual(values = cpal_abs)+
    theme_bw()+
    theme(legend.title = element_blank())
  ggsave("pfrid_bySevRatio_plot.pdf",pfrid_sev_ratio_plot,"pdf",out_file,1,1920,1080,"px",150)
  
  pfrid_sev_ratio_emd_plot <- ggplot(valid_df, aes(x= pfrid, y = sev_ratio_departure, color = emd_both))+
    geom_point()+
    scale_color_viridis_c(option = "B")+
    labs(color = "Total EMD",
         x = "PFRID",
         y = "Sev Ratio Departure"
    )+
    theme(
      panel.background = element_rect(fill = "grey20",
                                      colour = "grey20",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_line(color = "grey70",
                                      linewidth = .1),
      panel.grid.minor = element_blank() 
    )
  ggsave("pfrid_bySevRatioEmd_plot.pdf",pfrid_sev_ratio_emd_plot,"pdf",out_file,1,1920,1080,"px",150)
  
  ###################
  ###################
  ###################
  
  freq_sev_frcc_plot <- ggplot(valid_df, aes(x= FRCC_freq_dep, y = FRCC_sev_dep))+
    geom_point()+
    geom_point(x = meds[28], y = meds[29], size = 6)+
    geom_text(label = "Median",x = meds[28], y = (meds[29]-3))+
    labs(x = "FRCC Frequency Departure",
         y = "FRCC Severity Departure")+
    scale_color_manual(values = cpal_abs)+
    theme_bw()+
    theme(legend.title = element_blank())
  ggsave("freq_sev_dep_plot.pdf",freq_sev_frcc_plot,"pdf",out_file,1,1920,1080, "px",150)

  freq_sev_frcc_emd_plot <- ggplot(valid_df, aes(x= FRCC_freq_dep, y = FRCC_sev_dep, color = emd_both))+
    geom_point()+
    geom_point(x = meds[28], y = meds[29], size = 6)+
    geom_text(label = "Median",x = meds[28], y = (meds[29]-3), color = "white")+
    scale_color_viridis_c(option = "B")+
    labs(color = "Total EMD",
         x = "FRCC Frequency Departure",
         y = "FRCC Severity Departure"
    )+
    theme(
      panel.background = element_rect(fill = "grey20",
                                      colour = "grey20",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_line(color = "grey70",
                                      linewidth = .1),
      panel.grid.minor = element_blank()
    )
  ggsave("freq_sev_dep_emd_plot.pdf",freq_sev_frcc_emd_plot,"pdf",out_file,1,1920,1080, "px",150)

  freq_sev_frcc_reg_plot <- ggplot(valid_df, aes(x= FRCC_freq_dep, y = FRCC_sev_dep, color = FRCC_reg_dep))+
    geom_point()+
    geom_point(x = meds[28], y = meds[29], size = 6)+
    geom_text(label = "Median",x = meds[28], y = (meds[29]-3), color = "white")+
    scale_color_viridis_c(option = "B")+
    labs(color = "FRCC Regime Departure",
         x = "FRCC Frequency Departure",
         y = "FRCC Severity Departure"
    )+
    theme(
      panel.background = element_rect(fill = "grey20",
                                      colour = "grey20",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_line(color = "grey70",
                                      linewidth = .1),
      panel.grid.minor = element_blank()
    )
  ggsave("freq_sev_dep_regime_plot.pdf",freq_sev_frcc_reg_plot,"pdf",out_file,1,1920,1080, "px",150)

  #############################
  #############################
  #############################
  
  
  
  
  
  ###############################
  #############################
  ###############################
  
  dep_3d_scatter <- full_join(fri_departure, pbhs_departure, by = c("name", "time")) %>%
    mutate(cubeRoot = sign(fri)*abs(fri)^(1/3))%>%
    left_join(emd_4plot, by = c("name","time")) %>% plot_ly(x = ~cubeRoot, y = ~pbhs, z = ~emd_both, color = ~emd_both)%>%
    add_markers()%>%
    layout(scene = list(xaxis = list(title = "FRI^(1/3)"),
                        yaxis = list(title = "PBHS"),
                        zaxis = list(title = "EMD Total")))
  dep_3d_scatter
  
  pdf_paths <- paste0(out_file,"/",list.files(out_file))
  pdf_paths <- pdf_paths[!grepl("!S",pdf_paths)]
  pdf_combine(pdf_paths, paste0(out_file,"/!Slideshow.pdf"))
  
  
  return(output)
}

