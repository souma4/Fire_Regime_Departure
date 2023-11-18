pkgs <- c("sf", "tidyverse", "terra","RColorBrewer", "tmap", "units", "ggrepel","ggrepel","plotly","pdftools","corrplot")
invisible(lapply(pkgs, library, character.only = T))
panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  his <- hist(x, plot = FALSE)
  breaks <- his$breaks
  nB <- length(breaks)
  y <- his$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = rgb(0, 1, 1, alpha = 0.5), ...)
  # lines(density(x), col = 2, lwd = 2) # Uncomment to add density lines
}
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  Cor <- abs(cor(x, y)) # Remove abs function if desired
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1 + cex.cor * Cor) # Resize the text by level of correlation
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

production_figures_analysis <- function(valid_sf,context_sf, output_path, mask_join_name){
  valid_df <- st_drop_geometry(valid_sf)%>%
    as.data.frame()
  # out_file <- paste0(output_path,"/!summaries")
  # dir.create(out_file, showWarnings = F)
  # 
  # masks <- as.data.frame(masks)
  # name_index <- which(names(masks) == mask_join_name)
  # names(masks)[name_index] <- "name"
  # masks <- st_as_sf(masks)
  # 
  # 
  # valid_data <- check_data(data_list)
  # valid_df <- production_pulling(valid_data) %>%
  #   drop_na()%>%
  #   distinct(name, .keep_all = T)
  #          
  # 
  # 
  # valid_sf <- st_as_sf(left_join(valid_df, masks, by = "name")) %>%
  #   mutate(emd_freq_median_cat = case_when(emd_median_frequency <= quantile(valid_df$emd_median_frequency, 0.33) ~ 'A',
  #                                   emd_median_frequency > quantile(valid_df$emd_median_frequency, 0.33) & 
  #                                     emd_median_frequency <= quantile(valid_df$emd_median_frequency, 0.66) ~ 'B',
  #                                   emd_median_frequency > quantile(valid_df$emd_median_frequency, 0.66) ~ 'C'),
  #          emd_sev_median_cat = case_when(emd_median_severity <= quantile(valid_df$emd_median_severity, 0.33) ~ '3',
  #                                  emd_median_severity > quantile(valid_df$emd_median_severity, 0.33) & 
  #                                    emd_median_severity <= quantile(valid_df$emd_median_severity, 0.66) ~ '2',
  #                                  emd_median_severity > quantile(valid_df$emd_median_severity, 0.66) ~ '1'),
  #          
  #          emd_freq_median_norm_cat = case_when(emd_median_frequency_normalized <= quantile(valid_df$emd_median_frequency_normalized, 0.33) ~ 'A',
  #                                   emd_median_frequency_normalized > quantile(valid_df$emd_median_frequency_normalized, 0.33) & 
  #                                     emd_median_frequency_normalized <= quantile(valid_df$emd_median_frequency_normalized, 0.66) ~ 'B',
  #                                   emd_median_frequency_normalized > quantile(valid_df$emd_median_frequency_normalized, 0.66) ~ 'C'),
  #          emd_sev_median_norm_cat = case_when(emd_median_severity_normalized <= quantile(valid_df$emd_median_severity_normalized, 0.33) ~ '3',
  #                                  emd_median_severity_normalized > quantile(valid_df$emd_median_severity_normalized, 0.33) & 
  #                                    emd_median_severity_normalized <= quantile(valid_df$emd_median_severity_normalized, 0.66) ~ '2',
  #                                  emd_median_severity_normalized > quantile(valid_df$emd_median_severity_normalized, 0.66) ~ '1'),
  #          
  #          r_median = emd_median_frequency/max(emd_median_frequency),
  #          b_median = emd_median_severity/max(emd_median_severity),
  #          rgb_median_color = rgb(r_median,0,b_median),
  #          
  #          r_median_norm = emd_median_frequency_normalized/max(emd_median_frequency_normalized),
  #          b_median_norm = emd_median_severity_normalized/max(emd_median_severity_normalized),
  #          rgb_median_color_norm = rgb(r_median_norm,0,b_median_norm),
  #          #
  #          #
  #          emd_freq_minimum_cat = case_when(emd_minimum_frequency <= quantile(valid_df$emd_minimum_frequency, 0.33) ~ 'A',
  #                                          emd_minimum_frequency > quantile(valid_df$emd_minimum_frequency, 0.33) & 
  #                                            emd_minimum_frequency <= quantile(valid_df$emd_minimum_frequency, 0.66) ~ 'B',
  #                                          emd_minimum_frequency > quantile(valid_df$emd_minimum_frequency, 0.66) ~ 'C'),
  #          emd_sev_minimum_cat = case_when(emd_minimum_severity <= quantile(valid_df$emd_minimum_severity, 0.33) ~ '3',
  #                                         emd_minimum_severity > quantile(valid_df$emd_minimum_severity, 0.33) & 
  #                                           emd_minimum_severity <= quantile(valid_df$emd_minimum_severity, 0.66) ~ '2',
  #                                         emd_minimum_severity > quantile(valid_df$emd_minimum_severity, 0.66) ~ '1'),
  #          
  #          emd_freq_minimum_norm_cat = case_when(emd_minimum_frequency_normalized <= quantile(valid_df$emd_minimum_frequency_normalized, 0.33) ~ 'A',
  #                                               emd_minimum_frequency_normalized > quantile(valid_df$emd_minimum_frequency_normalized, 0.33) & 
  #                                                 emd_minimum_frequency_normalized <= quantile(valid_df$emd_minimum_frequency_normalized, 0.66) ~ 'B',
  #                                               emd_minimum_frequency_normalized > quantile(valid_df$emd_minimum_frequency_normalized, 0.66) ~ 'C'),
  #          emd_sev_minimum_norm_cat = case_when(emd_minimum_severity_normalized <= quantile(valid_df$emd_minimum_severity_normalized, 0.33) ~ '3',
  #                                              emd_minimum_severity_normalized > quantile(valid_df$emd_minimum_severity_normalized, 0.33) & 
  #                                                emd_minimum_severity_normalized <= quantile(valid_df$emd_minimum_severity_normalized, 0.66) ~ '2',
  #                                              emd_minimum_severity_normalized > quantile(valid_df$emd_minimum_severity_normalized, 0.66) ~ '1'),
  #          
  #          r_minimum = emd_minimum_frequency/max(emd_minimum_frequency),
  #          b_minimum = emd_minimum_severity/max(emd_minimum_severity),
  #          rgb_minimum_color = rgb(r_minimum,0,b_minimum),
  #          
  #          r_minimum_norm = emd_minimum_frequency_normalized/max(emd_minimum_frequency_normalized),
  #          b_minimum_norm = emd_minimum_severity_normalized/max(emd_minimum_severity_normalized),
  #          rgb_minimum_color_norm = rgb(r_minimum_norm,0,b_minimum_norm),
  #          signed_emd_median_frequency_normalized = sign(pfrid)*emd_median_frequency_normalized,
  #          signed_emd_median_frequency = sign(pfrid)*emd_median_frequency,
  #          
  #          signed_emd_median_severity_normalized = sign(sev_ratio_departure)*emd_median_severity_normalized,
  #          signed_emd_median_severity= sign(sev_ratio_departure)*emd_median_severity,
  #          
  #          signed_emd_minimum_frequency_normalized = sign(pfrid)*emd_minimum_frequency_normalized,
  #          signed_emd_minimum_frequency = sign(pfrid)*emd_minimum_frequency,
  #          
  #          signed_emd_minimum_severity_normalized = sign(sev_ratio_departure)*emd_minimum_severity_normalized,
  #          signed_emd_minimum_severity= sign(sev_ratio_departure)*emd_minimum_severity
  #          
  #          
  #          
  #          )%>%
  #   unite('bicat_median', emd_freq_median_cat, emd_sev_median_cat) %>%
  #   unite('bicat_median_norm',emd_freq_median_norm_cat,emd_sev_median_norm_cat)%>%
  #   mutate('numcat_median' = as.numeric(factor(bicat_median)), 'numcat_median_norm' = as.numeric(factor(bicat_median_norm)))%>%
  #   unite('bicat_minimum', emd_freq_minimum_cat, emd_sev_minimum_cat) %>%
  #   unite('bicat_minimum_norm',emd_freq_minimum_norm_cat,emd_sev_minimum_norm_cat)%>%
  #   mutate('numcat_minimum' = as.numeric(factor(bicat_minimum)), 'numcat_minimum_norm' = as.numeric(factor(bicat_minimum_norm)))
  # writeVector(vect(valid_sf), filename = paste0(out_file,"/",vectorOutputName,".gpkg"), overwrite = T)
  # #############
  ##############
  #############   summary stats
  #############
  ###########
  df_numerics <- select_if(valid_df, is.numeric)
  
  means <- base::colMeans(df_numerics, na.rm = T)
  vars <- base::apply(df_numerics, 2, var, na.rm = T)
  meds <- base::apply(df_numerics,2, median, na.rm = T)
  mins <- apply(df_numerics,2,min,na.rm=T)
  maxs <- apply(df_numerics,2,max,na.rm=T)
  summary_stats <- as.data.frame(rbind(means,vars,mins,meds,maxs))
  
  output <- list( stats = summary_stats, full_sf = valid_sf)
  saveRDS(output, file = paste0(out_file,"/!Summary_stats.rds"))
  #############
  ##############
  #############   PLOTS
  #############
  ###########
  
  name <- names(df_numerics)
  name_var <- name
  name <- gsub("_", " ", name)
  for(i in 1:dim(df_numerics)[2]){
    namei <- name[i]
    name_vari <- name_var[i]
    
    datai <- df_numerics[,i]
    
    plot_obj <- density_template(namei, datai)
    assign(paste0(name_vari,"_density"),plot_obj)
    
  }
  density_plots <- ggarrange(                                     get(paste0(name_var[1],"_density")),
                                                                  get(paste0(name_var[2],"_density")), get(paste0(name_var[3],"_density")),
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
                                                                  get(paste0(name_var[28],"_density")), get(paste0(name_var[29],"_density")),
                                                                  get(paste0(name_var[30],"_density")), get(paste0(name_var[31],"_density")),
                                                                  get(paste0(name_var[32],"_density")), get(paste0(name_var[33],"_density")),
                                                                  get(paste0(name_var[34],"_density")), get(paste0(name_var[35],"_density")),
                                                                  get(paste0(name_var[36],"_density")), get(paste0(name_var[37],"_density")),
                                                                  get(paste0(name_var[38],"_density")),
                                                                  nrow = 8,ncol = 5)
  ggsave("density_plots.pdf",density_plots,"pdf", out_file,2, 1920,1080,"px", 150)
  
  for(i in 1:dim(df_numerics)[2]){
    namei <- name[i]
    name_vari <- name_var[i]
    
    datai <- df_numerics[,i]
    
    plot_obj <- density_template(namei, datai, log = T)
    assign(paste0(name_vari,"_logdensity"),plot_obj)
    
  }
  logdensity_plots <- ggarrange(                                     get(paste0(name_var[1],"_logdensity")),
                                                                     get(paste0(name_var[2],"_logdensity")), get(paste0(name_var[3],"_logdensity")),
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
                                                                     get(paste0(name_var[28],"_logdensity")), get(paste0(name_var[29],"_logdensity")),
                                                                     get(paste0(name_var[30],"_logdensity")), get(paste0(name_var[31],"_logdensity")),
                                                                     get(paste0(name_var[32],"_logdensity")), get(paste0(name_var[33],"_logdensity")),
                                                                     get(paste0(name_var[34],"_logdensity")), get(paste0(name_var[35],"_logdensity")),
                                                                     get(paste0(name_var[36],"_logdensity")), get(paste0(name_var[37],"_logdensity")),
                                                                     get(paste0(name_var[38],"_logdensity")), 
                                                                     nrow = 8,ncol = 5)
  ggsave("densityLog_plots.pdf", logdensity_plots,"pdf",out_file,2,1920,1080,"px", 150)
  ###################
  ################### Corr plot
  ###################
  ###################
  
  pdf(file = paste0(out_file,"/correlation_plot_wxyplots.pdf"),
      width = 40,
      height = 40)
  pairs(df_numerics,
        upper.panel = panel.cor,    # Correlation panel
        lower.panel = panel.smooth) # Smoothed regression lines
  dev.off()
  pdf(file = paste0(out_file,"/correlation_plot_wcolor.pdf"),
      width = 40,
      height = 40)
  corrplot(cor(df_numerics))
  dev.off()
  
  
  ###############
  ############### Signed EMD Median
  ###############
  signed_labels <- data.frame(x = c(1,1,-1,-1),y = c(1,-1,-1,1),label = c("Less Frequent, More Severe Fire", "Less Frequent, Less Severe Fire", "More Frequent, Less Severe Fire", "More Frequent, More Severe Fire"))
  
  signed_emd_median_normalized_scatter <- ggplot(valid_sf, aes(x = signed_emd_median_frequency_normalized, y = signed_emd_median_severity_normalized))+
    geom_point() +
    geom_text(data = signed_labels, aes(x = x, y = y, label = label)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    labs(x = "Signed Normalized Frequency EMD",
         y = "Signed Normalized Severity EMD") + 
    
    theme_bw()
  ggsave("signed_emd_median_norm_scatter.pdf",signed_emd_median_normalized_scatter,"pdf",out_file,1,2560,1440,"px",150)
  
  signed_emd_median_scatter <- ggplot(valid_sf, aes(x = signed_emd_median_frequency, y = signed_emd_median_severity))+
    geom_point() +
    geom_text(data = signed_labels, aes(x = x, y = y, label = label)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    labs(x = "Signed Frequency EMD",
         y = "Signed Severity EMD") + 
    theme_bw()
  ggsave("signed_emd_median_scatter.pdf",signed_emd_median_scatter,"pdf",out_file,1,2560,1440,"px",150)
  
  
  
  ###########################
  ###########################PFRID
  ###########################
  ###########################
  pfrid_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = (pfrid)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "PFRID")+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
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
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("logPfrid_map.pdf", logpfrid_sfplot, "pdf", out_file,1,2560,1440,"px", 200)
  
  #######################
  ####################### Severity ratio
  ######################
  sevrat_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = (sev_ratio_departure)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Severity Ratio Departure")+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
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
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
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
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("sevRatioDepart_absRank_map.pdf",sevratabsrank_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  ########################
  #########################
  ######################### median FREQUENCY BY SEVERITY PLOTS
  ##########################
  emd_median_freqBySev_plot <- ggplot(valid_df,aes(x=emd_median_frequency, y = emd_median_severity, color = (emd_median_both)) )+
    geom_point()+
    scale_color_viridis_c(option = "B")+
    scale_x_continuous(minor_breaks = 0.001)+
    scale_y_continuous(breaks = c(0,0.5,1,1.5,2),
                       minor_breaks = 0.001)+
    geom_hline(yintercept=0, color = "grey70", linewidth = .1)+
    labs(color = "Median Total EMD",#bquote(paste("Log"["10"]*"(Total EMD)")
         x = "Median Frequency EMD",
         y = "Median Severity EMD"
    )+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_line(color = "grey70",
                                      linewidth = .1)
    )
  ggsave("emd_median_freq_by_sev_plot.pdf", emd_median_freqBySev_plot,"pdf",out_file,1,1920,1080,"px", 150)
  
  emd_median_freqBySev_norm_plot <- ggplot(valid_df,aes(x=emd_median_frequency_normalized, y = emd_median_severity_normalized, color = (emd_median_both_normalized)) )+
    geom_point()+
    scale_color_viridis_c(option = "B")+
    scale_x_continuous(minor_breaks = 0.001)+
    scale_y_continuous(breaks = c(0,0.5,1,1.5,2),
                       minor_breaks = 0.001)+
    geom_hline(yintercept=0, color = "grey70", linewidth = .1)+
    labs(color = "Median Normalized Total EMD",#bquote(paste("Log"["10"]*"(Total EMD)")
         x = "Median Normalized Frequency EMD",
         y = "Median Normalized Severity EMD"
    )+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_line(color = "grey70",
                                      linewidth = .1)
    )
  ggsave("emd_median_freq_by_sev_norm_plot.pdf", emd_median_freqBySev_norm_plot,"pdf",out_file,1,1920,1080,"px", 150)
  #############
  ########################
  #########################
  ######################### minimum FREQUENCY BY SEVERITY PLOTS
  ##########################
  emd_minimum_freqBySev_plot <- ggplot(valid_df,aes(x=emd_minimum_frequency, y = emd_minimum_severity, color = (emd_minimum_both)) )+
    geom_point()+
    scale_color_viridis_c(option = "B")+
    scale_x_continuous(minor_breaks = 0.001)+
    scale_y_continuous(breaks = c(0,0.5,1,1.5,2),
                       minor_breaks = 0.001)+
    geom_hline(yintercept=0, color = "grey70", linewidth = .1)+
    labs(color = "Minimum Total EMD",#bquote(paste("Log"["10"]*"(Total EMD)")
         x = "Minimum Frequency EMD",
         y = "Minimum Severity EMD"
    )+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_line(color = "grey70",
                                      linewidth = .1)
    )
  ggsave("emd_minimum_freq_by_sev_plot.pdf", emd_minimum_freqBySev_plot,"pdf",out_file,1,1920,1080,"px", 150)
  
  emd_minimum_freqBySev_norm_plot <- ggplot(valid_df,aes(x=emd_minimum_frequency_normalized, y = emd_minimum_severity_normalized, color = (emd_minimum_both_normalized)) )+
    geom_point()+
    scale_color_viridis_c(option = "B")+
    scale_x_continuous(minor_breaks = 0.001)+
    scale_y_continuous(breaks = c(0,0.5,1,1.5,2),
                       minor_breaks = 0.001)+
    geom_hline(yintercept=0, color = "grey70", linewidth = .1)+
    labs(color = "Minimum Normalized Total EMD",#bquote(paste("Log"["10"]*"(Total EMD)")
         x = "Minimum Normalized Frequency EMD",
         y = "Minimum Normalized Severity EMD"
    )+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_line(color = "grey70",
                                      linewidth = .1)
    )
  ggsave("emd_minimum_freq_by_sev_norm_plot.pdf", emd_minimum_freqBySev_norm_plot,"pdf",out_file,1,1920,1080,"px", 150)
  
  
  ###########
  ########### median PLOTLY PLOTS, MUST RUN INDEPENDENTLY.
  ##########
  
  p_median_scatter <- plot_ly(data = valid_df, x= ~emd_median_frequency, y= ~emd_median_severity, z= ~emd_median_both, color =  ~emd_median_both) %>%
    # the scatter plot of the data points 
    add_markers()%>%
    layout(scene = list(xaxis = list(title = 'Median EMD Frequency'),
                        yaxis = list(title = 'Median EMD Severity'),
                        zaxis = list(title = 'Median EMD Total')))
  p_median_scatter
  p_median_mesh <-plot_ly(data = valid_df, x= ~emd_median_frequency, y= ~emd_median_severity, z= ~emd_median_both, color =  ~emd_median_both) %>%
    # the scatter plot of the data points 
    add_mesh()%>%
    layout(scene = list(xaxis = list(title = 'Median EMD Frequency'),
                        yaxis = list(title = 'Median EMD Severity'),
                        zaxis = list(title = 'Median EMD Total')))
  p_median_mesh
  
  ###########
  ########### minimum PLOTLY PLOTS, MUST RUN INDEPENDENTLY.
  ##########
  
  p_minimum_scatter <- plot_ly(data = valid_df, x= ~emd_minimum_frequency, y= ~emd_minimum_severity, z= ~emd_minimum_both, color =  ~emd_minimum_both) %>%
    # the scatter plot of the data points 
    add_markers()%>%
    layout(scene = list(xaxis = list(title = 'Minimum EMD Frequency'),
                        yaxis = list(title = 'Minimum EMD Severity'),
                        zaxis = list(title = 'Minimum EMD Total')))
  p_minimum_scatter
  p_minimum_mesh <-plot_ly(data = valid_df, x= ~emd_minimum_frequency, y= ~emd_minimum_severity, z= ~emd_minimum_both, color =  ~emd_minimum_both) %>%
    # the scatter plot of the data points 
    add_mesh()%>%
    layout(scene = list(xaxis = list(title = 'Minimum EMD Frequency'),
                        yaxis = list(title = 'Minimum EMD Severity'),
                        zaxis = list(title = 'Minimum EMD Total')))
  p_minimum_mesh
  
  
  #################
  ################ Median UNNORMALIZED RAW AND BIVARIATE PLOTS
  #################
  
  emd_median_freq_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = emd_median_frequency))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Median Frequency EMD")+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_median_freq_map.pdf",emd_median_freq_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  emd_median_sev_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = emd_median_severity))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Median Severity EMD")+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_median_sev_map.pdf",emd_median_sev_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  
  emd_median_total_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = emd_median_both))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Median Total EMD")+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_median_total_map.pdf",emd_median_total_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  # emd_median_bivariate_cont_sfplot <- ggplot(valid_sf)+
  #   
  #   geom_sf(fill = pull(valid_sf,rgb_median_color))+
  #   geom_sf(data = context_sf, fill = NA)+
  #   scale_fill_viridis_c(option = "F")+
  #   ggtitle("Median EMD Bivariate Continuous Plot")+
  #   #labs(fill = "Total EMD")+
  #   theme(
  #     panel.background = element_rect(fill = "grey90",
  #                                     colour = "grey50",
  #                                     linewidth = 0.5, linetype = "solid"),
  #     panel.grid.major = element_blank(), 
  #     panel.grid.minor = element_blank()
  #   )
  # ggsave("emd_median_bivariate_continuous_map.pdf",emd_median_bivariate_cont_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  # 
  emd_median_bivariate_disc_sfplot <- ggplot(valid_sf)+
    
    geom_sf(aes(fill = bicat_median))+
    geom_sf(data = context_sf, fill = NA)+
    scale_fill_manual(values = c('A_1'='#e8e8e8','A_2'='#cbb8d7','A_3'='#9972af',
                                 'B_1'='#e4d9ac','B_2'='#c8ada0','B_3'='#976b82',
                                 'C_1'='#c8b35a','C_2'='#af8e53','C_3'='#804d36'),
                      guide = 'none')+
    
    ggtitle("Median EMD Bivariate Discrete Plot")+
    theme(
      panel.background = element_rect(fill = "white",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_median_bivariate_disc_map.pdf",emd_median_bivariate_disc_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  #################
  ################ Minimum UNNORMALIZED RAW AND BIVARIATE PLOTS
  #################
  
  emd_minimum_freq_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = emd_minimum_frequency))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Minimum Frequency EMD")+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_minimum_freq_map.pdf",emd_minimum_freq_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  emd_minimum_sev_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = emd_minimum_severity))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Minimum Severity EMD")+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_minimum_sev_map.pdf",emd_minimum_sev_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  
  emd_minimum_total_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = emd_minimum_both))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Minimum Total EMD")+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_minimum_total_map.pdf",emd_minimum_total_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  # emd_minimum_bivariate_cont_sfplot <- ggplot(valid_sf)+
  #   
  #   geom_sf(fill = pull(valid_sf,rgb_minimum_color))+
  #   geom_sf(data = context_sf, fill = NA)+
  #   scale_fill_viridis_c(option = "F")+
  #   ggtitle("Minimum EMD Bivariate Continuous Plot")+
  #   #labs(fill = "Total EMD")+
  #   theme(
  #     panel.background = element_rect(fill = "grey90",
  #                                     colour = "grey50",
  #                                     linewidth = 0.5, linetype = "solid"),
  #     panel.grid.major = element_blank(), 
  #     panel.grid.minor = element_blank()
  #   )
  # ggsave("emd_minimum_bivariate_continuous_map.pdf",emd_minimum_bivariate_cont_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  # 
  emd_minimum_bivariate_disc_sfplot <- ggplot(valid_sf)+
    
    geom_sf(aes(fill = bicat_minimum))+
    geom_sf(data = context_sf, fill = NA)+
    scale_fill_manual(values = c('A_1'='#e8e8e8','A_2'='#cbb8d7','A_3'='#9972af',
                                 'B_1'='#e4d9ac','B_2'='#c8ada0','B_3'='#976b82',
                                 'C_1'='#c8b35a','C_2'='#af8e53','C_3'='#804d36'),
                      guide = 'none')+
    
    ggtitle("Minimum EMD Bivariate Discrete Plot")+
    theme(
      panel.background = element_rect(fill = "white",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_minimum_bivariate_disc_map.pdf",emd_minimum_bivariate_disc_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  ######################
  ##################### Median NORMALIZED RAW AND BIVARIATE PLOTS
  ####################
  emd_median_freq_norm_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = emd_median_frequency_normalized))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Median Normalized Frequency EMD")+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_median_freq_norm_map.pdf",emd_median_freq_norm_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  emd_median_sev_norm_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = emd_median_severity_normalized))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Median Normalized Severity EMD")+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_median_sev_norm_map.pdf",emd_median_sev_norm_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  
  emd_median_total_norm_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = emd_median_both_normalized))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Median Normalized Total EMD")+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_median_total_norm_map.pdf",emd_median_total_norm_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  # emd_median_bivariate_cont_norm_sfplot <- ggplot(valid_sf)+
  #   
  #   geom_sf(fill = pull(valid_sf,rgb_median_color_norm))+
  #   geom_sf(data = context_sf, fill = NA)+
  #   scale_fill_viridis_c(option = "F")+
  #   #labs(fill = "Total EMD")+
  #   
  #   ggtitle("Median Normalized EMD Bivariate Continuous Plot")+
  #   theme(
  #     panel.background = element_rect(fill = "grey90",
  #                                     colour = "grey50",
  #                                     linewidth = 0.5, linetype = "solid"),
  #     panel.grid.major = element_blank(), 
  #     panel.grid.minor = element_blank()
  #   )
  # ggsave("emd_median_bivariate_continuous_norm_map.pdf",emd_median_bivariate_cont_norm_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  # 
  emd_median_bivariate_disc_norm_sfplot <- ggplot(valid_sf)+
    
    geom_sf(aes(fill = bicat_median_norm))+
    geom_sf(data = context_sf, fill = NA)+
    scale_fill_manual(values = c('A_1'='#e8e8e8','A_2'='#cbb8d7','A_3'='#9972af',
                                 'B_1'='#e4d9ac','B_2'='#c8ada0','B_3'='#976b82',
                                 'C_1'='#c8b35a','C_2'='#af8e53','C_3'='#804d36'),
                      guide = 'none')+
    
    ggtitle("Median Normalized EMD Bivariate Discrete Plot")+
    theme(
      panel.background = element_rect(fill = "white",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_median_bivariate_disc_norm_map.pdf",emd_median_bivariate_disc_norm_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  ######################
  ##################### Minimum NORMALIZED RAW AND BIVARIATE PLOTS
  ####################
  emd_minimum_freq_norm_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = emd_minimum_frequency_normalized))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Minimum Normalized Frequency EMD")+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_minimum_freq_norm_map.pdf",emd_minimum_freq_norm_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  emd_minimum_sev_norm_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = emd_minimum_severity_normalized))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Minimum Normalized Severity EMD")+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_minimum_sev_norm_map.pdf",emd_minimum_sev_norm_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  
  emd_minimum_total_norm_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = emd_minimum_both_normalized))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Minimum Normalized Total EMD")+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_minimum_total_norm_map.pdf",emd_minimum_total_norm_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  # emd_minimum_bivariate_cont_norm_sfplot <- ggplot(valid_sf)+
  #   
  #   geom_sf(fill = pull(valid_sf,rgb_minimum_color_norm))+
  #   geom_sf(data = context_sf, fill = NA)+
  #   scale_fill_viridis_c(option = "F")+
  #   #labs(fill = "Total EMD")+
  #   
  #   ggtitle("Minimum Normalized EMD Bivariate Continuous Plot")+
  #   theme(
  #     panel.background = element_rect(fill = "grey90",
  #                                     colour = "grey50",
  #                                     linewidth = 0.5, linetype = "solid"),
  #     panel.grid.major = element_blank(), 
  #     panel.grid.minor = element_blank()
  #   )
  # ggsave("emd_minimum_bivariate_continuous_norm_map.pdf",emd_minimum_bivariate_cont_norm_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  # 
  emd_minimum_bivariate_disc_norm_sfplot <- ggplot(valid_sf)+
    
    geom_sf(aes(fill = bicat_minimum_norm))+
    geom_sf(data = context_sf, fill = NA)+
    scale_fill_manual(values = c('A_1'='#e8e8e8','A_2'='#cbb8d7','A_3'='#9972af',
                                 'B_1'='#e4d9ac','B_2'='#c8ada0','B_3'='#976b82',
                                 'C_1'='#c8b35a','C_2'='#af8e53','C_3'='#804d36'),
                      guide = 'none')+
    
    ggtitle("Minimum Normalized EMD Bivariate Discrete Plot")+
    theme(
      panel.background = element_rect(fill = "white",
                                      colour = "grey90",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_minimum_bivariate_disc_norm_map.pdf",emd_minimum_bivariate_disc_norm_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  ######################
  ################### median EMD UNNORMALIZED LOG PLOTS
  ##################
  
  emd_median_logfreq_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = log10(emd_median_frequency)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = bquote(paste("Median Log"["10"]*"(Frequency EMD)")))+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_median_freqLog_map.pdf", emd_median_logfreq_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  emd_median_logsev_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = log(emd_median_severity)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = bquote(paste("Median Log"["10"]*"(Severity EMD)")))+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_median_sevLog_map.pdf",emd_median_logsev_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  
  emd_median_logtotal_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = log10(emd_median_both)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = bquote(paste("Median Log"["10"]*"(Total EMD)")))+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_median_totalLog_map.pdf",emd_median_logtotal_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  ######################
  ################### minimum EMD UNNORMALIZED LOG PLOTS
  ##################
  
  emd_minimum_logfreq_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = log10(emd_minimum_frequency)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = bquote(paste("Minimum Log"["10"]*"(Frequency EMD)")))+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_minimum_freqLog_map.pdf", emd_minimum_logfreq_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  emd_minimum_logsev_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = log(emd_minimum_severity)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = bquote(paste("Minimum Log"["10"]*"(Severity EMD)")))+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_minimum_sevLog_map.pdf",emd_minimum_logsev_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  
  emd_minimum_logtotal_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = log10(emd_minimum_both)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = bquote(paste("Minimum Log"["10"]*"(Total EMD)")))+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_minimum_totalLog_map.pdf",emd_minimum_logtotal_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  ######################
  ################### median EMD NORMALIZED LOG PLOTS
  ##################
  
  emd_median_logfreq_norm_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = log10(emd_median_frequency_normalized)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = bquote(paste("Median Log"["10"]*"(Normalized Frequency EMD)")))+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_median_freqLog_norm_map.pdf", emd_median_logfreq_norm_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  emd_median_logsev_norm_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = log(emd_median_severity_normalized)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = bquote(paste("Median Log"["10"]*"(Normalized Severity EMD)")))+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_median_sevLog_norm_map.pdf",emd_median_logsev_norm_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  
  emd_median_logtotal_norm_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = log10(emd_median_both_normalized)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = bquote(paste("Median Log"["10"]*"(Normalized Total EMD)")))+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_median_totalLog_norm_map.pdf",emd_median_logtotal_norm_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  ######################
  ################### median EMD NORMALIZED LOG PLOTS
  ##################
  
  emd_minimum_logfreq_norm_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = log10(emd_minimum_frequency_normalized)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = bquote(paste("Minimum Log"["10"]*"(Normalized Frequency EMD)")))+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_minimum_freqLog_norm_map.pdf", emd_minimum_logfreq_norm_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  emd_minimum_logsev_norm_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = log(emd_minimum_severity_normalized)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = bquote(paste("Minimum Log"["10"]*"(Normalized Severity EMD)")))+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_minimum_sevLog_norm_map.pdf",emd_minimum_logsev_norm_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  
  emd_minimum_logtotal_norm_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = log10(emd_minimum_both_normalized)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = bquote(paste("Minimum Log"["10"]*"(Normalized Total EMD)")))+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_minimum_totalLog_norm_map.pdf",emd_minimum_logtotal_norm_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  
  ######################
  ################### median EMD UNNORMALIZED RANKS
  ##################
  
  emd_median_rankfreq_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = rank(emd_median_frequency)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Median Rank Frequency EMD")+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_median_freqRank_map.pdf",emd_median_rankfreq_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  emd_median_ranksev_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = rank(emd_median_severity)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Median Rank Severity EMD")+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_median_sevRank_map.pdf",emd_median_ranksev_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  
  emd_median_ranktotal_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = rank(emd_median_both)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Median Rank Total EMD")+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_median_totalRank_map.pdf",emd_median_ranktotal_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  ######################
  ################### minimum EMD UNNORMALIZED RANKS
  ##################
  
  emd_minimum_rankfreq_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = rank(emd_minimum_frequency)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Minimum Rank Frequency EMD")+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_minimum_freqRank_map.pdf",emd_minimum_rankfreq_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  emd_minimum_ranksev_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = rank(emd_minimum_severity)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Minimum Rank Severity EMD")+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_minimum_sevRank_map.pdf",emd_minimum_ranksev_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  
  emd_minimum_ranktotal_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = rank(emd_minimum_both)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Minimum Rank Total EMD")+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_minimum_totalRank_map.pdf",emd_minimum_ranktotal_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  ######################
  ################### median EMD NORMALIZED RANKS
  ##################
  
  emd_median_rankfreq_norm_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = rank(emd_median_frequency_normalized)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Median Normalized Rank Frequency EMD")+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_median_freqRank_norm_map.pdf",emd_median_rankfreq_norm_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  emd_median_ranksev_norm_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = rank(emd_median_severity_normalized)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Median Normalized Rank Severity EMD")+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_median_sevRank_norm_map.pdf",emd_median_ranksev_norm_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  
  emd_median_ranktotal_norm_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = rank(emd_median_both_normalized)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Median Normalized Rank Total EMD")+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_median_totalRank_norm_map.pdf",emd_median_ranktotal_norm_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  ######################
  ################### minimum EMD NORMALIZED RANKS
  ##################
  
  emd_minimum_rankfreq_norm_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = rank(emd_minimum_frequency_normalized)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Minimum Normalized Rank Frequency EMD")+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_minimum_freqRank_norm_map.pdf",emd_minimum_rankfreq_norm_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  emd_minimum_ranksev_norm_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = rank(emd_minimum_severity_normalized)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Minimum Normalized Rank Severity EMD")+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_minimum_sevRank_norm_map.pdf",emd_minimum_ranksev_norm_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  
  emd_minimum_ranktotal_norm_sfplot <- ggplot(valid_sf)+
    geom_sf(data = context_sf, fill = NA)+
    geom_sf(aes(fill = rank(emd_minimum_both_normalized)))+
    scale_fill_viridis_c(option = "F")+
    labs(fill = "Minimum Normalized Rank Total EMD")+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
  ggsave("emd_minimum_totalRank_norm_map.pdf",emd_minimum_ranktotal_norm_sfplot,"pdf",out_file,1,2560,1440,"px", 200)
  
  
  
  #################
  ################# FRI AND PBHS ABSOLUTE PLOTS
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
  abs_med <- data.frame(time = c("Historic", "Contemporary"),fri = c(meds[8],meds[7]), pbhs =c(meds[17],meds[16]), emd_median_both = c(0,meds[27]), emd_median_frequency = c(0,meds[25]), emd_median_severity = c(0,meds[26]), emd_median_both_normalized = c(0,meds[30]), emd_median_frequency_normalized = c(0,meds[28]), emd_median_severity_normalized = c(0,meds[29]),
                                                                                                                  emd_minimum_both = c(0,meds[27+6]), emd_minimum_frequency = c(0,meds[25+6]), emd_minimum_severity = c(0,meds[26+6]), emd_minimum_both_normalized = c(0,meds[30+6]), emd_minimum_frequency_normalized = c(0,meds[28+6]), emd_minimum_severity_normalized = c(0,meds[29+6]))
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
  
  #################
  ################# FRI AND PBHS DEPARTURE PLOTS
  #################
  
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
  
  
  #################
  ################# Departures with emd unnormalized
  ##################
  
  filler <- data.frame(name = valid_df$name, time = "Historic",emd_median_frequency = 0,
                       emd_median_severity = 0, emd_median_both = 0, emd_median_frequency_normalized = 0,
                       emd_median_severity_normalized = 0, emd_median_both_normalized = 0, 
                       
                       emd_minimum_frequency = 0,
                       emd_minimum_severity = 0, emd_minimum_both = 0, emd_minimum_frequency_normalized = 0,
                       emd_minimum_severity_normalized = 0, emd_minimum_both_normalized = 0)
  emd_4plot <- data.frame(name = valid_df$name, emd_median_frequency = valid_df$emd_median_frequency,
                          emd_median_severity = valid_df$emd_median_severity,emd_median_both = valid_df$emd_median_both,
                          emd_median_frequency_normalized = valid_df$emd_median_frequency_normalized,
                          emd_median_severity_normalized = valid_df$emd_median_severity_normalized,
                          emd_median_both_normalized = valid_df$emd_median_both_normalized,
                          
                          emd_minimum_frequency = valid_df$emd_minimum_frequency,
                          emd_minimum_severity = valid_df$emd_minimum_severity,emd_minimum_both = valid_df$emd_minimum_both,
                          emd_minimum_frequency_normalized = valid_df$emd_minimum_frequency_normalized,
                          emd_minimum_severity_normalized = valid_df$emd_minimum_severity_normalized,
                          emd_minimum_both_normalized = valid_df$emd_minimum_both_normalized,
                          time = "Contemporary") %>%
    rbind(filler)%>%
    mutate(time = as_factor(time))
  
  dep_diff_emd_median_plot <-full_join(fri_departure, pbhs_departure, by = c("name", "time")) %>%
    mutate(cubeRoot = sign(fri)*abs(fri)^(1/3))%>%
    left_join(emd_4plot, by = c("name","time")) %>%
    
    ggplot(aes(x=cubeRoot,y = pbhs, color = emd_median_both, size = time))+
    geom_point()+
    geom_point(aes(x=fri, y = pbhs, color = emd_median_both), data = dep_med, size = 6)+
    geom_text(x= dep_med$fri[2],y = (dep_med$pbhs[2]-.05), label = "Median", color = "white", show.legend = F)+
    
    scale_size_manual(values = c(6, 2))+
    scale_color_viridis_c(option = "B")+
    scale_x_continuous(breaks = c(-5,0,5,10,15,20),
                       minor_breaks = c(0.001))+
    scale_y_continuous(minor_breaks = 0.001)+
    labs(color = "Median Total EMD",
         x = expression("FRI"^(1/3)),
         y = "PBHS"
    )+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank() ,
      panel.grid.minor = element_line(color = "grey70",
                                      linewidth = .1)
    )
  ggsave("freqPbhs_depEmd_median_plot.pdf",dep_diff_emd_median_plot,"pdf",out_file, 1,1920,1080,units = "px", 150)
  
  dep_diff_emd_minimum_plot <-full_join(fri_departure, pbhs_departure, by = c("name", "time")) %>%
    mutate(cubeRoot = sign(fri)*abs(fri)^(1/3))%>%
    left_join(emd_4plot, by = c("name","time")) %>%
    
    ggplot(aes(x=cubeRoot,y = pbhs, color = emd_minimum_both, size = time))+
    geom_point()+
    geom_point(aes(x=fri, y = pbhs, color = emd_minimum_both), data = dep_med, size = 6)+
    geom_text(x= dep_med$fri[2],y = (dep_med$pbhs[2]-.05), label = "Median", color = "white", show.legend = F)+
    
    scale_size_manual(values = c(6, 2))+
    scale_color_viridis_c(option = "B")+
    scale_x_continuous(breaks = c(-5,0,5,10,15,20),
                       minor_breaks = c(0.001))+
    scale_y_continuous(minor_breaks = 0.001)+
    labs(color = "Minimum Total EMD",
         x = expression("FRI"^(1/3)),
         y = "PBHS"
    )+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank() ,
      panel.grid.minor = element_line(color = "grey70",
                                      linewidth = .1)
    )
  ggsave("freqPbhs_depEmd_minimum_plot.pdf",dep_diff_emd_minimum_plot,"pdf",out_file, 1,1920,1080,units = "px", 150)
  
  
  
  
  freqEMD_dep_4plot <- full_join(fri_departure, pbhs_departure, by = c("name", "time")) %>%
    mutate(cubeRoot = sign(fri)*abs(fri)^(1/3))%>%
    left_join(emd_4plot, by = c("name","time"))
  freqEmd_dep_median_plot <-freqEMD_dep_4plot %>%
    
    ggplot(aes(x=cubeRoot,y = emd_median_frequency,color = time, size = time))+
    geom_point()+
    geom_point(aes(x=fri, y = emd_median_frequency), data = dep_med, size = 6)+
    geom_text(x= dep_med$fri[2],y = (dep_med$emd_median_frequency[2]-.05), label = "Median", color = "black", show.legend = F)+
    
    scale_size_manual(values = c(6, 2))+
    scale_color_manual(values = cpal_abs)+
    scale_x_continuous(breaks = c(-5,0,5,10,15,20),
                       minor_breaks = c(0.001))+
    scale_y_continuous(minor_breaks = 0.001)+
    labs(
         x = expression("FRI"^(1/3)),
         y = "Median EMD Frequency"
    )+
    theme_bw()+
    theme(legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_line(color = "grey70",
                                          linewidth = .1))
  ggsave("freqEmd_dep_median_plot.pdf",freqEmd_dep_median_plot,"pdf",out_file, 1,1920,1080,units = "px", 150)
  
  freqEmd_dep_minimum_plot <-freqEMD_dep_4plot %>%
    
    ggplot(aes(x=cubeRoot,y = emd_minimum_frequency,color = time, size = time))+
    geom_point()+
    geom_point(aes(x=fri, y = emd_minimum_frequency), data = dep_med, size = 6)+
    geom_text(x= dep_med$fri[2],y = (dep_med$emd_minimum_frequency[2]-.05), label = "Median", color = "black", show.legend = F)+
    
    scale_size_manual(values = c(6, 2))+
    scale_color_manual(values = cpal_abs)+
    scale_x_continuous(breaks = c(-5,0,5,10,15,20),
                       minor_breaks = c(0.001))+
    scale_y_continuous(minor_breaks = 0.001)+
    labs(
      x = expression("FRI"^(1/3)),
      y = "Minimum EMD Frequency"
    )+
    theme_bw()+
    theme(legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_line(color = "grey70",
                                          linewidth = .1))
  ggsave("freqEmd_dep_minimum_plot.pdf",freqEmd_dep_minimum_plot,"pdf",out_file, 1,1920,1080,units = "px", 150)
  
  
  
  ###############
  ############### Median Departures with normalized EMD
  ###############
  dep_diff_emd_median_norm_plot <-full_join(fri_departure, pbhs_departure, by = c("name", "time")) %>%
    mutate(cubeRoot = sign(fri)*abs(fri)^(1/3))%>%
    left_join(emd_4plot, by = c("name","time")) %>%
    
    ggplot(aes(x=cubeRoot,y = pbhs, color = emd_median_both_normalized, size = time))+
    geom_point()+
    geom_point(aes(x=fri, y = pbhs, color = emd_median_both_normalized), data = dep_med, size = 6)+
    geom_text(x= dep_med$fri[2],y = (dep_med$pbhs[2]-.05), label = "Median", color = "white", show.legend = F)+
    
    scale_size_manual(values = c(6, 2))+
    scale_color_viridis_c(option = "B")+
    scale_x_continuous(breaks = c(-5,0,5,10,15,20),
                       minor_breaks = c(0.001))+
    scale_y_continuous(minor_breaks = 0.001)+
    labs(color = "Median Normalized Total EMD",
         x = expression("FRI"^(1/3)),
         y = "PBHS"
    )+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank() ,
      panel.grid.minor = element_line(color = "grey70",
                                      linewidth = .1)
    )
  ggsave("freqPbhs_depEmd_median_norm_plot.pdf",dep_diff_emd_median_norm_plot,"pdf",out_file, 1,1920,1080,units = "px", 150)
  
  
  freqEMD_dep_median_norm_4plot <- full_join(fri_departure, pbhs_departure, by = c("name", "time")) %>%
    mutate(cubeRoot = sign(fri)*abs(fri)^(1/3))%>%
    left_join(emd_4plot, by = c("name","time"))
  freqEmd_dep_median_norm_plot <-freqEMD_dep_median_norm_4plot %>%
    
    ggplot(aes(x=cubeRoot,y = emd_median_frequency_normalized,color = time, size = time))+
    geom_point()+
    geom_point(aes(x=fri, y = emd_median_frequency_normalized), data = dep_med, size = 6)+
    geom_text(x= dep_med$fri[2],y = (dep_med$emd_median_frequency_normalized[2]-.05), label = "Median", color = "black", show.legend = F)+
    
    scale_size_manual(values = c(6, 2))+
    scale_color_manual(values = cpal_abs)+
    scale_x_continuous(breaks = c(-5,0,5,10,15,20),
                       minor_breaks = c(0.001))+
    scale_y_continuous(minor_breaks = 0.001)+
    labs(
      x = expression("FRI"^(1/3)),
      y = "Median Normalized Frequency EMD"
    )+
    theme_bw()+
    theme(legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_line(color = "grey70",
                                          linewidth = .1))
  ggsave("freqEmd_dep_median_norm_plot.pdf",freqEmd_dep_median_norm_plot,"pdf",out_file, 1,1920,1080,units = "px", 150)
  
  ###############
  ############### Minimum Departures with normalized EMD
  ###############
  dep_diff_emd_minimum_norm_plot <-full_join(fri_departure, pbhs_departure, by = c("name", "time")) %>%
    mutate(cubeRoot = sign(fri)*abs(fri)^(1/3))%>%
    left_join(emd_4plot, by = c("name","time")) %>%
    
    ggplot(aes(x=cubeRoot,y = pbhs, color = emd_minimum_both_normalized, size = time))+
    geom_point()+
    geom_point(aes(x=fri, y = pbhs, color = emd_minimum_both_normalized), data = dep_med, size = 6)+
    geom_text(x= dep_med$fri[2],y = (dep_med$pbhs[2]-.05), label = "Median", color = "white", show.legend = F)+
    
    scale_size_manual(values = c(6, 2))+
    scale_color_viridis_c(option = "B")+
    scale_x_continuous(breaks = c(-5,0,5,10,15,20),
                       minor_breaks = c(0.001))+
    scale_y_continuous(minor_breaks = 0.001)+
    labs(color = "Minimum Normalized Total EMD",
         x = expression("FRI"^(1/3)),
         y = "PBHS"
    )+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_blank() ,
      panel.grid.minor = element_line(color = "grey70",
                                      linewidth = .1)
    )
  ggsave("freqPbhs_depEmd_minimum_norm_plot.pdf",dep_diff_emd_minimum_norm_plot,"pdf",out_file, 1,1920,1080,units = "px", 150)
  
  
  
  freqEmd_dep_minimum_norm_plot <-freqEMD_dep_median_norm_4plot %>%
    
    ggplot(aes(x=cubeRoot,y = emd_minimum_frequency_normalized,color = time, size = time))+
    geom_point()+
    geom_point(aes(x=fri, y = emd_minimum_frequency_normalized), data = dep_med, size = 6)+
    geom_text(x= dep_med$fri[2],y = (dep_med$emd_minimum_frequency_normalized[2]-.05), label = "Median", color = "black", show.legend = F)+
    
    scale_size_manual(values = c(6, 2))+
    scale_color_manual(values = cpal_abs)+
    scale_x_continuous(breaks = c(-5,0,5,10,15,20),
                       minor_breaks = c(0.001))+
    scale_y_continuous(minor_breaks = 0.001)+
    labs(
      x = expression("FRI"^(1/3)),
      y = "Minimum Normalized Frequency EMD"
    )+
    theme_bw()+
    theme(legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_line(color = "grey70",
                                          linewidth = .1))
  ggsave("freqEmd_dep_minimum_norm_plot.pdf",freqEmd_dep_minimum_norm_plot,"pdf",out_file, 1,1920,1080,units = "px", 150)
  

  # interactive_freqEmd_dep_plot <- plot_ly(data = freqEMD_dep_4plot,x = ~cubeRoot, y = ~emd_frequency,
  #                text = ~paste("Name: ",name))
  # interactive_freqEmd_dep_plot


    # interactive_freqEmd_abs_plot <- plot_ly(data = freqEMD_dep_4plot,x = ~cubeRoot, y = ~emd_frequency,
    #                text = ~paste("Name: ",name))
    # interactive_freqEmd_dep_plot

  
  #######################
  #######################ratio changes
  #######################
   
  pfrid_sev_ratio_plot <- ggplot(valid_df, aes(x= pfrid, y = sev_ratio_departure))+
    geom_point()+
    labs(x = "PFRID",
        y = "Sev Ratio Departure")+
    scale_color_manual(values = cpal_abs)+
    theme_bw()+
    theme(legend.title = element_blank())
  ggsave("pfrid_bySevRatio_plot.pdf",pfrid_sev_ratio_plot,"pdf",out_file,1,1920,1080,"px",150)
  
  pfrid_sev_ratio_emd_median_plot <- ggplot(valid_df, aes(x= pfrid, y = sev_ratio_departure, color = emd_median_both))+
    geom_point()+
    scale_color_viridis_c(option = "B")+
    labs(color = "Median Total EMD",
         x = "PFRID",
         y = "Sev Ratio Departure"
    )+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_line(color = "grey70",
                                      linewidth = .1),
      panel.grid.minor = element_blank() 
    )
  ggsave("pfrid_bySevRatioEmd_median_plot.pdf",pfrid_sev_ratio_emd_median_plot,"pdf",out_file,1,1920,1080,"px",150)
  
  
  pfrid_sev_ratio_emd_median_norm_plot <- ggplot(valid_df, aes(x= pfrid, y = sev_ratio_departure, color = emd_median_both_normalized))+
    geom_point()+
    scale_color_viridis_c(option = "B")+
    labs(color = "Median Normalized Total EMD",
         x = "PFRID",
         y = "Sev Ratio Departure"
    )+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_line(color = "grey70",
                                      linewidth = .1),
      panel.grid.minor = element_blank() 
    )
  ggsave("pfrid_bySevRatioEmd_median_norm_plot.pdf",pfrid_sev_ratio_emd_median_norm_plot,"pdf",out_file,1,1920,1080,"px",150)
  ####
  pfrid_sev_ratio_emd_minimum_plot <- ggplot(valid_df, aes(x= pfrid, y = sev_ratio_departure, color = emd_minimum_both))+
    geom_point()+
    scale_color_viridis_c(option = "B")+
    labs(color = "Minimum Total EMD",
         x = "PFRID",
         y = "Sev Ratio Departure"
    )+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_line(color = "grey70",
                                      linewidth = .1),
      panel.grid.minor = element_blank() 
    )
  ggsave("pfrid_bySevRatioEmd_minimum_plot.pdf",pfrid_sev_ratio_emd_minimum_plot,"pdf",out_file,1,1920,1080,"px",150)
  
  
  pfrid_sev_ratio_emd_minimum_norm_plot <- ggplot(valid_df, aes(x= pfrid, y = sev_ratio_departure, color = emd_minimum_both_normalized))+
    geom_point()+
    scale_color_viridis_c(option = "B")+
    labs(color = "Minimum Normalized Total EMD",
         x = "PFRID",
         y = "Sev Ratio Departure"
    )+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_line(color = "grey70",
                                      linewidth = .1),
      panel.grid.minor = element_blank() 
    )
  ggsave("pfrid_bySevRatioEmd_minimum_norm_plot.pdf",pfrid_sev_ratio_emd_minimum_norm_plot,"pdf",out_file,1,1920,1080,"px",150)
  
  
  
  ###################
  ###################FRCC PLOTS
  ###################
  
  freq_sev_frcc_plot <- ggplot(valid_df, aes(x= FRCC_freq_dep, y = FRCC_sev_dep))+
    geom_point()+
    geom_point(x = meds[38], y = meds[39], size = 6)+
    geom_text(label = "Median",x = meds[38], y = (meds[39]-3))+
    labs(x = "FRCC Frequency Departure",
         y = "FRCC Severity Departure")+
    scale_color_manual(values = cpal_abs)+
    theme_bw()+
    theme(legend.title = element_blank())
  ggsave("freq_sev_dep_plot.pdf",freq_sev_frcc_plot,"pdf",out_file,1,1920,1080, "px",150)

  freq_sev_frcc_emd_median_plot <- ggplot(valid_df, aes(x= FRCC_freq_dep, y = FRCC_sev_dep, color = emd_median_both))+
    geom_point()+
    geom_point(x = meds[38], y = meds[39], size = 6)+
    geom_text(label = "Median",x = meds[38], y = (meds[39]-3), color = "white")+
    scale_color_viridis_c(option = "B")+
    labs(color = "Median Total EMD",
         x = "FRCC Frequency Departure",
         y = "FRCC Severity Departure"
    )+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_line(color = "grey70",
                                      linewidth = .1),
      panel.grid.minor = element_blank()
    )
  ggsave("freq_sev_dep_emd_median_plot.pdf",freq_sev_frcc_emd_median_plot,"pdf",out_file,1,1920,1080, "px",150)

  
  freq_sev_frcc_emd_median_norm_plot <- ggplot(valid_df, aes(x= FRCC_freq_dep, y = FRCC_sev_dep, color = emd_median_both_normalized))+
    geom_point()+
    geom_point(x = meds[38], y = meds[39], size = 6)+
    geom_text(label = "Median",x = meds[38], y = (meds[39]-3), color = "white")+
    scale_color_viridis_c(option = "B")+
    labs(color = "Median Normalized Total EMD",
         x = "FRCC Frequency Departure",
         y = "FRCC Severity Departure"
    )+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_line(color = "grey70",
                                      linewidth = .1),
      panel.grid.minor = element_blank()
    )
  ggsave("freq_sev_dep_emd_median_norm_plot.pdf",freq_sev_frcc_emd_median_norm_plot,"pdf",out_file,1,1920,1080, "px",150)
  ###
  freq_sev_frcc_emd_minimum_plot <- ggplot(valid_df, aes(x= FRCC_freq_dep, y = FRCC_sev_dep, color = emd_minimum_both))+
    geom_point()+
    geom_point(x = meds[38], y = meds[39], size = 6)+
    geom_text(label = "Median",x = meds[38], y = (meds[39]-3), color = "white")+
    scale_color_viridis_c(option = "B")+
    labs(color = "Minimum Total EMD",
         x = "FRCC Frequency Departure",
         y = "FRCC Severity Departure"
    )+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_line(color = "grey70",
                                      linewidth = .1),
      panel.grid.minor = element_blank()
    )
  ggsave("freq_sev_dep_emd_minimum_plot.pdf",freq_sev_frcc_emd_minimum_plot,"pdf",out_file,1,1920,1080, "px",150)
  
  
  freq_sev_frcc_emd_minimum_norm_plot <- ggplot(valid_df, aes(x= FRCC_freq_dep, y = FRCC_sev_dep, color = emd_minimum_both_normalized))+
    geom_point()+
    geom_point(x = meds[38], y = meds[39], size = 6)+
    geom_text(label = "Median",x = meds[38], y = (meds[39]-3), color = "white")+
    scale_color_viridis_c(option = "B")+
    labs(color = "Minimum Normalized Total EMD",
         x = "FRCC Frequency Departure",
         y = "FRCC Severity Departure"
    )+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
                                      linewidth = 0.5, linetype = "solid"),
      panel.grid.major = element_line(color = "grey70",
                                      linewidth = .1),
      panel.grid.minor = element_blank()
    )
  ggsave("freq_sev_dep_emd_minimum_norm_plot.pdf",freq_sev_frcc_emd_minimum_norm_plot,"pdf",out_file,1,1920,1080, "px",150)
  
  
  ###
  
  
  freq_sev_frcc_reg_plot <- ggplot(valid_df, aes(x= FRCC_freq_dep, y = FRCC_sev_dep, color = FRCC_reg_dep))+
    geom_point()+
    geom_point(x = meds[38], y = meds[39], size = 6)+
    geom_text(label = "Median",x = meds[38], y = (meds[39]-3), color = "white")+
    scale_color_viridis_c(option = "B")+
    labs(color = "FRCC Regime Departure",
         x = "FRCC Frequency Departure",
         y = "FRCC Severity Departure"
    )+
    theme(
      panel.background = element_rect(fill = "grey90",
                                      colour = "grey50",
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
    left_join(emd_4plot, by = c("name","time")) %>% plot_ly(x = ~cubeRoot, y = ~pbhs, z = ~emd_median_both, color = ~emd_median_both)%>%
    add_markers()%>%
    layout(scene = list(xaxis = list(title = "FRI^(1/3)"),
                        yaxis = list(title = "PBHS"),
                        zaxis = list(title = "Median EMD Total")))
  # dep_3d_scatter
  # 
  pdf_paths <- paste0(out_file,"/",list.files(out_file))
  pdf_paths <- pdf_paths[!grepl("!S",pdf_paths)]
  pdf_paths <- pdf_paths[!grepl(".gpkg", pdf_paths)]
  pdf_combine(pdf_paths, paste0(out_file,"/!Slideshow.pdf"))
  
  
  return(output)
}

