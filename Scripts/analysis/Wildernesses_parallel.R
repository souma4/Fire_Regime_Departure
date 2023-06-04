library(tidyverse)
library(terra)
library(sf)
library(sampling)
library(tictoc)
library(foreach)
library(doParallel)
library(ggrepel)
source("Scripts/functions.R")
set.seed(1234)

######tentative template
bps <- rast("data/landscape_data/LF2020_BPS_220_CONUS/Tif/LC20_BPS_220.tif")
bps_csv <- read_csv("data/landscape_data/LF2020_BPS_220_CONUS/CSV_Data/LF20_BPS_220.csv")
bps_csv$BPS_CODE <- as.factor(bps_csv$BPS_CODE)
#forest_cover <- rast("data/landscape_data/[FORESTCOVERMAP]")
##do some funneling stuff to only get the 1984 layer
fire_path <- "data/fires/"

mask_all <-st_read("data/masks/wilderness.shp")%>%
  st_transform(st_crs(bps))
mask_all$simpl_name <- gsub(" Wilderness","",mask_all$NAME1) %>%
  gsub(" ","_",.)
folder_subset <- list.dirs(fire_path, full.names = F)[-1]


#data_names <- c("mask","name","elev_fire_freq", "elev_fire_pres", "freq_bar", 
 #               "sev_violin", "sev_density_burned","sev_density_est", 
  #              "freq_sev","ave_sev_map","bps_map",  "hist_sev",
   #             "contemp_sev","hist_sev_class","contemp_sev_class","freq_dat_hist","freq_dat_contemp")

boot_iterations <- 500


tic("Full Loop") #
cores <- detectCores()/2/2
registerDoParallel(cores)
bps <- wrap(bps)
#forest_cover <- wrap(forest_cover)
#length(folder_subset)
stored_data <- foreach(i = 1:length(folder_subset), .export = c("mask_all","folder_subset","fire_path","bps", "boot_iterations"),
        .packages = c("tidyverse", "sf", "terra","sampling"),
        .inorder = FALSE, .errorhandling = "pass") %dopar% {

          
  bps <- unwrap(bps)
  #forest_cover <- unwrap(forest_cover)
  
  name<- list(folder_subset[i])
  
  mask <- filter(mask_all, simpl_name == folder_subset[i])
 mask_geom <- list(mask$geometry)
  
  shp_name <- list.files(paste0(fire_path,folder_subset[i]), pattern = ".shp")
  
  perims <- st_read(paste0(fire_path,folder_subset[i],"/",shp_name)) %>%
    st_transform(st_crs(bps))%>%
    arrange(Fire_Year)
  
  
  
  
  bps_mask <- crop(bps, vect(mask))
  #forest_cover_mask <- crop(forest_cover, vect(mask))
  #stored_data$bps_map[[i]] <- bps_mask
  buff <- st_buffer(perims, -150)
  
  title <- paste0(gsub("_"," ",name)," Wilderness")
  
  #####creating names 
  list_names <- perims %>%
    st_drop_geometry %>%
     dplyr::select(Fire_Year, Event_ID)
  
  list_names$Tif <- paste0(list_names$Event_ID, "_CBI_bc.tif")
  
  
  ######creating fire event ID's by year
  years <- as.numeric(unique(perims$Fire_Year))
  
  year_ids <- list(year = years, IDs = NULL)
  
  rm(perims)
  #####  loading in rasters by year, creating the mosaic by year, projecting 30m
  
  #for(j in 1:length(years)) {
  #  year_ids <- list_names %>%
  #    filter(Fire_Year == years[j])
  #  buffer_rast <- buff %>%
  #    filter(Fire_Year == years[[j]]) %>%
  #    rasterize(.,bps_mask)
    
  #  year_rast <- sapply(paste0(fire_path,folder_subset[i],"/", year_ids$Tif), rast)
  #  mos <- sprc(year_rast) %>%
  #    mosaic() %>%
   #   classify(cbind(c(0),c(NA)))
  #  mos_reproj <- project(mos, bps_mask)
   # mos_reproj <- mos_reproj *buffer_rast
    
  #  assign(paste0("mos_",years[j]), mos)
   # assign(paste0("mos_",years[j], "_30m"), mos_reproj)
    
    #writeRaster(get(paste0("mos_",years[j])), 
    #         filename = paste0(fire_path,folder_subset[i],"/",folder_subset[i],"_",years[j],".tif" ),
    #        overwrite = T)
    #writeRaster(get(paste0("mos_",years[j], "_30m")), 
    #           filename = paste0(fire_path,folder_subset[i],"/",folder_subset[i],"_",years[j],"_30m.tif" ),
    #          overwrite = T)
  #  rm(list = paste0("mos_",years[j]))
  #  rm(list = paste0("mos_",years[j],"_30m"))
   # rm(year_rast, mos, mos_reproj)
 # }
  
  ##### stacking 
  mos_stack_30m <- rast(paste0(fire_path,folder_subset[i],"/",folder_subset[i],"_",years,"_30m.tif"  ))
  names(mos_stack_30m) <- years
  
  
#  all_rast <-sapply(paste0(fire_path,folder_subset[i],"/", list_names$Tif), rast)
#  mos_all <- sprc(all_rast) %>%
#    mosaic() %>%
#    project(bps_mask)
  
  #ave_sev_map<-list(mos_all)
  
  ##### creation of 30m template
  template <- rast(extent = ext( bps_mask),
                   res = 30, crs = "EPSG:5070", vals = T)
  template_DF <- as.data.frame(template, cells = T, xy = T) %>%
     dplyr::select(cell, x, y)
  
  
  template_DF <- template_DF[,c( "x", "y","cell")]
  
  
  ##### creation of frame
  frame <- rast(crs = crs(bps_mask), ext = ext(bps_mask), vals = 1, resolution = 30) %>%
    rasterize(vect(mask),.)
  
  
  
  ##### creating extracted points
  
  activeCat(bps_mask) <- 9
  ext.1 <- terra::extract(frame, 
                          vect(template_DF, geom = c("x", "y")), xy = T) %>%
    drop_na()
  ext.2 <-  terra::extract(bps_mask, vect(ext.1, geom = c("x","y")), xy = T, layer = "FRI_ALLFIR") %>%
    drop_na() %>%
    dplyr::select(-one_of("ID", "FRI_ALLFIR"))
  #ext.2 <- terra::extract(forest_cover_mask, vect(ext.2, geom = c("x","y")),xy = T)%>%
  #  drop_na() %>%
#dplyr::select(-one_of("ID", "1984"))
  #, "FRI_ALLFIR"
  frame_domain <- template_DF %>%
    right_join(ext.2, by = c("x","y"))
  
  
  
  contemp_dat_full <- extract(mos_stack_30m,vect(frame_domain, geom = c("x","y")), xy = T)
 
  activeCat(bps_mask) <- 1
  codes <- extract(bps_mask,vect(frame_domain, geom = c("x","y")), xy =T)
  activeCat(bps_mask) <- 2
  zones <-extract(bps_mask,vect(frame_domain, geom = c("x","y")))
  activeCat(bps_mask) <- 3
  models <-extract(bps_mask,vect(frame_domain, geom = c("x","y")))
  
  rm(bps_mask)
  
  hist_dat_full <- full_join(codes, zones, by = "ID") %>%             
    full_join(., models, by = "ID")
  rm(codes, zones, models, ext.1, ext.2)
  
  hist_dat_full <- left_join(as.data.frame(hist_dat_full),bps_csv, by =c("BPS_CODE", "ZONE", "BPS_MODEL"))
  
  
  
  
  proportion <- .001
  sample.size <- trunc(dim(frame_domain)[1] * proportion)
  
  
  n <- list(sample.size)
  
  
  
  FRP_iter <- vector(length=boot_iterations)
  per_burn_high_sev <- vector(length=boot_iterations)
  combined_sev_est_its <- data.frame(data = NA, dim = c(sample.size*10, 2*boot_iterations))
  emd_freq <- vector(length = boot_iterations)
  emd_burned <- vector(length = boot_iterations)
  emd_est <- vector(length = boot_iterations)
  
  for (k in 1:boot_iterations){
  srs <- sample(1:nrow(frame_domain), sample.size)
  srs_locs <- frame_domain[srs,c(1,2)]
  contemp_dat <- contemp_dat_full[srs,] %>%
    dplyr::select(-one_of("x","y"))
  freq <- colSums(!is.na(t(contemp_dat[,-1])))
  
  contemp_dat$freq <- freq
  
  
  hist_dat <- hist_dat_full[srs,]
  #freq
  hist_dat_bps <- hist_dat %>%
    group_by(BPS_CODE) %>%
    summarise(n = n()) %>%
    mutate(relfreq = n/sum(n))

#  class_dat <- hist_dat %>%
#    rowwise %>%
#    mutate(class = class_select(PRC_SURFAC, PRC_MIXED, PRC_REPLAC)) %>%
#    mutate(freq = Fire_freq(class, FRI_SURFAC, FRI_MIXED, FRI_REPLAC)) %>% ###tweak this to meet your year range.
#    mutate(sev = Fire_sev(class)) %>%
#    ungroup()
  
  hist_dat_freq <- hist_dat %>%
    rowwise %>%
    mutate(freq = fire_freq_binom( FRI_ALLFIR))%>%
    ungroup()
  
  
  
  
  freq_dat_contemp <- contemp_dat %>% dplyr::select( freq)%>%mutate(time = "contemporary")
  freq_dat_hist <- hist_dat_freq %>% dplyr::select( freq) %>% mutate(time = "historic")
  freq_dat <- rbind(freq_dat_contemp, freq_dat_hist)

  
  
  FRP <- hist_dat_freq %>%
          summarize(meanFRP = (2021-1984)/mean(freq))
  FRP_iter[k] <- as.numeric(FRP)
    

  #EMD Implementation
  freqs <- seq(0,year_range, 1)
  fi <- freq_dat_contemp %>% 
    group_by(freq) %>%
    summarise(n = n()) %>%
    mutate(rel = n/sum(n)) %>%
    dplyr::select(freq,rel)
  mi <- freq_dat_hist %>%
    group_by(freq) %>%
    summarise(n=n()) %>%
    mutate(rel = n/sum(n)) %>%
    dplyr::select(freq, rel)
  normalized_emd_freq <- create_emd_hungarian_algo(fi,mi,"freq", normalized = F)/(year_range+1)

    emd_freq[k] <- normalized_emd_freq

  rm(freq_dat_contemp, freq_dat_hist)
  
  ###severity
  combined_sev_full <- contemp_dat %>% dplyr::select(-one_of(c("freq"))) %>% 
    pivot_longer(-c(ID), names_to = "year",values_to = "contemp_sev") %>%
    drop_na() %>%
    left_join(hist_dat, by = "ID") %>%
    mutate(class = class_select(PRC_SURFAC, PRC_MIXED, PRC_REPLAC)) %>%
    rowwise %>%
    mutate(hist_sev = Fire_sev(class)) %>% 
    mutate(contemp_class = class_select(x = contemp_sev, classify = T))%>%
    ungroup()
  contemp_sev <- combined_sev_full %>% 
    dplyr::select(contemp_sev) %>% 
    rename(sev = contemp_sev) %>%
    mutate(time = "contemporary")
  hist_sev<-  combined_sev_full %>%
    dplyr::select(hist_sev) %>% 
    rename(sev = hist_sev) %>%
    mutate(time = "historic")
  combined_sev_condensed <- rbind(contemp_sev, hist_sev)
 
  
  contemp_sev_class <- combined_sev_full %>% 
    dplyr::select(contemp_class) %>% 
    rename(sev = contemp_class) %>% 
    mutate(sev = as_factor(sev)) %>%
    mutate(sev = fct_relevel(sev,"low","mixed","high")) %>%
    mutate(time = "contemporary")
  
  
  
  hist_sev_class<-  combined_sev_full %>%
    dplyr::select(class) %>% 
    rename(sev = class) %>%
    mutate(sev = as_factor(sev)) %>%
    mutate(sev = fct_relevel(sev,"low","mixed","high")) %>%
    mutate(time = "historic")
  combined_class_condensed <- rbind(contemp_sev_class, hist_sev_class)
  
  rm(combined_sev_full)
  
  boot_sev <- contemp_dat %>% dplyr::select(-one_of(c("freq"))) %>% 
      pivot_longer(-c(ID), names_to = "year",values_to = "contemp_sev") %>%
      drop_na() %>%
      left_join(hist_dat, by = "ID") %>%
        mutate(class = class_select(PRC_SURFAC, PRC_MIXED, PRC_REPLAC))%>%
        group_by(class) %>%
        summarise(n = n()) %>%
        mutate(freq = n/sum(n)) %>%
        filter(class == "high")%>%
        dplyr::select(freq)
  
  rm(hist_dat,contemp_dat)
  per_burn_high_sev[k] <- as.numeric(boot_sev) 
  rm(boot_sev)
  
####severity EMD
  bins = seq(0,3, by = .1)
  fi <- contemp_sev %>% 
    mutate(bin = cut(sev, breaks = bins)) %>%
    group_by(bin) %>%
    summarise(n = n()) %>%
    mutate(rel = n/sum(n))%>% 
    select(bin, rel)

  mi <- hist_sev %>% 
    mutate(bin = cut(sev, breaks = bins)) %>%
    group_by(bin) %>%
    summarise(n = n()) %>%
    mutate(rel = n/sum(n)) %>%
    dplyr::select(bin, rel)
  
  normalized_emd_sev <- create_emd_hungarian_algo(fi,mi,"bin")
  #emd_df <- full_join(fi,mi, by = "new_bin", suffix = c(".fi",".mi"))
  #emd_df[is.na(emd_df)] <- 0
  #emd_df <- data.frame(emd_df) %>%
  #  mutate(di = rel.fi-rel.mi,  di_abs = abs(di))
  #emd <- sum(emd_df$di_abs)
  #normalized_emd_sev <- emd/dim(emd_df)[1]
  emd_burned[k] <-sqrt(normalized_emd_freq ^ 2 + normalized_emd_sev ^ 2) 
  
  rm(hist_sev)
#estimated severity for historical burned
hist_sev_est <- hist_dat_freq%>%
  uncount(., freq) %>% 
  mutate(class = class_select(PRC_SURFAC, PRC_MIXED, PRC_REPLAC)) %>%
  rowwise %>%
  mutate(hist_sev = Fire_sev(class)) %>%
  ungroup() %>%
  dplyr::select(hist_sev) %>%
  rename(sev = hist_sev) %>%
  mutate(time = "historic")

rm(hist_dat_freq)

name.sev <- paste0("sev.",k)
name.time <-paste0("time.",k)
combined_sev_est <- rbind(hist_sev_est, contemp_sev) %>%
  rename(!!name.sev := sev, !!name.time := time)


###do your emd analysis internal, will make this far lighter
bins = seq(0,3, by = .1)
fi <- contemp_sev %>% 
  mutate(bin = cut(sev, breaks = bins)) %>%
  group_by(bin) %>%
  summarise(n = n()) %>%
  mutate(rel = n/sum(n))%>% 
  select(bin, rel)

mi <- hist_sev_est %>% 
  mutate(bin = cut(sev, breaks = bins)) %>%
  group_by(bin) %>%
  summarise(n = n()) %>%
  mutate(rel = n/sum(n)) %>%
  dplyr::select(bin, rel)
normalized_emd_sev_est <- create_emd_hungarian_algo(fi,mi,"bin")
#emd_df <- full_join(fi,mi, by = "new_bin", suffix = c(".fi",".mi"))
#emd_df[is.na(emd_df)] <- 0
#emd_df <- data.frame(emd_df) %>%
#  mutate(di = rel.fi-rel.mi,  di_abs = abs(di))
#emd <- sum(emd_df$di_abs)
#normalized_emd_sev_est <- emd/dim(emd_df)[1]
  emd_est[k] <-sqrt(normalized_emd_freq ^ 2 + normalized_emd_sev_est ^ 2)
   rm(hist_sev_est,  contemp_sev)

  ##### extract values to points: contemporary
  #contemp_dat <- extract(mos_stack_30m,vect(srs, geom = c("x","y")))
  
  

  }
  emd_freq = list(emd_freq)
  emd_burned = list(emd_burned)
  emd_est = list(emd_est)
  hist_dat_bps <- list(hist_dat_bps)
combined_sev_est_its <- list(combined_sev_est_its)  
  ####frp
FRP_iter <- list(as_tibble(matrix(FRP_iter, ncol = 1), .name_repair = "minimal"))

mean_FRP <- freq_dat %>%
  group_by(time) %>%
  summarize(meanFRP = (2021-1984)/mean(freq))
mean_FRP <-list(mean_FRP)

  
#percent burn high severity
per_burn_high_sev <- list(as_tibble(matrix(per_burn_high_sev, ncol = 1), .name_repair = "minimal"))

contemp_relsev <- contemp_sev_class %>%
  group_by(sev) %>%
  summarise(n = n())%>%
  mutate(freq = n/sum(n))
hist_relsev <- hist_sev_class %>%
  group_by(sev) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n))
rm(contemp_sev_class, hist_sev_class)
contemp_relsev <-list(contemp_relsev)
hist_relsev <- list(hist_relsev)


  
  
  #######binomial classifcation of burned pixels
  
  
freq_compare_bar <- ggplot()+
  geom_bar(data = freq_dat, aes(x = freq, fill = time), position = "dodge") +
  ggtitle(paste0(title, " Frequency Bar Plot"))


freq_bar <- list(freq_compare_bar)

  #freq_qq <- ggplot(freq_dat, aes(sample=freq, fill = time))+
  #  geom_qq()
  

  
  ### elevation of fire frequency and presence
  topo <- rast("data/landscape_data/LF2020_Elev_220_CONUS/Tif/LC20_Elev_220.tif")
  x <- rep(srs_locs[,1],2)
  y <- rep(srs_locs[,2],2)
  freq_dat_xy <- cbind(freq_dat, x, y) 
  freq_dat_elev <- extract(topo, vect(freq_dat_xy, geom = c("x","y")))%>%
    cbind(freq_dat_xy)
  
  freq_dat_elev$pres <- 0
  freq_dat_elev$pres[which(freq_dat_elev$freq != 0)] <- 1
  freq_dat_elev$pres <- as.factor(freq_dat_elev$pres)
  freq_dat_elev$freq <- as.factor(freq_dat_elev$freq)
  
  elev_freq <- freq_dat_elev %>%
    ggplot(aes(x = freq, y = LC20_Elev_220, fill = time), position = "dodge")+
    geom_violin()+
    ggtitle(paste0(title, " Elevation and Fire Frequency Relation"))
  elev_fire_freq <- list(elev_freq)
  
  elev_pres <- freq_dat_elev %>%
    ggplot(aes(x = pres, y = LC20_Elev_220, fill = time), position = "dodge")+
    geom_violin()+
    ggtitle(paste0(title, " Elevation and Fire Presence Relation"))
  elev_fire_pres <- list(elev_pres)
  
  
  ####### severity for contemporary burned pixels
  
  sev_violin <- ggplot(combined_sev_condensed, aes(x = time, y = sev))+
    geom_violin()+
    stat_summary(fun=median, geom="point", size=2, color="red")+
    ggtitle(paste0(title, " Severity Violin Plot"))
  
  sev_violin <- list(sev_violin)
  
  
  sev_density <- ggplot(combined_sev_condensed, aes(x = sev, fill = time)) +
    geom_density( alpha =.4)+
    ggtitle(paste0(title, " Severity Density from Modern Burning"))
  
  sev_density_burned <- list(sev_density)
  
  #sev_qq <- ggplot(combined_sev_condensed, aes(sample = sev, color = time)) +
  #  geom_qq()
  
  
  
  freq_sev_each <- ggplot(combined_class_condensed, aes(x=sev, fill = time), position = "dodge")+
    geom_bar()+
    ggtitle(paste0(title, " Severity Class Bar Plot"))
  
  freq_sev_lm2h <- combined_class_condensed %>%
    mutate(sev = recode(sev, low = "low/mixed", mixed = "low/mixed")) %>%
    ggplot(aes(x = sev, fill = time), position = "dodge") +
    geom_bar() +
    ggtitle(paste0(title, " Severity Class low+mixed / high Bar Plot"))
  
  freq_sev_lm2h <- list(freq_sev_lm2h)
  freq_sev_each<- list(freq_sev_each)
  

  
  
  ########Severity for historic burned pixels

  
  sev_density_est <- ggplot(combined_sev_est, aes(x = sev, fill = time)) +
    geom_density( alpha =.4) +
    ggtitle(paste0())+
    ggtitle(paste0(title, " Severity Density From Estimated Burning"))
  rm(combined_sev_est)
  
  sev_density_est <- list(sev_density_est)
  
  

  return(c(name,
           mask_geom,
           freq_bar,
           FRP_iter,
           mean_FRP,
           elev_fire_freq,
           elev_fire_pres,
           sev_violin,
           sev_density_burned,
           freq_sev_each,
           freq_sev_lm2h,
           combined_sev_est_its,
           per_burn_high_sev,
           contemp_relsev,
           hist_relsev,
           n,
           hist_dat_bps,
           emd_burned,
           emd_est,
           emd_freq))

} 
toc()
stopImplicitCluster()
#

#save(stored_data,file = "data/stored_data.RData", overwrite = T)
data_names = c("name",
               "mask_geom",
               "freq_bar",
               "FRP_iter",
               "mean_FRP",
               "elev_fire_freq",
               "elev_fire_pres",
               "sev_violin",
               "sev_density_burned",
               "freq_sev_each",
               "freq_sev_lm2h",
               "combined_sev_est_its",
               "per_burn_high_sev",
               "contemp_relsev",
               "hist_relsev",
               "n",
               "hist_dat_bps",
               "emd_burned",
               "emd_est",
               "emd_freq")
index_num <- 1:length(data_names)
index_names <- data.frame(index = index_num, data_names = data_names)
ggnames<- index_names[c(3,6,7,8,9,10,11,12),]
mapnames<- data_names[c(19,20)]

#saving, uncomment if you want figures saved
#for(i in 1:length(folder_subset)){
#  for(j in 1:dim(ggnames)[1]){
#    index <- as.integer(ggnames[j,1])
#    subfolder <- as.character(ggnames[j,2])
#    png(paste0("figures/",subfolder,"/",folder_subset[i],"_",subfolder,".png"))
#    print(stored_data[[i]][[index]])
#    dev.off()
#  }


#  for(k in 1:length(mapnames)){
#    index <- which(names(stored_data) == mapnames[k])
#    title <- paste0(gsub("_"," ",stored_data$name[[i]])," Wilderness")
#    map <- gsub("_", " ",mapnames[k])
#    png(paste0("figures/",mapnames[k],"/",folder_subset[i],"_",mapnames[k],".png"))
#    plot(stored_data[[index]][[i]], main = paste0(title," ",map))
#    plot(stored_data$mask[[i]], add = T)
#    dev.off()
#  }
#}

#################################
#################################
#################################
#################################
#################################
##########deviations in freq and sev#######################
###deviation in freq

dev_freq = vector(length = length(folder_subset))
hist_mean_frp = vector(length = length(folder_subset))
mean_contemp_frp = vector(length = length(folder_subset))
outside_range_frp = vector(length = length(folder_subset))

for(i in 1:length(folder_subset)){
  hist_mean_frp[i] = mean(unlist(stored_data[[i]][[4]]))
  mean_contemp_frp[i] = as.numeric(stored_data[[i]][[5]][1,2])
  std_hist_frp = sd(unlist(stored_data[[i]][[4]]))#/(500^.5)
  dev_freq[i] = (contemp_mean_frp[i]-hist_mean_frp[i])/std_hist_frp
  range = range(stored_data[[i]][[4]])
  outside_range_frp[i] = as.numeric(contemp_mean_frp[i]<range[1] | contemp_mean_frp[i]>range[2])

}
  
###deviation in sev
dev_sev = vector(length = length(folder_subset))
hist_mean_pbhs = vector(length = length(folder_subset))
mean_contemp_pbhs = vector(length = length(folder_subset))
outside_range_pbhs = vector(length = length(folder_subset))
for(i in 1:length(folder_subset)){
  hist_mean_pbhs[i] = mean(unlist(stored_data[[i]][[15]][3,3]))
  mean_contemp_pbhs[i] = as.numeric(stored_data[[i]][[14]][3,3])
  std_hist_pbhs = sd(unlist(stored_data[[i]][[13]]))#/(500^.5)
  dev_sev[i] = (contemp_mean_pbhs[i]-hist_mean_pbhs[i])/std_hist_pbhs
  range = range(stored_data[[i]][[13]])
  outside_range_pbhs[i] = as.numeric(contemp_mean_pbhs[i]<range[1] | contemp_mean_pbhs[i]>range[2])
}
frp_all = tibble(name = folder_subset, 
                     dev_freq = dev_freq, 
                     hist = hist_mean_frp, 
                     contemp = mean_contemp_frp,
                     outside_range_frp = outside_range_frp)%>%
          mutate(ref.x = 0, diff.frp = contemp-hist) %>%
  pivot_longer(c(hist,contemp), names_to = "time", values_to = "mean_frp")
pbhs_all = tibble(name = folder_subset,
                     dev_sev= dev_sev,
                     hist = hist_mean_pbhs,
                     contemp = mean_contemp_pbhs,
                     outside_range_pbhs = outside_range_pbhs)%>%
  pivot_longer(c(hist,contemp), names_to = "time", values_to = "mean_pbhs")

all_val = left_join(frp_all, pbhs_all, by = c("name","time"))
name = gsub("_", " ", all_val$name)
all_val$name = name
options(repr.plot.width = 15, repr.plot.height =15)
hist_val = all_val %>% filter(time == "hist")
contemp_val = all_val %>% filter(time == "contemp")

absolute_plot <- ggplot()+
  geom_point(aes(x = mean_frp, y = mean_pbhs),hist_val, color = "grey", alpha = .6, size = 5)+
  geom_point(aes(x = mean_frp, y = mean_pbhs, color = name),contemp_val, alpha = .6, size = 5)+
  geom_line(aes(x = mean_frp, y = mean_pbhs, group = name),all_val)+
  scale_x_log10()+
  geom_text_repel( aes(x = mean_frp, y = mean_pbhs, label = name,  color = name), contemp_val,size = 5, max.overlaps = 25, show.legend = F)+
  labs(y = "Proportion Burned at High Severity", x = "Calculated FRP(yrs)")+
  theme_bw(base_size = 20)




frp_all_diff = tibble(name = folder_subset, 
                dev_freq = dev_freq, 
                hist = hist_mean_frp, 
                contemp = mean_contemp_frp,
                outside_range_frp = outside_range_frp)%>%
  mutate(hist_diff = 0, contemp_diff = contemp-hist) %>%
  pivot_longer(c(hist_diff, contemp_diff), names_to = "time", values_to = "diff_frp")


pbhs_all_diff = tibble(name = folder_subset,
                  dev_sev= dev_sev,
                  hist = hist_mean_pbhs,
                  contemp = mean_contemp_pbhs,
                  outside_range_pbhs = outside_range_pbhs)%>%
  mutate(hist_diff = 0, contemp_diff = contemp-hist)%>%
  pivot_longer(c(hist_diff, contemp_diff), names_to = "time", values_to = "diff_pbhs")

all_val_diff = left_join(frp_all_diff, pbhs_all_diff, by = c("name","time"))
all_val_diff$name = gsub("_", " ", all_val_diff$name)
options(repr.plot.width = 15, repr.plot.height =15)
hist_val_diff = all_val_diff %>% filter(time == "hist_diff")
contemp_val_diff = all_val_diff %>% filter(time == "contemp_diff")




diff_plot <- ggplot()+
  geom_point(aes(x = diff_frp, y = diff_pbhs),hist_val_diff, color = "grey", alpha = .6, size = 5)+
  geom_point(aes(x = diff_frp, y = diff_pbhs, color = name),contemp_val_diff, alpha = .6, size = 5)+
  geom_line(aes(x = diff_frp, y = diff_pbhs, group = name),all_val_diff)+
  #scale_x_log10()+
  geom_text_repel( aes(x = diff_frp, y = diff_pbhs, label = name,  color = name), contemp_val_diff,size = 5, max.overlaps = 25,  show.legend = F)+
  labs(y = "Difference in Proportion Burned at High Severity", x = "Difference in Calculated FRP(yrs)")+
  theme_bw(base_size = 20)
med_diff = all_val_diff %>%
  filter(time == "contemp_diff" ) %>%
  summarise(med_frp_diff = median(diff_frp), med_pbhs_diff = median(diff_pbhs)) %>%
  mutate(name = "Median")
med_vals = all_val %>%
  filter(time == "contemp") %>%
  summarise(med_frp = median(mean_frp), med_pbhs = median(mean_pbhs))%>%
  mutate(name = "Median")

absolute_plot +geom_point(aes(x=med_frp, y = med_pbhs), med_vals, color = "black", size = 5)+
  geom_text_repel( aes(x = med_frp, y = med_pbhs, label = name), med_vals,  color = 'black',size = 5, max.overlaps = 25,  show.legend = F)



diff_plot +geom_point(aes(x=med_frp_diff, y = med_pbhs_diff), med_diff, color = "black", size = 5)+
  geom_text_repel( aes(x = med_frp_diff, y = med_pbhs_diff, label = name), med_diff,  color = 'black',size = 5, max.overlaps = 25,  show.legend = F)


####EMD 
emd_burned <- vector(length = length(folder_subset))
emd_est <- vector(length = length(folder_subset))
emd_freq <- vector(length = length(folder_subset))
for(i in 1:length(folder_subset)){
  emd_burned[i] <- quantile(unlist(stored_data[[i]][[18]]), .5)
  
  emd_est[i] <- quantile(unlist(stored_data[[i]][[19]]), .5)
  
  emd_freq[i] <- quantile(unlist(stored_data[[i]][[20]]), .5)
}
#########MFRI vs EMD

hist_val_diff <- hist_val_diff %>%
  mutate(emd_burned = 0,emd_est = 0, emd_freq =0)
contemp_val_diff <- cbind(contemp_val_diff, emd_burned, emd_est, emd_freq)
all_val_diff <- rbind(hist_val_diff,contemp_val_diff)


emd_diff_plot <- ggplot()+
  geom_point(aes(x = diff_frp, y = emd_zero_burned),hist_val_diff, color = "grey", alpha = .6, size = 5)+
  geom_point(aes(x = diff_frp, y = emd_burned, color = name),contemp_val_diff, alpha = .6, size = 5)+
  #geom_line(aes(x = diff_frp, y = emd_burned, group = name),all_val_diff)+
  #scale_x_log10()+
  geom_text_repel( aes(x = diff_frp, y = emd_burned, label = name,  color = name), contemp_val_diff,size = 5, max.overlaps = 25,  show.legend = F)+
  labs(y = "Earth Mover's Distance", x = "Difference in Calculated FRP(yrs)")+
  theme_bw(base_size = 20)

med_diff_emd = all_val_diff %>%
  filter(time == "contemp_diff" ) %>%
  summarise(med_frp_diff = median(diff_frp), med_emd = median(emd_burned), med_emd_est = median(emd_est), med_emd_freq = median(emd_freq), name = "Median")

emd_diff_plot +geom_point(aes(x=med_frp_diff, y = med_emd), med_diff_emd, color = "black", size = 5)+
  geom_text_repel( aes(x = med_frp_diff, y = med_emd , label = name), med_diff_emd,  color = 'black',size = 5, max.overlaps = 25,  show.legend = F)

##########

emd_freq_diff_plot <- ggplot()+
  geom_point(aes(x = diff_frp, y = emd_zero_freq),hist_val_diff, color = "grey", alpha = .6, size = 5)+
  geom_point(aes(x = diff_frp, y = emd_freq, color = emd_burned),contemp_val_diff, alpha = .6, size = 5)+
  #geom_line(aes(x = diff_frp, y = emd_freq, group = name),all_val_diff)+
  #scale_x_log10()+
  geom_text_repel( aes(x = diff_frp, y = emd_freq, label = name,  color = name), contemp_val_diff,size = 5, max.overlaps = 25,  show.legend = F)+
  labs(y = "Earth Mover's Distance scaled by 30x", x = "Difference in Calculated FRP(yrs)")+
  theme_bw(base_size = 20)



emd_freq_diff_plot +geom_point(aes(x=med_frp_diff, y = med_emd_freq), med_diff_emd, color = "black", size = 5)+
  geom_text_repel( aes(x = med_frp_diff, y = med_emd_freq , label = name), med_diff_emd,  color = 'black',size = 5, max.overlaps = 25,  show.legend = F)


###emd 3d
library(plotly)
emd_diff_plot_3d <- plot_ly(data = contemp_val_diff,x = ~diff_frp, y = ~diff_pbhs, z = ~emd_burned, color = ~count, type = "scatter3d", mode = "markers")

emd_diff_plot_3d_gg <- ggplot()+
  geom_point(aes(x = diff_frp, y = diff_pbhs),hist_val_diff, color = "grey", alpha = .6, size = 5)+
  geom_point(aes(x = diff_frp, y = diff_pbhs, color = emd_burned),contemp_val_diff, alpha = .6, size = 5)+
  geom_line(aes(x = diff_frp, y = diff_pbhs, group = name),all_val_diff)+
  #scale_x_log10()+
  geom_text_repel( aes(x = diff_frp, y = diff_pbhs, label = name,  color = emd_burned), contemp_val_diff,size = 5, max.overlaps = 25,  show.legend = F)+
  labs(y = "Difference in Proportion Burned at High Severity", x = "Difference in Calculated FRP(yrs)")+
  theme_bw(base_size = 20)

###emd map
contemp_val_diff$geom <- NULL
for(i in 1:length(folder_subset)){
contemp_val_diff$geom[i] <- st_geometry(st_union(stored_data[[i]][[2]]))
}
states <- st_read("data/masks/na_states_aea.shp")
xlim = st_bbox(contemp_val_diff_sf)[c(1,3)]
ylim = st_bbox(contemp_val_diff_sf)[c(2,4)]
contemp_val_diff_sf <- st_as_sf(contemp_val_diff, crs = st_crs(mask_all))
emd_map <- ggplot()+
  geom_sf(data = states, fill = "grey95")+
  geom_sf(aes(fill = emd_burned),data = contemp_val_diff_sf)+
  coord_sf(xlim = xlim, ylim = ylim)+
  theme_bw()
emd_map +
  scale_color_viridis_c(option = "inferno", aesthetics = "fill")




contemp_val_diff_sf$emd_burned_rank <- as.factor(rank(-contemp_val_diff$emd_burned))
contemp_val_diff_sf$emd_freq_rank <- as.factor(rank(-contemp_val_diff$emd_freq))


emd_map_rank <- ggplot()+
  geom_sf(data = states, fill = "grey95")+
  geom_sf(aes(fill = emd_burned_rank),data = contemp_val_diff_sf)+
  coord_sf(xlim = xlim, ylim = ylim)+
  theme_bw()
emd_map_rank +
  scale_color_viridis_d(option = "inferno", aesthetics = "fill", direction = -1)
View(arrange(contemp_val_diff_sf, emd_freq_rank) %>%
  select(name, emd_freq_rank))
