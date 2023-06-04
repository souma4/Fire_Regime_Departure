pkgs <- c("tidyverse")
invisible(lapply(pkgs, library, character.only = T))
Elevation_fire_frequencies <- function(topo_rast_path,
                                       sample_locations,
                                       freq_dat){

topo <- rast("data/landscape_data/LF2020_Elev_220_CONUS/Tif/LC20_Elev_220.tif")
x <- rep(sample_locations[,1],2)
y <- rep(sample_locations[,2],2)
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
}