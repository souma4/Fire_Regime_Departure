library(tidyverse)
library(svglite)
library(plotly)
library(sf)
library(viridis)
library(terra)
library(RColorBrewer)
library(ggpubr)
library(ggridges)
library(randomcoloR)
library(ggrepel)
library(ggspatial)

library(effectsize)

# instantiate ----
path_hexcel <- file.path(
  getwd(), "data", "outputs",
  "med_grids", "med_hex_forested",
  "!summaries", "med_hex_forested.gpkg"
)

path_wilderness <- file.path(
  getwd(), "data", "outputs",
  "pas", "pas_forested",
  "!summaries", "pas_forested.gpkg"
)
path_firesheds <- file.path(
  getwd(), "data", "outputs",
  "firesheds", "firesheds_forested",
  "!summaries", "firesheds_forested.gpkg"
)

library(classInt)


path_states <- file.path(getwd(), "data", "masks", "cleaned", "wna_states.shp")

states_df <- st_read(path_states, quiet = T)

df_joined <- st_read(path_hexcel, quiet = T) %>%
  st_join(states_df) %>%
  filter(study_area_ha >= 0.5 * max(study_area_ha))
wilderness <- st_read(path_wilderness, quiet = T) %>%
  st_join(states_df)
firesheds <- st_read(path_firesheds, quiet = T) %>%
  st_join(states_df)
priority_landscapes <- st_read("data/masks/cleaned/priority_landscapes/S_USA.BdyPln_BIL_LandscpInvestment.shp") %>%
  st_transform(crs = st_crs(firesheds))
klamath <- priority_landscapes %>%
  filter(NAME == "Klamath River Basin")

mainpath <- file.path(getwd(), "figures", "Final_Figures", "main")
appendixpath <- file.path(getwd(), "figures", "Final_Figures", "appendix")
supplementalpath <- file.path(getwd(), "figures", "Final_Figures", "supplemental")
outpath <- file.path(getwd(), "figures", "Final_Figures")

percent_lessfrequent_moresevere <- df_joined %>%
  as.data.frame() %>%
  select(signed_emd_frequency, signed_emd_severity) %>%
  mutate(
    freq_dir = ifelse(sign(signed_emd_frequency) == 1, "less frequent", "more frequent"),
    sev_dir = ifelse(sign(signed_emd_severity) == 1, "more severe", "less severe")
  ) %>%
  group_by(freq_dir, sev_dir) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(rel_freq = n/ sum(n))

# Main Maps ----
study_area <- ggplot() +
  geom_sf(data = states_df, fill = "grey90", color = "black") +
  geom_sf(data = df_joined, fill = "grey40", color = "black") +
  annotation_scale(location = "bl", width_hint = 0.5, style = "ticks") +
  theme_bw()
ggsave(paste0(mainpath, "/study_area.jpeg"), study_area, width = 8, height = 6, units = "in")

fireRegimeDeparture_plot <- ggplot() +
  geom_sf(data = states_df, fill = "grey90", color = "black") +
  geom_sf(data = df_joined, aes(fill = emd_both), linewidth = 0) +
  scale_fill_viridis_c(option = "A") +
  labs(fill = "Fire regime departure") +
  theme_minimal() +
  theme(plot.margin = ggplot2::margin(0.1, 0.1, 1, 0, "cm"),
        axis.text = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "bottom")

 #ggsave(paste0(mainpath, "/klamath_departure.jpeg"), klamath_departure_plot, width = 10, height = 10, units = "in")

#fireshed departure plot

fireshedDeparture_plot <- ggplot() +
  geom_sf(data = states_df, fill = "grey90", color = "black") +
  geom_sf(data = firesheds, aes(fill = emd_both), linewidth = 0) +
  scale_fill_viridis_c(option = "A") +
  labs(fill = "Fire regime departure") +
  theme_minimal() +
  theme(plot.margin = ggplot2::margin(0.1, 0.1, 1, 0, "cm"),
        axis.text = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "bottom")


# Main boxplots ----
median_ranks <- df_joined %>%
  group_by(NAME) %>%
  summarise(across(c(emd_both, signed_emd_frequency, signed_emd_severity), median)) %>%
  mutate(across(c(emd_both, signed_emd_frequency, signed_emd_severity), rank, .names = "{.col}_rank")) %>%
  select(NAME, emd_both_rank, signed_emd_frequency_rank, signed_emd_severity_rank)
boxplot_df <- df_joined %>%
  as.data.frame() %>%
  select(NAME, signed_emd_frequency, signed_emd_severity, emd_both) %>%
  left_join(median_ranks, by = "NAME") %>%
  drop_na()


total_departure_boxplot <- boxplot_df %>%
  mutate(NAME = factor(NAME, levels = arrange(median_ranks, desc(emd_both_rank))$NAME)) %>%
  ggplot(aes(x = NAME, y = emd_both)) +
  geom_boxplot() +
  labs(x = "", y = "Fire regime departure") +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.title = element_text(size = 14)
  )
departure_CA2OR <- boxplot_df %>%
  group_by(NAME) %>%
  summarize(across(c(emd_both, signed_emd_frequency, signed_emd_severity), median))
# what percent larger is CA to OR
CA2OR_percent <- departure_CA2OR %>%
  filter(NAME %in% c("California", "Oregon"))
CA2OR_percent$emd_both[1] / CA2OR_percent$emd_both[2]
  
  


# test by state
state_wilcox <- pairwise.wilcox.test(
  boxplot_df$emd_both,
  boxplot_df$NAME,
  p.adjust.method = "bonferroni"
)



# diverging color ramp ----
## create color matrix
# colmat<-function(nquantiles=10, upperleft=rgb(0,150,235, maxColorValue=255),
#                  upperright=rgb(130,0,80, maxColorValue=255),
#                  bottomleft="grey", bottomright=rgb(255,230,15, maxColorValue=255),
#                  diverging = NULL,
#                  xlab="x label", ylab="y label"){
#   my.data<-seq(0,1,.01)
#   my.class<-classIntervals(my.data,n=nquantiles,style="quantile")
#
#
#   if(!is.null(diverging)){
#   my.pal.1<-findColours(my.class,c(upperleft,bottomleft))
#   my.pal.2<-findColours(my.class,c(upperright, bottomright))
#
#
#   col.matrix<-matrix(nrow = 101, ncol = 101, NA)
#   for(i in 1:101){
#     my.col<-c(paste(my.pal.1[i]),paste(my.pal.2[i]))
#
#       col.matrix[102-i,]<-findColours(my.class,my.col)
#
#     }
#   plot(c(1,1),pch=19,col=my.pal.1, cex=0.5,xlim=c(0,1),ylim=c(0,1),frame.plot=F, xlab=xlab, ylab=ylab,cex.lab=1.3)
#   for(i in 1:101){
#     col.temp<-col.matrix[i-1,]
#     points(my.data,rep((i-1)/100,101),pch=15,col=col.temp, cex=1)
#     }
#   seqs<-seq(0,100,(100/nquantiles))
#   seqs[1]<-1
#   col.matrix<-col.matrix[c(seqs), c(seqs)]
#   } else{
#     left_side <- findColours(my.class, c(upperleft, diverging$middle_left, bottomleft))
#     right_side <- findColours(my.class, c(upperright, diverging$middle_right, bottomright))
#     top_side <- findColours(my.class, c(upperleft, diverging$middle_top, upperright))
#     bottom_side <- findColours(my.class, c(bottomleft, diverging$middle_bottom, bottomright))
#     middle_column <- findColours(my.class, c(diverging$middle_bottom, diverging$middle, diverging$middle_top))
#     middle_row <- findColours(my.class, c(diverging$middle_left, diverging$middle, diverging$middle_right))
#     col.matrix <- matrix(nrow = 101, ncol = 101, NA)
#     col.matrix[1,] <- top_side
#     col.matrix[101,] <- bottom_side
#     col.matrix[,1] <- left_side
#     col.matrix[,101] <- right_side
#     col.matrix[50,] <- middle_row
#     col.matrix[,50] <- middle_column
#
#   }
#   }
#
# diverging <- list(middle_left = "#FEE0B6",
#                   middle_top = "#c2a5cf",
#                   middle_right = "#fde0ef",
#                   middle_bottom = "#d9f0d3",
#                   middle = "#f7f7f7")
# bivariate.map<-function(rasterx, rastery, colormatrix=col.matrix, nquantiles=10){
#   quanmean<-values(rasterx)
#   temp<-data.frame(quanmean, quantile=rep(NA, length(quanmean)))
#   brks<-with(temp, quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles))))
#   r1<-within(temp, quantile <- cut(quanmean, breaks = brks, labels = 2:length(brks),include.lowest = TRUE))
#   quantr<-data.frame(r1[,2])
#   quanvar<-values(rastery)
#   temp<-data.frame(quanvar, quantile=rep(NA, length(quanvar)))
#   brks<-with(temp, quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles))))
#   r2<-within(temp, quantile <- cut(quanvar, breaks = brks, labels = 2:length(brks),include.lowest = TRUE))
#   quantr2<-data.frame(r2[,2])
#   as.numeric.factor<-function(x) {as.numeric(levels(x))[x]}
#   col.matrix2<-colormatrix
#   cn<-unique(colormatrix)
#   for(i in 1:length(col.matrix2)){
#     ifelse(is.na(col.matrix2[i]),col.matrix2[i]<-1,col.matrix2[i]<-which(col.matrix2[i]==cn)[1])}
#   cols<-numeric(length(quantr[,1]))
#   for(i in 1:length(quantr[,1])){
#     a<-as.numeric.factor(quantr[i,1])
#     b<-as.numeric.factor(quantr2[i,1])
#     cols[i]<-as.numeric(col.matrix2[b,a])}
#   r<-rasterx
#   r[1:length(r)]<-cols
#   return(r)}
#
#
# col.matrix<-colmat(nquantiles=10)
# col.matrix<-colmat(nquantiles=10, upperleft="#5b4da1", upperright="#0d8943", bottomleft="#ea0d7f", bottomright="#f07621",
#                   xlab="My x label", ylab="My y label")


#what percentage of data is less frequent more severe
df_joined %>%
  filter(sign(signed_emd_frequency) == 1 & sign(signed_emd_severity) == 1) %>%
  nrow() / nrow(df_joined)
#89% of the data is less frequent and more severe


df_joined_div <- df_joined %>%
  mutate(
    color_ramp_freq_code = ifelse(signed_emd_frequency > quantile(emd_frequency, 0.5, na.rm = T, names = F), 1,
      ifelse(signed_emd_frequency < -quantile(emd_frequency, 0.5, na.rm = T, names = F), -1, 0)
    ),
    color_ramp_sev_code = ifelse(signed_emd_severity > quantile(emd_severity, 0.5, na.rm = T, names = F), 1,
      ifelse(signed_emd_severity < -quantile(emd_severity, 0.5, na.rm = T, names = F), -1, 0)
    ),
    color_ramp_color = case_when(
      color_ramp_freq_code == 1 & color_ramp_sev_code == 1 ~ "#c51b7d",
      color_ramp_freq_code == 1 & color_ramp_sev_code == -1 ~ "#4d9221",
      color_ramp_freq_code == -1 & color_ramp_sev_code == 1 ~ "#542788",
      color_ramp_freq_code == -1 & color_ramp_sev_code == -1 ~ "#b35806",
      color_ramp_freq_code == 0 & color_ramp_sev_code == 0 ~ "#f7f7f7",
      color_ramp_freq_code == 0 & color_ramp_sev_code == 1 ~ "#c2a5cf",
      color_ramp_freq_code == 0 & color_ramp_sev_code == -1 ~ "#d9f0d3",
      color_ramp_freq_code == 1 & color_ramp_sev_code == 0 ~ "#fde0ef",
      color_ramp_freq_code == -1 & color_ramp_sev_code == 0 ~ "#fee0b6"
    ),
    b = (signed_emd_frequency + max(emd_frequency)) / (2 * max(emd_frequency)),
    r = (signed_emd_severity + max(emd_severity)) / (2 * max(emd_severity)),
    g = 0.5,
    rgb = rgb(r, g, -(b - 1))
  ) %>%
  select(color_ramp_color, rgb)

#diverging color ramp
## create color matrix
# colmat<-function(nquantiles=10, upperleft=rgb(0,150,235, maxColorValue=255),
#                  upperright=rgb(130,0,80, maxColorValue=255), 
#                  bottomleft="grey", bottomright=rgb(255,230,15, maxColorValue=255), 
#                  diverging = NULL,
#                  xlab="x label", ylab="y label"){
#   my.data<-seq(0,1,.01)
#   my.class<-classIntervals(my.data,n=nquantiles,style="quantile")
#   
#  
#   if(!is.null(diverging)){
#   my.pal.1<-findColours(my.class,c(upperleft,bottomleft))
#   my.pal.2<-findColours(my.class,c(upperright, bottomright))
#   
#   
#   col.matrix<-matrix(nrow = 101, ncol = 101, NA)
#   for(i in 1:101){
#     my.col<-c(paste(my.pal.1[i]),paste(my.pal.2[i]))
#     
#       col.matrix[102-i,]<-findColours(my.class,my.col)
#       
#     }
#   plot(c(1,1),pch=19,col=my.pal.1, cex=0.5,xlim=c(0,1),ylim=c(0,1),frame.plot=F, xlab=xlab, ylab=ylab,cex.lab=1.3)
#   for(i in 1:101){
#     col.temp<-col.matrix[i-1,]
#     points(my.data,rep((i-1)/100,101),pch=15,col=col.temp, cex=1)
#     }
#   seqs<-seq(0,100,(100/nquantiles))
#   seqs[1]<-1
#   col.matrix<-col.matrix[c(seqs), c(seqs)]
#   } else{
#     left_side <- findColours(my.class, c(upperleft, diverging$middle_left, bottomleft))
#     right_side <- findColours(my.class, c(upperright, diverging$middle_right, bottomright))
#     top_side <- findColours(my.class, c(upperleft, diverging$middle_top, upperright))
#     bottom_side <- findColours(my.class, c(bottomleft, diverging$middle_bottom, bottomright))
#     middle_column <- findColours(my.class, c(diverging$middle_bottom, diverging$middle, diverging$middle_top))
#     middle_row <- findColours(my.class, c(diverging$middle_left, diverging$middle, diverging$middle_right))
#     col.matrix <- matrix(nrow = 101, ncol = 101, NA)
#     col.matrix[1,] <- top_side
#     col.matrix[101,] <- bottom_side
#     col.matrix[,1] <- left_side
#     col.matrix[,101] <- right_side
#     col.matrix[50,] <- middle_row
#     col.matrix[,50] <- middle_column
#   
#   }
#   }
# 
# diverging <- list(middle_left = "#FEE0B6",
#                   middle_top = "#c2a5cf",
#                   middle_right = "#fde0ef",
#                   middle_bottom = "#d9f0d3",
#                   middle = "#f7f7f7")
# bivariate.map<-function(rasterx, rastery, colormatrix=col.matrix, nquantiles=10){
#   quanmean<-values(rasterx)
#   temp<-data.frame(quanmean, quantile=rep(NA, length(quanmean)))
#   brks<-with(temp, quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles))))
#   r1<-within(temp, quantile <- cut(quanmean, breaks = brks, labels = 2:length(brks),include.lowest = TRUE))
#   quantr<-data.frame(r1[,2]) 
#   quanvar<-values(rastery)
#   temp<-data.frame(quanvar, quantile=rep(NA, length(quanvar)))
#   brks<-with(temp, quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles))))
#   r2<-within(temp, quantile <- cut(quanvar, breaks = brks, labels = 2:length(brks),include.lowest = TRUE))
#   quantr2<-data.frame(r2[,2])
#   as.numeric.factor<-function(x) {as.numeric(levels(x))[x]}
#   col.matrix2<-colormatrix
#   cn<-unique(colormatrix)
#   for(i in 1:length(col.matrix2)){
#     ifelse(is.na(col.matrix2[i]),col.matrix2[i]<-1,col.matrix2[i]<-which(col.matrix2[i]==cn)[1])}
#   cols<-numeric(length(quantr[,1]))
#   for(i in 1:length(quantr[,1])){
#     a<-as.numeric.factor(quantr[i,1])
#     b<-as.numeric.factor(quantr2[i,1])
#     cols[i]<-as.numeric(col.matrix2[b,a])}
#   r<-rasterx
#   r[1:length(r)]<-cols
#   return(r)}
# 
# 
#col.matrix<-colmat(nquantiles=10)
#col.matrix<-colmat(nquantiles=10, upperleft="#5b4da1", upperright="#0d8943", bottomleft="#ea0d7f", bottomright="#f07621",
#                   xlab="My x label", ylab="My y label")






df_joined_div <- df_joined %>%
  mutate(color_ramp_freq_code = ifelse(signed_emd_frequency > quantile(emd_frequency, 0.5, na.rm = T, names = F) , 1, 
                                       ifelse(signed_emd_frequency < -quantile(emd_frequency, 0.5, na.rm = T, names = F), -1, 0)),
         color_ramp_sev_code = ifelse(signed_emd_severity > quantile(emd_severity, 0.5, na.rm = T, names = F) , 1,
                                      ifelse(signed_emd_severity < -quantile(emd_severity, 0.5, na.rm = T, names = F), -1, 0)),
         color_ramp_color = case_when(color_ramp_freq_code == 1 & color_ramp_sev_code == 1 ~ "#c51b7d",
                                           color_ramp_freq_code == 1 & color_ramp_sev_code == -1 ~ "#4d9221",
                                           
                                           color_ramp_freq_code == -1 & color_ramp_sev_code == 1 ~ "#542788",
                                           color_ramp_freq_code == -1 & color_ramp_sev_code == -1 ~ "#b35806",
                                           
                                           color_ramp_freq_code == 0 & color_ramp_sev_code == 0 ~ "#f7f7f7",
                                           
                                           color_ramp_freq_code == 0 & color_ramp_sev_code == 1 ~ "#c2a5cf",
                                           color_ramp_freq_code == 0 & color_ramp_sev_code == -1 ~ "#d9f0d3",
                                           
                                           color_ramp_freq_code == 1 & color_ramp_sev_code == 0 ~ "#fde0ef",
                                           color_ramp_freq_code == -1 & color_ramp_sev_code == 0 ~ "#fee0b6"),
         b = (signed_emd_frequency + max(emd_frequency))/(2*max(emd_frequency)),
         r = (signed_emd_severity + max(emd_severity))/(2*max(emd_severity)),
         g = 0.5,
         rgb = rgb(r,g, -(b-1))) %>%
         select(color_ramp_color, rgb)
diverging_departure_plot <- ggplot() +
  geom_sf(data = states_df, fill = "grey90", color = "black") +
  geom_sf(data = df_joined_div, fill = df_joined_div$color_ramp_color, linewidth = 0) +
  theme_bw() +
  theme(axis.text = element_text(size = 12))
diverging_departure_plot
 ggsave(paste0(mainpath,"/diverging_departure_map.jpg"), plot = diverging_departure_plot, width = 8, height = 6, units = "in")

quantiles_freq <- quantile(df_joined$emd_frequency, c(0.33, .66), na.rm = T, names = F)
quantiles_sev <- quantile(df_joined$emd_severity, c(0.33, .66), na.rm = T, names = F)
diverging_departure_5x5 <- df_joined %>%
  mutate(
    color_ramp_freq_code = ifelse(between(signed_emd_frequency, quantiles_freq[1], quantiles_freq[2]), 1,
      ifelse(signed_emd_frequency > quantiles_freq[2], 2,
        ifelse(between(signed_emd_frequency, -quantiles_freq[1], -quantiles_freq[2]), -1,
          ifelse(signed_emd_frequency < -quantiles_freq[2], -2, 0)
        )
      )
    ),
    color_ramp_sev_code = ifelse(between(signed_emd_severity, quantiles_sev[1], quantiles_sev[2]), 1,
      ifelse(signed_emd_severity > quantiles_sev[2], 2,
        ifelse(between(signed_emd_severity, -quantiles_sev[1], -quantiles_sev[2]), -1,
          ifelse(signed_emd_severity < -quantiles_sev[2], -2, 0)
        )
      )
    ),
    color_ramp_color = case_when(
      color_ramp_freq_code == 2 & color_ramp_sev_code == 2 ~ "#c51b7d", # top right
      color_ramp_freq_code == 2 & color_ramp_sev_code == -2 ~ "#4d9221", # bottom right

      color_ramp_freq_code == -2 & color_ramp_sev_code == 2 ~ "#542788", # top left
      color_ramp_freq_code == -2 & color_ramp_sev_code == -2 ~ "#b35806", # bottom left

      color_ramp_freq_code == 0 & color_ramp_sev_code == 0 ~ "#f7f7f7", # middle

      color_ramp_freq_code == 0 & color_ramp_sev_code == 2 ~ "#c2a5cf", # top middle
      color_ramp_freq_code == 0 & color_ramp_sev_code == -2 ~ "#d9f0d3", # bottom middle

      color_ramp_freq_code == 2 & color_ramp_sev_code == 0 ~ "#fde0ef", # right middle
      color_ramp_freq_code == -2 & color_ramp_sev_code == 0 ~ "#fee0b6", # left middle
      # far right side
      color_ramp_freq_code == 2 & color_ramp_sev_code == 1 ~ "#e9a3c9", # rightup1
      color_ramp_freq_code == 2 & color_ramp_sev_code == -1 ~ "#A5B988", # rightdown1
      # far top
      color_ramp_freq_code == 1 & color_ramp_sev_code == 2 ~ "#C360A6", # topright1
      color_ramp_freq_code == -1 & color_ramp_sev_code == 2 ~ "#8B66AB", # topleft1
      # far left
      color_ramp_freq_code == -2 & color_ramp_sev_code == 1 ~ "#A9839F", # leftup1
      color_ramp_freq_code == -2 & color_ramp_sev_code == -1 ~ "#D89C5E", # leftdown1
      # far bottom
      color_ramp_freq_code == -1 & color_ramp_sev_code == -2 ~ "#C5A36c", # bottomleft1
      color_ramp_freq_code == 1 & color_ramp_sev_code == -2 ~ "#93C179", # bottomright1
      # middle column
      color_ramp_freq_code == 0 & color_ramp_sev_code == 1 ~ "#DCCEE3", # middleup1
      color_ramp_freq_code == 0 & color_ramp_sev_code == -1 ~ "#E8F3E4", # middledown1
      # middle row
      color_ramp_freq_code == 1 & color_ramp_sev_code == 0 ~ "#FAEBF3", # middleright1
      color_ramp_freq_code == -1 & color_ramp_sev_code == 0 ~ "#FAEBD6", # middleleft1
      # rightup1
      color_ramp_freq_code == 1 & color_ramp_sev_code == 1 ~ "#F1C6DE", # rightup1
      # rightdown1
      color_ramp_freq_code == 1 & color_ramp_sev_code == -1 ~ "#C6D5B5", # rightdown1
      # leftup1
      color_ramp_freq_code == -1 & color_ramp_sev_code == 1 ~ "#C2A8C0", # leftup1
      # leftdown1
      color_ramp_freq_code == -1 & color_ramp_sev_code == -1 ~ "#DFC6A1" # leftdown1
    )
  )
# test color palette
library(colorBlindness)
# build tiles for each color
tile_df <- data.frame(
  x = rep(-2:2, 5),
  y = rep(-2:2, each = 5),
  color = c("#b35806", "#c5a36c", "#d9f0d3", "#93c179", "#4d9221",
            "#d89c5e", "#dfc6a1", "#e8f3e4", "#c6d5b5", "#a5b988",
            "#fee0b6", "#faebd6", "#f7f7f7", "#faebf3", "#fde0ef",
            "#a9839f", "#c2a8c0", "#dccee3", "#f1c6de", "#e9a3c9",
            "#542788", "#8b66ab", "#c2a5cf", "#c360a6", "#c51b7d")
)
saveRDS(tile_df, paste0(mainpath, "/diverging_5x5_palette.rds"))
tile_plot <- ggplot() +
  geom_tile(data = tile_df, aes(x = x, y = y, fill = color), color = "black") +
  scale_fill_identity() +
  theme_void() +
  theme(
    axis.text = element_blank(),
    plot.margin = margin(0.1, 0, 0, 0.1, "cm")
  )
ggsave(paste0(mainpath, "/diverging_5x5_palette.svg"), tile_plot, width = 10, height = 10, units = "in", dpi = 300)
cvdPlot(tile_plot, c("origin", "deuteranope", "protanope", "desaturate"))

diverging_departure_5x5_plot<- ggplot() +
  geom_sf(data = states_df, fill = "grey90", color = "black", linewidth = 0.05) +
  geom_sf(data = diverging_departure_5x5, aes(fill = color_ramp_color), linewidth = 0) +
  scale_fill_identity() +
  theme_minimal()+
  theme(
        axis.text = element_blank(),
        plot.margin = margin(0.1, 0, 0, 0.1, "cm"))
  diverging_departure_5x5_fireshed_plot
ggsave(paste0(mainpath, "/diverging_departure_5x5_priority.jpg"), diverging_departure_5x5_fireshed_plot, width = 10, height = 10, units = "in", dpi = 300)



diverging_departure_continuous <- ggplot() +
  geom_sf(data = states_df, fill = "grey90", color = "black") +
  geom_sf(data = df_joined_div, aes(fill = rgb), linewidth = 0) +
  scale_fill_identity() +
  theme_bw()
diverging_departure_continuous
# ggsave(paste0(mainpath,"/diverging_departure_continuous.jpg"), diverging_departure_continuous, width = 10, height = 10, units = "in", dpi = 300)

#### fireshed
quantiles_freq <- quantile(firesheds$emd_frequency, c(0.33, .66), na.rm = T, names = F)
quantiles_sev <- quantile(firesheds$emd_severity, c(0.33, .66), na.rm = T, names = F)
diverging_departure_5x5 <- firesheds %>%
  mutate(
    color_ramp_freq_code = ifelse(between(signed_emd_frequency, quantiles_freq[1], quantiles_freq[2]), 1,
      ifelse(signed_emd_frequency > quantiles_freq[2], 2,
        ifelse(between(signed_emd_frequency, -quantiles_freq[1], -quantiles_freq[2]), -1,
          ifelse(signed_emd_frequency < -quantiles_freq[2], -2, 0)
        )
      )
    ),
    color_ramp_sev_code = ifelse(between(signed_emd_severity, quantiles_sev[1], quantiles_sev[2]), 1,
      ifelse(signed_emd_severity > quantiles_sev[2], 2,
        ifelse(between(signed_emd_severity, -quantiles_sev[1], -quantiles_sev[2]), -1,
          ifelse(signed_emd_severity < -quantiles_sev[2], -2, 0)
        )
      )
    ),
    color_ramp_color = case_when(
      color_ramp_freq_code == 2 & color_ramp_sev_code == 2 ~ "#c51b7d", # top right
      color_ramp_freq_code == 2 & color_ramp_sev_code == -2 ~ "#4d9221", # bottom right

      color_ramp_freq_code == -2 & color_ramp_sev_code == 2 ~ "#542788", # top left
      color_ramp_freq_code == -2 & color_ramp_sev_code == -2 ~ "#b35806", # bottom left

      color_ramp_freq_code == 0 & color_ramp_sev_code == 0 ~ "#f7f7f7", # middle

      color_ramp_freq_code == 0 & color_ramp_sev_code == 2 ~ "#c2a5cf", # top middle
      color_ramp_freq_code == 0 & color_ramp_sev_code == -2 ~ "#d9f0d3", # bottom middle

      color_ramp_freq_code == 2 & color_ramp_sev_code == 0 ~ "#fde0ef", # right middle
      color_ramp_freq_code == -2 & color_ramp_sev_code == 0 ~ "#fee0b6", # left middle
      # far right side
      color_ramp_freq_code == 2 & color_ramp_sev_code == 1 ~ "#e9a3c9", # rightup1
      color_ramp_freq_code == 2 & color_ramp_sev_code == -1 ~ "#A5B988", # rightdown1
      # far top
      color_ramp_freq_code == 1 & color_ramp_sev_code == 2 ~ "#C360A6", # topright1
      color_ramp_freq_code == -1 & color_ramp_sev_code == 2 ~ "#8B66AB", # topleft1
      # far left
      color_ramp_freq_code == -2 & color_ramp_sev_code == 1 ~ "#A9839F", # leftup1
      color_ramp_freq_code == -2 & color_ramp_sev_code == -1 ~ "#D89C5E", # leftdown1
      # far bottom
      color_ramp_freq_code == -1 & color_ramp_sev_code == -2 ~ "#C5A36c", # bottomleft1
      color_ramp_freq_code == 1 & color_ramp_sev_code == -2 ~ "#93C179", # bottomright1
      # middle column
      color_ramp_freq_code == 0 & color_ramp_sev_code == 1 ~ "#DCCEE3", # middleup1
      color_ramp_freq_code == 0 & color_ramp_sev_code == -1 ~ "#E8F3E4", # middledown1
      # middle row
      color_ramp_freq_code == 1 & color_ramp_sev_code == 0 ~ "#FAEBF3", # middleright1
      color_ramp_freq_code == -1 & color_ramp_sev_code == 0 ~ "#FAEBD6", # middleleft1
      # rightup1
      color_ramp_freq_code == 1 & color_ramp_sev_code == 1 ~ "#F1C6DE", # rightup1
      # rightdown1
      color_ramp_freq_code == 1 & color_ramp_sev_code == -1 ~ "#C6D5B5", # rightdown1
      # leftup1
      color_ramp_freq_code == -1 & color_ramp_sev_code == 1 ~ "#C2A8C0", # leftup1
      # leftdown1
      color_ramp_freq_code == -1 & color_ramp_sev_code == -1 ~ "#DFC6A1" # leftdown1
    )
  )
diverging_departure_5x5_fireshed_plot<- ggplot() +
  geom_sf(data = states_df, fill = "grey90", color = "black", linewidth = 0.05) +
  geom_sf(data = diverging_departure_5x5, aes(fill = color_ramp_color), linewidth = 0) +
  scale_fill_identity() +
  theme_minimal()+
  theme(
        axis.text = element_blank(),
        plot.margin = margin(0.1, 0, 0, 0.1, "cm"))
  diverging_departure_5x5_fireshed_plot
ggsave(paste0(mainpath, "/diverging_departure_5x5_priority.jpg"), diverging_departure_5x5_fireshed_plot, width = 10, height = 10, units = "in", dpi = 300)

# 5x5 scatter
diverging_5x5_scatter_fs <- ggplot() +
  geom_point(data = diverging_departure_5x5, aes(x = signed_emd_frequency, y = signed_emd_severity, color = color_ramp_color), size = 2) +
  scale_color_identity() +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  theme_bw() +
  lims(x = c(-3.5, 3.5), y = c(-3.5, 3.5)) +
  theme(axis.text = element_text(size = 12))












#continuous
diverging_departure_continuous <- ggplot() +
  geom_sf(data = df_joined_div, aes(fill = rgb),linewidth = 0)+
  scale_fill_identity()+
  theme_bw()
diverging_departure_continuous
ggsave(paste0(outpath,"/main/diverging_departure_map.jpg"), plot = diverging_departure_plot, width = 8, height = 6, units = "in")

####color palette
d <- expand.grid(x = seq(0, 1, 0.01), y = seq(0, 1, 0.01)) %>%
  mutate(fill_val = atan(y/x),
         transparency = x + y)

d <- d %>%
  mutate(mix2 = rgb(red = y, green = 0.5, blue = -(x-1)))
ggplot(d, aes(x, y)) + 
  geom_tile(aes(fill = mix2)) + 
  labs(x = "",
       y = "") +
  scale_fill_identity()+
  theme_void() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )





#### color palette
d <- expand.grid(x = seq(0, 1, 0.01), y = seq(0, 1, 0.01)) %>%
  mutate(
    fill_val = atan(y / x),
    transparency = x + y
  )

d <- d %>%
  mutate(mix2 = rgb(red = y, green = 0.5, blue = -(x - 1)))
continuous_colorpallette <- ggplot(d, aes(x, y)) +
  geom_tile(aes(fill = mix2)) +
  labs(
    x = "",
    y = ""
  ) +
  scale_fill_identity() +
  theme_void() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )
# ggsave(paste0(mainpath,"/continuous_colorpallette.jpg"), plot = continuous_colorpallette, width = 8, height = 8, units = "in")

# Figure 3 -------

#### figure emd_spatial
emd_spatial <- ggarrange(diverging_departure_5x5_plot,  fireRegimeDeparture_plot,
                         # frccDeparture_plot, NULL, frcc_departure_boxplot,
                         ncol = 2,
                         # nrow = 2,
                         common.legend = FALSE,
                         
                         # heights = c(1, 1),
                         labels = c(
                           "A)", "B)"
                           #          "C", "", "D"
                         )
)
emd_spatial
ggsave(paste0(mainpath, "/figure3.jpg"), emd_spatial, width = 14, height = 6, units = "in")

### hexel intersections human use ----
# wilderness NPS
protected_area_mask <- rast("data/masks/cleaned/protected_areas_mask.tif")

prop_protected_vec <- terra::extract(protected_area_mask, vect(df_joined), fun = mean, na.rm = T)
prop_protected <- data.frame(df_joined, proportion_protected = prop_protected_vec$protected)

prop_protected_percents <- prop_protected %>%
  mutate(quantile = cut(proportion_protected,
    breaks = seq(0, 1, 0.2),
    labels = c("0-20", "20-40", "40-60", "60-80", "80-100"), include.lowest = T
  ))
# check for ANOVA assumptions
# normality
shapiro.test(prop_protected_percents$emd_both) # not normal

# kruksal wallis
wnps_kruskal <- kruskal.test(emd_both ~ quantile, data = prop_protected_percents) # significant
# pairwise comparisons
wnps_pairwise_kruskal <- pairwise.wilcox.test(prop_protected_percents$emd_both, prop_protected_percents$quantile, p.adjust.method = "bonferroni")


# plot
n_fun <- function(x, y_pos = 3.5) {
  return(data.frame(y = y_pos, label = paste0("n = ", sum(!is.na(x)))))
}

protected_area_summary <- prop_protected_percents %>%
  group_by(quantile) %>%
  summarize(
    mean_emd = mean(emd_both),
    n = n(),
    se = sd(emd_both) / sqrt(n),
    low = mean_emd - 1.96 * se,
    high = mean_emd + 1.96 * se,
    n_lab = paste0("n = ", n),
    .groups = "drop"
  )

proportion_protected_box <- prop_protected_percents %>%
  ggplot(aes(x = quantile, y = emd_both)) +
  geom_boxplot() +
  labs(x = "Percent Wilderness & National Park", y = "Mean Fire regime departure") +
  stat_summary(fun.data = n_fun, geom = "text", size = 7) +
  ylim(0, 3.5) +
  theme_bw()+
  theme(axis.text = element_text(size = 8*3),
        axis.title = element_text(size = 10*3))

proportion_protected_gradient <- protected_area_summary %>%
  ggplot() +
  geom_errorbar(aes(x = quantile, ymin = low, ymax = high), width = 0.25) +
  geom_point(aes(x = quantile, y = mean_emd), size = 3) +
  labs(x = "Percent Wilderness & National Park", y = "Mean Fire regime departure") +
  geom_text(aes(x = quantile, y = 0, label = n_lab), size = 7) +
  ylim(0, 2) +
  scale_size_binned("Number of Hexels", n.breaks = 10) +
  theme_bw()+
  theme(axis.text = element_text(size = 8*3),
        axis.title = element_text(size = 10*3))


# human footprint
human_footprint_r <- rast("data/masks/cleaned/human_footprint/wildareas-v3-1993-human-footprint.tif", lyrs = 1)
extract_human_footprint <- df_joined %>%
  terra::vect() %>%
  terra::project(crs(human_footprint_r)) %>%
  terra::extract(human_footprint_r, ., fun = mean, na.rm = T)

df_joined_human_footprint <- cbind(df_joined, extract_human_footprint) %>%
  rename(human_footprint = wildareas.v3.1993.human.footprint) # %>%

# bins
human_footprint_cuts <- df_joined_human_footprint %>%
  mutate(quantile = cut(human_footprint,
    breaks = c(seq(0, 10, 2), 90),
    labels = c("0-2", "2-4", "4-6", "6-8", "8-10", ">10"), include.lowest = T
  ))
# check for ANOVA assumptions
# normality
shapiro.test(human_footprint_cuts$emd_both) # not normal

# kruksal wallis
hf_kruskal <- kruskal.test(emd_both ~ quantile, data = human_footprint_cuts) # significant
# pairwise comparisons
hf_pairwise_kruskal <- pairwise.wilcox.test(human_footprint_cuts$emd_both, human_footprint_cuts$quantile, p.adjust.method = "bonferroni")

# plot

human_footprint_summary <- human_footprint_cuts %>%
  group_by(quantile) %>%
  summarize(
    mean_emd = mean(emd_both),
    n = n(),
    se = sd(emd_both) / sqrt(n),
    low = mean_emd - 1.96 * se,
    high = mean_emd + 1.96 * se,
    n_lab = paste0("n = ", n),
    .groups = "drop"
  )
hf_box <- human_footprint_cuts %>%
  ggplot(aes(x = quantile, y = emd_both)) +
  geom_boxplot() +
  labs(x = "Average human footprint", y = "Mean Fire regime departure") +
  stat_summary(fun.data = n_fun, geom = "text", size = 7) +
  ylim(0, 3.5) +
  theme_bw()+
  theme(axis.text = element_text(size = 8*3),
        axis.title = element_text(size = 10*3))

human_footprint_gradient <- human_footprint_summary %>%
  ggplot() +
  geom_errorbar(aes(x = quantile, ymin = low, ymax = high), width = 0.25) +
  geom_point(aes(x = quantile, y = mean_emd), size = 3) +
  labs(x = "Average human footprint", y = "Mean Fire regime departure") +
  geom_text(aes(x = quantile, y = 0, label = n_lab), size = 7) +
  ylim(0, 2.2) +
  theme_bw()+
  theme(axis.text = element_text(size = 8*3),
        axis.title = element_text(size = 10*3))

# public lands
public_lands <- vect("data/masks/cleaned/protected_areas_database/PADUS3_0Geopackage.gpkg", layer = "PADUS3_0Designation")
hexcels_pl_crs <- project(vect(df_joined), crs(public_lands))
template <- rast(ext = ext(hexcels_pl_crs), res = 30, crs = crs(hexcels_pl_crs), vals = T)
public_lands_raster <- rasterize(public_lands, template, field = 1, background = 0)

prop_public <- terra::extract(public_lands_raster, hexcels_pl_crs, "mean")
prop_public_df <- cbind(df_joined, prop_public) %>%
  rename(prop_public = layer)


# group by 10 percent bins
prop_public_df <- prop_public_df %>%
  mutate(quantile = cut(prop_public,
    breaks = seq(0, 1, 0.2),
    labels = c("0-20", "20-40", "40-60", "60-80", "80-100"), include.lowest = T
  ))
# check for ANOVA assumptions
# normality
shapiro.test(prop_public_df$emd_both) # not normal

# kruksal wallis
public_kruskal <- kruskal.test(emd_both ~ quantile, data = prop_public_df) # significant
# pairwise comparisons
public_pairwise_kruskal <- pairwise.wilcox.test(prop_public_df$emd_both, prop_public_df$quantile, p.adjust.method = "bonferroni")


# plot
prop_public_summary <- prop_public_df %>%
  group_by(quantile) %>%
  summarize(
    mean_emd = mean(emd_both),
    n = n(),
    se = sd(emd_both) / sqrt(n),
    low = mean_emd - 1.96 * se,
    high = mean_emd + 1.96 * se,
    n_lab = paste0("n = ", n),
    .groups = "drop"
  )


proportion_public_box <- prop_public_df %>%
  ggplot(aes(x = quantile, y = emd_both)) +
  geom_boxplot() +
  labs(x = "Percent public lands", y = "Mean Fire regime departure") +
  stat_summary(fun.data = n_fun, geom = "text", size = 7) +
  ylim(0, 3.5) +
  theme_bw()+
  theme(axis.text = element_text(size = 8*3),
        axis.title = element_text(size = 10*3))

proportion_public_gradient <- prop_public_summary %>%
  ggplot() +
  geom_errorbar(aes(x = quantile, ymin = low, ymax = high), width = 0.25) +
  geom_point(aes(x = quantile, y = mean_emd), size = 3) +
  labs(x = "Percent public lands", y = "Mean Fire regime departure") +
  geom_text(aes(x = quantile, y = 0, label = n_lab), size = 7) +
  ylim(0, 2) +
  theme_bw()+
  theme(axis.text = element_text(size = 8*3),
        axis.title = element_text(size = 10*3))

# combined plot
#bind rows and label by dataset
full_df <- bind_rows(human_footprint_summary %>% mutate(dataset = "Human footprint"),
                     prop_public_summary %>% mutate(dataset = "Public lands (%)"),
                     protected_area_summary %>% mutate(dataset = "Protected lands (%)"))

combined_gradient <- full_df %>%
  ggplot() +
  geom_errorbar(aes(x = quantile, ymin = low, ymax = high, color = dataset), width = 0.25, linewidth = 2) +
  geom_point(aes(x = quantile, y = mean_emd, color = dataset), size = 4) +
  labs(x = "", y = "Mean Fire regime departure") +
  #geom_text(aes(x = quantile, y = 0, label = n_lab), size = 7) +
  facet_grid(~dataset, scales = "free_x") +
  ylim(0, 2.2) +
  theme_bw()+
  theme(axis.text = element_text(size = 8*3),
        axis.title = element_text(size = 10*3)) +
  theme(legend.position = "none") +
  scale_color_manual(values = c("Human footprint" = "black", "Public lands (%)" = "red", "Protected lands (%)" = "blue")) +
  theme(strip.text = element_text(size = 10*3),
        strip.background = element_rect(fill = "#DDDDDD"))
ggsave(paste0(mainpath, "/figure_4.pdf"), combined_gradient,
  width = 24, height = 8, units = "in", dpi = 300
)
combined_box <- ggarrange(hf_box, proportion_public_box,
                               proportion_protected_box,
                               ncol = 2, nrow = 2,
                               labels = c("A)", "B)", "C)")
)
#ggsave(paste0(mainpath, "/human_footprint_public_lands_box.jpg"), combined_box,
#       width = 16, height = 16, units = "in", dpi = 300
#)

#alternative plot

human_footprint_cuts$proportion_public <- prop_public_df$quantile

summarized <- human_footprint_cuts %>%
  group_by(quantile, proportion_public) %>%
  summarize(median_departure = median(emd_both))

alternative_plot <- ggplot(summarized, aes(y=quantile, x = proportion_public, size = median_departure, color = median_departure)) +
  geom_point()+
  labs(y = "Average human footprint",
       x = "Percent public lands") +
  scale_size_continuous(name = "Median Fire regime departure") +
  scale_color_viridis_c(option = "A", limits = c(0, max(df_joined$emd_both))) +
  theme_bw()

### Fireshed plots ----



template <- rast(firesheds, res = 30) %>%
  rasterize(priority_landscapes, ., field = 1, background = 0)

percent_priority <- terra::zonal(template, vect(firesheds), fun = "mean", na.rm = TRUE) %>%
  cbind(firesheds) %>%
  rename(proportion_priority = "layer")
# split into those greater than 50%
greaterthan_50 <- percent_priority %>%
  mutate(
    is_50priority = ifelse(proportion_priority >= 0.5, "Priority", "Non-priority"),
    fill = ifelse(proportion_priority >= 0.5, "#ff4855", "grey40")
  )
#check normality assumptions
shapiro.test(greaterthan_50$emd_both) # not normal

# wilcoxon
greaterthan_50$is_50priority <- factor(greaterthan_50$is_50priority, levels = c("Non-priority", "Priority"))
wilcox.test(emd_both ~ is_50priority, data = greaterthan_50) # significant
#calculate w
ranks <- greaterthan_50 %>%
  mutate(rank = rank(emd_both)) %>%
  group_by(is_50priority) %>%
  summarize(n = n(), w = sum(rank),median = median(rank), .groups = "drop")
N <- ranks$n[1]
M <- ranks$n[2]
U1 <- N*M+(N*(N+1))/2 - ranks$w[1]
U1
U2 <- N*M+(M*(M+1))/2 - ranks$w[2]
U2

test_df <- data.frame(
  group = c("Non-Priority", "Priority"),
  n = c(N, M),
  U = c(U1, U2)
)
z <- (U1 - (N*M/2)) / sqrt((N*M*(N+M+1))/12)
r <- 1-(2*U1)/(N*M*(N+M+1))
#log odds
logistic <- glm(is_50priority ~ emd_both, data = greaterthan_50, family = "binomial")
summary(logistic)
log_odds <- exp(coef(logistic)[2])
logLik(logistic)
plot(logistic$data$emd_both, logistic$fitted.values, 
     xlab = "Fire regime departure", ylab = "Probability of Priority Landscapes",
     type = "p", pch = 16)



greater_50_plot <- ggplot(data = greaterthan_50, aes(x = is_50priority, y = emd_both, fill = fill)) +
  geom_boxplot() +
  labs(x = "", y = "Fire regime departure") +
  theme_bw() +
  scale_fill_identity() +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 26),
    axis.text.y = element_text(size = 20),
    axis.title = element_text(size = 26),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "none",
    plot.margin = ggplot2::margin(1,0,0,0.2, "cm")
  )
greater_50_plot_95ci <- greater_50_plot +
  stat_summary(
    geom = "errorbar",
    fun.data = ~ mean_se(., mult = 1.96),
    width = 0.1,
    linewidth = 0.5
  )
# save
ggsave(paste0(mainpath, "/greaterthan_50_priority.jpg"), greater_50_plot_95ci, width = 6, height = 12, units = "in", dpi = 300)

#Figure 5 ----
greaterthan_50 <- st_as_sf(greaterthan_50)
priority_landscapes_plot <- ggplot()+
  geom_sf(data = states_df, fill = "grey90", color = "black") +
  geom_sf(data = greaterthan_50, aes(fill = is_50priority), linewidth = 0) +
  scale_fill_manual(values = c( "grey40", "#ff4855"), labels = c( "Non-priority", "Priority")) +
  geom_sf(data = priority_landscapes, alpha = 0, color = "black", linewidth = 0.8)+
  theme_minimal() +
  theme(plot.margin = ggplot2::margin(0.1, 0, 0.1, -0.5, "cm"),
        axis.text = element_blank(),,
        legend.title = element_blank(),
        legend.text = element_text(size = 26),
        legend.key.size = unit(2.5, "cm"),
        legend.margin = margin(0, 0, 0, 0))

combined_fireshed_simple <- ggarrange(diverging_departure_5x5_fireshed_plot, NULL,  NULL, priority_landscapes_plot, NULL, greater_50_plot_95ci,
                                      ncol = 3, nrow = 2, labels = c("A)", "","", "B)", "", "C)"), widths = c(.95, -.05, .30),
                                      font.label = list(size = 26, face = "bold"))
#save
ggsave(paste0(mainpath, "/firesheds_simple.svg"), combined_fireshed_simple, width = 18, height = 20, units = "in", dpi = 600)

# Threshold plot  ----
load(paste0(mainpath, "/thresholds.RData"))
df_mutate <- df_mutate %>%
  filter(CBI_total > 0) %>%
  drop_na(CBI_total) %>%
  drop_na(pct_mortality)
threshold_plot <- ggplot()+
  geom_ribbon(aes(x = c(0,x[1]),  ymin = 0, ymax = 100), thresholds, fill = gradientPalette[1], alpha = .4)+
  geom_ribbon(aes(x = c(x[1],x[2]),  ymin = 0, ymax = 100), thresholds, fill = gradientPalette[2], alpha = .4)+
  geom_ribbon(aes(x = c(x[2],3),  ymin = 0, ymax = 100), thresholds, fill = gradientPalette[3], alpha = .4)+
  
  #geom_line(aes(x=x, y =y*100, color = des), curve,size = 1.2)+
  geom_vline(aes(xintercept = x, color = des), thresholds, linewidth =1.2, linetype = "dashed", show.legend = F)+
  #geom_point(aes(x = x, y = y, color = des), thresholds, size = 2.3, show.legend = F) +
  #geom_segment(x = x[1], xend = x[1], y = 0, yend = y[1],  data = thresholds, size = .75, color =cbPalette[1], linetype = "dashed") +
  #geom_segment(x = x[2], xend = x[2], y = 0, yend = y[2], data = thresholds, size = .75, color = cbPalette[2], linetype = "dotdash") +
  #geom_segment(x = 0, xend = x[1], y = y[1], yend = y[1],  data = thresholds, size = .75, color =cbPalette[1], linetype = "dashed") +
  #geom_segment(x = 0, xend = x[2], y = y[2], yend = y[2], data = thresholds, size = .75, color = cbPalette[2], linetype = "dotdash") +
  geom_line(aes(x=CBI, y = prediction*100, color = des), curve, size = 1.2)+
  #geom_line(aes(x=x, y = Mixed*100), multinomial_curves, size = 1.2, color = gradientPalette[2])+
  #geom_line(aes(x=x, y = High*100), multinomial_curves, size = 1.2, color = gradientPalette[3])+
  
  geom_point(aes(x=CBI_total, y = pct_mortality), df_mutate, show.legend = F) +
  geom_text_repel(aes(x=c(1.5,2.2),y=c(50, 50), 
                      label = paste0("Threshold: ", round(x,2))), force = 70,
                  color = c("#015C01",  "#1565C0" ),
                  direction = "x", show.legend = F, seed = 1, size = 6)+
  
  annotate("text", x = pos.x[1],  y = 105, label = category_text[1], size = 8)+
  annotate("text", x = pos.x[2],  y = 105, label = category_text[2], size = 8)+
  annotate("text", x = pos.x[3],  y = 105, label = category_text[3], size = 8)+
  
  
  scale_y_continuous(
    name = "Percent tree mortality",
    breaks = c(0,25,50,75,100),
    #sec.axis = sec_axis( trans=~.*1, name="Probability of Class")
  ) +
  labs(x = "CBI") +
  guides(color = guide_legend(
    override.aes=list(shape = 0)))+
  scale_color_manual(values = gradientPalette[c(1,3,3,1)],
                     name = "Fitted regression",
                     labels = c(expression('T'[lm]),
                                expression('T'[mh]))) +
  
  
  
  theme_classic() +
  theme(legend.position = "topleft",
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 25),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 30))

threshold_plot
#save
ggsave("Figures/Final_figures/main/thresholds.jpg", threshold_plot, width = 12, height = 8, units = "in", dpi = 300)

# sensitivity ----
colors <- distinctColorPalette(30)

census <- as.data.frame(st_read("data/outputs/med_grids/sensitivity/census.gpkg"))
n_iters <- readRDS("data/outputs/med_grids/sensitivity/med_hex_n_iters.rds")
p_areas <- readRDS("data/outputs/med_grids/sensitivity/med_hex_p_areas.rds")
run_sensitivity <- readRDS("data/outputs/med_grids/sensitivity/med_hex_run_sensitivity.rds")
miller_thode <- as.data.frame(st_read("data/outputs/med_grids/sensitivity/miller_thode.gpkg"))




# Combine all data into one data frame for plotting
combine_dfs <- function(df_list, names) {
  map2_dfr(df_list, names, ~ dplyr::mutate(., dataset = .y))
}
combined_df <- bind_rows(
 dplyr::mutate(df_joined, dataset = "df_joined"),
 dplyr::mutate(census, dataset = "census"),
 dplyr::mutate(miller_thode, dataset = "miller_thode"),
  combine_dfs(n_iters, paste0("n_iters_", seq_along(n_iters))),
  combine_dfs(p_areas, paste0("p_areas_", seq_along(p_areas))),
  combine_dfs(run_sensitivity, paste0("run_sensitivity_", seq_along(run_sensitivity)))
)

# Melt the data frame to long format for plotting
vars <- c("emd_frequency", "emd_severity", "emd_both")
long_df <- pivot_longer(combined_df, vars, names_to = "variable", values_to = "value") %>%
  mutate(variable = case_when(
    variable == "emd_frequency" ~ "Fire frequency departure",
    variable == "emd_severity" ~ "Fire severity departure",
    variable == "emd_both" ~ "Fire regime departure",
    TRUE ~ variable
  )) %>%
  pivot_longer(paste0(vars, "_var"), names_to = "variable_var", values_to = "value_var") %>%
  mutate(variable_var = case_when(
    variable_var == "emd_frequency_var" ~ "Fire frequency departure Variance",
    variable_var == "emd_severity_var" ~ "Fire severity departure Variance",
    variable_var == "emd_both_var" ~ "Fire regime departure Variance",
    TRUE ~ variable_var
  ), value_var = sqrt(value_var))


# Boxplots
iters <- c("10", "25", "50", "100", "200")
areas <- as.character(c(0.001, 0.01, 0.1, 0.5))

iter_names <- paste0("N iterations: ", iters)
area_names <- paste0("Proportion Area: ", areas)
run_names <- paste0("Repeated Run: ", seq_along(run_sensitivity))
dataset_names <- c("Census", "Original", "Miller & Thode","No edge filter", "No NDVI filter", iter_names, area_names, run_names)

# Density plot
dataset_names <- c("Census", "Miller & Thode","No edge filter", "No NDVI filter", iter_names, area_names, run_names)
# density_plot <- ggplot(long_df, aes(x = value, color = dataset)) +
#        facet_wrap(~ variable, scales = "free") +
#        geom_density(aes(y = after_stat(density)), alpha = 0.6) +
#        labs(x = "Bias", y = "Density") +
#        scale_color_manual(values = colors,
#        labels = dataset_names) +
#        theme_bw() +
#        theme(legend.title = element_blank(),
#                             legend.text = element_text(size = 10),
#                             legend.key.size = unit(0.5, "cm"))
# density_plot
# #save plot
# ggsave(paste0(appendixpath,"/sensitivity/emd_sensitivity_density.jpg"), plot = density_plot, width = 12, height = 6, units = "in")

# bias against original (df_joined)

# Function to calculate pairwise bias
calculate_pairwise_bias <- function(df1, df2, vars) {
  joined_df <- left_join(df1[, c(vars, "name")], df2[, c(vars, "name")], by = "name") %>%
    drop_na()
  pairwise_bias <- purrr::map(vars, ~ (joined_df[[paste0(., ".y")]] - joined_df[[paste0(., ".x")]]))
  bias_df <- do.call(data.frame, pairwise_bias)
  colnames(bias_df) <- vars
  return(bias_df)
}
df_joined <- as.data.frame(df_joined)
# Calculate pairwise bias
census_bias <- calculate_pairwise_bias(df_joined, census, vars)
miller_thode_bias <- calculate_pairwise_bias(df_joined, miller_thode, vars)

# Function to calculate pairwise bias for lists of data frames
calculate_pairwise_bias_list <- function(df1, df_list, vars) {
  purrr::map(df_list, function(df2) {
    joined_df <- left_join(df1[, c(vars, "name")], df2[, c(vars, "name")], by = "name") %>%
      drop_na()
    pairwise_bias <- purrr::map(vars, ~ (joined_df[[paste0(., ".y")]] - joined_df[[paste0(., ".x")]]))
    bias_df <- do.call(data.frame, pairwise_bias)
    colnames(bias_df) <- vars
    return(bias_df)
  })
}

n_iters_bias <- calculate_pairwise_bias_list(df_joined, n_iters, vars)
p_areas_bias <- calculate_pairwise_bias_list(df_joined, p_areas, vars)
run_sensitivity_bias <- calculate_pairwise_bias_list(df_joined, run_sensitivity, vars)

# Combine all biases into one data frame for plotting
bias_df <- bind_rows(
  mutate(census_bias, dataset = "Census"),
  mutate(miller_thode_bias, dataset = "Miller & Thode"),
  map_dfr(seq_along(n_iters_bias), ~ mutate(n_iters_bias[[.x]], dataset = paste0("N iterations: ", .x))),
  map_dfr(seq_along(p_areas_bias), ~ mutate(p_areas_bias[[.x]], dataset = paste0("Proportion Area: ", .x))),
  map_dfr(seq_along(run_sensitivity_bias), ~ mutate(run_sensitivity_bias[[.x]], dataset = paste0("Repeated Run: ", .x)))
)

long_df_bias <- pivot_longer(bias_df, vars, names_to = "variable", values_to = "value") %>%
  mutate(variable = case_when(
    variable == "emd_frequency" ~ "Fire frequency departure",
    variable == "emd_severity" ~ "Fire severity departure",
    variable == "emd_both" ~ "Fire regime departure",
    TRUE ~ variable
  )) %>%
  mutate(dataset_class = case_when(
    dataset == "Census" ~ "Census",
    dataset == "Miller & Thode" ~ "Miller & Thode",
    grepl("N iterations", dataset) ~ "N iterations",
    grepl("Proportion Area", dataset) ~ "Proportion Area",
    grepl("Repeated Run", dataset) ~ "Repeated Runs"
  ))
# Remove rows with NA bias
bias_df <- na.omit(bias_df)

##### Means and standard deviations for gaussians, as well as 95th, 97.5th, and 99th percentiles
table_gaussian <- long_df_bias %>%
  group_by(dataset, variable) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    n = n(),
    percentile_95 = quantile(value, 0.95),
    percentile_975 = quantile(value, 0.975),
    percentile_99 = quantile(value, 0.99)
  ) %>%
  mutate(se = sd / sqrt(n)) %>%
  mutate(
    upper = mean + 1.96 * se,
    lower = mean - 1.96 * se
  )
# convert to wide format so that dataset is included in the columns
dataset_names <- c("Census", "Miller & Thode", iter_names, area_names, run_names)
table_gaussian_wide <- pivot_wider(table_gaussian, names_from = variable, values_from = c(mean, sd, n, se, upper, lower, percentile_95, percentile_975, percentile_99)) %>%
  mutate_if(is.numeric, ~ round(., 3))
table_gaussian_wide$dataset <- dataset_names
View(table_gaussian_wide)
# bias  Density plot



classes <- unique(long_df_bias$dataset)
colors <- distinctColorPalette(length(classes))
linetypes <- c("solid", "dashed", "F1", "4C88C488", rep("dotted", 5), rep("dotdash", 4), rep("longdash", 10))
each_variable <- unique(long_df_bias$variable)
bias_density_plots <- vector("list", length(each_variable))
for (i in seq_along(each_variable)) {
  bias_density_plots[[i]] <- long_df_bias %>%
    filter(variable == each_variable[i]) %>%
    ggplot(aes(x = value)) +
    geom_density(show.legend = FALSE) +
    stat_density(aes(color = dataset, linetype = dataset), geom = "line", position = "identity") +
    labs(x = paste0(each_variable[i], " bias"), y = "") +
    scale_color_manual(
      name = "Dataset",
      values = colors,
      labels = dataset_names
    ) +
    scale_linetype_manual(
      name = "Dataset",
      values = linetypes,
      labels = dataset_names
    ) +
    theme_bw() +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      legend.key.size = unit(0.5, "cm")
    )
}
bias_density_plots[[1]] <- bias_density_plots[[1]] +
  labs(y = "Density")
bias_density_plots[[3]] <- bias_density_plots[[3]] +
  geom_rect(xmin = -0.4, xmax = 0, ymin = 0, ymax = 1, fill = NA, color = "black")
# Save the bias plot
# ggsave(filename = paste0(appendixpath,"/sensitivity/bias_density.jpg"), plot = bias_density_plot, width = 12, height = 12, units = "in")
# plot at  window
bias_density_plot_window <- bias_density_plots[[3]] +
  coord_cartesian(xlim = c(-0.4, 0), ylim = c(0, 1))
combined_bias <- ggarrange(bias_density_plots[[1]], bias_density_plots[[2]], bias_density_plots[[3]], bias_density_plot_window, ncol = 4, nrow = 1, common.legend = TRUE)
ggsave(filename = paste0(appendixpath, "/sensitivity/bias_density_window.jpg"), plot = combined_bias, width = 12, height = 6, units = "in", dpi = 600)

# Calculate the mean bias for each dataset then convert to long format
mean_bias <- bias_df %>%
  filter(!dataset %in% c("Census", "Miller & Thode", paste0("Repeated Run: ", 1:10))) %>%
  group_by(dataset) %>%
  summarise(across(c(emd_frequency, emd_severity, emd_both), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = c(emd_frequency, emd_severity, emd_both), names_to = "variable", values_to = "mean") %>%
  mutate(variable = case_when(
    variable == "emd_frequency" ~ "Fire frequency departure",
    variable == "emd_severity" ~ "Fire severity departure",
    variable == "emd_both" ~ "Fire regime departure",
    TRUE ~ variable
  )) %>%
  mutate(x = ifelse(grepl("N iterations", dataset), as.numeric(gsub("N iterations: ", "", dataset)),
    ifelse(grepl("Proportion Area", dataset), as.numeric(gsub("Proportion Area: ", "", dataset)), NA)
  )) %>%
  mutate(x = ifelse(grepl("N iterations", dataset), as.numeric(iters[x]),
    ifelse(grepl("Proportion Area", dataset), as.numeric(areas[x]), NA)
  )) %>%
  mutate(dataset_class = ifelse(grepl("N iterations", dataset), "Number of Iterations",
    ifelse(grepl("Proportion Area", dataset), "Proportion of area", NA)
  ))


# calculate the standard error and 95% confidence interval for the mean bias in each dataset. the final
# data frame is converted to long format for plotting, and contains the mean bias, and the upper and lower
# bounds of the 95% confidence interval
sd_bias <- bias_df %>%
  filter(!dataset %in% c("Census", "Miller & Thode", paste0("Repeated Run: ", 1:10))) %>%
  group_by(dataset) %>%
  summarise(across(c(emd_frequency, emd_severity, emd_both), sd, na.rm = TRUE)) %>%
  pivot_longer(cols = c(emd_frequency, emd_severity, emd_both), names_to = "variable", values_to = "sd") %>%
  mutate(variable = case_when(
    variable == "emd_frequency" ~ "Fire frequency departure",
    variable == "emd_severity" ~ "Fire severity departure",
    variable == "emd_both" ~ "Fire regime departure",
    TRUE ~ variable
  )) %>%
  right_join(mean_bias, by = c("dataset", "variable"))

n_bias <- bias_df %>%
  filter(!dataset %in% c("Census", "Miller & Thode", paste0("Repeated Run: ", 1:10))) %>%
  group_by(dataset) %>%
  summarise(across(c(emd_frequency, emd_severity, emd_both), ~ n())) %>%
  pivot_longer(cols = c(emd_frequency, emd_severity, emd_both), names_to = "variable", values_to = "n") %>%
  mutate(variable = case_when(
    variable == "emd_frequency" ~ "Fire frequency departure",
    variable == "emd_severity" ~ "Fire severity departure",
    variable == "emd_both" ~ "Fire regime departure",
    TRUE ~ variable
  )) %>%
  right_join(sd_bias, by = c("dataset", "variable"))

se_bias <- n_bias %>%
  mutate(se = sd / sqrt(n)) %>%
  mutate(
    upper = mean + 1.96 * se,
    lower = mean - 1.96 * se
  ) %>%
  select(dataset, variable, dataset_class, x, mean, upper, lower) %>%
  pivot_longer(cols = c(mean, upper, lower), names_to = "stat", values_to = "value")



# plot the mean bias using dataset classes
mean_bias_plot <- ggplot(se_bias, aes(x = x, y = value, group = stat)) +
  geom_line(aes(color = stat, linetype = stat, linewidth = stat)) +
  facet_wrap(~ dataset_class + variable, scales = "free_x") +
  labs(x = "Parameter value", y = "Bias") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  scale_color_manual(values = c("mean" = "black", "upper" = "red", "lower" = "red")) +
  scale_linetype_manual(values = c("mean" = "solid", "upper" = "dashed", "lower" = "dashed")) +
  scale_linewidth_manual(values = c("mean" = 2, "upper" = 1, "lower" = 1)) +
  theme_bw() +
  ylim(-0.1, 0.1) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "None"
  )
mean_bias_plot
# Save the bias plot
ggsave(filename = paste0(appendixpath, "/sensitivity/bias_plot.jpg"), plot = mean_bias_plot, width = 10, height = 8, units = "in")



# supplemental ----





## general supplemental plots ----
other_paths <- c(
  path_hexcel,
  path_wilderness,
  "data/outputs/firesheds/firesheds_forested/!summaries/firesheds_forested.gpkg",
  "data/outputs/HUCs/HUC08/!summaries/HUC08.gpkg",
  "data/outputs/HUCs/HUC10/!summaries/HUC10.gpkg"
)
other_datasets <- vector("list", length = length(other_paths))
for (i in seq_along(other_paths)) {
  other_datasets[[i]] <- st_read(other_paths[i], quiet = T) %>%
    st_join(states_df)
}
other_dataset_names <- c(
  "median_hexcel",
  "wilderness",
  "firesheds",
  "HUC08",
  "HUC10"
)

## boxplots for emd ----
median_ranks_supp <- list("list", length(other_paths))
supplemental_boxplot_df <- list("list", length(other_paths))
for (i in 1:length(other_datasets)) {
  median_ranks_supp[[i]] <- other_datasets[[i]] %>%
    group_by(NAME) %>%
    summarise(across(c(emd_both, signed_emd_frequency, signed_emd_severity, FRCC_freq_dep, FRCC_sev_dep, FRCC_reg_dep), median)) %>%
    mutate(across(c(emd_both, signed_emd_frequency, signed_emd_severity, FRCC_freq_dep, FRCC_sev_dep, FRCC_reg_dep), rank, .names = "{.col}_rank")) %>%
    select(NAME, emd_both_rank, signed_emd_frequency_rank, signed_emd_severity_rank, FRCC_freq_dep_rank, FRCC_sev_dep_rank, FRCC_reg_dep_rank)
  supplemental_boxplot_df[[i]] <- other_datasets[[i]] %>%
    as.data.frame() %>%
    select(NAME, signed_emd_frequency, signed_emd_severity, emd_both) %>%
    left_join(median_ranks_supp[[i]], by = "NAME") %>%
    drop_na()
}
supplement_boxplot_df <- vector("list", length(other_datasets))
for (i in seq_along(other_datasets)) {
  supplement_boxplot_df[[i]] <- other_datasets[[i]] %>%
    as.data.frame() %>%
    select(NAME, signed_emd_frequency, signed_emd_severity, emd_both, FRCC_freq_dep, FRCC_sev_dep, FRCC_reg_dep) %>%
    left_join(median_ranks, by = "NAME") %>%
    drop_na()
}
total_departure_boxplot_supplemental <- vector("list", length = length(other_datasets))
for (i in seq_along(supplement_boxplot_df)) {
  total_departure_boxplot_supplemental[[i]] <- supplement_boxplot_df[[i]] %>%
    mutate(NAME = factor(NAME, levels = arrange(median_ranks_supp[[i]], desc(emd_both_rank))$NAME)) %>%
    ggplot(aes(x = NAME, y = emd_both)) +
    geom_boxplot() +
    labs(x = "", y = "Fire regime departure") +
    theme_bw() +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
  # ggsave(paste0(supplementalpath,"/",other_dataset_names[[i]],"_emd_boxplot.jpg"), plot = total_departure_boxplot_supplemental[[i]], width = 8, height = 6, units = "in")
}
frequency_departure_boxplot_supplemental <- vector("list", length = length(other_datasets))
for (i in seq_along(supplement_boxplot_df)) {
  frequency_departure_boxplot_supplemental[[i]] <- supplement_boxplot_df[[i]] %>%
    mutate(NAME = factor(NAME, levels = arrange(median_ranks_supp[[i]], desc(signed_emd_frequency_rank))$NAME)) %>%
    ggplot(aes(x = NAME, y = signed_emd_frequency)) +
    geom_boxplot() +
    labs(x = "", y = "Fire frequency departure") +
    theme_bw() +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
  # ggsave(paste0(supplementalpath,"/",other_dataset_names[[i]],"_emd_boxplot.jpg"), plot = total_departure_boxplot_supplemental[[i]], width = 8, height = 6, units = "in")
}
severity_departure_boxplot_supplemental <- vector("list", length = length(other_datasets))
for (i in seq_along(supplement_boxplot_df)) {
  severity_departure_boxplot_supplemental[[i]] <- supplement_boxplot_df[[i]] %>%
    mutate(NAME = factor(NAME, levels = arrange(median_ranks_supp[[i]], desc(signed_emd_severity_rank))$NAME)) %>%
    ggplot(aes(x = NAME, y = signed_emd_severity)) +
    geom_boxplot() +
    labs(x = "", y = "Fire severity departure") +
    theme_bw() +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
  # ggsave(paste0(supplementalpath,"/",other_dataset_names[[i]],"_emd_boxplot.jpg"), plot = total_departure_boxplot_supplemental[[i]], width = 8, height = 6, units = "in")
}

# ## FRCC boxplots ----
# FRCC_reg_dep_boxplot_supplemental <- vector("list", length = length(other_datasets))
# for (i in seq_along(supplement_boxplot_df)) {
#   FRCC_reg_dep_boxplot_supplemental[[i]] <- supplement_boxplot_df[[i]] %>%
#     mutate(NAME = factor(NAME, levels = arrange(median_ranks_supp[[i]], desc(FRCC_reg_dep_rank))$NAME)) %>%
#     ggplot(aes(x = NAME, y = FRCC_reg_dep)) +
#     geom_boxplot() +
#     labs(x = "", y = "FRCC departure") +
#     theme_bw() +
#     geom_hline(yintercept = 0, linetype = "solid", color = "black") +
#     theme(
#       axis.text.x = element_text(angle = 45, hjust = 1),
#       panel.grid.major.x = element_blank(),
#       panel.grid.minor.x = element_blank()
#     )
#   # ggsave(paste0(supplementalpath,"/",other_dataset_names[[i]],"_emd_boxplot.jpg"), plot = total_departure_boxplot_supplemental[[i]], width = 8, height = 6, units = "in")
# }
# 
# FRCC_freq_dep_boxplot_supplemental <- vector("list", length = length(other_datasets))
# for (i in seq_along(supplement_boxplot_df)) {
#   FRCC_freq_dep_boxplot_supplemental[[i]] <- supplement_boxplot_df[[i]] %>%
#     mutate(NAME = factor(NAME, levels = arrange(median_ranks_supp[[i]], desc(FRCC_freq_dep_rank))$NAME)) %>%
#     ggplot(aes(x = NAME, y = FRCC_freq_dep)) +
#     geom_boxplot() +
#     labs(x = "", y = "FRCC frequency departure") +
#     theme_bw() +
#     geom_hline(yintercept = 0, linetype = "solid", color = "black") +
#     theme(
#       axis.text.x = element_text(angle = 45, hjust = 1),
#       panel.grid.major.x = element_blank(),
#       panel.grid.minor.x = element_blank()
#     )
#   # ggsave(paste0(supplementalpath,"/",other_dataset_names[[i]],"_emd_boxplot.jpg"), plot = total_departure_boxplot_supplemental[[i]], width = 8, height = 6, units = "in")
# }
# 
# FRCC_sev_dep_boxplot_supplemental <- vector("list", length = length(other_datasets))
# for (i in seq_along(supplement_boxplot_df)) {
#   FRCC_sev_dep_boxplot_supplemental[[i]] <- supplement_boxplot_df[[i]] %>%
#     mutate(NAME = factor(NAME, levels = arrange(median_ranks_supp[[i]], desc(FRCC_sev_dep_rank))$NAME)) %>%
#     ggplot(aes(x = NAME, y = FRCC_sev_dep)) +
#     geom_boxplot() +
#     labs(x = "", y = "FRCC Fire severity departure") +
#     theme_bw() +
#     geom_hline(yintercept = 0, linetype = "solid", color = "black") +
#     theme(
#       axis.text.x = element_text(angle = 45, hjust = 1),
#       panel.grid.major.x = element_blank(),
#       panel.grid.minor.x = element_blank()
#     )
#   # ggsave(paste0(supplementalpath,"/",other_dataset_names[[i]],"_emd_boxplot.jpg"), plot = total_departure_boxplot_supplemental[[i]], width = 8, height = 6, units = "in")
# }

# combined into 6 panel plot
combined_boxplot_supplemental <- vector("list", length = length(other_datasets))
for (i in seq_along(combined_boxplot_supplemental)) {
  combined_boxplot_supplemental[[i]] <- ggarrange(frequency_departure_boxplot_supplemental[[i]],# FRCC_freq_dep_boxplot_supplemental[[i]],
    severity_departure_boxplot_supplemental[[i]], #FRCC_sev_dep_boxplot_supplemental[[i]],
    total_departure_boxplot_supplemental[[i]], #FRCC_reg_dep_boxplot_supplemental[[i]],
    ncol = 1, nrow = 3
  )
  ggsave(paste0(supplementalpath, "/", other_dataset_names[i], "_emd_boxplots.jpg"), plot = combined_boxplot_supplemental[[i]], width = 10, height = 15, units = "in")
}


## supplemental maps ----
# map fire regime emd
supplemental_departure_plot <- vector(mode = "list", length = length(other_paths))
for (i in seq_along(other_paths)) {
  supplemental_departure_plot[[i]] <- ggplot() +
    geom_sf(data = states_df, fill = "grey50", color = "black") +
    geom_sf(data = other_datasets[[i]], aes(fill = emd_both), linewidth = 0) +
    scale_fill_viridis_c(option = "A") +
    labs(fill = "Fire regime departure") +
    theme_bw() +
    theme(plot.margin = ggplot2::margin(0.1, 0.1, 1, 0, "cm"))
  # ggsave(paste0(supplementalpath,"/",other_dataset_names[[i]],"_fireRegimeDeparture_map.jpg"), plot = supplemental_departure_plot[[i]], width = 8, height = 6, units = "in")
}


# map fire frequency emd
firefreqDeparture_plot <- vector("list", length = length(other_datasets))
for (i in seq_along(other_datasets)) {
  firefreqDeparture_plot[[i]] <- ggplot() +
    geom_sf(data = states_df, fill = "grey50", color = "black") +
    geom_sf(data = other_datasets[[i]], aes(fill = emd_frequency), linewidth = 0) +
    scale_fill_viridis_c(option = "A") +
    labs(fill = "Fire frequency departure") +
    theme_bw() +
    theme(plot.margin = ggplot2::margin(0.1, 0.1, 1, 0, "cm"))
  # ggsave(paste0(supplementalpath,"/",other_dataset_names[[i]],"_firefreqDeparture_map.jpg"), plot = firefreqDeparture_plot[[i]], width = 8, height = 6, units = "in")
}

# map fire severity emd
firesevDeparture_plot <- vector("list", length = length(other_datasets))
for (i in seq_along(other_datasets)) {
  firesevDeparture_plot[[i]] <- ggplot() +
    geom_sf(data = states_df, fill = "grey50", color = "black") +
    geom_sf(data = other_datasets[[i]], aes(fill = emd_severity), linewidth = 0) +
    scale_fill_viridis_c(option = "A") +
    labs(fill = "Fire severity departure") +
    theme_bw() +
    theme(plot.margin = ggplot2::margin(0.1, 0.1, 1, 0, "cm"))
  # ggsave(paste0(supplementalpath,"/",other_dataset_names[[i]],"_firefreqDeparture_map.jpg"), plot = firefreqDeparture_plot[[i]], width = 8, height = 6, units = "in")
}



# do the same with FRCC
supplemental_frcc_dep_plot <- vector(mode = "list", length = length(other_paths))
for (i in seq_along(other_paths)) {
  supplemental_frcc_dep_plot[[i]] <- ggplot() +
    geom_sf(data = states_df, fill = "grey50", color = "black") +
    geom_sf(data = other_datasets[[i]], aes(fill = FRCC_reg_dep), linewidth = 0) +
    scale_fill_viridis_c(option = "A") +
    labs(fill = "FRCC  departure") +
    theme_bw() +
    theme(
      plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0, "cm"),
      legend.title = element_text(size = 14)
    )
  # ggsave(paste0(supplementalpath,"/",other_dataset_names[[i]],"_frccDeparture_map.jpg"), plot = supplemental_frcc_dep_plot[[i]], width = 8, height = 6, units = "in")
}
# FRCCregDeparture_plot <- vector("list", length = length(other_datasets))
# for (i in seq_along(other_datasets)) {
#   FRCCregDeparture_plot[[i]] <- ggplot() +
#     geom_sf(data = states_df, fill = "grey50", color = "black") +
#     geom_sf(data = other_datasets[[i]], aes(fill = FRCC_reg_dep), linewidth = 0) +
#     scale_fill_viridis_c(option = "A") +
#     labs(fill = "FRCC  departure") +
#     theme_bw() +
#     theme(plot.margin = ggplot2::margin(0.1, 0.1, 1, 0, "cm"))
#   # ggsave(paste0(supplementalpath,"/",other_dataset_names[[i]],"_FRCCregDeparture_map.jpg"), plot = FRCCregDeparture_plot[[i]], width = 8, height = 6, units = "in")
# }
# 
# # map FRCC frequency emd
# FRCCfreqDeparture_plot <- vector("list", length = length(other_datasets))
# for (i in seq_along(other_datasets)) {
#   FRCCfreqDeparture_plot[[i]] <- ggplot() +
#     geom_sf(data = states_df, fill = "grey50", color = "black") +
#     geom_sf(data = other_datasets[[i]], aes(fill = FRCC_freq_dep), linewidth = 0) +
#     scale_fill_viridis_c(option = "A") +
#     labs(fill = "FRCC frequency\ndeparture") +
#     theme_bw() +
#     theme(plot.margin = ggplot2::margin(0.1, 0.1, 1, 0, "cm"))
#   # ggsave(paste0(supplementalpath,"/",other_dataset_names[[i]],"_FRCCfreqDeparture_map.jpg"), plot = FRCCfreqDeparture_plot[[i]], width = 8, height = 6, units = "in")
# }
# 
# # map FRCC severity emd
# FRCCsevDeparture_plot <- vector("list", length = length(other_datasets))
# for (i in seq_along(other_datasets)) {
#   FRCCsevDeparture_plot[[i]] <- ggplot() +
#     geom_sf(data = states_df, fill = "grey50", color = "black") +
#     geom_sf(data = other_datasets[[i]], aes(fill = FRCC_sev_dep), linewidth = 0) +
#     scale_fill_viridis_c(option = "A") +
#     labs(fill = "FRCC severity\ndeparture") +
#     theme_bw() +
#     theme(plot.margin = ggplot2::margin(0.1, 0.1, 1, 0, "cm"))
#   # ggsave(paste0(supplementalpath,"/",other_dataset_names[[i]],"_FRCCsevDeparture_map.jpg"), plot = FRCCsevDeparture_plot[[i]], width = 8, height = 6, units = "in")
# }


# combined into six panel plot
departure_maps <- vector("list", length = length(other_datasets))
for (i in seq_along(other_datasets)) {
  departure_maps[[i]] <- ggarrange(firefreqDeparture_plot[[i]], #FRCCfreqDeparture_plot[[i]],
    firesevDeparture_plot[[i]], #FRCCsevDeparture_plot[[i]],
    supplemental_departure_plot[[i]], #FRCCregDeparture_plot[[i]],
    ncol = 1, nrow = 3
  )
  ggsave(paste0(supplementalpath, "/", other_dataset_names[[i]], "_departure_maps.jpg"), plot = departure_maps[[i]], width = 10, height = 15, units = "in")
}


# rank maps
# map fire regime emd
fireRegimeDeparture_plot <- vector(mode = "list", length = length(other_paths))
for (i in seq_along(other_paths)) {
  fireRegimeDeparture_plot[[i]] <- ggplot() +
    geom_sf(data = states_df, fill = "grey50", color = "black") +
    geom_sf(data = other_datasets[[i]], aes(fill = rank(emd_both)), linewidth = 0) +
    scale_fill_viridis_c(option = "A") +
    labs(fill = "Fire regime departure ranked") +
    theme_bw() +
    theme(plot.margin = ggplot2::margin(0.1, 0.1, 1, 0, "cm"))
  # ggsave(paste0(supplementalpath,"/",other_dataset_names[[i]],"_fireRegimeDeparture_map.jpg"), plot = supplemental_departure_plot[[i]], width = 8, height = 6, units = "in")
}


# map fire frequency emd
firefreqDeparture_plot <- vector("list", length = length(other_datasets))
for (i in seq_along(other_datasets)) {
  firefreqDeparture_plot[[i]] <- ggplot() +
    geom_sf(data = states_df, fill = "grey50", color = "black") +
    geom_sf(data = other_datasets[[i]], aes(fill = rank(emd_frequency)), linewidth = 0) +
    scale_fill_viridis_c(option = "A") +
    labs(fill = "Fire frequency departure Ranked") +
    theme_bw() +
    theme(plot.margin = ggplot2::margin(0.1, 0.1, 1, 0, "cm"))
  # ggsave(paste0(supplementalpath,"/",other_dataset_names[[i]],"_firefreqDeparture_map.jpg"), plot = firefreqDeparture_plot[[i]], width = 8, height = 6, units = "in")
}

# map fire severity emd
firesevDeparture_plot <- vector("list", length = length(other_datasets))
for (i in seq_along(other_datasets)) {
  firesevDeparture_plot[[i]] <- ggplot() +
    geom_sf(data = states_df, fill = "grey50", color = "black") +
    geom_sf(data = other_datasets[[i]], aes(fill = rank(emd_severity)), linewidth = 0) +
    scale_fill_viridis_c(option = "A") +
    labs(fill = "Fire severity departure ranked") +
    theme_bw() +
    theme(plot.margin = ggplot2::margin(0.1, 0.1, 1, 0, "cm"))
  # ggsave(paste0(supplementalpath,"/",other_dataset_names[[i]],"_firefreqDeparture_map.jpg"), plot = firefreqDeparture_plot[[i]], width = 8, height = 6, units = "in")
}

# do the same for frcc
# FRCCregDeparture_plot <- vector("list", length = length(other_datasets))
# for (i in seq_along(other_datasets)) {
#   FRCCregDeparture_plot[[i]] <- ggplot() +
#     geom_sf(data = states_df, fill = "grey50", color = "black") +
#     geom_sf(data = other_datasets[[i]], aes(fill = rank(FRCC_reg_dep)), linewidth = 0) +
#     scale_fill_viridis_c(option = "A") +
#     labs(fill = "FRCC  departure\nranked") +
#     theme_bw() +
#     theme(plot.margin = ggplot2::margin(0.1, 0.1, 1, 0, "cm"))
#   # ggsave(paste0(supplementalpath,"/",other_dataset_names[[i]],"_FRCCregDeparture_map.jpg"), plot = FRCCregDeparture_plot[[i]], width = 8, height = 6, units = "in")
# }
# 
# # map FRCC frequency emd
# FRCCfreqDeparture_plot <- vector("list", length = length(other_datasets))
# for (i in seq_along(other_datasets)) {
#   FRCCfreqDeparture_plot[[i]] <- ggplot() +
#     geom_sf(data = states_df, fill = "grey50", color = "black") +
#     geom_sf(data = other_datasets[[i]], aes(fill = rank(FRCC_freq_dep)), linewidth = 0) +
#     scale_fill_viridis_c(option = "A") +
#     labs(fill = "FRCC frequency\ndeparture ranked") +
#     theme_bw() +
#     theme(plot.margin = ggplot2::margin(0.1, 0.1, 1, 0, "cm"))
#   # ggsave(paste0(supplementalpath,"/",other_dataset_names[[i]],"_FRCCfreqDeparture_map.jpg"), plot = FRCCfreqDeparture_plot[[i]], width = 8, height = 6, units = "in")
# }
# 
# # map FRCC severity emd
# FRCCsevDeparture_plot <- vector("list", length = length(other_datasets))
# for (i in seq_along(other_datasets)) {
#   FRCCsevDeparture_plot[[i]] <- ggplot() +
#     geom_sf(data = states_df, fill = "grey50", color = "black") +
#     geom_sf(data = other_datasets[[i]], aes(fill = rank(FRCC_sev_dep)), linewidth = 0) +
#     scale_fill_viridis_c(option = "A") +
#     labs(fill = "FRCC severity\ndeparture ranked") +
#     theme_bw() +
#     theme(plot.margin = ggplot2::margin(0.1, 0.1, 1, 0, "cm"))
#   # ggsave(paste0(supplementalpath,"/",other_dataset_names[[i]],"_FRCCsevDeparture_map.jpg"), plot = FRCCsevDeparture_plot[[i]], width = 8, height = 6, units = "in")
# }

# combined into six panel plot
rank_maps <- vector("list", length = length(other_datasets))
for (i in seq_along(other_datasets)) {
   rank_maps[[i]] <- ggarrange(firefreqDeparture_plot[[i]], #FRCCfreqDeparture_plot[[i]],
    firesevDeparture_plot[[i]], #FRCCsevDeparture_plot[[i]],
    fireRegimeDeparture_plot[[i]], #FRCCregDeparture_plot[[i]],
    ncol = 1, nrow = 3
  )
  ggsave(paste0(supplementalpath, "/", other_dataset_names[[i]], "_rank_maps.jpg"), plot = rank_maps[[i]], width = 10, height = 15, units = "in")
}


## FRCC and EMD relationships ----

# Frequency Departure
plot_objects_frequency <- vector("list", length = length(other_datasets))
for (i in seq_along(other_datasets)) {
  df_emdFreqBypfrid <- other_datasets[[i]] %>%
    select(name, pfrid, signed_emd_frequency) %>%
    drop_na()

  model <- lm(signed_emd_frequency ~ pfrid, data = df_emdFreqBypfrid)
  R_squared <- round(summary(model)$adj.r.squared, digits = 2)
  R_df <- data.frame(R = paste0("R² = ", R_squared), x = 12, y = 2.5)

  plot_objects_frequency[[i]] <- ggplot(df_emdFreqBypfrid, aes(x = pfrid, y = signed_emd_frequency)) +
    geom_point(alpha = 0.3) +
    theme_bw() +
    xlim(c(0, 100)) +
    ylim(c(0, 4)) +
    labs(x = "FRCC fire return interval departure", y = "Fire frequency departure") +
    geom_text(data = R_df, aes(x = x, y = y, label = R))
}

# Severity Departure
plot_objects_severity <- vector("list", length = length(other_datasets))
for (i in seq_along(other_datasets)) {
  df_emdSevBySev <- other_datasets[[i]] %>%
    select(name, percent_severity_departure, signed_emd_severity) %>%
    drop_na()

  model <- lm(signed_emd_severity ~ percent_severity_departure, data = df_emdSevBySev)
  R_squared <- round(summary(model)$adj.r.squared, digits = 2)
  R_df <- data.frame(R = paste0("R² = ", R_squared), x = 12, y = 2.5)

  plot_objects_severity[[i]] <- ggplot(df_emdSevBySev, aes(x = percent_severity_departure, y = signed_emd_severity)) +
    geom_point(alpha = 0.3) +
    theme_bw() +
    xlim(c(0, 100)) +
    ylim(c(0, 4)) +
    labs(x = "FRCC severity departure", y = "Fire severity departure") +
    geom_text(data = R_df, aes(x = x, y = y, label = R))
}

# Total Departure
plot_objects_total <- vector("list", length = length(other_datasets))
for (i in seq_along(other_datasets)) {
  model <- lm(emd_both ~ FRCC_reg_dep, data = other_datasets[[i]])
  R_squared <- round(summary(model)$adj.r.squared, digits = 2)
  MSE <- deviance(model) / df.residual(model)
  RMSE <- round(sqrt(MSE), digits = 2)

  R_df <- data.frame(label = paste0("R² = ", R_squared, "\nRMSE = ", RMSE), x = 12, y = 2.5)

  plot_objects_total[[i]] <- ggplot(other_datasets[[i]], aes(x = FRCC_reg_dep, y = emd_both)) +
    geom_point(alpha = 0.3) +
    theme_bw() +
    xlim(c(0, 100)) +
    ylim(c(0, 4)) +
    labs(x = "FRCC regime departure", y = "Fire regime departure") +
    geom_vline(xintercept = 66, color = "black", linetype = "dashed") +
    geom_smooth(method = "lm", color = "red") +
    geom_text(data = R_df, aes(x = x, y = y, label = label))

  t_df <- data.frame(x = 66, y = 3.5, label = "FRCC high departure")
  plot_objects_total[[i]] <- plot_objects_total[[i]] +
    geom_text(data = t_df, aes(x = x, y = y, label = label))
}

# Save the plots
for (i in seq_along(other_datasets)) {
  plot_combined <- ggarrange(plot_objects_frequency[[i]], plot_objects_severity[[i]],
    plot_objects_total[[i]], NULL,
    ncol = 2, nrow = 2
  )
  save_plot_path <- file.path(supplementalpath, paste0(other_dataset_names[[i]], "_FRCCRelationship.png"))
  ggsave(save_plot_path, plot_combined, width = 12, height = 8, units = "in")
}

## EMD components ----

# Signed Labels
signed_labels <- data.frame(
  x = c(1, 1, -1, -1)*2.5,
  y = c(1, -1, -1, 1)*2.5,
  label = c(
    "Less frequent,\n more severe",
    "Less frequent,\n less severe",
    "More frequent,\n less severe",
    "More frequent,\n more severe"
  )
)

# Generic EMD Plot
signed_emd_df <- df_joined %>%
  select(name, signed_emd_frequency, signed_emd_severity) %>%
  drop_na()
generic_emd_plot <- ggplot(signed_emd_df, aes(x = signed_emd_frequency, y = signed_emd_severity)) +
  geom_point(alpha = 0.3, size = 3) +
  theme_bw() +
  xlim(c(-3, 3)) +
  ylim(c(-3, 3)) +
  labs(x = "Fire frequency departure", y = "Fire severity departure") +
  geom_vline(xintercept = 0, color = "black", linetype = "solid") +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  geom_text(data = signed_labels, aes(x = x, y = y, label = label), size = 24/.pt) +
  theme(axis.title = element_text(size = 24), axis.text = element_text(size = 16))

# Save the Generic EMD Plot
save_plot_path <- file.path(mainpath, "figure_2.pdf")
ggsave(save_plot_path, generic_emd_plot, width = 10, height = 10, units = "in", dpi = 300)



signed_labels <- data.frame(
  x = c(3, 3, -3, -3),
  y = c(4, -4, -4, 4),
  label = c(
    "Less frequent,\n more severe",
    "Less frequent,\n less severe",
    "More frequent,\n less severe",
    "More frequent,\n more severe"
  )
)
# Supplemental EMD Plots
for (i in seq_along(other_datasets)) {
  signed_emd_df <- other_datasets[[i]] %>%
    select(name, signed_emd_frequency, signed_emd_severity) %>%
    drop_na()

  emd_plot <- ggplot(signed_emd_df, aes(x = signed_emd_frequency, y = signed_emd_severity)) +
    geom_point(alpha = 0.3) +
    theme_bw() +
    xlim(c(-4, 4)) +
    ylim(c(-4, 4)) +
    labs(x = "Fire frequency departure", y = "Fire severity departure") +
    geom_vline(xintercept = 0, color = "black", linetype = "solid") +
    geom_hline(yintercept = 0, color = "black", linetype = "solid") +
    geom_text(data = signed_labels, aes(x = x, y = y, label = label))

  save_plot_path <- file.path(supplementalpath, paste0(other_dataset_names[[i]], "_emd_components.png"))
  ggsave(save_plot_path, emd_plot, width = 6, height = 6, units = "in")
}








# Figure 6: residual plot ------
percentile_res <- function(x) {
  ecdf(x)(x)
}

df_joined <- st_read(path_hexcel, quiet = T) %>%
  st_join(states_df) %>%
  filter(study_area_ha >= 0.5 * max(study_area_ha))


residual_df <- df_joined %>%
  mutate( emd_percentile = percentile_res(emd_frequency),
          residual = (emd_percentile - percentile_res(FRCC_freq_dep))) 
freq_bias <- data.frame( mean = mean(residual_df$residual), SD = sd(residual_df$residual),
                         MAD = mad(residual_df$residual), MAE = mean(abs(residual_df$residual)))
freq_res <- ggplot()+
  geom_sf(data = states_df, fill = "grey70")+
  geom_sf(data = residual_df, aes(fill = residual), linewidth = 0 )+
  scale_fill_distiller(expression(Delta[percentile]), palette = "RdYlBu", limits = c(-1, 1)) +
  theme_minimal()+
  theme(legend.position = "right",
        axis.text = element_blank()) 
  labs(title = "Frequency Difference")

#repeat for severity
residual_df <- df_joined %>%
  mutate( emd_percentile = percentile_res(emd_severity),
          residual = (emd_percentile - percentile_res(FRCC_sev_dep)))
sev_bias <- data.frame( mean = mean(residual_df$residual), SD = sd(residual_df$residual),
                        MAD = mad(residual_df$residual), MAE = mean(abs(residual_df$residual)))
sev_res <- ggplot()+
  geom_sf(data = states_df, fill = "grey70")+
  geom_sf(data = residual_df, aes(fill = residual), linewidth = 0 )+
  scale_fill_distiller(expression(Delta[percentile]), palette = "RdYlBu", limits = c(-1, 1))+
  theme_minimal()+
  theme(legend.position = "right",
        axis.text = element_blank())
  labs(title = "Severity Difference")

# repeat for regime

residual_df <- df_joined %>%
  mutate( emd_percentile = percentile_res(emd_both),
          residual = (emd_percentile - percentile_res(FRCC_reg_dep)))
reg_bias <- data.frame( mean = mean(residual_df$residual), SD = sd(residual_df$residual), 
                        MAD = mad(residual_df$residual), MAE = mean(abs(residual_df$residual)))
reg_res <- ggplot()+
  geom_sf(data = states_df, fill = "grey70")+
  geom_sf(data = residual_df, aes(fill = residual), linewidth = 0 )+
  scale_fill_distiller(expression(Delta[percentile]), palette = "RdYlBu", limits = c(-1, 1))+
  theme_minimal()+
  theme(legend.position = "right",
        axis.text = element_blank())
  labs(title = "Fire-Regime Difference")

# save the plots
multi_res <- ggarrange(freq_res, sev_res, reg_res, ncol = 2, nrow = 2, common.legend = TRUE, legend = "right",
                       labels = c("A)", "B)", "C)", ""))
save_plot_path <- file.path(mainpath, "residuals.svg")
ggsave(save_plot_path, multi_res, width = 12, height = 12, units = "in")
  
#build for all supplemental datasets

for (i in seq_along(other_datasets)) {
  residual_df <- other_datasets[[i]] %>%
    mutate( emd_percentile = percentile_res(emd_frequency),
            residual = (emd_percentile - percentile_res(FRCC_freq_dep))*100)
  freq_res <- ggplot()+
    geom_sf(data = states_df, fill = "grey70")+
    geom_sf(data = residual_df, aes(fill = residual), linewidth = 0 )+
    scale_fill_distiller(expression(Delta[percentile]), palette = "RdYlBu", limits = c(-100, 100))+
    theme_bw()+
    theme(legend.position = "right") +
    labs(title = "Fire frequency departure")
  
  residual_df <- other_datasets[[i]] %>%
    mutate( emd_percentile = percentile_res(emd_severity),
            residual = (emd_percentile - percentile_res(FRCC_sev_dep))*100)
  sev_res <- ggplot()+
    geom_sf(data = states_df, fill = "grey70")+
    geom_sf(data = residual_df, aes(fill = residual), linewidth = 0 )+
    scale_fill_distiller(expression(Delta[percentile]), palette = "RdYlBu", limits = c(-100, 100))+
    theme_bw()+
    theme(legend.position = "right") +
    labs(title = "Fire severity departure")
  
  residual_df <- other_datasets[[i]] %>%
    mutate( emd_percentile = percentile_res(emd_both),
            residual = (emd_percentile - percentile_res(FRCC_reg_dep))*100)
  reg_res <- ggplot()+
    geom_sf(data = states_df, fill = "grey70")+
    geom_sf(data = residual_df, aes(fill = residual), linewidth = 0 )+
    scale_fill_distiller(expression(Delta[percentile]), palette = "RdYlBu", limits = c(-100, 100))+
    theme_bw()+
    theme(legend.position = "right") +
    labs(title = "Fire regime departure")
  
  # save the plots
  multi_res <- ggarrange(freq_res, sev_res, reg_res, nrow = 3)
  
  save_plot_path <- file.path(supplementalpath, paste0(other_dataset_names[[i]], "_residuals.png"))
  ggsave(save_plot_path, multi_res, width = 12, height = 12, units = "in")
}





######## MISCELANEOUS PLOTS. COULD BE INTERESTING FOR OTHERS ######## ----





## rad plot ----


rad_df <- df_joined %>%
  mutate( rad_label = ifelse(emd_both <= 2, "Resist",
                             ifelse(emd_both <=3, "Direct", "Accept")))
rad_plot <- ggplot()+
  geom_sf(data = states_df, fill = "grey70")+
  geom_sf(data = rad_df, aes(fill = rad_label), linewidth = 0 )+
  scale_fill_manual("Response", values = c(  "Accept" = "red", "Direct" = "yellow","Resist" = "lightgreen"))+
  theme_minimal()+
  theme(legend.position = "bottom")


####
#### ridgeline plot
# total_departure_ridge <- df_joined_boxplot %>%
#        mutate(NAME = factor(NAME, levels = arrange(median_ranks, emd_both_rank)$NAME )) %>%
#        ggplot(aes(x = emd_both, y = NAME, fill = after_stat(x))) +
#        geom_density_ridges_gradient(scale = 3, rel_min_height = 0.005) +
#        scale_fill_viridis(option = "C") +
#        labs(x = "Fire-Regime Departure", y = "") +
#        theme_bw()+
#        theme(
#                      legend.position="none",
#                      panel.spacing = unit(0.1, "lines"),
#                      strip.text.x = element_text(size = 8)
#               )


# # Ridge plot for frequency departure
# frequency_departure_ridge <- df_joined_boxplot %>%
#        mutate(NAME = factor(NAME, levels = arrange(median_ranks, signed_emd_frequency_rank)$NAME )) %>%
#        ggplot(aes(x = signed_emd_frequency, y = NAME, fill = after_stat(x))) +
#        geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005) +
#        scale_fill_distiller(type = "div", palette = "RdBu",
#        values =c(0,
#         (0-min(df_joined_boxplot$signed_emd_frequency))/(max(df_joined_boxplot$signed_emd_frequency)-min(df_joined_boxplot$signed_emd_frequency)),
#          1)) +
#        labs(x = "Fire frequency departure", y = "") +
#        theme_bw() +
#        theme(
#                      legend.position="none",
#                      panel.spacing = unit(0.1, "lines"),
#                      strip.text.x = element_text(size = 8)
#               )


# # Ridge plot for Fire severity departure
# severity_departure_ridge <- df_joined_boxplot %>%
#        mutate(NAME = factor(NAME, levels = arrange(median_ranks, signed_emd_severity_rank)$NAME )) %>%
#        ggplot(aes(x = signed_emd_severity, y = NAME, fill = ..x..)) +
#        geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005) +
#        scale_fill_distiller(type = "div", palette = "RdBu",
#        values =c(0,
#         (0-min(df_joined_boxplot$signed_emd_severity))/(max(df_joined_boxplot$signed_emd_severity)-min(df_joined_boxplot$signed_emd_severity)),
#          1)) +
#        labs(x = "Fire severity departure", y = "") +
#        theme_bw() +
#        theme(
#                      legend.position="none",
#                      panel.spacing = unit(0.1, "lines"),
#                      strip.text.x = element_text(size = 8)
#               )



################
################
################
