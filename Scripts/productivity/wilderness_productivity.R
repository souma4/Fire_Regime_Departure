pkgs <- c("sf", "tidyverse", "terra","RColorBrewer", "tmap", "units", "ggrepel", "ggpubr","plotly", "reticulate")
invisible(lapply(pkgs, library, character.only = T))
source("scripts/productivity/productivity_source.R")
crop_states <- st_read("data/masks/cleaned/wna_states.shp")
wildernesses_wna <- st_read("data/masks/cleaned/wilderness_cleaned.shp") %>%
  st_intersection(crop_states) %>%
  group_by(DispN) %>%
  summarize(geometry = st_union(geometry))
crop_states <- crop_states %>%
  filter(!CODE %in% c("ND","SD","NE","KS","OK","TX"))
load("data/outputs/wilderness/wilderness_all/!stored_data.RData")

summary_stats_all <- production_figures_analysis(stored_data,wildernesses_wna,crop_states,"data/outputs/wilderness/wilderness_all","DispN", "wilderness_all_emd")

load("data/outputs/wilderness/wilderness_forested/!stored_data.RData")
summary_stats_forest <- production_figures_analysis(stored_data,wildernesses_wna, crop_states, "data/outputs/wilderness/wilderness_forested","DispN", "wilderness_forested_emd")


### sensitivity analysis

load("data/outputs/wilderness/wilderness_sensitivity/Miller_Thode/!stored_data.RData")
summary_stats_sensitivity <- production_figures_analysis(stored_data,wildernesses_wna,crop_states,"data/outputs/wilderness/wilderness_sensitivity","DispN", "wilderness_sensitivity_emd")

#Miller Thode Check
for_df <- as.data.frame(summary_stats_forest$full_sf)%>%
  select(name, emd_median_both, emd_median_both_normalized) 
sens_df <- as.data.frame(summary_stats_sensitivity$full_sf)%>%
  select(name, emd_median_both, emd_median_both_normalized)
combined_df <- left_join(for_df, sens_df, by = "name")%>%
  mutate(diff_emd = emd_median_both.x-emd_median_both.y,
         diff_emd_normalized = emd_median_both_normalized.x- emd_median_both_normalized.y)
n_wilderness <- c(nrow(for_df), nrow(sens_df))
#bias
ggplot(combined_df, aes(x = diff_emd)) +
  geom_histogram(aes(y = after_stat(count/sum(count))), bins = 12) +
  labs(x = "Calculated thresholds - Miller & Thode, Unnormalized emd",
       y = "Relative frequency") +
  theme_bw()

ggplot(combined_df, aes(x = diff_emd_normalized)) +
  geom_histogram(aes(y = after_stat(count/sum(count))), bins = 12) +
  labs(x = "Calculated thresholds - Miller & Thode, Normalized emd",
       y = "Relative frequency") +
  theme_bw()
#data_distribution
combined_df %>%
  select(all_of(c("emd_median_both.x", "emd_median_both.y"))) %>%
  rename(ours = emd_median_both.x,
         mt06 = emd_median_both.y) %>%
  pivot_longer(everything(), names_to = "Study", values_to = "EMD") %>%
ggplot( aes(x = EMD, color = Study))+
  geom_density()+
  theme_bw()


combined_df %>%
  select(all_of(c("emd_median_both_normalized.x", "emd_median_both_normalized.y"))) %>%
  rename(ours = emd_median_both_normalized.x,
         mt06 = emd_median_both_normalized.y) %>%
  pivot_longer(everything(), names_to = "Study", values_to = "Normalized_EMD") %>%
  ggplot( aes(x = Normalized_EMD, color = Study))+
  geom_density()+
  theme_bw()


#ranks
rank_check <- combined_df %>%
  mutate(rank_x = rank(emd_median_both.x),
         rank_y = rank(emd_median_both.y),
         rank_x_normalized = rank(emd_median_both_normalized.x),
         rank_y_normalized  = rank(emd_median_both_normalized.y),
         rank_check = (rank_x == rank_y),
         rank_check_normalized = (rank_x_normalized == rank_y_normalized))
proportion_ranked_unchanged <- rank_check %>%
  select(name, rank_check, rank_check_normalized) %>%
  summarize(prop_unchanged = sum(rank_check)/length(rank_check),
            prop_unchanged_normalized = sum(rank_check_normalized)/length(rank_check_normalized),
  )
average_rank_change <- rank_check %>%
  mutate(diff_rank = abs(rank_x-rank_y),
         diff_rank_normalized = abs(rank_x_normalized - rank_y_normalized)) %>%
  summarize(average_rank_change = mean(diff_rank),
            average_rank_change_normalized = mean(diff_rank_normalized),
            median_rank_change = median(diff_rank),
            median_rank_change_normalized = median(diff_rank_normalized)
  )


#Now check repeated operations
folder_names <- c("one","two","three","four","five","six","seven","eight",
                  "nine","ten")
data_list_all <- list(length = 10)
for (i in 1:10){
  load(paste0("data/outputs/wilderness/wilderness_sensitivity/",folder_names[i],"/!stored_data.RData"))
  
  data_list <- stored_data
  valid_data <- check_data(data_list)
  valid_df <- production_pulling(valid_data) %>%
    drop_na()%>%
    distinct(name, .keep_all = T) %>%
    select(name, emd_median_both, emd_median_both_normalized)
  new_col_names <- c("name", paste0("emd_both_",i), paste0("emd_both_normalized_",i))
  names(valid_df) <- new_col_names
  data_list_all[[i]] <- valid_df
  
}


all_lengths <- sapply(data_list_all, nrow)
med_length <- all_lengths %>% median()
range_lengths <- range(all_lengths)

combined_df <- for_df %>%
  left_join(data_list_all[[1]]) %>%
  left_join(data_list_all[[2]]) %>%
  left_join(data_list_all[[3]]) %>%
  left_join(data_list_all[[4]]) %>%
  left_join(data_list_all[[5]]) %>%
  left_join(data_list_all[[6]]) %>%
  left_join(data_list_all[[7]]) %>%
  left_join(data_list_all[[8]]) %>%
  left_join(data_list_all[[9]]) %>%
  left_join(data_list_all[[10]]) 

emd_colnames <- c("name", "emd_median_both", paste0("emd_both_",1:10))
emd_norm_colnames <- c("name", "emd_median_both_normalized", paste0("emd_both_normalized_",1:10))

new_emd_colnames <- c("name","main",as.character(1:10))
emd_sens_df <- combined_df %>%
  select(all_of(emd_colnames)) 
names(emd_sens_df) <- new_emd_colnames
emd_sens_df_long <- emd_sens_df %>%
pivot_longer(!name, names_to = "Run", values_to = "EMD")


emd_norm_sens_df <- combined_df %>%
  select(all_of(emd_norm_colnames)) 
names(emd_norm_sens_df) <- new_emd_colnames
emd_sens_norm_df_long <- emd_norm_sens_df %>%
  pivot_longer(!name, names_to = "Run", values_to = "Normalized_EMD")

#bias
emd_bias <- emd_sens_df %>%
  select_if(is.numeric) %>%
  mutate(across(everything(), ~ (main-.x))) %>%
  select(!main) %>%
  pivot_longer(everything(), names_to = "Run", values_to = "Bias")
bias_density <- ggplot(emd_bias, aes(x = Bias, color = Run)) +
  geom_density()+
  theme_bw()

emd_norm_bias <- emd_norm_sens_df %>%
  select_if(is.numeric) %>%
  mutate(across(everything(), ~ (main-.x))) %>%
  select(!main) %>%
  pivot_longer(everything(), names_to = "Run", values_to = "Bias")
norm_bias_density <- ggplot(emd_norm_bias, aes(x = Bias, color = Run)) +
  geom_density()+
  theme_bw()


#distribution of values
sens_hist_plot <- ggplot(emd_sens_df_long, aes(x = EMD, fill = Run)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 15, position = "dodge") +
  ylab("Relative Frequency") +
  theme_bw()
sens_density_plot <- ggplot(emd_sens_df_long, aes(x = EMD, color = Run)) +
  geom_density( alpha = .2) +
  theme_bw()




sens_hist_plot_norm <- ggplot(emd_sens_norm_df_long, aes(x = Normalized_EMD, fill = Run)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 15, position = "dodge") +
  ylab("Relative Frequency") +
  theme_bw()
sens_density_plot_norm <- ggplot(emd_sens_norm_df_long, aes(x = Normalized_EMD, color = Run)) +
  geom_density( alpha = .2) +
  theme_bw()


#change in ranks

emd_rank <- emd_sens_df %>%
  select_if(is.numeric) %>%
  mutate_all(~ rank(.))
emd_rank_change <- emd_rank %>%
  mutate(across( everything(), ~ abs(main-.x))) %>%
  select(!main) %>%
  pivot_longer(everything(), names_to = "Run", values_to = "Rank_change") %>%
  group_by(Run) %>%
  summarize(mean_change = mean(Rank_change),
            median_change = median(Rank_change))
averages <- colMeans(emd_rank_change[,2:3])


emd_rank_norm <- emd_norm_sens_df %>%
  select_if(is.numeric) %>%
  mutate_all(~ rank(.))
emd_rank_norm_change <- emd_rank_norm %>%
  mutate(across( everything(), ~ abs(main-.x))) %>%
  select(!main) %>%
  pivot_longer(everything(), names_to = "Run", values_to = "Rank_change") %>%
  group_by(Run) %>%
  summarize(mean_change = mean(Rank_change),
            median_change = median(Rank_change))
averages_norm <- colMeans(emd_rank_norm_change[,2:3])

  


