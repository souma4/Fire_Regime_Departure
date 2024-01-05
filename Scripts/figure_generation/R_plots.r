library(tidyverse)
library(plotly)
library(sf)
library(viridis)
library(terra)
library(RColorBrewer)
library(ggpubr)
library(ggridges)
library(randomcoloR)


path1 = file.path(getwd(), "data", "outputs", 
                  "med_grids", "med_hex_forested", 
                  "!summaries", "med_hex_forested.gpkg")

path_states = file.path(getwd(),"data","masks","cleaned","wna_states.shp")

states_df = st_read(path_states, quiet = T) 

df_joined = st_read(path1,quiet = T) %>%
                     st_join(states_df) %>%
              mutate(FRI_departure = contemp_fri - hist_fri,
                     CBI_departure = median_contemporary_severity -median_historical_severity) 

##correlations
correlations <- df_joined %>%
  st_drop_geometry() %>%
  select(all_of(c("emd_both_normalized", "signed_emd_frequency_normalized", "signed_emd_severity_normalized", "pfrid","percent_severity_departure","FRCC_reg_dep"))) %>%
  cor(use = "everything")

relevant_correlations <- c(correlations[2,4],correlations[3,5], correlations[1,6])

outpath = file.path(getwd(), "figures", "Final_Figures")
#Maps
study_area <- ggplot() + 
       geom_sf(data = states_df, fill = "grey90", color = "black") +
       geom_sf(data = df_joined, fill = "grey40", color = "black") +
       theme_bw()


fireRegimeDeparture_plot <- ggplot() +
  geom_sf(data = states_df, fill = "grey90", color = "black") +
  geom_sf(data = df_joined, aes(fill = emd_both_normalized), linewidth = 0) +
  scale_fill_viridis_c(option = "A") +
  labs(fill = "Fire-Regime\nDepature") +
  theme_bw() +
   theme(plot.margin = margin(0.1, 0.1, 1, 0, "cm")) 
ggsave(paste0(outpath,"/fireRegimeDeparture_map.jpg"), plot = fireRegimeDeparture_plot, width = 8, height = 6, units = "in")

frccDeparture_plot <- ggplot() +
geom_sf(data = states_df, fill = "grey90", color = "black") +
geom_sf(data = df_joined, aes(fill = FRCC_reg_dep), linewidth = 0) +
scale_fill_viridis_c(option = "A") +
labs(fill = "FRCC  Depature") +
theme_bw()+
   theme(plot.margin = margin(0.1, 0.1, 0.1, 0, "cm"),
         legend.title=element_text(size=14))

#diverging color ramp
df_joined_div <- df_joined %>%
  mutate(color_ramp_freq_code = ifelse(signed_emd_frequency_normalized > quantile(emd_frequency_normalized, 0.5, na.rm = T, names = F) , 1, 
                                       ifelse(signed_emd_frequency_normalized < -quantile(emd_frequency_normalized, 0.5, na.rm = T, names = F), -1, 0)),
         color_ramp_sev_code = ifelse(signed_emd_severity_normalized > quantile(emd_severity_normalized, 0.5, na.rm = T, names = F) , 1,
                                      ifelse(signed_emd_severity_normalized < -quantile(emd_severity_normalized, 0.5, na.rm = T, names = F), -1, 0)),
         color_ramp_color = case_when(color_ramp_freq_code == 1 & color_ramp_sev_code == 1 ~ "#c51b7d",
                                           color_ramp_freq_code == 1 & color_ramp_sev_code == -1 ~ "#4d9221",
                                           
                                           color_ramp_freq_code == -1 & color_ramp_sev_code == 1 ~ "#542788",
                                           color_ramp_freq_code == -1 & color_ramp_sev_code == -1 ~ "#b35806",
                                           
                                           color_ramp_freq_code == 0 & color_ramp_sev_code == 0 ~ "#f7f7f7",
                                           
                                           color_ramp_freq_code == 0 & color_ramp_sev_code == 1 ~ "#c2a5cf",
                                           color_ramp_freq_code == 0 & color_ramp_sev_code == -1 ~ "#d9f0d3",
                                           
                                           color_ramp_freq_code == 1 & color_ramp_sev_code == 0 ~ "#fde0ef",
                                           color_ramp_freq_code == -1 & color_ramp_sev_code == 0 ~ "#fee0b6")) %>%
         select(color_ramp_color)
diverging_departure_plot <- ggplot() +
  geom_sf(data = states_df, fill = "grey90", color = "black") +
  geom_sf(data = df_joined_div, fill = df_joined_div$color_ramp_color, linewidth = 0) +
  theme_bw()
diverging_departure_plot
ggsave(paste0(outpath,"/diverging_departure_map.jpg"), plot = diverging_departure_plot, width = 8, height = 6, units = "in")



####histograms
# Calculate median ranks of states for each metric
median_ranks <- df_joined %>%
  group_by(NAME) %>%
  summarise(across(c(emd_both_normalized, signed_emd_frequency_normalized, signed_emd_severity_normalized), median)) %>%
  mutate(across(c(emd_both_normalized, signed_emd_frequency_normalized, signed_emd_severity_normalized), rank, .names = "{.col}_rank")) %>%
  select(NAME, emd_both_normalized_rank, signed_emd_frequency_normalized_rank, signed_emd_severity_normalized_rank)

# Prepare data for boxplot
df_joined_boxplot <- df_joined %>%
 as.data.frame() %>%
  select(NAME, signed_emd_frequency_normalized, signed_emd_severity_normalized, emd_both_normalized) %>%
  left_join(median_ranks, by = "NAME") %>%
  drop_na()

# Boxplots for total departure
total_departure_boxplot <- df_joined_boxplot %>%
  mutate(NAME = factor(NAME, levels = arrange(median_ranks,desc( emd_both_normalized_rank))$NAME )) %>%
  ggplot(aes(x = NAME, y = emd_both_normalized)) +
  geom_boxplot() +
  labs(x = "", y = "Fire-Regime Departure") +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
       theme(axis.text.x = element_text(angle = 45, hjust = 1),
       panel.grid.major.x = element_blank(),
       panel.grid.minor.x = element_blank())

# Save the plot
ggsave(paste0(outpath,"/emd_boxplot.jpg"), plot = total_departure_boxplot, width = 8, height = 6, units = "in")

# Repeat the process for frequency departure and severity departure
frequency_departure_boxplot <- df_joined_boxplot %>%
  mutate(NAME = factor(NAME, levels = arrange(median_ranks,desc( signed_emd_frequency_normalized_rank))$NAME )) %>%
  ggplot(aes(x = NAME, y = signed_emd_frequency_normalized)) +
  geom_boxplot() +
  labs(x = "", y = "Frequency Departure") +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
       theme(axis.text.x = element_text(angle = 45, hjust = 1),
       panel.grid.major.x = element_blank(),
       panel.grid.minor.x = element_blank())

# Save the plot
ggsave(paste0(outpath,"/emd_frequency_boxplot.jpg"), plot = frequency_departure_boxplot, width = 8, height = 6, units = "in")

# Boxplots for severity departure
severity_departure_boxplot <- df_joined_boxplot %>%
  mutate(NAME = factor(NAME, levels = arrange(median_ranks,desc( signed_emd_severity_normalized_rank))$NAME )) %>%
  ggplot(aes(x = NAME, y = signed_emd_severity_normalized)) +
  geom_boxplot() +
  labs(x = "", y = "Severity Departure") +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
       theme(axis.text.x = element_text(angle = 45, hjust = 1),
       panel.grid.major.x = element_blank(),
       panel.grid.minor.x = element_blank())

# Save the plot
ggsave(paste0(outpath,"/emd_severity_boxplot.jpg"), plot = severity_departure_boxplot, width = 8, height = 6, units = "in")
# Combine boxplots
combined_boxplot <- ggarrange(frequency_departure_boxplot, severity_departure_boxplot, total_departure_boxplot, ncol = 2, nrow = 2)

# Save the combined plot
ggsave(paste0(outpath,"/emd_boxplots.jpg"), plot = combined_boxplot, width = 12, height = 8, units = "in")


##do the same for FRCC
# Calculate median ranks of states for each metric
median_ranks <- df_joined %>%
  group_by(NAME) %>%
  summarise(across(c(FRCC_reg_dep, FRCC_freq_dep, FRCC_sev_dep), median)) %>%
  mutate(across(c(FRCC_reg_dep, FRCC_freq_dep, FRCC_sev_dep), rank, .names = "{.col}_rank")) %>%
  select(NAME, FRCC_reg_dep_rank, FRCC_freq_dep_rank, FRCC_sev_dep_rank)

# Prepare data for boxplot
df_joined_boxplot <- df_joined %>%
 as.data.frame() %>%
  select(NAME, FRCC_freq_dep, FRCC_sev_dep, FRCC_reg_dep) %>%
  left_join(median_ranks, by = "NAME") %>%
  drop_na()

# Boxplots for total departure
frcc_departure_boxplot <- df_joined_boxplot %>%
  mutate(NAME = factor(NAME, levels = arrange(median_ranks,desc( FRCC_reg_dep_rank))$NAME )) %>%
  ggplot(aes(x = NAME, y = FRCC_reg_dep)) +
  geom_boxplot() +
  labs(x = "", y = "FRCC Departure") +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
       theme(axis.text.x = element_text(angle = 45, hjust = 1),
       panel.grid.major.x = element_blank(),
       panel.grid.minor.x = element_blank())

# Save the plot
ggsave(paste0(outpath,"/emd_boxplot.jpg"), plot = total_departure_boxplot, width = 8, height = 6, units = "in")

# Repeat the process for frequency departure and severity departure
frccfreq_departure_boxplot <- df_joined_boxplot %>%
  mutate(NAME = factor(NAME, levels = arrange(median_ranks,desc( FRCC_freq_dep_rank))$NAME )) %>%
  ggplot(aes(x = NAME, y = FRCC_freq_dep)) +
  geom_boxplot() +
  labs(x = "", y = "FRCC Frequency Departure") +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
       theme(axis.text.x = element_text(angle = 45, hjust = 1),
       panel.grid.major.x = element_blank(),
       panel.grid.minor.x = element_blank())

# Save the plot
ggsave(paste0(outpath,"/emd_frequency_boxplot.jpg"), plot = frequency_departure_boxplot, width = 8, height = 6, units = "in")

# Boxplots for severity departure
frccsev_departure_boxplot <- df_joined_boxplot %>%
  mutate(NAME = factor(NAME, levels = arrange(median_ranks,desc( FRCC_freq_dep_rank))$NAME )) %>%
  ggplot(aes(x = NAME, y = FRCC_sev_dep)) +
  geom_boxplot() +
  labs(x = "", y = "FRCC Severity Departure") +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
       theme(axis.text.x = element_text(angle = 45, hjust = 1),
       panel.grid.major.x = element_blank(),
       panel.grid.minor.x = element_blank())





####figure 5
figure5 <- ggarrange(fireRegimeDeparture_plot, NULL, total_departure_boxplot,
#frccDeparture_plot, NULL, frcc_departure_boxplot,
ncol = 3,  common.legend = FALSE,
widths = c(2.8,-0,1.2), labels = c("A", "", "B"#,
                                #"C", "", "D"
                                ))
ggsave(paste0(outpath,"/figure5.jpg"), figure5, width = 7, height = 6, units = "in")





####ridgeline plots
total_departure_ridge <- df_joined_boxplot %>%
       mutate(NAME = factor(NAME, levels = arrange(median_ranks, emd_both_normalized_rank)$NAME )) %>%
       ggplot(aes(x = emd_both_normalized, y = NAME, fill = after_stat(x))) +
       geom_density_ridges_gradient(scale = 3, rel_min_height = 0.005) +
       scale_fill_viridis(option = "C") +
       labs(x = "Fire-Regime Departure", y = "") +
       theme_bw()+
       theme(
                     legend.position="none",
                     panel.spacing = unit(0.1, "lines"),
                     strip.text.x = element_text(size = 8)
              )

# Save the plot
ggsave(paste0(outpath,"/emd_ridgeline.jpg"), plot = total_departure_ridge, width = 8, height = 6, units = "in")

# Ridge plot for frequency departure
frequency_departure_ridge <- df_joined_boxplot %>%
       mutate(NAME = factor(NAME, levels = arrange(median_ranks, signed_emd_frequency_normalized_rank)$NAME )) %>%
       ggplot(aes(x = signed_emd_frequency_normalized, y = NAME, fill = after_stat(x))) +
       geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005) +
       scale_fill_distiller(type = "div", palette = "RdBu",
       values =c(0,
        (0-min(df_joined_boxplot$signed_emd_frequency_normalized))/(max(df_joined_boxplot$signed_emd_frequency_normalized)-min(df_joined_boxplot$signed_emd_frequency_normalized)),
         1)) +
       labs(x = "Frequency Departure", y = "") +
       theme_bw() +
       theme(
                     legend.position="none",
                     panel.spacing = unit(0.1, "lines"),
                     strip.text.x = element_text(size = 8)
              )

# Save the plot
ggsave(paste0(outpath,"/emd_frequency_ridgeline.jpg"), plot = frequency_departure_ridge, width = 8, height = 6, units = "in")

# Ridge plot for severity departure
severity_departure_ridge <- df_joined_boxplot %>%
       mutate(NAME = factor(NAME, levels = arrange(median_ranks, signed_emd_severity_normalized_rank)$NAME )) %>%
       ggplot(aes(x = signed_emd_severity_normalized, y = NAME, fill = ..x..)) +
       geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005) +
       scale_fill_distiller(type = "div", palette = "RdBu",
       values =c(0,
        (0-min(df_joined_boxplot$signed_emd_severity_normalized))/(max(df_joined_boxplot$signed_emd_severity_normalized)-min(df_joined_boxplot$signed_emd_severity_normalized)),
         1)) +
       labs(x = "Severity Departure", y = "") +
       theme_bw() +
       theme(
                     legend.position="none",
                     panel.spacing = unit(0.1, "lines"),
                     strip.text.x = element_text(size = 8)
              )

# Save the plot
ggsave(paste0(outpath,"/emd_severity_ridgeline.jpg"), plot = severity_departure_ridge, width = 8, height = 6, units = "in")


################
################
################
#sensitivity
colors <- distinctColorPalette(30)

census <- as.data.frame(read_sf("data/outputs/med_grids/sensitivity/census.gpkg"))
n_iters <- readRDS("data/outputs/med_grids/sensitivity/med_hex_n_iters.rds")
p_areas <- readRDS("data/outputs/med_grids/sensitivity/med_hex_p_areas.rds")
run_sensitivity <- readRDS("data/outputs/med_grids/sensitivity/med_hex_run_sensitivity.rds")
miller_thode <- as.data.frame(read_sf("data/outputs/med_grids/sensitivity/miller_thode.gpkg"))




# Combine all data into one data frame for plotting
combine_dfs <- function(df_list, names) {
  map2_dfr(df_list, names, ~mutate(., dataset = .y))
}
combined_df <- bind_rows(
  mutate(df_joined, dataset = "df_joined"),
  mutate(census, dataset = "census"),
  mutate(miller_thode, dataset = "miller_thode"),
  combine_dfs(n_iters, paste0("n_iters_", seq_along(n_iters))),
  combine_dfs(p_areas, paste0("p_areas_", seq_along(p_areas))),
  combine_dfs(run_sensitivity, paste0("run_sensitivity_", seq_along(run_sensitivity)))

)

# Melt the data frame to long format for plotting
long_df <- pivot_longer(combined_df, vars, names_to = "variable", values_to = "value")%>%
  mutate(variable = case_when(
    variable == "emd_frequency_normalized" ~ "Frequency Departure",
    variable == "emd_severity_normalized" ~ "Severity Departure",
    variable == "emd_both_normalized" ~ "Fire-Regime Departure",
    TRUE ~ variable
  )) %>%
  pivot_longer(paste0(vars,"_var"), names_to = "variable_var", values_to = "value_var") %>%
  mutate(variable_var = case_when(
    variable_var == "emd_frequency_normalized_var" ~ "Frequency Departure Variance",
    variable_var == "emd_severity_normalized_var" ~ "Severity Departure Variance",
    variable_var == "emd_both_normalized_var" ~ "Fire-Regime Departure Variance",
    TRUE ~ variable_var
  ), value_var = sqrt(value_var))


# Boxplots
iters <-c("10","25","50","100","200")
areas <- as.character(c(0.001, 0.01, 0.1,0.5))

iter_names <- paste0("N iterations: ", iters)
area_names <- paste0("Proportion Area: ", areas)
run_names <- paste0("Repeated Run: ", seq_along(run_sensitivity))
dataset_names <- c("Census", "Original", "Miller & Thode", iter_names, area_names, run_names)

boxplots <- ggplot(long_df, aes(x = "", y = value, fill = dataset)) +
  facet_wrap(~ variable, scales = "free") +
  geom_boxplot(outlier.size = 0.25) +
  labs(y = "Value", x = "") +
  scale_fill_manual(values = colors,
    labels = dataset_names)+
  theme_bw() +
  theme(legend.title = element_blank(),
         legend.text = element_text(size = 10),
         legend.key.size = unit(0.5, "cm"))
boxplots

boxplots_var <- ggplot(long_df, aes(x = "", y = value_var, fill = dataset)) +
  facet_wrap(~ variable_var, scales = "free") +
  geom_boxplot(outlier.size = 0.25) +
  labs(y = "Standard Deviations", x = "") +
  scale_fill_manual(values = colors,
    labels = dataset_names)+
  theme_bw() +
  theme(legend.title = element_blank(),
         legend.text = element_text(size = 10),
         legend.key.size = unit(0.5, "cm"))
boxplots_var


# Density plot
dataset_names <- c("Census",  "Miller & Thode", iter_names, area_names, run_names)
density_plot <- ggplot(long_df, aes(x = value, fill = dataset)) +
       facet_wrap(~ variable, scales = "free") +
       geom_density(aes(y = after_stat(density)), alpha = 0.6) +  
       labs(x = "Bias", y = "Density") +
       scale_fill_manual(values = colors,
       labels = dataset_names) +
       theme_bw() +
       theme(legend.title = element_blank(),
                            legend.text = element_text(size = 10),
                            legend.key.size = unit(0.5, "cm"))
density_plot

# bias against original (df_joined)

# Function to calculate pairwise bias
calculate_pairwise_bias <- function(df1, df2, vars) {
       joined_df <- left_join(df1[, c(vars, "name")], df2[, c(vars, "name")], by = "name") %>%
       drop_na()
       pairwise_bias <- map(vars, ~ (joined_df[[paste0(., ".y")]] - joined_df[[paste0(., ".x")]])) 
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
  map(df_list, function(df2) {
    joined_df <- left_join(df1[, c(vars, "name")], df2[, c(vars, "name")], by = "name") %>%
    drop_na()
    pairwise_bias <- map(vars, ~ (joined_df[[paste0(., ".y")]] - joined_df[[paste0(., ".x")]])) 
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
       map_dfr(seq_along(n_iters_bias), ~mutate(n_iters_bias[[.x]], dataset = paste0("N iterations: ", .x))),
       map_dfr(seq_along(p_areas_bias), ~mutate(p_areas_bias[[.x]], dataset = paste0("Proportion Area: ", .x))),
       map_dfr(seq_along(run_sensitivity_bias), ~mutate(run_sensitivity_bias[[.x]], dataset = paste0("Repeated Run: ", .x)))
)

long_df_bias <- pivot_longer(bias_df, vars, names_to = "variable", values_to = "value") %>%
  mutate(variable = case_when(
    variable == "emd_frequency_normalized" ~ "Frequency Departure",
    variable == "emd_severity_normalized" ~ "Severity Departure",
    variable == "emd_both_normalized" ~ "Fire-Regime Departure",
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

#bias boxplots
bias_boxplots <- ggplot(long_df_bias, aes(x = "", y = value, fill = dataset)) +
  facet_wrap(~ dataset_class + variable, scales = "fixed",ncol = 3) +
  geom_boxplot(outlier.size = 0.25) +
  labs(y = "Bias", x = "") +
  scale_fill_manual(values = colors,
    labels = dataset_names)+
  theme_bw() +
  theme(legend.title = element_blank(),
         legend.text = element_text(size = 10),
         legend.key.size = unit(0.5, "cm"))
bias_boxplots

#bias  Density plot
dataset_names <- c("Census",  "Miller & Thode", iter_names, area_names, run_names)
bias_density_plot <- ggplot(long_df_bias, aes(x = value, fill = dataset)) +
        facet_wrap(~ dataset_class + variable, scales = "free_y",ncol = 3) +
       geom_density(aes(y = after_stat(density)), alpha = 0.6) +  
       labs(x = "Bias", y = "Density") +
       scale_fill_manual(values = colors,
       labels = dataset_names) +
       theme_bw() +
       theme(legend.title = element_blank(),
                            legend.text = element_text(size = 10),
                            legend.key.size = unit(0.5, "cm"))
bias_density_plot
# Save the bias plot
ggsave(filename = "bias_plot.svg", plot = bias_plot, width = 10, height = 8, units = "in")


# Calculate the mean bias for each dataset then convert to long format
mean_bias <- bias_df %>%
  filter(!dataset %in% c("Census", "Miller & Thode", paste0("Repeated Run: ", 1:10))) %>%
  group_by(dataset) %>%
  summarise(across(c(emd_frequency_normalized, emd_severity_normalized, emd_both_normalized), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = c(emd_frequency_normalized, emd_severity_normalized, emd_both_normalized), names_to = "variable", values_to = "mean") %>%
  mutate(variable = case_when(
    variable == "emd_frequency_normalized" ~ "Frequency Departure",
    variable == "emd_severity_normalized" ~ "Severity Departure",
    variable == "emd_both_normalized" ~ "Fire-Regime Departure",
    TRUE ~ variable
  )) %>%
  mutate(x = ifelse(grepl("N iterations", dataset), as.numeric(gsub("N iterations: ", "", dataset)), 
                     ifelse(grepl("Proportion Area", dataset), as.numeric(gsub("Proportion Area: ", "", dataset)), NA))) %>%
  mutate(x = ifelse(grepl("N iterations", dataset), as.numeric(iters[x]),
                     ifelse(grepl("Proportion Area", dataset), as.numeric(areas[x]), NA))) %>%
  mutate(dataset_class = ifelse(grepl("N iterations", dataset), "Number of Iterations",
                                ifelse(grepl("Proportion Area", dataset), "Proportion of area", NA)))
  

#calculate the standard error and 95% confidence interval for the mean bias in each dataset. the final 
#data frame is converted to long format for plotting, and contains the mean bias, and the upper and lower
#bounds of the 95% confidence interval
sd_bias <- bias_df  %>%
filter(!dataset %in% c("Census", "Miller & Thode", paste0("Repeated Run: ", 1:10))) %>%
group_by(dataset) %>%
summarise(across(c(emd_frequency_normalized, emd_severity_normalized, emd_both_normalized), sd, na.rm = TRUE)) %>%
pivot_longer(cols = c(emd_frequency_normalized, emd_severity_normalized, emd_both_normalized), names_to = "variable", values_to = "sd") %>%
mutate(variable = case_when(
variable == "emd_frequency_normalized" ~ "Frequency Departure",
variable == "emd_severity_normalized" ~ "Severity Departure",
variable == "emd_both_normalized" ~ "Fire-Regime Departure",
TRUE ~ variable
)) %>% 
right_join(mean_bias, by = c("dataset", "variable"))

n_bias <- bias_df  %>%
filter(!dataset %in% c("Census", "Miller & Thode", paste0("Repeated Run: ", 1:10))) %>%
group_by(dataset) %>%
summarise(across(c(emd_frequency_normalized, emd_severity_normalized, emd_both_normalized), ~n())) %>%
pivot_longer(cols = c(emd_frequency_normalized, emd_severity_normalized, emd_both_normalized), names_to = "variable", values_to = "n") %>%
mutate(variable = case_when(
variable == "emd_frequency_normalized" ~ "Frequency Departure",
variable == "emd_severity_normalized" ~ "Severity Departure",
variable == "emd_both_normalized" ~ "Fire-Regime Departure",
TRUE ~ variable
)) %>%
right_join(sd_bias, by = c("dataset", "variable"))

se_bias <- n_bias %>%
mutate(se = sd/sqrt(n)) %>%
mutate(upper = mean + 1.96*se,
       lower = mean - 1.96*se) %>%
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "None")
mean_bias_plot




