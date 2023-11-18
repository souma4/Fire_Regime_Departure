pkgs <- c("tidyverse")
invisible(lapply(pkgs, library, character.only = T))

source("Scripts\\Modules\\simulation_functions.R")
# create severity distributions
create_severity_distributions <- function(historical_sample, #raw historical sample from sample_pixels.R
                                          historical_sample_freq, #Historical sampled frequencies from create_frequency_distributions.R
                                          contemporary_sample_sev, #contemporary sampled severities from sample_pixels.R
                                          contemporary4historical = F,#whether to calculate historical severity using contemporary fire incidence, defaults to false because this is interesting but not for our study
                                          k #the iteration we are on
                                          ){
  #converts contemporary to data frame of severities
contemporary_sev_full <- contemporary_sample_sev %>%  
  pivot_longer(-c(ID), names_to = "year",values_to = "contemp_sev") %>%
  mutate(across(contemp_sev, ~ifelse(.<0, NA, .))) %>%
  drop_na() 

#cleans contemporary severity data
contemporary_sev <<- contemporary_sev_full %>% 
  dplyr::select(contemp_sev) %>% 
  rename(sev = contemp_sev) %>%
  mutate(time = "Contemporary")
  

#converts contemporary severity data to classes based on classification thresholds
contemporary_sev_class <<- contemporary_sev %>%
mutate(sev = as_factor(class_select(x = sev, classify = T))) %>%
  mutate(sev = fct_relevel(sev,"Low","Mixed","High")) 



if (contemporary4historical == T){
  #joins the BPS to each contemporary fire, then simulates fire severity
historical_sev_full <- contemporary_sev_full %>%
  left_join(historical_sample, by = "ID") %>%
  mutate(class = class_select(PRC_SURFAC, PRC_MIXED, PRC_REPLAC)) %>%
  mutate(hist_sev = Fire_sev(class, low.thresh_upper_bound,low.thresh_low_bound, high.thresh_upper_bound, high.thresh_low_bound)) %>% 
  mutate(contemp_class = class_select(x = contemp_sev, classify = T))%>%
  ungroup()



historical_sev<<-  historical_sev_full %>%
  dplyr::select(hist_sev) %>% 
  rename(sev = hist_sev) %>%
  mutate(time = "Historical")



} else{
  #calculates an estimated fire severity for each estimated fire in the historical record
historical_sev_full <- historical_sample_freq %>%
  uncount(., freq) %>% 
  mutate(class = class_select(PRC_SURFAC, PRC_MIXED, PRC_REPLAC)) %>%
  mutate(hist_sev = Fire_sev(class, low.thresh_upper_bound,low.thresh_low_bound, high.thresh_upper_bound, high.thresh_low_bound)) %>%
  ungroup() 
historical_sev <<- historical_sev_full %>%
  dplyr::select(hist_sev) %>%
  rename(sev = hist_sev) %>%
  mutate(time = "Historical")


name.sev <- paste0("sev.",k)
name.time <-paste0("time.",k)


}
#combined historical and contemporary
combined_sev <<- rbind(contemporary_sev, historical_sev) %>%
  mutate( time = fct_relevel(time,c("Historical","Contemporary")))


historical_sev_class<<-  historical_sev_full %>%
  dplyr::select(class) %>% 
  rename(sev = class) %>%
  mutate(sev = as_factor(sev)) %>%
  mutate(sev = fct_relevel(sev,"Low","Mixed","High")) %>%
  mutate(time = "Historical")
combined_sev_class <<- rbind(contemporary_sev_class, historical_sev_class)%>%

  group_by(time, sev) %>%
  summarise(n = n())%>%
  mutate(relfreq = n/sum(n), time = fct(time,c("Historical","Contemporary"))) 

}

#Creates normalized fire severity distributions from raw fire severity data, combined sev_norm asks whether you want the combined_severities
normalize_severity <- function(historical_sev, contemporary_sev, combined_sev_norm = T){
    hist_mean <- mean(historical_sev$sev, na.rm = T)
    hist_sd <- sd(historical_sev$sev, na.rm = T)
    if(is.na(hist_sd)){
      return("Error: no historical variance")
    }
    contemporary_sev_norm<<- contemporary_sev %>% 
      mutate(sev = (sev-hist_mean)/hist_sd)
    historical_sev_norm <<- historical_sev %>% 
      mutate(sev = (sev-hist_mean)/hist_sd)
    if(combined_sev_norm == T){
    combined_sev <<- combined_sev%>% 
      mutate(sev = (sev-hist_mean)/hist_sd)
    }
    return("Passed")
}