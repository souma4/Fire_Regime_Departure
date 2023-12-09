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
  contemporary_sev_full <- melt(contemporary_sample_sev,id.vars = "ID",
                                variable.name = "year",
                                value.name = "contemp_sev")[, ifelse(.SD < 0, NA, .SD)] |>
                                na.omit(contemporary_sev_full)

  contemporary_sev_full <- contemporary_sample_sev %>%  
  pivot_longer(-c(ID), names_to = "year",values_to = "contemp_sev") %>%
  mutate(across(contemp_sev, ~ifelse(.<0, NA, .))) %>%
  drop_na() 

#cleans contemporary severity data
  contemporary_sev <<- contemporary_sev_full[,.(contemp_sev)][,list(sev := .(contemp_sev), time := "contemporary")]
  
  
  contemporary_sev <<- contemporary_sev_full %>% 
  dplyr::select(contemp_sev) %>% 
  rename(sev = contemp_sev) %>%
  mutate(time = "Contemporary")
  

#converts contemporary severity data to classes based on classification thresholds
  contemporary_sev_class <<- contemporary_sev[,sev := as.factor(class_select(x  = .(sev), classify = T))][
    ,sev := fct_relevel(sev, "Low", "Mixed", "high")
  ]
    
  contemporary_sev_class <<- contemporary_sev %>%
mutate(sev = as_factor(class_select(x = sev, classify = T))) %>%
  mutate(sev = fct_relevel(sev,"Low","Mixed","High")) 



if (contemporary4historical == T){
  #joins the BPS to each contemporary fire, then simulates fire severity
  historical_sev_full <- contemporary_sev_full[historical_sample, on = "ID"][
    ,list(class := class_select(PRC_SURFAC, PRC+MIXED, PRC_REPLACE),
          hist_sev = Fire_sev(class, low.thresh_upper_bound, low.thresh_low_bound, high.thresh_upper_bound, high.thresh_low_bound),
          contemp_class = class_select(x = contemp_sev, classify = T))
    
  ]
  
  
  historical_sev<<-  historical_sev_full[,hist_sev][,`:=`(sev = hist_sev, hist_sev = NULL, time = "Historical")]
  
  
  
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
  #TODO
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
  combined_sev <<- rbind(contemporary_sev, historical_sev)[,time := fct_relevel(time,c("Historical","Contemporary"))]
  
  
  historical_sev_class<<-  historical_sev_full[,class][,`:=`(sev = class,
                                                            class = NULL,
                                                            sev = as.factor(sev),
                                                            sev = fct_relevel(sev, "Low", "Mixed", "High"),
                                                            time = "Historical")]
  

  combined_sev_class <<- rbind(contemporary_sev_class, historical_sev_class)[,n = .N, by = .(time, sev)][, .(relfreq := n/sum(n),
                                                                                                            time := fct_relevel(time, c("Historical", "Contemporary")))]
    

  
  
  
  combined_sev <<- rbind(contemporary_sev, historical_sev)[,time := fct_relevel(time, c("Historical", "Contemporary"))]

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
    
    contemporary_sev_norm<<- contemporary_sev[,sev := (sev-hist_mean)/hist_sd]
    historical_sev_norm <<- historical_sev[,sev := (sev-hist_mean)/hist_sd]
    if(combined_sev_norm == T){
      combined_sev <<- combined_sev[,sev := (sev-hist_mean)/hist_sd]
    }
    return("Passed")
    
    
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