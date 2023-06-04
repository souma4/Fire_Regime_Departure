pkgs <- c("tidyverse")
invisible(lapply(pkgs, library, character.only = T))

source("Scripts\\Modules\\simulation_functions.R")
create_severity_distributions <- function(historical_sample,
                                          historical_sample_freq,
                                          contemporary_sample_sev,
                                          contemporary4historical = F,
                                          k){
contemporary_sev_full <- contemporary_sample_sev %>%  
  pivot_longer(-c(ID), names_to = "year",values_to = "contemp_sev") %>%
  drop_na() 


contemporary_sev <<- contemporary_sev_full %>% 
  dplyr::select(contemp_sev) %>% 
  rename(sev = contemp_sev) %>%
  mutate(time = "Contemporary")

contemporary_sev_class <<- contemporary_sev %>%
mutate(sev = as_factor(class_select(x = sev, classify = T))) %>%
  mutate(sev = fct_relevel(sev,"Low","Mixed","High")) 



if (contemporary4historical == T){
historical_sev_full <- contemporary_sev_full %>%
  left_join(historical_sample, by = "ID") %>%
  mutate(class = class_select(PRC_SURFAC, PRC_MIXED, PRC_REPLAC)) %>%
  mutate(hist_sev = Fire_sev(class, low.thresh, high.thresh)) %>% 
  mutate(contemp_class = class_select(x = contemp_sev, classify = T))%>%
  ungroup()



historical_sev<<-  historical_sev_full %>%
  dplyr::select(hist_sev) %>% 
  rename(sev = hist_sev) %>%
  mutate(time = "Historical")



} else{
historical_sev_full <- historical_sample_freq %>%
  uncount(., freq) %>% 
  mutate(class = class_select(PRC_SURFAC, PRC_MIXED, PRC_REPLAC)) %>%
  mutate(hist_sev = Fire_sev(class, low.thresh, high.thresh)) %>%
  ungroup() 
historical_sev <<- historical_sev_full %>%
  dplyr::select(hist_sev) %>%
  rename(sev = hist_sev) %>%
  mutate(time = "Historical")


name.sev <- paste0("sev.",k)
name.time <-paste0("time.",k)


}
combined_sev <<- rbind(contemporary_sev, historical_sev) %>%
  mutate( time = fct_relevel(time,c("Historical","Contemporary")))

#if (contemporary4historical == F){ 
#  combined_sev <<- combined_sev %>%
#  rename(!!name.sev := sev, !!name.time := time)
#}

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