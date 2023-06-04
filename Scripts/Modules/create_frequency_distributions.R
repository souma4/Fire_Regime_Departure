pkgs <- c("tidyverse")
invisible(lapply(pkgs, library, character.only = T))

source("scripts\\modules\\simulation_functions.R")

create_frequency_distributions <- function(contemporary_sample_freq,
                                           historical_sample){




#freq
historical_sample_bps <- historical_sample %>%
  group_by(BPS_CODE) %>%
  summarise(n = n()) %>%
  mutate(relfreq = n/sum(n))

#  class_dat <- historical_sample %>%
#    rowwise %>%
#    mutate(class = class_select(PRC_SURFAC, PRC_MIXED, PRC_REPLAC)) %>%
#    mutate(freq = Fire_freq(class, FRI_SURFAC, FRI_MIXED, FRI_REPLAC)) %>% ###tweak this to meet your year range.
#    mutate(sev = Fire_sev(class)) %>%
#    ungroup()

historical_sample_freq <<- historical_sample %>%
  mutate(freq = fire_freq_binom(FRI_ALLFIR))%>%
  ungroup()




contemporary_freq <<- contemporary_sample_freq %>% dplyr::select(freq)%>%mutate(time = "Contemporary")
historical_freq <<- historical_sample_freq %>% dplyr::select( freq) %>% mutate(time = "Historical")
freq_dat <<- rbind(contemporary_freq, historical_freq)%>%
  mutate(time = fct_relevel(time,c("Historical","Contemporary"))) %>%
  group_by(time, freq) %>%
  summarise(n = n())%>%
  mutate(relfreq = n/sum(n))

}