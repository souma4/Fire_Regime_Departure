#pkgs <- c("tidyverse")
pkgs <- c("data.table")
invisible(lapply(pkgs, library, character.only = T))

source("scripts\\modules\\simulation_functions.R")
#This function creates frequency distributions
create_frequency_distributions <- function(contemporary_sample_freq, #this iterations extraced contemporary frequencies
                                           historical_sample #this iterations sampled historical data
                                           ){

 
#create simulated fire frequencies for historical data
  historical_sample_freq <- historical_sample[, freq := fire_freq_binom(FRI_ALLFIR)]
  #historical_sample_freq <<- historical_sample %>%
  #mutate(freq = fire_freq_binom(FRI_ALLFIR))%>%
  #ungroup()



#clean data so that it's only freq and where it comes from
  
  contemporary_freq <- contemporary_sample_freq[,.(freq),][,time := "Contemporary"]
  historical_freq <- historical_sample_freq[,.(freq),][,time := "Historical"]
  
  
  #contemporary_freq <<- contemporary_sample_freq %>% dplyr::select(freq)%>%mutate(time = "Contemporary")
#historical_freq <<- historical_sample_freq %>% dplyr::select(freq) %>% mutate(time = "Historical")

#bind the two datasets then convert to relative frequency distribtions

freq_dat <- rbind(contemporary_freq, historical_freq)[,time := fct_relevel(time,c("Historical","Contemporary"))
                                                       ][,.N, .(time, freq)
                                                         ][
                                                          ][,relfreq := N/sum(N), by = time] 

#freq_dat <<- rbind(contemporary_freq, historical_freq)%>%
#  mutate(time = fct_relevel(time,c("Historical","Contemporary"))) %>%
#  group_by(time, freq) %>%
#  summarise(n = n())%>%
#  mutate(relfreq = n/sum(n))
output <- list(contemporary_freq = contemporary_freq,
               historical_freq = historical_freq,
               freq_dat = freq_dat)
return(output)


}

#this creates normalized frequency values from raw frequencies, freq_dat_norm asks whether you want the combined fire frequencies
normalize_frequency <- function(historical_freq, contemporary_freq, freq_dat_norm = T){
  hist_mean <- mean(historical_freq$freq, na.rm = T)
  
    hist_sd <- sd(historical_freq$freq, na.rm = T)
  if(is.na(hist_sd)){
    return("Error: no historical variance")
  }
    contemporary_freq_norm <<- copy(contemporary_freq)[,freq := (freq-hist_mean)/hist_sd]
    historical_freq_norm <<- copy(historical_freq)[, freq := (freq-hist_mean)/hist_sd]
    if (freq_dat_norm == T){
      freq_dat <<- freq_dat[,freq := (freq-hist_mean)/hist_sd]
      
    }
  
    
  #   contemporary_freq_norm <<- contemporary_freq %>% 
  #   mutate(freq = (freq-hist_mean)/hist_sd)
  # historical_freq_norm <<- historical_freq %>% 
  #   mutate(freq = (freq-hist_mean)/hist_sd)
  # if (freq_dat_norm == T){
  # freq_dat <<- freq_dat %>% 
  #   mutate(freq = (freq-hist_mean)/hist_sd)
  # 
  # }
  
  return("Passed")
}
