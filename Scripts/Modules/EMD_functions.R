pkgs <- c("tidyverse","transport")
invisible(lapply(pkgs, library, character.only = T))




#Calculates emd from raw contemporary and raw historical data. freq if TRUE calculates for frequency, if FALSE calculates for severity
emd_Calculation  <- function(contemporary_dat,historical_dat, freq ){
  
  
  if(freq == T){
    #creates bins from unique fire frequencies
    unique_values <- c(contemporary_dat$freq,historical_dat$freq) %>%
                      unique() %>%
                      sort()
    
    dx <- unique_values[2]-unique_values[1] 
    bins <- seq(min(unique_values),max(unique_values), dx)
    #creates relative frequency distributions
    fi <- contemporary_dat %>% 
      group_by(freq) %>%
      summarise(n = n()) %>%
      mutate(rel = n/sum(n)) %>%
      dplyr::select(freq,rel) %>%
      rename(bin = freq)
   
    mi <- historical_dat %>%
      group_by(freq) %>%
      summarise(n=n()) %>%
      mutate(rel = n/sum(n)) %>%
      dplyr::select(freq, rel) %>%
      rename(bin = freq)
    
    
  }else{
  #if severity is being calculated
    #creates 16 bins to group fire severities. This is verbose because normalized severity changes the scale in highly variable and continuous ways
  bins = sort(unique(seq(min(round(c(contemporary_dat$sev,historical_dat$sev),2)),max(round(c(contemporary_dat$sev,historical_dat$sev),2)), length = 16)))
  bin_df <- data.frame(bin = bins[-1])%>%
    mutate( index = row_number())
  dx <- bins[2]-bins[1]
  #relative frequencies distributions for each fire severity bin
  fi <- contemporary_dat %>% 
    mutate(bin = cut(sev, breaks = bins, labels = F, include.lowest = T)) %>%
    group_by(bin) %>%
    summarise(n = n()) %>%
    mutate( rel = n/sum(n))%>% 
    right_join(bin_df, by = join_by("bin" == "index")) %>%
    dplyr::select(bin.y, rel) %>%
    rename(bin = bin.y)%>%
    mutate(bin = ifelse(is.na(bin),0,bin))
  
  mi <- historical_dat %>% 
    mutate(bin = cut(sev, breaks = bins, labels = F)) %>%
    group_by(bin) %>%
    summarise(n = n()) %>%
    mutate( rel = n/sum(n)) %>%
    right_join(bin_df, by = join_by("bin" == "index")) %>%
    dplyr::select(bin.y, rel) %>%
    rename(bin = bin.y)%>%
    mutate(bin = ifelse(is.na(bin),0,bin))
  
  }
  
  
  #join the datasets, input 0 for NA
  emd_df <- full_join(fi,mi, by = "bin", suffix = c(".fi",".mi"))
  emd_df[is.na(emd_df)] <- 0
  #Calculate EMD using transport::
  emd <- wasserstein1d(emd_df$bin, emd_df$bin, 1, emd_df$rel.fi, emd_df$rel.mi)
  
  return(emd)
}
