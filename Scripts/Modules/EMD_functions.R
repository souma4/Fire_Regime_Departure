pkgs <- c("tidyverse","transport")
invisible(lapply(pkgs, library, character.only = T))





create_emd_hungarian_algo  <- function(contemporary_dat,historical_dat, freq , bin_vec){
  
  
  if(freq == T){
    bins <- seq(0,bin_vec, 1)
    dx <- bins[1]-bins[0]
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
  bins = seq(0,3, by = .1)
  dx <- .1
  fi <- contemporary_dat %>% 
    mutate(bin = cut(sev, breaks = bins, labels = F, include.lowest = T)*dx) %>%
    group_by(bin) %>%
    summarise(n = n()) %>%
    mutate( rel = n/sum(n))%>% 
    right_join(data.frame(bin = bins[-1]),by = "bin") %>%
    dplyr::select(bin, rel) %>%
    mutate(bin = ifelse(is.na(bin),0,bin))
  
  mi <- historical_dat %>% 
    mutate(bin = cut(sev, breaks = bins, labels = F)*dx) %>%
    group_by(bin) %>%
    summarise(n = n()) %>%
    mutate( rel = n/sum(n)) %>%
    right_join(data.frame(bin = bins[-1]),by = "bin")%>%
    dplyr::select(bin, rel)%>%
      mutate(bin = ifelse(is.na(bin),0,bin))
  }
  
  
  
  emd_df <- full_join(fi,mi, by = "bin", suffix = c(".fi",".mi"))
  emd_df[is.na(emd_df)] <- 0
  
  emd <- wasserstein1d(emd_df$bin, emd_df$bin, 1, emd_df$rel.fi, emd_df$rel.mi)
  
  return(emd)
}