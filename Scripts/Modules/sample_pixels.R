
pkgs <- c("sampling","tidyverse")
invisible(lapply(pkgs, library, character.only = T))

sample_pixels <- function(iteration = 1,
                          contemporary_data_sev,
                          contemporary_data_freq,
                          historical_full_data){
srs_cells <- srs[,iteration]
#srs_locs <- frame_domain %>%
#  filter(cell %in% srs_cells) %>%
#  select(all_of(c("x","y")))
contemporary_sample_freq <<- contemporary_data_freq %>%
  filter(ID %in% srs_cells) %>%
  dplyr::select(-one_of("x","y"))

contemporary_sample_sev <<- contemporary_data_sev %>%
  filter(ID %in% srs_cells) %>%
  dplyr::select(-one_of("x","y"))

historical_sample <<- historical_full_data%>%
  filter(ID %in% srs_cells)
}