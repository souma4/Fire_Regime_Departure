#This script takes our filtered out and extracted points, and pulls out the samples within for a single iteration run
#pkgs <- c("sampling","tidyverse")
pkgs <- c("sampling","data.table")
invisible(lapply(pkgs, library, character.only = T))

sample_pixels <- function(iteration = 1, #Number of the iteration. This indexes columns of srs created in sampling_scheme.R, defaults to one if only sampling once
                          contemporary_data_sev, #Our filtered dataframe of contemporary severities
                          contemporary_data_freq, #Our filtered dataframe of contemporary frequencies
                          historical_data, #our filtered historical data
                          srs #the cell numbers generated in srs from sampling_scheme.R
                          ){
srs_cells <- srs[,iteration] #grab the iteration of srs

#Filter to the valid cells for this iteration among all dataframes
contemporary_sample_freq <- copy(contemporary_data_freq)[ID %in% srs_cells, !c("x","y")]
#contemporary_sample_freq <<- contemporary_data_freq %>%
#  filter(ID %in% srs_cells) %>%
#  dplyr::select(-one_of("x","y"))
contemporary_sample_sev <- copy(contemporary_data_sev)[ID %in% srs_cells, !c("x","y")]
#contemporary_sample_sev <<- contemporary_data_sev %>%
#  filter(ID %in% srs_cells) %>%
#  dplyr::select(-one_of("x","y")) 

historical_sample <- copy(historical_data)[ID %in% srs_cells]
# historical_sample <<- historical_data%>%
#   filter(ID %in% srs_cells)
output <- list(contemporary_sample_freq = contemporary_sample_freq,
               contemporary_sample_sev = contemporary_sample_sev,
               historical_sample = historical_sample)
return(output)
}