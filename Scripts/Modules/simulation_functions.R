low.thresh = 1.55
high.thresh = 1.98
year_caps <- c(1985,2020)
year_range <- year_caps[2]-year_caps[1]
class_select <- function(surface =0, mixed = 0, high = 0,x = floor(runif(length(surface), min = 0, max = 99)), classify = F){
  if(classify == F){
  surface[is.na(surface)] <- 0
  mixed[is.na(mixed)] <- 0
  high[is.na(high)] <- 0
  return(ifelse(x<surface, "Low",
         ifelse(x<surface+mixed, "Mixed",
                ifelse(x<100, "High", NA))))
 
  }else{
    return(ifelse(x<low.thresh, "Low",
            ifelse( x<high.thresh, "Mixed",
                  ifelse(x>high.thresh, "High",NA))))
  }
}
#Fire_freq <- function(class,fri_low,fri_mix,fri_high){
#  switch(class,
#         "low" = year_range/fri_low,
#         "mixed" = year_range/fri_mix,
#         "high" = year_range / fri_high,
#         0)
#}
Fire_sev <- function(class, low.thresh, high.thresh) {
  case_when(
    class == "Low" ~ runif(length(class), 0.00, low.thresh),
    class == "Mixed" ~ runif(length(class), low.thresh, high.thresh),
    class == "High" ~ runif(length(class), high.thresh, 3.00),
    TRUE ~ 0
  )
}
fire_freq_binom <- function(FRI){
  sapply(FRI, function(fri) rbinom(1,year_range,1/fri))
}

