#creates simulation functions

#calculated thresholds from calculated_thresholds.R I input these manually to save overhead cost
low.thresh_low_bound = 1.33
low.thresh_median = 1.56
low.thresh_upper_bound = 1.72

high.thresh_low_bound = 1.92
high.thresh_median = 2.01
high.thresh_upper_bound = 2.17


# Comment these out for regular analysis
#this is a sensitivity test based on thresholds from miller and thode 2007
# low.thresh_low_bound  = 1.24
# low.thresh_median = 1.24
# low.thresh_upper_bound = 1.24
# 
# high.thresh_low_bound = 2.25
# high.thresh_median = 2.25
# high.thresh_upper_bound = 2.25


#years we want to investigate
year_caps <- c(1985,2020)
#the range of these years
year_range <- year_caps[2]-year_caps[1]

#Function performs two functions (i know, it was a mistake) Both are classifying fire severity
#if classify = F (default), x should be left default
#if classify = T, x should be the fire severity of the location
class_select <- function(surface =0, mixed = 0, high = 0, #Values should be set to the percent of the respective severity class from BPS
                         x = floor(runif(1, min = 0, max = 99)),  #Generate random number
                         classify = F){
  if(classify == F){
  #na's get classified to 0
  surface[is.na(surface)] <- 0
  mixed[is.na(mixed)] <- 0
  high[is.na(high)] <- 0
  #return a fire severity based on the random number draw
  return(ifelse(x<surface, "Low",
         ifelse(x<surface+mixed, "Mixed",
                ifelse(x<100, "High", NA))))
 
  }else{
    #if classify == T, simply classify the severity using our thresholds
    return(ifelse(x<low.thresh_median, "Low",
            ifelse( x<high.thresh_median, "Mixed",
                  ifelse(x>high.thresh_median, "High",NA))))
  }
}

#inputs a fire severity class and our thresholds. Given a severity class, draws from a uniform distribution between the respective bounds
Fire_sev <- function(class, low.thresh_upper_bound,low.thresh_low_bound, high.thresh_upper_bound, high.thresh_low_bound) {
  case_when(
    class == "Low" ~ runif(length(class), 0.00, low.thresh_upper_bound),
    class == "Mixed" ~ runif(length(class), low.thresh_low_bound, high.thresh_upper_bound),
    class == "High" ~ runif(length(class), high.thresh_low_bound, 3.00),
    TRUE ~ 0
  )
}
#inputs an FRI, outputs an estimate fire frequency following a binomial distribution
fire_freq_binom <- function(FRI){
  sapply(FRI, function(fri) rbinom(1,year_range,1/fri))
}

