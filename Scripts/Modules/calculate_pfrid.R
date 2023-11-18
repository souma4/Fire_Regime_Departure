#Simply calculates PFRID (Safford & Van de Water, 2014)
#inputs are MFRI's for the landscape
calculate_pfrid <- function(contemporary_fri, historic_fri){
  pfrid <- ifelse(contemporary_fri > historic_fri,
                  (1-(historic_fri/contemporary_fri))*100,
                    ifelse((contemporary_fri < historic_fri),
                    (-1*(1-(contemporary_fri/historic_fri)))*100,
                      0))
  return(pfrid)
}
calculate_percent_severity_departure <- function(contemporary_pbhs, historic_pbhs){
  percent_severity_departure <- ifelse(contemporary_pbhs > historic_pbhs,
                  (1-(historic_pbhs/contemporary_pbhs))*100,
                  ifelse((contemporary_pbhs < historic_pbhs),
                         (-1*(1-(contemporary_pbhs/historic_pbhs)))*100,
                         0))
  return(percent_severity_departure)
}
