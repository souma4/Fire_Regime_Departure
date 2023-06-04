calculate_pfrid <- function(contemporary_fri, historic_fri){
  pfrid <- ifelse(contemporary_fri > historic_fri,
                  (1-(historic_fri/contemporary_fri))*100,
                    ifelse((contemporary_fri < historic_fri),
                    (-(1-(historic_fri/contemporary_fri))*100),
                      NA))
  return(pfrid)
}
