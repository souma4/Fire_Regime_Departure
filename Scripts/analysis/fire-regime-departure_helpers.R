check_data <- function(data){
  out <- vector(length = length(data))
  for(i in 1:length(data)){
    check <- try(data[[i]][[1]][[1,2]], silent = T)
    
    
    out[i] <- ifelse(inherits(check, "try-error"), "invalid", check)
  }
  valid_indices <- which(out != "invalid")
  
  
  stored_data_valid <- data[valid_indices]
  return(stored_data_valid)
}
production_pulling <- function(stored_data_valid){
  
  len <- length(stored_data_valid)
  new_cols <- stored_data_valid[[1]][[1]][,1]
  new_cols <- gsub(" ","_", new_cols)
  full_df <- data.frame(index = 1:len)
  full_df[,new_cols] <- NA
  for(i in 1:len){
    interdat <- stored_data_valid[[i]][[1]][,2]
    full_df[i, 2] <- interdat[1]
    full_df[i,3:(length(new_cols)+1)] <- as.numeric(interdat[-1])
  }
  return(full_df)
  
}