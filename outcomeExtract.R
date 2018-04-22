outcomeExtract <- function(outcomeData){
  
  # Turns raw outcome data into objects that can be analyzed by Stan
  
  list2env(outcomeData, environment())
  
  # Local names for easier manipulation
  # name - root name: variable_period_"cat"n_cat 
  # mName - measurement name: "M"_name
  # num - number of measurements
  
  name = paste("outcome_", "yr", year, "_cat", n_cat,  sep = "")

  num = 
    length(
      grep(
        paste("^", name, "_*", sep = ""),
        names(data_raw)
      )
    )
  
  I_num = NULL  # vector containing number of non-missing outcomes
  I_ind = NULL  # indicies of non-missing outcomes (concatenated into a single vector)
  outcome_ = rep(NA,sum(I_num)) # outcomes (concatenated into a single vector)
  
  for (m in 1:num){
    I_ = !is.na(data_raw[,paste(name, m, sep = "_")]) 
    I_num = c(I_num, sum(I_))
    I_ind = c(I_ind, which(I_))
    rm(I_, m)
  }
  
  pos = 1 # keeps track of position in index vector
  
  for (m in 1:num){
    outcome_[pos:(pos + I_num[m] - 1)] <- data_raw[I_ind[pos:(pos + I_num[m] - 1)],paste(name, m, sep = "_")]
    pos = pos + I_num[m]
    rm(m)
  }
  
  rm(pos)
  
  # assign to global environment
  
  assign(paste(name, "num", sep = "_"), num, envir = globalenv())
  assign(paste("I", name, "num", sep = "_"), I_num, envir = globalenv())
  assign(paste("I", name, "ind", sep = "_"), I_ind, envir = globalenv())
  assign(name, outcome_, envir = globalenv())
  
}
