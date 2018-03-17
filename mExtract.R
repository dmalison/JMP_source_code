mExtract <- function(mData){

  # Turns raw vector of measurements into objects that can be analyzed by Stan
  
  # variable: "anchor", "R", "N", or "C"
  # period (survey wave): 0,1,2,3,4
  # n_cat (number of measurement categories): 3 or 5
  
  for (i in names(mData)){
    assign(i, mData[[i]])
  }
  
  # Local names for easier manipulation
    # name - root name: variable_period_"cat"n_cat 
    # mName - measurement name: "M"_name
    # num - number of measurements
  
  if (variable == "C"){
    name = paste(variable, period, sep = "_")
  } else if (variable == "R" | variable == "N"){
    name = paste(variable, period, paste("cat", n_cat, sep = ""), sep = "_")
  } 
  
  mName = paste("M", name, sep = "_")
  
  num = 
    length(
      grep(
        paste("^", mName, "_*", sep = ""),
        names(data_raw)
      )
    )
  
  I_num = NULL  # vector containing number of non-missing measurements
  I_ind = NULL  # indicies of non-missing measurements (concatenated into a single vector)
  M_ = rep(NA,sum(I_num)) # measurements (concatenated into a single vector)
  
  for (m in 1:num){
    I_ = !is.na(data_raw[,paste(mName, m, sep = "_")]) 
    I_num = c(I_num, sum(I_))
    I_ind = c(I_ind, which(I_))
    rm(I_, m)
  }
  
  pos = 1 # keeps track of position in index vector
  
  for (m in 1:num){
    
    M_[pos:(pos + I_num[m] - 1)] <- data_raw[I_ind[pos:(pos + I_num[m] - 1)],paste(mName, m, sep = "_")]
    pos = pos + I_num[m]
    rm(m)
  }
  
  rm(pos)
  
  # make assignments in global environment
  
  assign(paste(name, "num", sep = "_"), num, envir = globalenv())
  assign(paste("I", name, "num", sep = "_"), I_num, envir = globalenv())
  assign(paste("I", name, "ind", sep = "_"), I_ind, envir = globalenv())
  assign(paste("M", name, sep = "_"), M_, envir = globalenv())
  
}
