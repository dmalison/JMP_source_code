mSim <- function(mData){
  
  # Simulates relationship quality, non-cognitive, and cognitive measurements
  
  for (i in names(mData)){
    assign(i, mData[[i]])
  }
  
  # Local names for easier manipulation
    # name - root name: variable_period_"cat"n_cat 
    # mName - measurement name: "M"_name
    # num - number of measurements
  
  if (variable == "C"){
    name <- paste(paste(variable, period, sep = "_"), sep = "")
  } else {
    name <- paste(paste(variable, period, sep = "_"), "_cat", n_cat, sep = "")
  }
  
  num <- stan_data[[paste(name, "_num", sep = "")]]
  
  gamma_M <- get(paste("gamma_","M_",  name, sep = ""))  # factor loadings
  
  if (variable == "C"){
    
    mu_M <- get(paste("mu_","M_",  name, sep = "")) # measurement mean
    sigma_M <- get(paste("sigma_","M_",  name, sep = "")) # measurement std dev
    
  } else {
    
    c_M <- get(paste("c_","M_",  name, sep = "")) # threshold parameters
    
  }
  
  theta <- get(paste("theta", period, sep = "_")) # latent variables
  
  for (m in 1:num){
    
    if (variable == "C") {
      
      M <- mu_M[m] + gamma_M[m] * theta[,variable] + rnorm(N, sd = sigma_M[m])
      
    } else {
      
      U <- theta[,variable] * gamma_M[m]  + rlogis(N)
      M <- cut(U, c(-Inf, c_M[m,], Inf), labels = F)
      
      rm(U)
      
    }
    
    M_name <- paste("M_",name, "_", m, sep = "")
    M[which(is.na(data_raw[[M_name]]))] <- NA
    data_raw[[M_name]] <- M
    
  }
  
  assign("data_raw", data_raw, envir = globalenv())
  
}
