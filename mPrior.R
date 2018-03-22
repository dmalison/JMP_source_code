library("MASS")        # used to estimate ordered logits for prior parameters

mPrior <- function(mData) {
  
  # Constructs prior parameters for measurements 
  
  # variable: "anchor", "R", "N", or "C"
  # period (survey wave): 0,1,2,3,4
  # n_cat (number of measurement categories): 3 or 5
  
  list2env(mData, environment())

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
  
  if (variable == "C") { 
    
    # use factor analysis to create priors for cognitive measurements
    # covM - covariance matrix of measurements
    
    # covariance matrix
    covM <-
      cov(
        data_raw[,paste(mName, 1:num, sep = "_")], 
        use = "pairwise.complete"
      )
    
    factor_analysis <- 
      factanal(covmat = covM,factors = 1)
    
    assign(
      paste("mu", mName, "mean", sep = "_"), 
      colMeans(data_raw[,paste(mName, 1:num, sep = "_")], na.rm = T),
      envir = globalenv()
    )
    
    assign(
      paste("gamma", mName, "mean", sep = "_"), 
      as.numeric(factor_analysis$loadings)*sqrt(diag(covM)),
      envir = globalenv()
    )
    
    assign(
      paste("sigma", mName, "mean", sep = "_"), 
      sqrt(factor_analysis$uniquenesses*diag(covM)),
      envir = globalenv()
    )
  } else if (variable == "R" | variable == "N") {
    
    # use ordered logistic regression for relationship quality and non-cognitive measurements
    # use measurement average as proxy for theta
    
    # gamma_mean -  factor loading prior mean
    # c_mean - threshold crossing prior mean
    
    gamma_mean = rep(NA, num) 
    c_mean = list() 
    
    eq_M = M ~ theta
    theta = data_raw[[paste("theta", variable, period, sep = "_")]]
    
    for (m in 1:num){
      
      data = 
        data.frame(
          M = as.factor(data_raw[,paste(mName, m, sep = "_")]),
          theta = theta
        )
      
      polr_fit <- 
        polr(eq_M, 
             data = data 
        )
      
      gamma_mean[m] = polr_fit$coefficients["theta"]
      c_mean[[m]] = polr_fit$zeta
      
    }
    
    assign(paste("gamma_M", name, "mean", sep = "_"), gamma_mean, envir = globalenv())
    assign(paste("c_M", name, "mean", sep = "_"), c_mean, envir = globalenv())
    
  }
  
} 
