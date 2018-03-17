rExtract <- function(period){
  
  # Turns raw vector of relationship indicators into objects that can be analyzed by Stan
  
  # *_ind1: Indicies of observations in a relationship this period
  # *_N1: Number of observations in a relationship this period
  # *_ind0: Indicies of observations not in a relationship this period
  # *_N0: Number of observations not in a relationship this period
  # *_ind_nomiss: Indicies of non-missing observations this period
  
  # *_N: Number of non-missing observations who were in a relationship in previous period
  # *_ind: Indices of non-missing observations who were in a relationship in previous period 
  # *: Observed choices for non-missing observations who were in a relationship in previous period
  
  # create local names
  
  R_name = paste("R",period,sep = "_")
  R = data_raw[[R_name]]
  
  assign(
    paste(R_name, "ind1", sep = "_"), 
    which(R == 1), 
    envir = globalenv()
  )
  assign(
    paste(R_name, "N1", sep = "_"), 
    sum(R == 1, na.rm = T), 
    envir = globalenv()
  )
  assign(
    paste(R_name, "ind0", sep = "_"), 
    which(R == 0), 
    envir = globalenv()
  )
  assign(
    paste(R_name, "N0", sep = "_"), 
    sum(R == 0, na.rm = T), 
    envir = globalenv()
  )
  assign(
    paste(R_name, "ind", "nomiss", sep = "_"), 
    which(!is.na(R)),
    envir = globalenv()
  )
  
  if (period >= 1){
    
    Rm1_name = paste("R",i-1,sep = "_")
    Rm1 = data_raw[[Rm1_name]]
    
    assign(paste(R_name, "ind", sep = "_"), which(!is.na(R) & Rm1 == 1), envir = globalenv())
    assign(paste(R_name, "N", sep = "_"), sum(!is.na(R) & Rm1 == 1), envir = globalenv())
    assign(paste(R_name), R[which(!is.na(R) & Rm1 == 1)], envir = globalenv())
    
  } else if (period == 0){
    
    assign(paste(R_name, "ind", sep = "_"), which(!is.na(R)), envir = globalenv())
    assign(paste(R_name, "N", sep = "_"), sum(!is.na(R)), envir = globalenv())
    assign(paste(R_name), R[which(!is.na(R))], envir = globalenv())
    
  }
  
}

