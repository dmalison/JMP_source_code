# Makes ICC plots

# Prologue ----------------------------------------------------------------

setwd("~/bin/JMP/JMP_source_code")
rm(list = ls())
load('~/bin/JMP/work/fit')

library("rstan")       # used to sample from posterior using MCMC

# RStan recommends calling the following lines before use

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Extract objects from stan_fit  ---------------------------------------

for (i in c("theta_0", "theta_1")){
  assign(i,extract(fit_stan, pars = i)[[1]])
  rm(i)
}

n_draws <- nrow(extract(fit_stan, pars = "lp__")[[1]])

dimnames(theta_0)[[3]] <- "R"
dimnames(theta_1)[[3]] <- c("R","N")

N <- stan_data$N

# Choose what measurements to extract/simulate --------------------------------------------------

n_out <- 1001 # How many points on x-axis in plots

variable <- "R" # latent variable
period <- 0 # period
n_cat <- 3 # number of response categories
theta <- "theta_0"

# Construct local names ---------------------------------------------------

name <- paste(paste(variable, period, sep = "_"), "_cat", n_cat, sep = "")

num <- stan_data[[paste(name, "_num", sep = "")]] # how many measurements
I_num <- stan_data[[paste("I_", name, "_num", sep = "")]] # how many non-missing observations per measurements
I_ind <- stan_data[[paste("I_", name, "_ind", sep = "")]] # indicies of measurements
M <- stan_data[[paste("M_", name, sep = "")]] # observed measurements 

gamma_M <- extract(fit_stan, pars = paste("gamma_","M_",  name, sep = ""))[[1]] # factor loading draws
c_M <- extract(fit_stan, pars = paste("c_","M_",  name, sep = ""))[[1]] # threshold parameter draws

pos <- 1 # keeps track of position in concatenated vectors

# Construct plots ---------------------------------------------------------

pos = 1

for (m in 1:num){
  
  index = get(theta)[,,variable] * gamma_M[,m] 
    # latent index for measurement M 
    # each column contains posterior draws for one observation
  
  index_out <- # indicies of x-axis in plots (captures 99% of draws, range rounded to integers)
    seq(
      from = floor(quantile(index, probs = .005)), 
      to = ceiling(quantile(index, probs = .995)),
      length.out = n_out
    )
  
  M_ind <- I_ind[pos:(pos + I_num[m]-1)] # indices of non-missing measurements
  
  x <- index[,M_ind] # pick columns whose observations are non-missing
  y <- M[pos:(pos + I_num[m]-1)] # collect measurements of non-missing observations
  
  pos = pos + I_num[m] # update position in concatented vectors
  
  # Parametric predictions
  
  model.p <- list() 
  # each element of model.p is a matrix containing predicted probabilities for a particular category
  # columns of matrices correspond to particular point on x-axis, rows correspond to different posterior draws
  
  index_mat <- matrix(rep(index_out, each = n_draws), ncol = n_out)
  
  model.p[[1]] <- plogis(c_M[,m,1] - index_mat) # lowest category
  
  for (k in 2:(n_cat - 1)){ # middle categories
    model.p[[k]] <- plogis(c_M[,m,k] - index_mat) - plogis(c_M[,m,k - 1] - index_mat)
  }
  
  model.p[[n_cat]] <- # highest category
    1 - plogis(c_M[,m,n_cat - 1] -index_mat)
  
  # Nadaraya-Watson kernel regression
  
  model.np <- list() 
  # each element of model.p is a matrix containing nonparameteric estimates for a particular category
  # columns of matrices correspond to particular point on x-axis, rows correspond to different posterior draws
  
  for (k in 1:n_cat) {
    model.np[[k]] <- matrix(nrow = n_draws, ncol = n_out)
    
    for (i in 1:n_draws){
      
      # for each posterior draw, compute predict probabilities at each point on x-axis in plot
      
      model.np[[k]][i,] <- ksmooth(x[i,], y == k, kernel = "normal", bandwidth = .1, x.points = index_out)$y
      rm(i)
      
    }
    rm(k)
  }
  
  # Create plots and save as pngs
  
  plot_colors <- rainbow(n_cat, v = .75) 
  
  # png(
  #   paste("~/out/Plots/",paste(name, m, sep = "_"),".png", sep = ""),
  #   width = 300, 
  #   height = 240
  #   )
  
  plot(NA,
       xlim = range(index_out),
       ylim = c(0,1),
       bty = "n",
       xlab = "Index",
       ylab = "Probability",
       axes = F,
       main = 
         substitute(
           paste(
             "Measurement ", m, " of ", theta[period]^variable),
           list(m = m, period = period, variable = variable)
         )
  )
  
  axis(1, seq(min(index_out), max(index_out), by = 1))
  axis(2, seq(0,1 , by = .2))
  
  for (k in 1:n_cat){
    
    lines(
      index_out,
      colMeans(model.p[[k]]),
      col = plot_colors[k]
    )
    
    lines(
      index_out,
      colMeans(model.np[[k]], na.rm =T),
      col = plot_colors[k],
      lty= 3
    )
    
  }
  
  legend(
    "right",
    legend = 1:n_cat,
    col = plot_colors, 
    bty = "n",
    lty = 1
  )
  
  # dev.off()
  
  print(m)
  
}


