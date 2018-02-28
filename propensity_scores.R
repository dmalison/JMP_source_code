# Creates propensity score plots

# Prologue ----------------------------------------------------------------

rm(list = ls())
library("rstan")

setwd("~/bin/R/JMP/JMP_source_code")
load("~/bin/R/JMP/work/fit")

# Extract data from stan_data -------------------------------------

N <- stan_data$N
X <- stan_data$X
X_num <- stan_data$X_num

for (i in 0:4){
  
  R = rep(NA, N)
  name = paste("R", i, sep = "_")
  R[stan_data[[paste(name, "ind0", sep = "_")]]] <- 0
  R[stan_data[[paste(name, "ind1", sep = "_")]]] <- 1
 
  assign(name, R)
  
  rm(R, name, i)
     
}

# Extract parameters from fit_stan ------------------------------------------------------

for (i in c(parNames, "theta_0")){
  x <- extract(fit_stan, pars = i)[[1]]
  assign(i,x)
  rm(i,x)
}

# Construct propensity scores ----------------------------------------------

p_0 <- 
  pnorm(
    tcrossprod(alpha_p[,,1],X) + 
      gamma_p_[,1,1] * theta_0[,,1] + gamma_p_[,2,1] * theta_0[,,1]^2
  )

# Construct propensity score histograms ----------------------------------------

x_out <- seq(0,1, length.out = 21)
sep_col <- rgb(1,0,0,.5)
tog_col <- rgb(0,0,1,.5)

separated <- 
  hist(p_0[,intersect(stan_data$R_0_ind1, stan_data$R_1_ind0)], breaks = x_out, plot = F)
together  <- 
  hist(p_0[,intersect(stan_data$R_0_ind1, stan_data$R_1_ind1)], breaks = x_out, plot = F)

plot(
  NA,
  xlim = c(0,1),
  ylim = c(0,ceiling(max(separated$density,together$density)*2)/2),
  xlab = "Propensity Score",
  ylab = "Density",
  bty = "n",
  main = "Year 1"
)

plot(
  separated, 
  freq = F,
  add = T,
  col = sep_col
  )

plot(
  together, 
  freq = F,
  add = T,
  col = tog_col
)

legend(
  x = "top",
  legend = c(expression(paste(R[1] == 0, "  ")), expression(R[1] == 1)),
  bty = "n",
  fill = c(sep_col, tog_col),
  horiz = T,
  x.intersp = .5
  )
