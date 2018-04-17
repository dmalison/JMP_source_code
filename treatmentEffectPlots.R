# Creates propensity score plots

# Prologue ----------------------------------------------------------------

rm(list = ls())
library("rstan")

setwd("~/bin/JMP/JMP_source_code")
load("~/bin/JMP/work/fit")

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

for (i in c(parNames, "theta_0", "theta_1", "theta_2", "theta_3", "theta_4", "lambda")){
  x <- extract(fit_stan, pars = i)[[1]]
  assign(i,x)
  rm(i,x)
}

nDraws = length(extract(fit_stan, pars = "lp__")[[1]])

# Construct counterfactual relationship quality ---------------------------

ind1 = intersect(stan_data$R_0_ind1, stan_data$R_1_ind0)

theta_1[,ind1,1] = 
  tcrossprod(alpha_1[,,1], X[ind1,]) + c_1[,1]*lambda[,ind1] + 
  gamma_1[,,1] * (theta_0[,ind1,1] - tcrossprod(alpha_0[,,1], X[ind1,]) - c_0[,1]*lambda[,ind1])

hist(colMeans(theta_1[,ind1,1]))
hist(colMeans(theta_1[,stan_data$R_1_ind1,1]))

hist(colMeans(lambda[,ind1]))
hist(colMeans(lambda[,stan_data$R_1_ind1]))

hist(colMeans(epsilon_0[,ind1]))
hist(colMeans((theta_0[,ind1,1] - tcrossprod(alpha_0[,,1], X[ind1,]) - c_0[,1]*lambda[,ind1])))
hist(colMeans((theta_0[,stan_data$R_1_ind1,1] - tcrossprod(alpha_0[,,1], X[stan_data$R_1_ind1,]) - c_0[,1]*lambda[,stan_data$R_1_ind1])))

dim(theta_0)
