# Estimates model parameters using simulated data
# Parameter values chosen to match posterior means from fit

# Prologue ----------------------------------------------------------------

setwd("~/bin/JMP/JMP_source_code")
rm(list = ls())
load("~/bin/JMP/work/fit")

library("rstan")       # used to sample from posterior using MCMC
library("foreign")     # used to import data from Stata
library("mvtnorm")     # used to draw from multivariate normal

# RStan recommends calling the following lines before use

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Create simulated data frame ---------------------------------------------------------------

N = 5000             # number of observations for simulated data set

data_raw <- read.dta("~/data/Fragile_Families/extract/extract_noretro.dta") # load data created by Stata extract do file

ind <- sample(1:nrow(data_raw), N, replace = T)
data_raw = data_raw[ind,]
rm(ind)

# Extract parameters from fit_stan -----------------------------------------

for (i in parNames){
  x <- extract(fit_stan, pars = i)[[1]]
  assign(i,colMeans(x))
  rm(i,x)
}

# Simulate structural process -----------------------------------------------

theta_C_2 = rnorm(N)

# Simulate measurements ---------------------------------------------------

mFrame <- # contains variable, period, and number of categories for all measurements
  data.frame(
    variable = "C",
    period = 2,
    n_cat = NA
  )

source("mSim.R")  

for (i in 1:nrow(mFrame))  mSim(mFrame[i,])
  
# Extract measurements ---------------------------------------------------

source("mExtract.R")

for (i in 1:nrow(mFrame))  mExtract(mFrame[i,])
  
# Construct priors --------------------------------------------------------

source("mPrior.R")

for (i in (1:nrow(mFrame))) mPrior(mFrame[i,])

# Create lists ------------------------------------------------------------
{
  parNames =
    c("mu_M_C_2", "sigma_M_C_2", "gamma_M_C_2")
  
  parTrue <- list()
  
  for (i in parNames){
    parTrue[[i]] <- get(i)
  }
  
  latentTrue <- list()
  
  for (i in "theta_C_2") {
    latentTrue[[i]] <- get(i)
    rm(i)
  }
  
}
{
  stan_data <- list()
  
  # set dimensions for stan 
  
  for (i in 
       c("N",
         "C_2_num", "I_C_2_num", "I_C_2_ind", "M_C_2",
         "mu_M_C_2_mean", "gamma_M_C_2_mean", "sigma_M_C_2_mean"
       )
  )
  {
    stan_data[[i]] <- get(i)
    rm(i)
  }
}

rm(list = c(names(stan_data), names(parTrue), names(latentTrue)))

# Fit model with stan -----------------------------------------------------

fit_stan = stan(
  file = 'model_M_C_2.stan',
  data = stan_data,
  # chains = 1,
  # iter = 10,
  # warmup = 5,
  # refresh = 1
  chains = 8,
  iter = 750,
  warmup = 500,
  refresh = 10
)

save(list = c("stan_data", "fit_stan", "parNames", "parTrue", "latentTrue"), file = "~/bin/JMP/work/sim")

