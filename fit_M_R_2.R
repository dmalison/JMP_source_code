# Header ----------------------------------------------------------------

setwd("~/bin/JMP/JMP_source_code")

rm(list = ls())

library("rstan")       # used to sample from posterior using MCMC
library("foreign")     # used to import data from Stata

# RStan recommends calling the following lines before use

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Load data ---------------------------------------------------------------

data_raw <- read.dta("~/data/Fragile_Families/extract/extract_noretro.dta") # load data created by Stata extract do file
N <- nrow(data_raw) # number of observations

# Extract measurements ---------------------------------------------------

mFrame <- # contains variable, period, and number of categories
  data.frame(
    variable = c("R","R"),
    period = c(2,2),
    n_cat = c(3,5),
    stringsAsFactors = F
  )

source("mExtract.R")

for (i in 1:nrow(mFrame)) mExtract(mFrame[i,])

# Construct prior parameters --------------------------------------------------------

source("mPrior.R")

for (i in (1:nrow(mFrame))) mPrior(mFrame[i,])

rm(mFrame)

# Make lists --------------------------------------------------------------

# parNames: list of parameter names
parNames = c("gamma_M_R_2_cat3", "c_M_R_2_cat3", "gamma_M_R_2_cat5", "c_M_R_2_cat5")

# stan_data: list of objects used by stan program
stan_data <- list()

for (i in 
     c("N",
       "R_2_cat3_num", "I_R_2_cat3_num", "I_R_2_cat3_ind", "M_R_2_cat3",
       "gamma_M_R_2_cat3_mean", "c_M_R_2_cat3_mean",
       "R_2_cat5_num", "I_R_2_cat5_num", "I_R_2_cat5_ind", "M_R_2_cat5",
       "gamma_M_R_2_cat5_mean", "c_M_R_2_cat5_mean"
     )
)
{
  stan_data[[i]] <- get(i)
  rm(i)
}

rm(list = c(names(stan_data), names(parNames)))

# Fit model with stan -----------------------------------------------------

fit_stan = stan(
  file = 'model_M_R_2.stan',
  data = stan_data,
  # chains = 1,
  # iter = 10,
  # warmup = 5,
  # refresh = 1
  chains = 8,
  iter = 1250,
  warmup = 1000,
  refresh = 10
)

save(list = c("stan_data", "fit_stan", "parNames"), file = "~/bin/JMP/work/fit_M_R_2")
