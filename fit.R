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

# Construct covariate matrix ---------------------------------------------

# Covariate list
covariates <- c("educ_cat_m",     # Mother's education at baseline
                "race_m",         # Mother's race
                "birthage_m",     # Mother's age at baseline
                "educ_cat_f",     # Father's education at baseline
                "race_f",         # Father's race at baseline
                "rellength",      # How long parents knew each other at baseline
                "siblings",       # Indicator for whether mother and father have other children together
                "half_siblings",  # Indicator for whether mother or father have children with other partners
                "female",         # Child gender
                "religious_m"    # How often mother attends religious services
)

# Turn discrete variables into factors so that it is easy to make design matrix with lm

for (var in c("educ_cat_m", 
              "race_m",
              "educ_cat_f",
              "race_f", 
              "religious_m")){
  data_raw[[var]] <- factor(data_raw[[var]]) 
  rm(var)
} 

# construct X by taking the design matrix from a regressing a constant on covariates
data_raw$temp = 1
ols <- lm(temp ~ ., data = data_raw[, c("temp", covariates)])
X <- model.matrix(ols)

rm(covariates, ols)

# Turn X into numeric so that it can be loaded into stan
X <- matrix(as.numeric(X), nrow = dim(X)[1], ncol = dim(X)[2], dimnames = list(NULL, colnames(X)))

# Take out column means
X <- X - rep(colMeans(X), each = N)
X <- X[,-1]

X_num <- ncol(X)


# Extract measurements ---------------------------------------------------

mFrame <- # contains variable, period, and number of categories for all measurements
  data.frame(
    variable = 
      c(
        "R", 
        "R", "R", "N", 
        "R", "R", "N", "C",
        "R", "R", "N", "C", 
        "R", "R", "N", "C"
      ),
    period = 
      c(
        0,
        1, 1, 1,
        2, 2, 2, 2,
        3, 3, 3, 3,
        4, 4, 4, 4
      ),
    n_cat = 
      c(
        3, 
        3, 5, 5, 
        3, 5, 3, NA, 
        3, 5, 3, NA,
        3, 5, 3, NA
      ),
    stringsAsFactors = F
  )

source("mExtract.R")

for (i in 1:nrow(mFrame)) mExtract(mFrame[i,])

# Extract indicators ------------------------------------------------------

source("rExtract.R")

for (i in 0:5) rExtract(i)

# Extract outcomes ---------------------------------------------------

outcomeData <- # contains year and number of categories for all outcomes
  data.frame(
    year = 
      c(
        9,9,
        15,15
      ),
    n_cat = 
      c(
        2,5,
        2,4
      ),
    stringsAsFactors = F
  )

source("outcomeExtract.R")

for (i in 1:nrow(outcomeData)) outcomeExtract(outcomeData[i,])

# Get measurement parameters --------------------------------------------------------

load("~/bin/JMP/work/measurement_parameters")
list2env(measurementPars, globalenv())

# Make lists --------------------------------------------------------------

# parNames: list of parameter names
{
  parNames =
    c(
      "corr_lambda", "sigma_lambda",
      "alpha_0", "sigma_0", 
      "alpha_1", "beta_1", "delta_1", "gamma_1", "xi_1", "corr_1", "sigma_1",  
      "alpha_2", "beta_2", "delta_2", "gamma_2", "xi_2", "corr_2", "sigma_2",  
      "alpha_3", "beta_3", "delta_3", "gamma_3", "xi_3", "corr_3", "sigma_3",  
      "alpha_4", "beta_4", "delta_4", "gamma_4", "xi_4", "corr_4", "sigma_4",  
      "alpha_p", "gamma_p_", "c_p",
      "alpha_outcome_yr9_cat2", "gamma_outcome_yr9_cat2", "c_outcome_yr9_cat2",
      "alpha_outcome_yr9_cat5", "gamma_outcome_yr9_cat5", "c_outcome_yr9_cat5",
      "alpha_outcome_yr15_cat2", "gamma_outcome_yr15_cat2", "c_outcome_yr15_cat2",
      "alpha_outcome_yr15_cat4", "gamma_outcome_yr15_cat4", "c_outcome_yr15_cat4"
      )
}
# stan_data: list of objects used by stan program
{
  # set dimensions for stan 
  
  dim(I_R_4_cat5_num) <- 1
  dim(gamma_M_R_4_cat5) <- 1

  # Create stan_data object
  
  stan_data <- list()
  
  for (i in 
       c("N",
         "X_num", "X",
         "R_0_cat3_num", "I_R_0_cat3_num", "I_R_0_cat3_ind", "M_R_0_cat3",
         "R_1_cat3_num", "I_R_1_cat3_num", "I_R_1_cat3_ind", "M_R_1_cat3",
         "R_1_cat5_num", "I_R_1_cat5_num", "I_R_1_cat5_ind", "M_R_1_cat5",
         "N_1_cat5_num", "I_N_1_cat5_num", "I_N_1_cat5_ind", "M_N_1_cat5",
         "R_2_cat3_num", "I_R_2_cat3_num", "I_R_2_cat3_ind", "M_R_2_cat3",
         "R_2_cat5_num", "I_R_2_cat5_num", "I_R_2_cat5_ind", "M_R_2_cat5",
         "N_2_cat3_num", "I_N_2_cat3_num", "I_N_2_cat3_ind", "M_N_2_cat3",
         "C_2_num", "I_C_2_num", "I_C_2_ind", "M_C_2",
         "R_3_cat3_num", "I_R_3_cat3_num", "I_R_3_cat3_ind", "M_R_3_cat3",
         "R_3_cat5_num", "I_R_3_cat5_num", "I_R_3_cat5_ind", "M_R_3_cat5",
         "N_3_cat3_num", "I_N_3_cat3_num", "I_N_3_cat3_ind", "M_N_3_cat3",
         "C_3_num", "I_C_3_num", "I_C_3_ind", "M_C_3",
         "R_4_cat3_num", "I_R_4_cat3_num", "I_R_4_cat3_ind", "M_R_4_cat3",
         "R_4_cat5_num", "I_R_4_cat5_num", "I_R_4_cat5_ind", "M_R_4_cat5",   
         "N_4_cat3_num", "I_N_4_cat3_num", "I_N_4_cat3_ind", "M_N_4_cat3",
         "C_4_num", "I_C_4_num", "I_C_4_ind", "M_C_4",
         "R_0_N", "R_0_ind", "R_0_", "R_0_N0", "R_0_ind0", "R_0_N1", "R_0_ind1", 
         "R_1_N", "R_1_ind", "R_1_", "R_1_N0", "R_1_ind0", "R_1_N1", "R_1_ind1", 
         "R_2_N", "R_2_ind", "R_2_", "R_2_N0", "R_2_ind0", "R_2_N1", "R_2_ind1", 
         "R_3_N", "R_3_ind", "R_3_", "R_3_N0", "R_3_ind0", "R_3_N1", "R_3_ind1", 
         "R_4_N", "R_4_ind", "R_4_", "R_4_N0", "R_4_ind0", "R_4_N1", "R_4_ind1", 
         "R_5_N", "R_5_ind", "R_5_", "R_5_N0", "R_5_ind0", "R_5_N1", "R_5_ind1", 
         "outcome_yr9_cat2_num", "I_outcome_yr9_cat2_num", "I_outcome_yr9_cat2_ind", "outcome_yr9_cat2",
         "outcome_yr9_cat5_num", "I_outcome_yr9_cat5_num", "I_outcome_yr9_cat5_ind", "outcome_yr9_cat5",
         "outcome_yr15_cat2_num", "I_outcome_yr15_cat2_num", "I_outcome_yr15_cat2_ind", "outcome_yr15_cat2",
         "outcome_yr15_cat4_num", "I_outcome_yr15_cat4_num", "I_outcome_yr15_cat4_ind", "outcome_yr15_cat4",
         "gamma_M_R_0_cat3", "c_M_R_0_cat3",
         "gamma_M_R_1_cat3", "c_M_R_1_cat3",
         "gamma_M_R_1_cat5", "c_M_R_1_cat5",
         "gamma_M_N_1_cat5", "c_M_N_1_cat5",
         "gamma_M_R_2_cat3", "c_M_R_2_cat3",
         "gamma_M_R_2_cat5", "c_M_R_2_cat5",
         "gamma_M_N_2_cat3", "c_M_N_2_cat3",
         "mu_M_C_2", "gamma_M_C_2", "sigma_M_C_2",
         "gamma_M_R_3_cat3", "c_M_R_3_cat3",
         "gamma_M_R_3_cat5", "c_M_R_3_cat5",
         "gamma_M_N_3_cat3", "c_M_N_3_cat3",
         "mu_M_C_3", "gamma_M_C_3", "sigma_M_C_3",
         "gamma_M_R_4_cat3", "c_M_R_4_cat3",
         "gamma_M_R_4_cat5", "c_M_R_4_cat5",
         "gamma_M_N_4_cat3", "c_M_N_4_cat3",
         "mu_M_C_4", "gamma_M_C_4", "sigma_M_C_4"
       )
  )
  {
    stan_data[[i]] <- get(i)
    rm(i)
  }
}

rm(list = c(names(stan_data), names(parNames)))

# Fit model with stan -----------------------------------------------------

fit_stan = stan(
  file = 'model.stan',
  data = stan_data,
  pars = c(parNames,
           "lambda",
           "theta_0",
           "theta_1",
           "theta_2",
           "theta_3",
           "theta_4",
           "alpha_0_tilde",
           "alpha_1_tilde",
           "alpha_2_tilde",
           "alpha_3_tilde",
           "alpha_4_tilde",
           "alpha_p_tilde"
  ),
  chains = 1,
  iter = 10,
  warmup = 5,
  refresh = 1,
  # chains = 8,
  # iter = 1625,
  # warmup = 1000,
  # refresh = 10,
  save_warmup = F,
  init_r = .5,
  control = list(max_treedepth = 12, adapt_delta = .8)
)

save(list = c("stan_data", "fit_stan", "parNames"), file = "~/bin/JMP/work/fit")
