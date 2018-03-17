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

# Construct covariate matrix ---------------------------------------------

# Covariate list
covariates <- 
  c("educ_cat_m",     # Mother's education at baseline
    "race_m",         # Mother's race
    "birthage_m",     # Mother's age at baseline
    "educ_cat_f",     # Father's education at baseline
    "race_f",         # Father's race at baseline
    "rellength",      # How long parents knew each other at baseline
    "siblings",       # Indicator for whether mother and father have other children together
    "half_siblings",  # Indicator for whether mother or father have children with other partners
    "female",         # Child gender
    "religious_m"
  )

# Covariates

for (var in 
     c("educ_cat_m", 
       "race_m",
       "educ_cat_f",
       "race_f",
       "religious_m")
){
  data_raw[[var]] <- factor(data_raw[[var]]) 
  # Turn discrete variables into factors so that it is easy to make design matrix with lm
  rm(var)
} 

data_raw$temp = 1
ols <- lm(temp ~ ., data = data_raw[, c("temp", covariates)])
X <- model.matrix(ols)
rm(covariates, ols)

X <- matrix(as.numeric(X), nrow = dim(X)[1], ncol = dim(X)[2], dimnames = list(NULL, colnames(X)))
# Turn X into numeric matrix to make it easier to analyze and load into stan
X_num <- ncol(X)

# Extract parameters from fit_stan -----------------------------------------

for (i in parNames){
  x <- extract(fit_stan, pars = i)[[1]]
  assign(i,colMeans(x))
  rm(i,x)
}

# Simulate structural process -----------------------------------------------

R_0 <- data_raw$R_0 # use observed initial relationship status

# theta_0

theta_0 <-
  (X %*% alpha_0 +
  rnorm(N, sd = sigma_0))*R_0

colnames(theta_0) <- "R"

# R_1

R_1 <-
  (
    (
      X %*% alpha_p[,1] +
        gamma_p_[1] * theta_0[,1] -
        rnorm(N)
    ) > 0
  )*R_0

# theta_1

theta_1 <- 
  X %*% alpha_1 +
  (R_0*(1-R_1)) %*% beta_1 +
  R_1 %*% delta_1 + 
  (theta_0 %*% gamma_1)*c(1-R_1) + (theta_0 %*% xi_1)*c(R_1) +
  rmvnorm(N, sigma = diag(as.vector(sigma_1)) %*% corr_1 %*% diag(as.vector(sigma_1)))
  
theta_1[,1] <- theta_1[,1]*R_1

colnames(theta_1) <- c("R", "N")

# R_2

R_2 <-
  (
    (
      X %*% alpha_p[,2] +
        gamma_p_[2] * theta_1[,1] -
        rnorm(N)
    ) > 0
  )*R_1

# theta_2

theta_2 <- 
  X %*% alpha_2 +
  (R_1*(1-R_2)) %*% beta_2 +
  R_2 %*% delta_2 + 
  (theta_1 %*% gamma_2)*c(1-R_2) + (theta_1 %*% xi_2)*c(R_2) +
  rmvnorm(N, sigma = diag(as.vector(sigma_2)) %*% corr_2 %*% diag(as.vector(sigma_2)))

theta_2[,1] <- theta_2[,1]*R_2

colnames(theta_2) <- c("R", "N", "C")

# R_3

R_3 <-
  (
    (
      X %*% alpha_p[,3] +
        gamma_p_[3] * theta_2[,1] -
        rnorm(N)
    ) > 0
  )*R_2

# theta_3

theta_3 <- 
  X %*% alpha_3 +
  (R_2*(1-R_3)) %*% beta_3 +
  R_3 %*% delta_3 + 
  (theta_2 %*% gamma_3)*c(1-R_3) + (theta_2 %*% xi_3)*c(R_3) +
  rmvnorm(N, sigma = diag(as.vector(sigma_3)) %*% corr_3 %*% diag(as.vector(sigma_3)))

theta_3[,1] <- theta_3[,1]*R_3

colnames(theta_3) <- c("R", "N", "C")

# R_4

R_4 <-
  (
    (
      X %*% alpha_p[,4] +
        gamma_p_[4] * theta_3[,1] -
        rnorm(N)
    ) > 0
  )*R_3

# theta_4

theta_4 <- 
  X %*% alpha_4 +
  (R_3*(1-R_4)) %*% beta_4 +
  R_4 %*% delta_4 + 
  (theta_3 %*% gamma_4)*c(1-R_4) + (theta_3 %*% xi_4)*c(R_4) +
  rmvnorm(N, sigma = diag(as.vector(sigma_4)) %*% corr_4 %*% diag(as.vector(sigma_4)))

theta_4[,1] <- theta_4[,1]*R_4

colnames(theta_4) <- c("R", "N", "C")


# Simulate measurements ---------------------------------------------------

mFrame <- # contains variable, period, and number of categories for all measurements
  data.frame(
    variable = 
      c(
        "R", 
        "R", "R", "N", 
        "R", "R", "N", "C",
        "R", "R", "N", "C", 
        "R", "R", "N", "C"#,
        #        "anchor"
      ),
    period = 
      c(
        0,
        1, 1, 1,
        2, 2, 2, 2,
        3, 3, 3, 3,
        4, 4, 4, 4#,
        #        NA
      ),
    n_cat = 
      c(
        3, 
        3, 5, 5, 
        3, 5, 3, NA, 
        3, 5, 3, NA,
        3, 5, 3, NA#,
        #        NA
      ),
    stringsAsFactors = F
  )

source("mSim.R")  

for (i in 1:nrow(mFrame))  mSim(mFrame[i,])
  
# Extract measurements ---------------------------------------------------

source("mExtract.R")

for (i in 1:nrow(mFrame))  mExtract(mFrame[i,])
  
# Extract indicators ------------------------------------------------------

# assign missing to indicators

for (i in 1:4){
  
  rName = paste("R", i, sep = "_")
  R <- get(rName)
  
  R[which(is.na(data_raw[[rName]]))] <- NA
  data_raw[[rName]] <- R
  
  rm(R, i, rName)
}

source("rExtract.R")

for (i in 0:4) rExtract(i)

# Construct priors --------------------------------------------------------

source("mPrior.R")

for (i in (1:nrow(mFrame))) mPrior(mFrame[i,])

# Create lists ------------------------------------------------------------
{
  parNames =
    c(
      #      "corr_lambda", "c", 
      "alpha_0", "sigma_0", #"c_0",
      "alpha_1", "beta_1", "gamma_1", "delta_1", "xi_1", "corr_1", "sigma_1", #"c_1",
      "alpha_2", "beta_2", "gamma_2", "xi_2", "delta_2", "corr_2", "sigma_2", #"c_2",
      "alpha_3", "beta_3", "gamma_3", "xi_3", "delta_3", "corr_3", "sigma_3",  #"c_3",
      "alpha_4", "beta_4", "gamma_4", "xi_4", "delta_4", "corr_4", "sigma_4",  #"c_4", 
      "alpha_p", "gamma_p_", #"c_p",
      "gamma_M_R_0_cat3", "c_M_R_0_cat3",
      "gamma_M_R_1_cat3", "c_M_R_1_cat3", 
      "gamma_M_R_1_cat5", "c_M_R_1_cat5",
      "gamma_M_N_1_cat5", "c_M_N_1_cat5",
      "gamma_M_R_2_cat3", "c_M_R_2_cat3",
      "gamma_M_R_2_cat5", "c_M_R_2_cat5",
      "gamma_M_N_2_cat3", "c_M_N_2_cat3",
      "mu_M_C_2", "sigma_M_C_2", "gamma_M_C_2",
      "gamma_M_R_3_cat3", "c_M_R_3_cat3", 
      "gamma_M_R_3_cat5", "c_M_R_3_cat5",
      "gamma_M_N_3_cat3", "c_M_N_3_cat3",
      "mu_M_C_3", "sigma_M_C_3", "gamma_M_C_3",
      "gamma_M_R_4_cat3", "c_M_R_4_cat3",
      "gamma_M_R_4_cat5", "c_M_R_4_cat5",
      "gamma_M_N_4_cat3", "c_M_N_4_cat3",
      "mu_M_C_4", "sigma_M_C_4", "gamma_M_C_4"
      # "alpha_anchor", "gamma_anchor"
    )
  
  parTrue <- list()
  
  for (i in parNames){
    parTrue[[i]] <- get(i)
  }
  
  latentTrue <- list()
  
  for (i in 
       c("theta_0", 
         "theta_1",
         "theta_2",
         "theta_3",
         "theta_4")) {
    latentTrue[[i]] <- get(i)
    rm(i)
  }
  
}
{
  stan_data <- list()
  
  # set dimensions for stan 
  
  dim(I_R_4_cat5_num) <- 1
  dim(gamma_M_R_4_cat5_mean) <- 1
  
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
         # "anchor_num", "I_anchor_num", "I_anchor_ind", "anchor", 
         "R_0_N", "R_0_ind", "R_0", "R_0_N0", "R_0_ind0", "R_0_N1", "R_0_ind1", "R_0_ind_nomiss",
         "R_1_N", "R_1_ind", "R_1", "R_1_N0", "R_1_ind0", "R_1_N1", "R_1_ind1", "R_1_ind_nomiss",
         "R_2_N", "R_2_ind", "R_2", "R_2_N0", "R_2_ind0", "R_2_N1", "R_2_ind1", "R_2_ind_nomiss",
         "R_3_N", "R_3_ind", "R_3", "R_3_N0", "R_3_ind0", "R_3_N1", "R_3_ind1", "R_3_ind_nomiss",
         "R_4_N", "R_4_ind", "R_4", "R_4_N0", "R_4_ind0", "R_4_N1", "R_4_ind1", "R_4_ind_nomiss",
         "gamma_M_R_0_cat3_mean", "c_M_R_0_cat3_mean",
         "gamma_M_R_1_cat3_mean", "c_M_R_1_cat3_mean",
         "gamma_M_R_1_cat5_mean", "c_M_R_1_cat5_mean",
         "gamma_M_N_1_cat5_mean", "c_M_N_1_cat5_mean",
         "gamma_M_R_2_cat3_mean", "c_M_R_2_cat3_mean",
         "gamma_M_R_2_cat5_mean", "c_M_R_2_cat5_mean",
         "gamma_M_N_2_cat3_mean", "c_M_N_2_cat3_mean",
         "mu_M_C_2_mean", "gamma_M_C_2_mean", "sigma_M_C_2_mean",
         "gamma_M_R_3_cat3_mean", "c_M_R_3_cat3_mean",
         "gamma_M_R_3_cat5_mean", "c_M_R_3_cat5_mean",
         "gamma_M_N_3_cat3_mean", "c_M_N_3_cat3_mean",
         "mu_M_C_3_mean", "gamma_M_C_3_mean", "sigma_M_C_3_mean",
         "gamma_M_R_4_cat3_mean", "c_M_R_4_cat3_mean",
         "gamma_M_R_4_cat5_mean", "c_M_R_4_cat5_mean",
         "gamma_M_N_4_cat3_mean", "c_M_N_4_cat3_mean",
         "mu_M_C_4_mean", "gamma_M_C_4_mean", "sigma_M_C_4_mean"
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
  file = 'model.stan',
  data = stan_data,
   pars = 
    c(
      parNames,
#      "lambda",
      "theta_0", 
      "theta_1", 
      "theta_2", 
      "theta_3", 
      "theta_4",
   ),
  # include = T,
  chains = 1,
  iter = 10,
  warmup = 5,
  refresh = 1,
  # chains = 8,
  # iter = 750,
  # warmup = 500,
  # refresh = 10,
  init_r = .5,
  control = list(max_treedepth = 10, adapt_delta = .8)
)

save(list = c("stan_data", "fit_stan", "parNames", "parTrue", "latentTrue"), file = "~/bin/JMP/work/sim")

