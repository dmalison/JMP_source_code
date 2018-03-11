# Prologue ----------------------------------------------------------------

setwd("~/bin/JMP/JMP_source_code")
rm(list = ls())

library("rstan")       # used to sample from posterior using MCMC
library("foreign")     # used to import data from Stata
library("mvtnorm")     # used to draw from multivariate normal
library("MASS")        # used to estimate ordered logits for prior parameters

# RStan recommends calling the following lines before use

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Load data ---------------------------------------------------------------

data_raw <- read.dta("~/data/Fragile_Families/extract/extract_noretro.dta") # load data created by Stata extract do file
N <- nrow(data_raw) # number of observations

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

# Construct covariate matrix ---------------------------------------------

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
X_num <- ncol(X)

# Extract measurements ---------------------------------------------------

extract <- function(variable, period, n_cat){
  
  # turns measurements from raw data into objects that can be analyzed by Stan
  
  # variable: "anchor", "R", "N", or "C" - which latent variable?
  # period: 0,1,2,3,4 - which survey wave?
  # n_cat: 3 or 5 - how many measurement categories?
  
  # construct local names
  
  variable = as.character(variable)
  
  if (variable == "anchor"){
    name = variable
    M_name = variable
  } else if (variable == "C"){
    name = paste(variable, period, sep = "_")
    M_name = paste("M", name, sep = "_")
  } else if (variable == "R" | variable == "N"){
    name = paste(variable, period, paste("cat", n_cat, sep = ""), sep = "_")
    M_name = paste("M", name, sep = "_")
  } else {
    stop("invalid variable")
  }
  
  num = # number of measurements 
    length(
      grep(
        paste("^", M_name, "_*", sep = ""),
        names(data_raw)
      )
    )
  
  I_num = NULL  # number of non-missing measurements
  I_ind = NULL  # indicies of non-missing measurements (concatenated into a single vector)
  
  for (m in 1:num){
    I_ = !is.na(data_raw[,paste(M_name, m, sep = "_")]) 
    I_num = c(I_num, sum(I_))
    I_ind = c(I_ind, which(I_))
    rm(I_, m)
  }
  
  M_ = rep(NA,sum(I_num)) # measurements (concatenated into a single vector)
  
  pos = 1
  
  for (m in 1:num){
    
    M_[pos:(pos + I_num[m] - 1)] <- data_raw[I_ind[pos:(pos + I_num[m] - 1)],paste(M_name, m, sep = "_")]
    pos = pos + I_num[m]
    rm(m)
  }
  
  rm(pos)
  
  list(name = name, num = num, I_num = I_num, I_ind = I_ind, M = M_)
  
}

M_frame <- # contains variable, period, and number of categories for all measurements
  data.frame(
    variable = 
      c(
        "R", 
        "R", "R", "N", 
        "R", "R", "N", "C",
        "R", "R", "N", "C", 
        "R", "R", "N", "C",
        "anchor"
      ),
    period = 
      c(
        0,
        1, 1, 1,
        2, 2, 2, 2,
        3, 3, 3, 3,
        4, 4, 4, 4,
        NA
      ),
    n_cat = 
      c(
        3, 
        3, 5, 5, 
        3, 5, 3, NA, 
        3, 5, 3, NA,
        3, 5, 3, NA,
        NA
      )
  )

# extract measurements

# *_num: Number of measurements
# I_*_num: Number of non-missing observations for each measurement
# I_*_ind: Indices of non-missing observations for each measurement
# M_*: measurements

for (i in 1:nrow(M_frame)){
  
  M_ <- extract(M_frame[i,1], M_frame[i,2], M_frame[i,3])
  
  assign(paste(M_$name, "num", sep = "_"), M_$num)
  assign(paste("I", M_$name, "num", sep = "_"), M_$I_num)
  assign(paste("I", M_$name, "ind", sep = "_"), M_$I_ind)
  
  if (M_frame[i,1] == "anchor"){
    assign(M_$name, M_$M)
  } else {
    assign(paste("M", M_$name, sep = "_"), M_$M)
  }
  
  rm(i, M_)
  
}

# Extract indicators ------------------------------------------------------

# *_N: Number of non-missing observations who were in a relationship in previous period
# *_ind: Indices of non-missing observations who were in a relationship in previous period 
# *: Observed choices for non-missing observations who were in a relationship in previous period
# *_N0: Number of observations not in a relationship this period
# *_ind0: Indicies of observations not in a relationship this period
# *_N1: Number of observations in a relationship this period
# *_ind1: Indicies of observations in a relationship this period

R_0_N = sum(!is.na(data_raw$R_0))
R_0_ind = which(!is.na(data_raw$R_0))
R_0 = data_raw$R_0
R_0_ind1 = which(R_0 == 1)
R_0_N1 = sum(R_0 == 1, na.rm = T)
R_0_ind0 = which(R_0 == 0)
R_0_N0 = sum(R_0 == 0, na.rm = T)

for (i in 1:4){
  
  # create local names
  
  R_name = paste("R",i,sep = "_")
  R = data_raw[[R_name]]
  
  Rm1_name = paste("R",i-1,sep = "_")
  Rm1 = data_raw[[Rm1_name]]
  
  assign(paste(R_name, "ind", sep = "_"), which(!is.na(R) & Rm1 == 1))
  assign(paste(R_name, "N", sep = "_"), sum(!is.na(R) & Rm1 == 1))
  assign(paste(R_name), R[which(!is.na(R) & Rm1 == 1)])
  assign(paste(R_name, "ind1", sep = "_"), which(R == 1))
  assign(paste(R_name, "N1", sep = "_"), sum(R == 1, na.rm = T))
  assign(paste(R_name, "ind0", sep = "_"), which(R == 0))
  assign(paste(R_name, "N0", sep = "_"), sum(R == 0, na.rm = T))
  
  rm(i, R_name, R, Rm1_name, Rm1)
  
}

R_2_ind_nomiss = c(R_2_ind0, R_2_ind1)
R_3_ind_nomiss = c(R_3_ind0, R_3_ind1)
R_4_ind_nomiss = c(R_4_ind0, R_4_ind1)

# Construct prior parameters --------------------------------------------------------

M_prior <- function(variable, period, n_cat) {
  
  # Constructs prior parameters for ordered logits using measurement averages as proxies
  
  # variable: "anchor", "R", "N", or "C" - which latent variable?
  # period: 0,1,2,3,4 - which survey wave?
  # n_cat: 3 or 5 - how many measurement categories?
  
  # construct local names
  
  name = paste(variable, period, paste("cat", n_cat, sep = ""), sep = "_")
  M_name = paste("M", name, sep = "_")
  theta_name = paste("theta", variable, period, sep = "_")
  
  num = # number of measurements
    length(
      grep(
        paste("^", M_name, "_*", sep = ""),
        names(data_raw)
      )
    )
  
  gamma_mean = rep(NA, num) # factor loading prior mean
  c_mean = list() # threshold crossing prior mean
  
  eq_M = M ~ theta # ordered logit measurement equation
  
  theta = data_raw[[theta_name]] # use measurement average as a proxy
  
  # run ordered logit for each measurement
  
  for (m in 1:num){
    
    data = 
      data.frame(
        M = as.factor(data_raw[,paste(M_name, m, sep = "_")]),
        theta = theta
      )
    
    polr_fit <- 
      polr(eq_M, 
           data = data 
      )
    
    gamma_mean[m] = polr_fit$coefficients["theta"]
    c_mean[[m]] = polr_fit$zeta
    
  }
  
  list(name = name, gamma_mean = gamma_mean, c_mean = c_mean)
  
} 

# priors for relationship quality and non-cogntive measurements
{
  for (i in (1:(nrow(M_frame) - 1))[-c(8,12,16)]){
    
    prior <- M_prior(M_frame[i,1], M_frame[i,2], M_frame[i,3])
    
    assign(paste("gamma_M", prior$name, "mean", sep = "_"), prior$gamma_mean)
    assign(paste("c_M", prior$name, "mean", sep = "_"), prior$c_mean)
    
    rm(i, prior)
    
  }
  rm(M_frame)
}
# prior for cognitive measurements
{
  # set priors for period 4 cognitive measurements
  
  M_C_4_cov <- # covariance matrix for period 4 cognitive measurements
    cov(
      data_raw[,c("M_C_4_1", "M_C_4_2", "M_C_4_3", "M_C_4_4")], 
      use = "pairwise.complete"
    )
  factor_analysis <- # use factor analysis to obtain priors
    factanal(
      covmat = 
        M_C_4_cov
      , 
      factors = 1
    )
  
  mu_M_C_4_mean    <- colMeans(data_raw[,c("M_C_4_1", "M_C_4_2", "M_C_4_3", "M_C_4_4")], na.rm = T)
  gamma_M_C_4_mean <- as.numeric(factor_analysis$loadings)*sqrt(diag(M_C_4_cov))
  sigma_M_C_4_mean <- sqrt(factor_analysis$uniquenesses*diag(M_C_4_cov))
  
  rm(M_C_4_cov, factor_analysis)
  
  # set priors for period 3 cognitive measurement
  
  gamma_M_C_3_2_mean =
    c(
      cov(data_raw$M_C_3_1, data_raw$M_C_3_2, use = "pairwise.complete")/gamma_M_C_4_mean[1]
    )
  
  mu_M_C_3_mean    = colMeans(data_raw[,c("M_C_3_1", "M_C_3_2")], na.rm = T)
  sigma_M_C_3_mean = 
    c(
      sqrt(var(data_raw[,"M_C_3_1"], na.rm = T) - gamma_M_C_4_mean[1]^2) ,
      sqrt(var(data_raw[,"M_C_3_2"], na.rm = T) - gamma_M_C_3_2_mean^2) 
    )
  
  # set priors for period 2 cognitive measurement
  
  mu_M_C_2_mean    = mean(data_raw$M_C_2_1, na.rm = T)
  sigma_M_C_2_mean = sqrt(var(data_raw$M_C_2_1, na.rm = T) - gamma_M_C_4_mean[1]^2)
  
}

# Make lists --------------------------------------------------------------

# parNames: list of parameter names
{
  parNames =
    c(
      #      "corr_lambda", "c", 
      "alpha_0", "sigma_0", #"c_0",
      "alpha_1", "beta_1", "gamma_1", "xi_1", "delta_1", "corr_1", "sigma_1", #"c_1",
      "alpha_2", "beta_2", "gamma_2", "xi_2", "delta_2", "corr_2", "sigma_2", #"c_2",
      "alpha_3", "beta_3", "gamma_3", "xi_3", "delta_3", "corr_3", "sigma_3", #"c_3",
      # "alpha_4", "beta_4", "gamma_4", "xi_4", "delta_4", "corr_4", "sigma_4", #"c_4", 
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
      "mu_M_C_3", "sigma_M_C_3", "gamma_M_C_3"
      # "gamma_M_R_4_cat3", "c_M_R_4_cat3", 
      # "gamma_M_R_4_cat5", "c_M_R_4_cat5",
      # "gamma_M_N_4_cat3", "c_M_N_4_cat3",
      # "mu_M_C_4", "sigma_M_C_4", "gamma_M_C_4",
      # "alpha_anchor", "gamma_anchor"
    )
}
# stan_data: list of objects used by stan program
{
  
  # set dimensions for stan 
  
  dim(mu_M_C_2_mean) = 1
  dim(sigma_M_C_2_mean) = 1
  
  dim(I_R_4_cat5_num) <- 1
  dim(gamma_M_R_4_cat5_mean) <- 1
  dim(I_C_2_num) <- 1
  
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
         "anchor_num", "I_anchor_num", "I_anchor_ind", "anchor", 
         "R_0_N", "R_0_ind", "R_0", "R_0_N0", "R_0_ind0", "R_0_N1", "R_0_ind1",
         "R_1_N", "R_1_ind", "R_1", "R_1_N0", "R_1_ind0", "R_1_N1", "R_1_ind1",
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
         "mu_M_C_2_mean", "sigma_M_C_2_mean", 
         "gamma_M_R_3_cat3_mean", "c_M_R_3_cat3_mean", 
         "gamma_M_R_3_cat5_mean", "c_M_R_3_cat5_mean",
         "gamma_M_N_3_cat3_mean", "c_M_N_3_cat3_mean",
         "mu_M_C_3_mean", "gamma_M_C_3_2_mean", "sigma_M_C_3_mean", 
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

rm(list = c(names(stan_data), names(parNames)))

# Fit model with stan -----------------------------------------------------

fit_stan = stan(
  file = 'model.stan',
  data = stan_data,
  pars = c(parNames,
           #           "lambda",
           "theta_0", 
           "theta_1",
           "theta_2", 
           "theta_3"#, "theta_4"
  ),
  include = T,
  # chains = 1,
  # iter = 10,
  # warmup = 5,
  # refresh = 1,
  chains = 8,
  iter = 750,
  warmup = 500,
  refresh = 10,
  init_r = .5,
  control = list(max_treedepth = 10, adapt_delta = .8)
)

save(list = c("stan_data", "fit_stan", "parNames"), file = "~/bin/JMP/work/fit")
