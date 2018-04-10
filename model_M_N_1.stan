functions {
  
  // Adds relationship quality measurement
  void add_N_measurement_lp(
    int N_num, 
    int[] I_N_num, 
    int[] I_N_ind, 
    int[] M_N, 
    vector gamma_M,
    vector[] c_M,
    vector theta
  )
  {
    int pos = 1;
    for (m in 1:N_num)
    {
      for (n in pos:(pos + I_N_num[m] - 1))
      {
        int ind_ = I_N_ind[n];
        M_N[n] ~ ordered_logistic(gamma_M[m]*theta[ind_], c_M[m]);
      }
      pos = pos + I_N_num[m];
    }
  }
  
}
data {
  
  /*** number of observations ***/
    
    int<lower = 1> N;     
    
    /*** measurements ***/
      
      /*
      *_num: Number of measurements
    I_*_num: Number of non-missing observations for each measurement
    I_*_ind: Indices of non-missing observations for each measurement
    M_*: measurements
    */
      
    int<lower = 1> N_1_cat5_num;
    int<lower = 1, upper = N> I_N_1_cat5_num[N_1_cat5_num];
    int<lower = 1, upper = N> I_N_1_cat5_ind[sum(I_N_1_cat5_num)];
    int<lower = 1, upper = 5> M_N_1_cat5[sum(I_N_1_cat5_num)];
    
    vector<lower = 0>[N_1_cat5_num] gamma_M_N_1_cat5_mean;
    ordered[4] c_M_N_1_cat5_mean[N_1_cat5_num];
    
}
transformed data {
  
  // prior mean for cutoff parameters
  real normal_sigma_prior = 1;
  
  // prior standard deviation for factor loading parameters
  real gamma_sd_prior = 1; 
  
  // convert gamma mean and sd to shape parameters
  vector<lower = 0>[N_1_cat5_num] gamma_M_N_1_cat5_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_N_1_cat5_mean);
  vector<lower = 0>[N_1_cat5_num] gamma_M_N_1_cat5_beta  = pow(gamma_sd_prior, -2)*gamma_M_N_1_cat5_mean;
  
}
parameters {
  
  // latent variable
  vector[N] theta_N_1;
  
  // measurement parameters
  vector<lower = 0>[N_1_cat5_num] gamma_M_N_1_cat5;
  ordered[4] c_M_N_1_cat5[N_1_cat5_num];
  
}
transformed parameters {
}
model {
  
  // priors
  gamma_M_N_1_cat5 ~ gamma(gamma_M_N_1_cat5_alpha, gamma_M_N_1_cat5_beta);
  for (m in 1:N_1_cat5_num){
    c_M_N_1_cat5[m] ~ normal(c_M_N_1_cat5_mean[m], normal_sigma_prior);
  }
  
  // latent variable
  theta_N_1 ~ normal(0,1);
  
  // measurements
  add_N_measurement_lp(
    N_1_cat5_num,
    I_N_1_cat5_num,
    I_N_1_cat5_ind,
    M_N_1_cat5,
    gamma_M_N_1_cat5,
    c_M_N_1_cat5,
    theta_N_1
  )
  
}
generated quantities {
}

