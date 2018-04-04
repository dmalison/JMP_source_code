functions {
  
  // Adds relationship quality measurement
  void add_R_measurement_lp(
    int R_num, 
    int[] I_R_num, 
    int[] I_R_ind, 
    int[] M_R, 
    vector gamma_M,
    vector[] c_M,
    vector theta
    )
    {
    int pos = 1;
      for (m in 1:R_num)
      {
        for (n in pos:(pos + I_R_num[m] - 1))
        {
          int ind_ = I_R_ind[n];
          M_R[n] ~ ordered_logistic(gamma_M[m]*theta[ind_], c_M[m]);
        }
        pos = pos + I_R_num[m];
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
      
    int<lower = 1> R_2_cat3_num;
    int<lower = 1, upper = N> I_R_2_cat3_num[R_2_cat3_num];
    int<lower = 1, upper = N> I_R_2_cat3_ind[sum(I_R_2_cat3_num)];
    int<lower = 1, upper = 3> M_R_2_cat3[sum(I_R_2_cat3_num)];

    int<lower = 1> R_2_cat5_num;
    int<lower = 1, upper = N> I_R_2_cat5_num[R_2_cat5_num];
    int<lower = 1, upper = N> I_R_2_cat5_ind[sum(I_R_2_cat5_num)];
    int<lower = 1, upper = 5> M_R_2_cat5[sum(I_R_2_cat5_num)];

  /*** priors ***/
        
    vector<lower = 0>[R_2_cat3_num] gamma_M_R_2_cat3_mean;
    ordered[2] c_M_R_2_cat3_mean[R_2_cat3_num];
    
    vector<lower = 0>[R_2_cat5_num] gamma_M_R_2_cat5_mean;
    ordered[4] c_M_R_2_cat5_mean[R_2_cat5_num];
    
}
transformed data {
  
    // prior mean for cutoff parameters
    real normal_sigma_prior = 1;
    
    // prior standard deviation for factor loading parameters
    real gamma_sd_prior = 1; 
    
    // convert gamma mean and sd to shape parameters
    vector<lower = 0>[R_2_cat3_num] gamma_M_R_2_cat3_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_R_2_cat3_mean);
    vector<lower = 0>[R_2_cat3_num] gamma_M_R_2_cat3_beta  = pow(gamma_sd_prior, -2)*gamma_M_R_2_cat3_mean;

    vector<lower = 0>[R_2_cat5_num] gamma_M_R_2_cat5_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_R_2_cat5_mean);
    vector<lower = 0>[R_2_cat5_num] gamma_M_R_2_cat5_beta  = pow(gamma_sd_prior, -2)*gamma_M_R_2_cat5_mean;

}
parameters {

  // latent variable
  vector[N] theta_R_2;
  
  // measurement parameters
  vector<lower = 0>[R_2_cat3_num] gamma_M_R_2_cat3;
  ordered[2] c_M_R_2_cat3[R_2_cat3_num];

  vector<lower = 0>[R_2_cat5_num] gamma_M_R_2_cat5;
  ordered[4] c_M_R_2_cat5[R_2_cat5_num];

}
transformed parameters {
}
model {

  // priors
  gamma_M_R_2_cat3 ~ gamma(gamma_M_R_2_cat3_alpha, gamma_M_R_2_cat3_beta);
  for (m in 1:R_2_cat3_num){
    c_M_R_2_cat3[m] ~ normal(c_M_R_2_cat3_mean[m], normal_sigma_prior);
  }

 gamma_M_R_2_cat5 ~ gamma(gamma_M_R_2_cat5_alpha, gamma_M_R_2_cat5_beta);
  for (m in 1:R_2_cat5_num){
    c_M_R_2_cat5[m] ~ normal(c_M_R_2_cat5_mean[m], normal_sigma_prior);
  }

  // latent variable
  theta_R_2 ~ normal(0,1);

  // measurements
  
  add_R_measurement_lp(
    R_2_cat3_num,
    I_R_2_cat3_num,
    I_R_2_cat3_ind,
    M_R_2_cat3,
    gamma_M_R_2_cat3,
    c_M_R_2_cat3,
    theta_R_2
  )
  
  add_R_measurement_lp(
    R_2_cat5_num,
    I_R_2_cat5_num,
    I_R_2_cat5_ind,
    M_R_2_cat5,
    gamma_M_R_2_cat5,
    c_M_R_2_cat5,
    theta_R_2
  )
  
}
generated quantities {
}

