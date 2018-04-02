functions {
  vector standardize(vector X)
  {
    //normalizes vector to have zero mean and unit standard deviation
    
    real Xmean = mean(X);
    real Xsd = sd(X);
    
    return (X - Xmean)/Xsd;
  }
  
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
  
  void add_C_measurement_lp(
    int C_num, 
    int[] I_C_num, 
    int[] I_C_ind, 
    vector M_C, 
    vector mu_M,
    vector gamma_M,
    vector sigma_M,
    vector theta
  ) 
  {
    int pos = 1;
    for (m in 1:C_num){
      M_C[pos:(pos + I_C_num[m] - 1)] ~
        normal(
          mu_M[m] + gamma_M[m] * theta[I_C_ind[pos:(pos + I_C_num[m] - 1)]],
          sigma_M[m]
        );
      pos = pos + I_C_num[m];
    }
  }
  
  void draw_epsilon_lag1_mix_lp(
    row_vector epsilon,
    real p,
    row_vector delta,
    matrix L_corr
  )
  {
    target += 
      log_sum_exp(
        log(1 - p) + multi_normal_cholesky_lpdf(epsilon | rep_row_vector(0,3), L_corr),
        log(p) + multi_normal_cholesky_lpdf(epsilon | append_col(0,delta), L_corr)
      );
  }
  
  void draw_epsilon_lag2_mix_lp(
    row_vector epsilon,
    real p1,
    real p2,
    row_vector beta,
    row_vector delta,
    matrix L_corr
  )
  {
    vector[3] log_probs;
    log_probs[1] = log(1 - p1)           + multi_normal_cholesky_lpdf(epsilon | rep_row_vector(0,3), L_corr);
    log_probs[2] = log(p1) + log(1 - p2) + multi_normal_cholesky_lpdf(epsilon | append_col(0,beta),  L_corr);
    log_probs[3] = log(p1) + log(p2)     + multi_normal_cholesky_lpdf(epsilon | append_col(0,beta + delta), L_corr);
    
    target += log_sum_exp(log_probs);
  }
  
}
data {
  
  /*** number of observations ***/
    
    int<lower = 1> N;     
    
    /*** covariates ***/
      
      int<lower = 1> X_num; // number of covariates                        
      matrix[N,X_num] X;    // covariate matrix
      
      /*** measurements ***/
        
        /*
        *_num: Number of measurements
      I_*_num: Number of non-missing observations for each measurement
      I_*_ind: Indices of non-missing observations for each measurement
      M_*: measurements
      */
        
        /*** theta_R_0 ***/
        
        int<lower = 1> R_0_cat3_num;
      int<lower = 1, upper = N> I_R_0_cat3_num[R_0_cat3_num];
      int<lower = 1, upper = N> I_R_0_cat3_ind[sum(I_R_0_cat3_num)];
      int<lower = 1, upper = 3> M_R_0_cat3[sum(I_R_0_cat3_num)];
      
      /*** theta_R_1 ***/
        
        int<lower = 1> R_1_cat3_num;
      int<lower = 1, upper = N> I_R_1_cat3_num[R_1_cat3_num];
      int<lower = 1, upper = N> I_R_1_cat3_ind[sum(I_R_1_cat3_num)];
      int<lower = 1, upper = 3> M_R_1_cat3[sum(I_R_1_cat3_num)];
      
      int<lower = 1> R_1_cat5_num;
      int<lower = 1, upper = N> I_R_1_cat5_num[R_1_cat5_num];
      int<lower = 1, upper = N> I_R_1_cat5_ind[sum(I_R_1_cat5_num)];
      int<lower = 1, upper = 5> M_R_1_cat5[sum(I_R_1_cat5_num)];
      
      /*** theta_N_1 ***/
        
        int<lower = 1> N_1_cat5_num;
      int<lower = 1, upper = N> I_N_1_cat5_num[N_1_cat5_num];
      int<lower = 1, upper = N> I_N_1_cat5_ind[sum(I_N_1_cat5_num)];
      int<lower = 1, upper = 5> M_N_1_cat5[sum(I_N_1_cat5_num)];
      
      /*** theta_R_2 ***/
        
        int<lower = 1> R_2_cat3_num;
      int<lower = 1, upper = N> I_R_2_cat3_num[R_2_cat3_num];
      int<lower = 1, upper = N> I_R_2_cat3_ind[sum(I_R_2_cat3_num)];
      int<lower = 1, upper = 3> M_R_2_cat3[sum(I_R_2_cat3_num)];
      
      int<lower = 1> R_2_cat5_num;
      int<lower = 1, upper = N> I_R_2_cat5_num[R_2_cat5_num];
      int<lower = 1, upper = N> I_R_2_cat5_ind[sum(I_R_2_cat5_num)];
      int<lower = 1, upper = 5> M_R_2_cat5[sum(I_R_2_cat5_num)];
      
      /*** theta_N_2 ***/
        
        int<lower = 1> N_2_cat3_num;
      int<lower = 1, upper = N> I_N_2_cat3_num[N_2_cat3_num];
      int<lower = 1, upper = N> I_N_2_cat3_ind[sum(I_N_2_cat3_num)];
      int<lower = 1, upper = 3> M_N_2_cat3[sum(I_N_2_cat3_num)];
      
      /*** theta_C_2 ***/
        
        int<lower = 1> C_2_num;
      int<lower = 1, upper = N> I_C_2_num[C_2_num];
      int<lower = 1, upper = N> I_C_2_ind[sum(I_C_2_num)];
      vector[sum(I_C_2_num)] M_C_2;
      
      /*** theta_R_3 ***/
        
        int<lower = 1> R_3_cat3_num;
      int<lower = 1, upper = N> I_R_3_cat3_num[R_3_cat3_num];
      int<lower = 1, upper = N> I_R_3_cat3_ind[sum(I_R_3_cat3_num)];
      int<lower = 1, upper = 3> M_R_3_cat3[sum(I_R_3_cat3_num)];
      
      int<lower = 1> R_3_cat5_num;
      int<lower = 1, upper = N> I_R_3_cat5_num[R_3_cat5_num];
      int<lower = 1, upper = N> I_R_3_cat5_ind[sum(I_R_3_cat5_num)];
      int<lower = 1, upper = 5> M_R_3_cat5[sum(I_R_3_cat5_num)];
      
      /*** theta_N_3 ***/
        
        int<lower = 1> N_3_cat3_num;
      int<lower = 1, upper = N> I_N_3_cat3_num[N_3_cat3_num];
      int<lower = 1, upper = N> I_N_3_cat3_ind[sum(I_N_3_cat3_num)];
      int<lower = 1, upper = 3> M_N_3_cat3[sum(I_N_3_cat3_num)];
      
      /*** theta_C_3 ***/
        
        int<lower = 1> C_3_num;
      int<lower = 1, upper = N> I_C_3_num[C_3_num];
      int<lower = 1, upper = N> I_C_3_ind[sum(I_C_3_num)];
      vector[sum(I_C_3_num)] M_C_3;
      
      /*** theta_R_4 ***/
        
        int<lower = 1> R_4_cat3_num;
      int<lower = 1, upper = N> I_R_4_cat3_num[R_4_cat3_num];
      int<lower = 1, upper = N> I_R_4_cat3_ind[sum(I_R_4_cat3_num)];
      int<lower = 1, upper = 3> M_R_4_cat3[sum(I_R_4_cat3_num)];
      
      int<lower = 1> R_4_cat5_num;
      int<lower = 1, upper = N> I_R_4_cat5_num[R_4_cat5_num];
      int<lower = 1, upper = N> I_R_4_cat5_ind[sum(I_R_4_cat5_num)];
      int<lower = 1, upper = 5> M_R_4_cat5[sum(I_R_4_cat5_num)];
      
      /*** theta_N_4 ***/
        
        int<lower = 1> N_4_cat3_num;
      int<lower = 1, upper = N> I_N_4_cat3_num[N_4_cat3_num];
      int<lower = 1, upper = N> I_N_4_cat3_ind[sum(I_N_4_cat3_num)];
      int<lower = 1, upper = 3> M_N_4_cat3[sum(I_N_4_cat3_num)];
      
      /*** theta_C_4 ***/
        
        int<lower = 1> C_4_num;
      int<lower = 1, upper = N> I_C_4_num[C_4_num];
      int<lower = 1, upper = N> I_C_4_ind[sum(I_C_4_num)];
      vector[sum(I_C_4_num)] M_C_4;
      
      /*** anchors ***/
        
        // int<lower = 1> anchor_num;
      // int<lower = 1> I_anchor_num[anchor_num];
      // int<lower = 1, upper = N> I_anchor_ind[sum(I_anchor_num)];
      // int<lower = 0, upper = 1> anchor[sum(I_anchor_num)];
      
      /*** relationship indicators ***/
        
        /* 
        *_N: Number of non-missing observations who were in a relationship in previous period
      *_ind: Indices of non-missing observations who were in a relationship in previous period 
      *_: Observed choices for non-missing observations who were in a relationship in previous period
      *_N0: Number of observations not in a relationship this period
      *_ind0: Indicies of observations not in a relationship this period
      *_N1: Number of observations in a relationship this period
      *_ind1: Indicies of observations in a relationship this period
      *_ind_nomiss: Indicies of observations that are non-missing this period
      */
        
        /*** R_0 ***/
        
        int<lower = 0, upper = N> R_0_N;
      int<lower = 0, upper = N> R_0_ind[R_0_N];
      int<lower = 0, upper = 1> R_0_[N];
      int<lower = 0, upper = N> R_0_N0;
      int<lower = 0, upper = N> R_0_ind0[R_0_N0];
      int<lower = 0, upper = N> R_0_N1;
      int<lower = 0, upper = N> R_0_ind1[R_0_N1];
      
      /*** R_1 ***/
        
        int<lower = 0, upper = N> R_1_N;
      int<lower = 0, upper = N> R_1_ind[R_1_N];
      int<lower = 0, upper = 1> R_1_[R_1_N];
      int<lower = 0, upper = N> R_1_N0;
      int<lower = 0, upper = N> R_1_ind0[R_1_N0];
      int<lower = 0, upper = N> R_1_N1;
      int<lower = 0, upper = N> R_1_ind1[R_1_N1];
      
      /*** R_2 ***/
        
        int<lower = 0, upper = N> R_2_N;
      int<lower = 0, upper = N> R_2_ind[R_2_N];
      int<lower = 0, upper = 1> R_2_[R_2_N];
      int<lower = 0, upper = N> R_2_N0;
      int<lower = 0, upper = N> R_2_ind0[R_2_N0];
      int<lower = 0, upper = N> R_2_N1;
      int<lower = 0, upper = N> R_2_ind1[R_2_N1];
      int<lower = 0, upper = N> R_2_N_nomiss;
      int<lower = 0, upper = N> R_2_ind_nomiss[R_2_N_nomiss];
      int<lower = 0, upper = N> R_2_N_miss;
      int<lower = 0, upper = N> R_2_ind_miss[R_2_N_miss];
      
      /*** R_3 ***/
        
        int<lower = 0, upper = N> R_3_N;
      int<lower = 0, upper = N> R_3_ind[R_3_N];
      int<lower = 0, upper = 1> R_3_[R_3_N];
      int<lower = 0, upper = N> R_3_N0;
      int<lower = 0, upper = N> R_3_ind0[R_3_N0];
      int<lower = 0, upper = N> R_3_N1;
      int<lower = 0, upper = N> R_3_ind1[R_3_N1];
      int<lower = 0, upper = N> R_3_N_nomiss;
      int<lower = 0, upper = N> R_3_ind_nomiss[R_3_N_nomiss];
      int<lower = 0, upper = N> R_3_N_miss;
      int<lower = 0, upper = N> R_3_ind_miss[R_3_N_miss];
      
      /*** R_4 ***/
        
        int<lower = 0, upper = N> R_4_N;
      int<lower = 0, upper = N> R_4_ind[R_4_N];
      int<lower = 0, upper = 1> R_4_[R_4_N];
      int<lower = 0, upper = N> R_4_N0;
      int<lower = 0, upper = N> R_4_ind0[R_4_N0];
      int<lower = 0, upper = N> R_4_N1;
      int<lower = 0, upper = N> R_4_ind1[R_4_N1];
      int<lower = 0, upper = N> R_4_ind_nomiss[R_4_N0 + R_4_N1];
      
      /*** prior parameters ***/
        
        /*** theta_0 ***/
        
        vector<lower = 0>[R_0_cat3_num] gamma_M_R_0_cat3_mean;
      ordered[2] c_M_R_0_cat3_mean[R_0_cat3_num];
      
      /*** theta_1 ***/
        
        vector<lower = 0>[R_1_cat3_num] gamma_M_R_1_cat3_mean;
      ordered[2] c_M_R_1_cat3_mean[R_1_cat3_num];
      
      vector<lower = 0>[R_1_cat5_num] gamma_M_R_1_cat5_mean;
      ordered[4] c_M_R_1_cat5_mean[R_1_cat5_num];
      
      vector<lower = 0>[N_1_cat5_num] gamma_M_N_1_cat5_mean;
      ordered[4] c_M_N_1_cat5_mean[N_1_cat5_num];
      
      /*** theta_2 ***/
        
        vector<lower = 0>[R_2_cat3_num] gamma_M_R_2_cat3_mean;
      ordered[2] c_M_R_2_cat3_mean[R_2_cat3_num];
      
      vector<lower = 0>[R_2_cat5_num] gamma_M_R_2_cat5_mean;
      ordered[4] c_M_R_2_cat5_mean[R_2_cat5_num];
      
      vector<lower = 0>[N_2_cat3_num] gamma_M_N_2_cat3_mean;
      ordered[2] c_M_N_2_cat3_mean[N_2_cat3_num];
      
      vector[C_2_num] mu_M_C_2_mean;
      vector<lower = 0>[C_2_num] gamma_M_C_2_mean;
      vector<lower = 0>[C_2_num] sigma_M_C_2_mean;
      
      /*** theta_3 ***/
        
        vector<lower = 0>[R_3_cat3_num] gamma_M_R_3_cat3_mean;
      ordered[2] c_M_R_3_cat3_mean[R_3_cat3_num];
      
      vector<lower = 0>[R_3_cat5_num] gamma_M_R_3_cat5_mean;
      ordered[4] c_M_R_3_cat5_mean[R_3_cat5_num];
      
      vector<lower = 0>[N_3_cat3_num] gamma_M_N_3_cat3_mean;
      ordered[2] c_M_N_3_cat3_mean[N_3_cat3_num];
      
      vector[C_3_num] mu_M_C_3_mean;
      vector<lower = 0>[C_3_num] gamma_M_C_3_mean; 
      vector<lower = 0>[C_3_num] sigma_M_C_3_mean;
      
      /*** theta_4 ***/
        
        vector<lower = 0>[R_4_cat3_num] gamma_M_R_4_cat3_mean;
      ordered[2] c_M_R_4_cat3_mean[R_4_cat3_num];
      
      vector<lower = 0>[R_4_cat5_num] gamma_M_R_4_cat5_mean;
      ordered[4] c_M_R_4_cat5_mean[R_4_cat5_num];
      
      vector<lower = 0>[N_4_cat3_num] gamma_M_N_4_cat3_mean;
      ordered[2] c_M_N_4_cat3_mean[N_4_cat3_num];
      
      vector[C_4_num] mu_M_C_4_mean;
      vector<lower = 0>[C_4_num] gamma_M_C_4_mean;
      vector<lower = 0>[C_4_num] sigma_M_C_4_mean;
      
}
transformed data {
  
  cholesky_factor_corr[3] L_corr_2 = diag_matrix(rep_vector(1,3));
  cholesky_factor_corr[3] L_corr_3 = diag_matrix(rep_vector(1,3));
  
  /*** declare X_R, X_Q ***/
    
    // standardized covariate matrix (orthonormal, mean-zero columns)
  
  matrix[X_num, X_num] X_R;
  matrix[N, X_num] X_Q; 
  // X_Q without constant term
  matrix[N, X_num - 1] X_Q_nocons; 
  
  /*** declare relationship indicator vectors for full sample ***/
    
    // need to assign these because Stan can't read in NAs
  // NAs set to 0 (integrated out of likelihood)
  
  vector[N] R_0 = rep_vector(0, N);
  vector[N] R_1 = rep_vector(0, N); 
  vector[N] R_2 = rep_vector(0, N);
  vector[N] R_3 = rep_vector(0, N);
  vector[N] R_4 = rep_vector(0, N);
  
  /*** prior parameters ***/
  
  // prior mean for standardized coefficients 
  real normal_mu_prior = 0; 
  // prior variance for standardized coefficients and threshold parameters
  real<lower = 0> normal_sigma_prior = 1; 
  // prior standard deviation for correlation parameters
  real lkj_sd_prior = .5; 
  // prior standard deviation for factor loading and st dev parameters
  real gamma_sd_prior = 1; 
  
  // convert lkj sd to lkj eta parameter
  
  real lkj_eta_prior_2 = .5/square(lkj_sd_prior)*(1 - square(lkj_sd_prior)); 
  real lkj_eta_prior_3 = .5/square(lkj_sd_prior)*(1 - 2*square(lkj_sd_prior)); 
  real lkj_eta_prior_4 = .5/square(lkj_sd_prior)*(1 - 3*square(lkj_sd_prior));   
  
  // convert gamma mean and sd to alpha and beta parameters
  
  /*** theta_0 ***/
  
  vector<lower = 0>[R_0_cat3_num] gamma_M_R_0_cat3_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_R_0_cat3_mean);
  vector<lower = 0>[R_0_cat3_num] gamma_M_R_0_cat3_beta  = pow(gamma_sd_prior, -2)*gamma_M_R_0_cat3_mean;
  
  /*** theta_1 ***/
  
  vector<lower = 0>[R_1_cat3_num] gamma_M_R_1_cat3_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_R_1_cat3_mean);
  vector<lower = 0>[R_1_cat3_num] gamma_M_R_1_cat3_beta  = pow(gamma_sd_prior, -2)*gamma_M_R_1_cat3_mean;
  
  vector<lower = 0>[R_1_cat5_num] gamma_M_R_1_cat5_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_R_1_cat5_mean);
  vector<lower = 0>[R_1_cat5_num] gamma_M_R_1_cat5_beta  = pow(gamma_sd_prior, -2)*gamma_M_R_1_cat5_mean;
  
  vector<lower = 0>[N_1_cat5_num] gamma_M_N_1_cat5_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_N_1_cat5_mean);
  vector<lower = 0>[N_1_cat5_num] gamma_M_N_1_cat5_beta  = pow(gamma_sd_prior, -2)*gamma_M_N_1_cat5_mean;
  
  /*** theta_2 ***/
  
  vector<lower = 0>[R_2_cat3_num] gamma_M_R_2_cat3_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_R_2_cat3_mean);
  vector<lower = 0>[R_2_cat3_num] gamma_M_R_2_cat3_beta  = pow(gamma_sd_prior, -2)*gamma_M_R_2_cat3_mean;
  
  vector<lower = 0>[R_2_cat5_num] gamma_M_R_2_cat5_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_R_2_cat5_mean);
  vector<lower = 0>[R_2_cat5_num] gamma_M_R_2_cat5_beta  = pow(gamma_sd_prior, -2)*gamma_M_R_2_cat5_mean;
  
  vector<lower = 0>[N_2_cat3_num] gamma_M_N_2_cat3_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_N_2_cat3_mean);
  vector<lower = 0>[N_2_cat3_num] gamma_M_N_2_cat3_beta  = pow(gamma_sd_prior, -2)*gamma_M_N_2_cat3_mean;
  
  vector<lower = 0>[C_2_num] gamma_M_C_2_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_C_2_mean);
  vector<lower = 0>[C_2_num] gamma_M_C_2_beta  = pow(gamma_sd_prior, -2)*gamma_M_C_2_mean;
  
  vector<lower = 0>[C_2_num] sigma_M_C_2_alpha = pow(gamma_sd_prior, -2)*square(sigma_M_C_2_mean);
  vector<lower = 0>[C_2_num] sigma_M_C_2_beta  = pow(gamma_sd_prior, -2)*sigma_M_C_2_mean;
  
  /*** theta_3 ***/
  
  vector<lower = 0>[R_3_cat3_num] gamma_M_R_3_cat3_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_R_3_cat3_mean);
  vector<lower = 0>[R_3_cat3_num] gamma_M_R_3_cat3_beta  = pow(gamma_sd_prior, -2)*gamma_M_R_3_cat3_mean;
  
  vector<lower = 0>[R_3_cat5_num] gamma_M_R_3_cat5_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_R_3_cat5_mean);
  vector<lower = 0>[R_3_cat5_num] gamma_M_R_3_cat5_beta  = pow(gamma_sd_prior, -2)*gamma_M_R_3_cat5_mean;
  
  vector<lower = 0>[N_3_cat3_num] gamma_M_N_3_cat3_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_N_3_cat3_mean);
  vector<lower = 0>[N_3_cat3_num] gamma_M_N_3_cat3_beta  = pow(gamma_sd_prior, -2)*gamma_M_N_3_cat3_mean;
  
  vector<lower = 0>[C_3_num] gamma_M_C_3_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_C_3_mean);
  vector<lower = 0>[C_3_num] gamma_M_C_3_beta  = pow(gamma_sd_prior, -2)*gamma_M_C_3_mean;
  
  vector<lower = 0>[C_3_num] sigma_M_C_3_alpha = pow(gamma_sd_prior, -2)*square(sigma_M_C_3_mean);
  vector<lower = 0>[C_3_num] sigma_M_C_3_beta  = pow(gamma_sd_prior, -2)*sigma_M_C_3_mean;
  
  /*** theta_4 ***/
  
  vector<lower = 0>[R_4_cat3_num] gamma_M_R_4_cat3_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_R_4_cat3_mean);
  vector<lower = 0>[R_4_cat3_num] gamma_M_R_4_cat3_beta  = pow(gamma_sd_prior, -2)*gamma_M_R_4_cat3_mean;
  
  vector<lower = 0>[R_4_cat5_num] gamma_M_R_4_cat5_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_R_4_cat5_mean);
  vector<lower = 0>[R_4_cat5_num] gamma_M_R_4_cat5_beta  = pow(gamma_sd_prior, -2)*gamma_M_R_4_cat5_mean;
  
  vector<lower = 0>[N_4_cat3_num] gamma_M_N_4_cat3_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_N_4_cat3_mean);
  vector<lower = 0>[N_4_cat3_num] gamma_M_N_4_cat3_beta  = pow(gamma_sd_prior, -2)*gamma_M_N_4_cat3_mean;
  
  vector<lower = 0>[C_4_num] gamma_M_C_4_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_C_4_mean);
  vector<lower = 0>[C_4_num] gamma_M_C_4_beta  = pow(gamma_sd_prior, -2)*gamma_M_C_4_mean;
  
  vector<lower = 0>[C_4_num] sigma_M_C_4_alpha = pow(gamma_sd_prior, -2)*square(sigma_M_C_4_mean);
  vector<lower = 0>[C_4_num] sigma_M_C_4_beta  = pow(gamma_sd_prior, -2)*sigma_M_C_4_mean;
  
  /*** assign X_R and X_Q ***/
  
  X_R = qr_R(X)[1:X_num,] / sqrt(N - 1);
  X_Q = qr_Q(X)[, 1:X_num] * sqrt(N - 1); 
  X_Q_nocons = X_Q[,2:X_num];
  
  /*** assign relationship indicators ***/
  
  R_0           = to_vector(R_0_);
  R_1[R_1_ind]  = to_vector(R_1_);
  R_2[R_2_ind]  = to_vector(R_2_);
  R_3[R_3_ind]  = to_vector(R_3_);
  R_4[R_4_ind]  = to_vector(R_4_);
  
}
parameters {

/*** theta_0 ***/

// matrix[N,1] theta_0;

/*** theta_1 ***/

// matrix[X_num-1,2] alpha_1_tilde_raw;
// real beta_1_raw;
// row_vector[2] gamma_1_raw;
// real xi_1_raw;
// real delta_1_raw;
// cholesky_factor_corr[2] L_corr_1;
// 
// matrix[R_1_N1, 2] epsilon_1;
// vector[R_1_N0]    epsilon_N_1_R1eq0;

/*** theta_2 ***/

// vector[N] theta_R_2;
// vector[N] theta_N_2;
vector[N] theta_C_2;

/*** theta_3 ***/

// vector[X_num + 5] coef_C_3_raw;
// matrix[X_num-1,3] alpha_3_tilde_raw;
// row_vector[2] beta_3_raw;
// real<lower = 0> gamma_3_11_raw;
// real<lower = 0> gamma_3_22_raw;
// real<lower = 0> gamma_3_33_raw;
// row_vector[4] gamma_3_raw;
// row_vector[2] xi_3_raw;
// row_vector[2] delta_3_raw;
// cholesky_factor_corr[3] L_corr_3;
// cholesky_factor_corr[2] L_corr_3_raw;
// row_vector[3] epsilon_3[N];

/*** theta_4 ***/

// matrix[X_num-1,3] alpha_4_tilde_raw;
// row_vector[2] beta_4_raw;
// row_vector[7] gamma_4_raw;
// row_vector[2] xi_4_raw;
// row_vector[2] delta_4_raw;
// cholesky_factor_corr[3] L_corr_4;
// 
// matrix[R_4_N1, 3] epsilon_4;
// matrix[R_4_N0, 2] epsilon_NC_4_R4eq0;

/*** M_R_0 ***/

// vector<lower = 0>[R_0_cat3_num] gamma_M_R_0_cat3;
// ordered[2] c_M_R_0_cat3[R_0_cat3_num];

/*** M_R_1 ***/

// vector<lower = 0>[R_1_cat3_num] gamma_M_R_1_cat3;
// ordered[2] c_M_R_1_cat3[R_1_cat3_num];
// 
// vector<lower = 0>[R_1_cat5_num] gamma_M_R_1_cat5;
// ordered[4] c_M_R_1_cat5[R_1_cat5_num];

/*** M_N_1 ***/

// vector<lower = 0>[N_1_cat5_num] gamma_M_N_1_cat5;
// ordered[4] c_M_N_1_cat5[N_1_cat5_num];

/*** M_R_2 ***/

// vector<lower = 0>[R_2_cat3_num] gamma_M_R_2_cat3;
// ordered[2] c_M_R_2_cat3[R_2_cat3_num];
// 
// vector<lower = 0>[R_2_cat5_num] gamma_M_R_2_cat5;
// ordered[4] c_M_R_2_cat5[R_2_cat5_num];

/*** M_N_2 ***/

// vector<lower = 0>[N_2_cat3_num] gamma_M_N_2_cat3;
// ordered[2] c_M_N_2_cat3[N_2_cat3_num];

/*** M_C_2 ***/

vector[C_2_num] mu_M_C_2;
vector<lower = 0>[C_2_num] gamma_M_C_2;
vector<lower = 0>[C_2_num] sigma_M_C_2;

/*** M_R_3 ***/

// vector<lower = 0>[R_3_cat3_num] gamma_M_R_3_cat3;
// ordered[2] c_M_R_3_cat3[R_3_cat3_num];
// 
// vector<lower = 0>[R_3_cat5_num] gamma_M_R_3_cat5;
// ordered[4] c_M_R_3_cat5[R_3_cat5_num];

/*** M_N_3 ***/

// vector<lower = 0>[N_3_cat3_num] gamma_M_N_3_cat3;
// ordered[2] c_M_N_3_cat3[N_3_cat3_num];

/*** M_C_3 ***/

// vector[C_3_num] mu_M_C_3;
// vector<lower = 0>[C_3_num] gamma_M_C_3;
// vector<lower = 0>[C_3_num] sigma_M_C_3;

/*** M_R_4 ***/

// vector<lower = 0>[R_4_cat3_num] gamma_M_R_4_cat3;
// ordered[2] c_M_R_4_cat3[R_4_cat3_num];
// 
// vector<lower = 0>[R_4_cat5_num] gamma_M_R_4_cat5;
// ordered[4] c_M_R_4_cat5[R_4_cat5_num];

/*** M_N_4 ***/

// vector<lower = 0>[N_4_cat3_num] gamma_M_N_4_cat3;
// ordered[2] c_M_N_4_cat3[N_4_cat3_num];

/*** M_C_4 ***/

// vector[C_4_num] mu_M_C_4;
// vector<lower = 0>[C_4_num] gamma_M_C_4;
// vector<lower = 0>[C_4_num] sigma_M_C_4;

}
transformed parameters {
}
model {
  
  /*** priors ***/
    
  /*** M_R_0 ***/  
    
    //   gamma_M_R_0_cat3 ~ gamma(gamma_M_R_0_cat3_alpha, gamma_M_R_0_cat3_beta);

    //   for (m in 1:R_0_cat3_num){
      //     c_M_R_0_cat3[m] ~ normal(c_M_R_0_cat3_mean[m], normal_sigma_prior);
      //   }
  // 
    /*** M_R_1 ***/
    // 
    // gamma_M_R_1_cat3 ~ gamma(gamma_M_R_1_cat3_alpha, gamma_M_R_1_cat3_beta);
  // 
    // for (m in 1:R_1_cat3_num){
      //   c_M_R_1_cat3[m] ~ normal(c_M_R_1_cat3_mean[m], normal_sigma_prior);
      // }
  // 
    // gamma_M_R_1_cat5 ~ gamma(gamma_M_R_1_cat5_alpha, gamma_M_R_1_cat5_beta);
  // 
    // for (m in 1:R_1_cat5_num){
      //   c_M_R_1_cat5[m] ~ normal(c_M_R_1_cat5_mean[m], normal_sigma_prior);
      // }
  // 
    /*** M_N_1 ***/
    // 
    // gamma_M_N_1_cat5 ~ gamma(gamma_M_N_1_cat5_alpha, gamma_M_N_1_cat5_beta);
  // 
    // for (m in 1:N_1_cat5_num){
      //   c_M_N_1_cat5[m] ~ normal(c_M_N_1_cat5_mean[m], normal_sigma_prior);
      // }
  // 
    /*** M_R_2 ***/
    
  //   gamma_M_R_2_cat3 ~ gamma(gamma_M_R_2_cat3_alpha, gamma_M_R_2_cat3_beta);
  // 
  // for (m in 1:R_2_cat3_num){
  //   c_M_R_2_cat3[m] ~ normal(c_M_R_2_cat3_mean[m], normal_sigma_prior);
  // }
  // 
  // gamma_M_R_2_cat5 ~ gamma(gamma_M_R_2_cat5_alpha, gamma_M_R_2_cat5_beta);
  // 
  // for (m in 1:R_2_cat5_num){
  //   c_M_R_2_cat5[m] ~ normal(c_M_R_2_cat5_mean[m], normal_sigma_prior);
  // }
  
  /*** M_N_2 ***/
    
  //   gamma_M_N_2_cat3 ~ gamma(gamma_M_N_2_cat3_alpha, gamma_M_N_2_cat3_beta);
  // 
  // for (m in 1:N_2_cat3_num){
  //   c_M_N_2_cat3[m] ~ normal(c_M_N_2_cat3_mean[m], normal_sigma_prior);
  // }
  
  /*** M_C_2 ***/
    
    mu_M_C_2 ~ normal(mu_M_C_2_mean, normal_sigma_prior);
    gamma_M_C_2 ~ gamma(gamma_M_C_2_alpha, gamma_M_C_2_beta);
    sigma_M_C_2 ~ gamma(sigma_M_C_2_alpha, sigma_M_C_2_beta);
  
  /*** M_R_3 ***/
    
  //   gamma_M_R_3_cat3 ~ gamma(gamma_M_R_3_cat3_alpha, gamma_M_R_3_cat3_beta);
  // 
  // for (m in 1:R_3_cat3_num){
  //   c_M_R_3_cat3[m] ~ normal(c_M_R_3_cat3_mean[m], normal_sigma_prior);
  // }
  // 
  // gamma_M_R_3_cat5 ~ gamma(gamma_M_R_3_cat5_alpha, gamma_M_R_3_cat5_beta);
  // 
  // for (m in 1:R_3_cat5_num){
  //   c_M_R_3_cat5[m] ~ normal(c_M_R_3_cat5_mean[m], normal_sigma_prior);
  // }
  // 
  /*** M_N_3 ***/
  //   
  //   gamma_M_N_3_cat3 ~ gamma(gamma_M_N_3_cat3_alpha, gamma_M_N_3_cat3_beta);
  // 
  // for (m in 1:N_3_cat3_num){
  //   c_M_N_3_cat3[m] ~ normal(c_M_N_3_cat3_mean[m], normal_sigma_prior);
  // }
  // 
  /*** M_C_3 ***/
    
  //   mu_M_C_3 ~ normal(mu_M_C_3_mean, normal_sigma_prior);
  // gamma_M_C_3 ~ gamma(gamma_M_C_3_alpha, gamma_M_C_3_beta);
  // sigma_M_C_3 ~ gamma(sigma_M_C_3_alpha, sigma_M_C_3_beta);
  
  /*** M_R_4 ***/
    // 
    // gamma_M_R_4_cat3 ~ gamma(gamma_M_R_4_cat3_alpha, gamma_M_R_4_cat3_beta);
  // 
    // for (m in 1:R_4_cat3_num){
      //   c_M_R_4_cat3[m] ~ normal(c_M_R_4_cat3_mean[m], normal_sigma_prior);
      // }
  // 
    // gamma_M_R_4_cat5 ~ gamma(gamma_M_R_4_cat5_alpha, gamma_M_R_4_cat5_beta);
  // 
    // for (m in 1:R_4_cat5_num){
      //   c_M_R_4_cat5[m] ~ normal(c_M_R_4_cat5_mean[m], normal_sigma_prior);
      // }
  // 
    /*** M_N_4 ***/
    // 
    // gamma_M_N_4_cat3 ~ gamma(gamma_M_N_4_cat3_alpha, gamma_M_N_4_cat3_beta);
  // 
    // for (m in 1:N_4_cat3_num){
      //   c_M_N_4_cat3[m] ~ normal(c_M_N_4_cat3_mean[m], normal_sigma_prior);
      // }
  // 
    /*** M_C_4 ***/
    // 
    // mu_M_C_4 ~ normal(mu_M_C_4_mean, normal_sigma_prior);
  // gamma_M_C_4 ~ gamma(gamma_M_C_4_alpha, gamma_M_C_4_beta);
  // sigma_M_C_4 ~ gamma(sigma_M_C_4_alpha, sigma_M_C_4_beta);
  
  /*** state variables ***/
  
  /*** theta_2 ***/
  
  // theta_R_2 ~ normal(0,1);  
  // theta_N_2 ~ normal(0,1);  
  theta_C_2 ~ normal(0,1);
  
  /*** theta_3 ***/
    

  /*** theta_4 ***/
    // 
    // to_vector(epsilon_4) ~ normal(0,1);
  // to_vector(epsilon_NC_4_R4eq0)
  //                      ~ normal(0,1);
  

  /*** measurements ***/
    
    /*** theta_R_0 ***/
    // {
      //     int pos = 1;
      //     for (m in 1:R_0_cat3_num){
        //       for (n in pos:(pos + I_R_0_cat3_num[m] - 1)){
          //         int ind_ = I_R_0_cat3_ind[n];
          //         M_R_0_cat3[n] ~ ordered_logistic(gamma_M_R_0_cat3[m]*theta_0[ind_,1], c_M_R_0_cat3[m]);
          //       }
        //       pos = pos + I_R_0_cat3_num[m];
        //     }
      // }
  /*** theta_R_1 ***/
    // {
      //     int pos = 1;
      //     for (m in 1:R_1_cat3_num){
        //       for (n in pos:(pos + I_R_1_cat3_num[m] - 1)){
          //         int ind_ = I_R_1_cat3_ind[n];
          //         M_R_1_cat3[n] ~ ordered_logistic(gamma_M_R_1_cat3[m]*theta_1[ind_,1], c_M_R_1_cat3[m]);
          //       }
        //       pos = pos + I_R_1_cat3_num[m];
        //     }
      // }
  // {
    //     int pos = 1;
    //     for (m in 1:R_1_cat5_num){
      //       for (n in pos:(pos + I_R_1_cat5_num[m] - 1)){
        //         int ind_ = I_R_1_cat5_ind[n];
        //         M_R_1_cat5[n] ~ ordered_logistic(gamma_M_R_1_cat5[m]*theta_1[ind_,1], c_M_R_1_cat5[m]);
        //       }
      //       pos = pos + I_R_1_cat5_num[m];
      //     }
    // }
  /*** theta_N_1 ***/
    // {
      //     int pos = 1;
      //     for (m in 1:N_1_cat5_num){
        //       for (n in pos:(pos + I_N_1_cat5_num[m] - 1)){
          //         int ind_ = I_N_1_cat5_ind[n];
          //         M_N_1_cat5[n] ~ ordered_logistic(gamma_M_N_1_cat5[m]*theta_1[ind_,2], c_M_N_1_cat5[m]);
          //       }
        //       pos = pos + I_N_1_cat5_num[m];
        //     }
      // }
  /*** theta_R_2 ***/
    
  //   add_R_measurement_lp(
  //     R_2_cat3_num,
  //     I_R_2_cat3_num,
  //     I_R_2_cat3_ind,
  //     M_R_2_cat3,
  //     gamma_M_R_2_cat3,
  //     c_M_R_2_cat3,
  //     theta_R_2
  //   )
  // 
  // add_R_measurement_lp(
  //   R_2_cat5_num,
  //   I_R_2_cat5_num,
  //   I_R_2_cat5_ind,
  //   M_R_2_cat5,
  //   gamma_M_R_2_cat5,
  //   c_M_R_2_cat5,
  //   theta_R_2
  // )
  
  /*** theta_N_2 ***/
    
    // add_N_measurement_lp(
    //   N_2_cat3_num,
    //   I_N_2_cat3_num,
    //   I_N_2_cat3_ind,
    //   M_N_2_cat3,
    //   gamma_M_N_2_cat3,
    //   c_M_N_2_cat3,
    //   theta_N_2
    // )
  
  /*** theta_C_2 ***/
    
    add_C_measurement_lp(
      C_2_num,
      I_C_2_num,
      I_C_2_ind,
      M_C_2,
      mu_M_C_2,
      gamma_M_C_2,
      sigma_M_C_2,
      theta_C_2
    )
  
  /*** theta_R_3 ***/
    
    // add_R_measurement_lp(
      //   R_3_cat3_num,
      //   I_R_3_cat3_num,
      //   I_R_3_cat3_ind,
      //   M_R_3_cat3,
      //   gamma_M_R_3_cat3,
      //   c_M_R_3_cat3,
      //   theta_3[,1]
      // )
  // 
    // add_R_measurement_lp(
      //   R_3_cat5_num,
      //   I_R_3_cat5_num,
      //   I_R_3_cat5_ind,
      //   M_R_3_cat5,
      //   gamma_M_R_3_cat5,
      //   c_M_R_3_cat5,
      //   theta_3[,1]
      // )
  
  /*** theta_N_3 ***/
    
    // add_N_measurement_lp(
      //   N_3_cat3_num,
      //   I_N_3_cat3_num,
      //   I_N_3_cat3_ind,
      //   M_N_3_cat3,
      //   gamma_M_N_3_cat3,
      //   c_M_N_3_cat3,
      //   theta_3[,2]
      // )
  
  /*** theta_C_3 ***/
    
    // add_C_measurement_lp(
    //   C_3_num,
    //   I_C_3_num,
    //   I_C_3_ind,
    //   M_C_3,
    //   mu_M_C_3,
    //   gamma_M_C_3,
    //   sigma_M_C_3,
    //   theta_3[,3]
    // )
  
  /*** theta_R_4 ***/
    // {
      //     int pos = 1;
      //     for (m in 1:R_4_cat3_num){
        //       for (n in pos:(pos + I_R_4_cat3_num[m] - 1)){
          //         int ind_ = I_R_4_cat3_ind[n];
          //         M_R_4_cat3[n] ~ ordered_logistic(gamma_M_R_4_cat3[m]*theta_4[ind_,1], c_M_R_4_cat3[m]);
          //       }
        //       pos = pos + I_R_4_cat3_num[m];
        //     }
      // }
  // {
    //     int pos = 1;
    //     for (m in 1:R_4_cat5_num){
      //       for (n in pos:(pos + I_R_4_cat5_num[m] - 1)){
        //         int ind_ = I_R_4_cat5_ind[n];
        //         M_R_4_cat5[n] ~ ordered_logistic(gamma_M_R_4_cat5[m]*theta_4[ind_,1], c_M_R_4_cat5[m]);
        //       }
      //       pos = pos + I_R_4_cat5_num[m];
      //     }
    // }
  /*** theta_N_4 ***/
    // {
      //     int pos = 1;
      //     for (m in 1:N_4_cat3_num){
        //       for (n in pos:(pos + I_N_4_cat3_num[m] - 1)){
          //         int ind_ = I_N_4_cat3_ind[n];
          //         M_N_4_cat3[n] ~ ordered_logistic(gamma_M_N_4_cat3[m]*theta_4[ind_,2], c_M_N_4_cat3[m]);
          //       }
        //       pos = pos + I_N_4_cat3_num[m];
        //     }
      // }
  /*** theta_C_4 ***/
    // {
      //     int pos = 1;
      //     for (m in 1:C_4_num){
        //       M_C_4[pos:(pos + I_C_4_num[m] - 1)] ~
          //           normal(
            //             mu_M_C_4[m] + gamma_M_C_4[m] * theta_4[I_C_4_ind[pos:(pos + I_C_4_num[m] - 1)],3],
            //             sigma_M_C_4[m]
            //         );
        //       pos = pos + I_C_4_num[m];
        //       }
      // }
  /*** anchors ***/
    // {
      //   int pos = 1;
      //   for (m in 1:anchor_num){
        //     int ind[I_anchor_num[m]] = I_anchor_ind[pos:(pos + I_anchor_num[m] - 1)];
        //     anchor[pos:(pos + I_anchor_num[m] - 1)] ~ 
          //       bernoulli_logit(
            //         X_Q[ind,] * alpha_anchor_tilde[,m] +
              //           gamma_anchor[1,m]*theta_4[ind,2] + gamma_anchor[2,m]*theta_4[ind,3]
            //       )
        //     ;
        //     pos = pos + I_anchor_num[m];
        //   }
      // } 
  
}
generated quantities {
}

