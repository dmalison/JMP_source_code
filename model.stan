functions {
  vector standardize(vector X){
    //normalizes vector to have zero mean and unit standard deviation
    
    real Xmean = mean(X);
    real Xsd = sd(X);
    
    return (X - Xmean)/Xsd;
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
    
    // int<lower = 1> R_1_cat3_num;                       
    // int<lower = 1, upper = N> I_R_1_cat3_num[R_1_cat3_num];                  
    // int<lower = 1, upper = N> I_R_1_cat3_ind[sum(I_R_1_cat3_num)];           
    // int<lower = 1, upper = 3> M_R_1_cat3[sum(I_R_1_cat3_num)];  
    // 
    // int<lower = 1> R_1_cat5_num;                       
    // int<lower = 1, upper = N> I_R_1_cat5_num[R_1_cat5_num];                  
    // int<lower = 1, upper = N> I_R_1_cat5_ind[sum(I_R_1_cat5_num)];           
    // int<lower = 1, upper = 5> M_R_1_cat5[sum(I_R_1_cat5_num)];   

    /*** theta_N_1 ***/

    // int<lower = 1> N_1_cat5_num;                       
    // int<lower = 1, upper = N> I_N_1_cat5_num[N_1_cat5_num];                  
    // int<lower = 1, upper = N> I_N_1_cat5_ind[sum(I_N_1_cat5_num)];           
    // int<lower = 1, upper = 5> M_N_1_cat5[sum(I_N_1_cat5_num)];   

    /*** theta_R_2 ***/

    // int<lower = 1> R_2_cat3_num;                       
    // int<lower = 1, upper = N> I_R_2_cat3_num[R_2_cat3_num];                  
    // int<lower = 1, upper = N> I_R_2_cat3_ind[sum(I_R_2_cat3_num)];           
    // int<lower = 1, upper = 3> M_R_2_cat3[sum(I_R_2_cat3_num)];  
    // 
    // int<lower = 1> R_2_cat5_num;                       
    // int<lower = 1, upper = N> I_R_2_cat5_num[R_2_cat5_num];                  
    // int<lower = 1, upper = N> I_R_2_cat5_ind[sum(I_R_2_cat5_num)];           
    // int<lower = 1, upper = 5> M_R_2_cat5[sum(I_R_2_cat5_num)];   

    /*** theta_N_2 ***/

    // int<lower = 1> N_2_cat3_num;                       
    // int<lower = 1, upper = N> I_N_2_cat3_num[N_2_cat3_num];                  
    // int<lower = 1, upper = N> I_N_2_cat3_ind[sum(I_N_2_cat3_num)];           
    // int<lower = 1, upper = 3> M_N_2_cat3[sum(I_N_2_cat3_num)];
    
    /*** theta_C_2 ***/

    // int<lower = 1> C_2_num;                       
    // int<lower = 1, upper = N> I_C_2_num[C_2_num];                  
    // int<lower = 1, upper = N> I_C_2_ind[sum(I_C_2_num)];           
    // vector[sum(I_C_2_num)] M_C_2;               
    
    /*** theta_R_3 ***/

    // int<lower = 1> R_3_cat3_num;                       
    // int<lower = 1, upper = N> I_R_3_cat3_num[R_3_cat3_num];                  
    // int<lower = 1, upper = N> I_R_3_cat3_ind[sum(I_R_3_cat3_num)];           
    // int<lower = 1, upper = 3> M_R_3_cat3[sum(I_R_3_cat3_num)];  
    // 
    // int<lower = 1> R_3_cat5_num;                       
    // int<lower = 1, upper = N> I_R_3_cat5_num[R_3_cat5_num];                  
    // int<lower = 1, upper = N> I_R_3_cat5_ind[sum(I_R_3_cat5_num)];           
    // int<lower = 1, upper = 5> M_R_3_cat5[sum(I_R_3_cat5_num)];   

    /*** theta_N_3 ***/

    // int<lower = 1> N_3_cat3_num;                       
    // int<lower = 1, upper = N> I_N_3_cat3_num[N_3_cat3_num];                  
    // int<lower = 1, upper = N> I_N_3_cat3_ind[sum(I_N_3_cat3_num)];           
    // int<lower = 1, upper = 3> M_N_3_cat3[sum(I_N_3_cat3_num)];
    
    /*** theta_C_3 ***/

    // int<lower = 1> C_3_num;                       
    // int<lower = 1, upper = N> I_C_3_num[C_3_num];                  
    // int<lower = 1, upper = N> I_C_3_ind[sum(I_C_3_num)];           
    // vector[sum(I_C_3_num)] M_C_3;               

    /*** theta_R_4 ***/

    // int<lower = 1> R_4_cat3_num;                       
    // int<lower = 1, upper = N> I_R_4_cat3_num[R_4_cat3_num];                  
    // int<lower = 1, upper = N> I_R_4_cat3_ind[sum(I_R_4_cat3_num)];           
    // int<lower = 1, upper = 3> M_R_4_cat3[sum(I_R_4_cat3_num)];  
    // 
    // int<lower = 1> R_4_cat5_num;                       
    // int<lower = 1, upper = N> I_R_4_cat5_num[R_4_cat5_num];                  
    // int<lower = 1, upper = N> I_R_4_cat5_ind[sum(I_R_4_cat5_num)];           
    // int<lower = 1, upper = 5> M_R_4_cat5[sum(I_R_4_cat5_num)];   

    /*** theta_N_4 ***/

    // int<lower = 1> N_4_cat3_num;                       
    // int<lower = 1, upper = N> I_N_4_cat3_num[N_4_cat3_num];                  
    // int<lower = 1, upper = N> I_N_4_cat3_ind[sum(I_N_4_cat3_num)];           
    // int<lower = 1, upper = 3> M_N_4_cat3[sum(I_N_4_cat3_num)];
    
    /*** theta_C_4 ***/

    // int<lower = 1> C_4_num;                       
    // int<lower = 1, upper = N> I_C_4_num[C_4_num];                  
    // int<lower = 1, upper = N> I_C_4_ind[sum(I_C_4_num)];           
    // vector[sum(I_C_4_num)] M_C_4;               
    
    /*** anchors ***/
    
    // int<lower = 1> anchor_num;
    // int<lower = 1> I_anchor_num[anchor_num];
    // int<lower = 1, upper = N> I_anchor_ind[sum(I_anchor_num)];
    // int<lower = 0, upper = 1> anchor[sum(I_anchor_num)];

  /*** relationship indicators ***/

    /* 
      *_N: Number of non-missing observations who were in a relationship in previous period
      *_ind: Indices of non-missing observations who were in a relationship in previous period 
      *: Observed choices for non-missing observations who were in a relationship in previous period
      *_N0: Number of observations not in a relationship this period
      *_ind0: Indicies of observations not in a relationship this period
      *_N1: Number of observations in a relationship this period
      *_ind1: Indicies of observations in a relationship this period
    */

    int<lower = 0, upper = N> R_0_N;
    int<lower = 0, upper = N> R_0_ind[R_0_N];
    int<lower = 0, upper = 1> R_0[N];
    int<lower = 0, upper = N> R_0_N0;
    int<lower = 0, upper = N> R_0_ind0[R_0_N0];
    int<lower = 0, upper = N> R_0_N1;
    int<lower = 0, upper = N> R_0_ind1[R_0_N1];

    int<lower = 0, upper = N> R_1_N;
    int<lower = 0, upper = N> R_1_ind[R_1_N];
    int<lower = 0, upper = 1> R_1[R_1_N];
    int<lower = 0, upper = N> R_1_N0;
    int<lower = 0, upper = N> R_1_ind0[R_1_N0];
    int<lower = 0, upper = N> R_1_N1;
    int<lower = 0, upper = N> R_1_ind1[R_1_N1];
    // 
    // int<lower = 0, upper = N> R_2_N;
    // int<lower = 0, upper = N> R_2_ind[R_2_N];
    // int<lower = 0, upper = 1> R_2[R_2_N];
    // int<lower = 0, upper = N> R_2_N0;
    // int<lower = 0, upper = N> R_2_ind0[R_2_N0];
    // int<lower = 0, upper = N> R_2_N1;
    // int<lower = 0, upper = N> R_2_ind1[R_2_N1];
    // 
    // int<lower = 0, upper = N> R_3_N;
    // int<lower = 0, upper = N> R_3_ind[R_3_N];
    // int<lower = 0, upper = 1> R_3[R_3_N];
    // int<lower = 0, upper = N> R_3_N0;
    // int<lower = 0, upper = N> R_3_ind0[R_3_N0];
    // int<lower = 0, upper = N> R_3_N1;
    // int<lower = 0, upper = N> R_3_ind1[R_3_N1];
    // 
    // int<lower = 0, upper = N> R_4_N;
    // int<lower = 0, upper = N> R_4_ind[R_4_N];
    // int<lower = 0, upper = 1> R_4[R_4_N];
    // int<lower = 0, upper = N> R_4_N0;
    // int<lower = 0, upper = N> R_4_ind0[R_4_N0];
    // int<lower = 0, upper = N> R_4_N1;
    // int<lower = 0, upper = N> R_4_ind1[R_4_N1];

  /*** prior parameters ***/
  
  vector<lower = 0>[R_0_cat3_num] gamma_M_R_0_cat3_mean;
  ordered[2] c_M_R_0_cat3_mean[R_0_cat3_num];
  
  // vector<lower = 0>[R_1_cat3_num] gamma_M_R_1_cat3_mean;
  // ordered[2] c_M_R_1_cat3_mean[R_1_cat3_num];
  // 
  // vector<lower = 0>[R_1_cat5_num] gamma_M_R_1_cat5_mean;
  // ordered[4] c_M_R_1_cat5_mean[R_1_cat5_num];
  // 
  // vector<lower = 0>[N_1_cat5_num] gamma_M_N_1_cat5_mean;
  // ordered[4] c_M_N_1_cat5_mean[N_1_cat5_num];
  // 
  // vector<lower = 0>[R_2_cat3_num] gamma_M_R_2_cat3_mean;
  // ordered[2] c_M_R_2_cat3_mean[R_2_cat3_num];
  // 
  // vector<lower = 0>[R_2_cat5_num] gamma_M_R_2_cat5_mean;
  // ordered[4] c_M_R_2_cat5_mean[R_2_cat5_num];
  // 
  // vector<lower = 0>[N_2_cat3_num] gamma_M_N_2_cat3_mean;
  // ordered[2] c_M_N_2_cat3_mean[N_2_cat3_num];
  // 
  // vector[C_2_num] mu_M_C_2_mean;
  // vector<lower = 0>[C_2_num] sigma_M_C_2_mean;
  // 
  // vector<lower = 0>[R_3_cat3_num] gamma_M_R_3_cat3_mean;
  // ordered[2] c_M_R_3_cat3_mean[R_3_cat3_num];
  // 
  // vector<lower = 0>[R_3_cat5_num] gamma_M_R_3_cat5_mean;
  // ordered[4] c_M_R_3_cat5_mean[R_3_cat5_num];
  // 
  // vector<lower = 0>[N_3_cat3_num] gamma_M_N_3_cat3_mean;
  // ordered[2] c_M_N_3_cat3_mean[N_3_cat3_num];
  // 
  // vector[C_3_num] mu_M_C_3_mean;
  // real<lower = 0> gamma_M_C_3_2_mean;
  // vector<lower = 0>[C_3_num] sigma_M_C_3_mean;
  // 
  // vector<lower = 0>[R_4_cat3_num] gamma_M_R_4_cat3_mean;
  // ordered[2] c_M_R_4_cat3_mean[R_4_cat3_num];
  // 
  // vector<lower = 0>[R_4_cat5_num] gamma_M_R_4_cat5_mean;
  // ordered[4] c_M_R_4_cat5_mean[R_4_cat5_num];
  // 
  // vector<lower = 0>[N_4_cat3_num] gamma_M_N_4_cat3_mean;
  // ordered[2] c_M_N_4_cat3_mean[N_4_cat3_num];
  // 
  // vector[C_4_num] mu_M_C_4_mean;
  // vector<lower = 0>[C_4_num] gamma_M_C_4_mean;
  // vector<lower = 0>[C_4_num] sigma_M_C_4_mean;

}
transformed data {
  
  /*** declare X_R, X_Q ***/

  matrix[X_num, X_num] X_R;
  matrix[N, X_num] X_Q; 
  matrix[N, X_num - 1] X_Q_nocons; // X_Q without constant term

  /*** declare indexes ***/

  int RN_ind[2];
  int RC_ind[2];  
  int NC_ind[2];

  /*** declare relationship indicator vectors for full sample ***/

  // vector[N] R_0_full;
  // vector[N] R_1_full; 
  // vector[N] R_2_full;
  // vector[N] R_3_full;
  // vector[N] R_4_full;

  /*** prior parameters ***/

  real normal_mu_prior = 0; // prior mean for standardized coefficients 
  real<lower = 0> normal_sigma_prior = 3; // prior variance for standardized coefficients and threshold parameters
  
  real lkj_sd_prior = .25; // prior standard deviation for correlation parameters
  real lkj_eta_prior_2 = .5/square(lkj_sd_prior)*(1 - square(lkj_sd_prior)); // convert sd to eta parameter
  real lkj_eta_prior_3 = .5/square(lkj_sd_prior)*(1 - 2*square(lkj_sd_prior)); 
  real lkj_eta_prior_4 = .5/square(lkj_sd_prior)*(1 - 3*square(lkj_sd_prior));   
  
  real gamma_sd_prior = .5; // prior standard deviation for factor loading and st dev parameters
  
  vector<lower = 0>[R_0_cat3_num] gamma_M_R_0_cat3_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_R_0_cat3_mean);
  vector<lower = 0>[R_0_cat3_num] gamma_M_R_0_cat3_beta  = pow(gamma_sd_prior, -2)*gamma_M_R_0_cat3_mean;

  // vector<lower = 0>[R_1_cat3_num] gamma_M_R_1_cat3_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_R_1_cat3_mean);
  // vector<lower = 0>[R_1_cat3_num] gamma_M_R_1_cat3_beta  = pow(gamma_sd_prior, -2)*gamma_M_R_1_cat3_mean;
  // 
  // vector<lower = 0>[R_1_cat5_num] gamma_M_R_1_cat5_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_R_1_cat5_mean);
  // vector<lower = 0>[R_1_cat5_num] gamma_M_R_1_cat5_beta  = pow(gamma_sd_prior, -2)*gamma_M_R_1_cat5_mean;
  // 
  // vector<lower = 0>[N_1_cat5_num] gamma_M_N_1_cat5_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_N_1_cat5_mean);
  // vector<lower = 0>[N_1_cat5_num] gamma_M_N_1_cat5_beta  = pow(gamma_sd_prior, -2)*gamma_M_N_1_cat5_mean;
  // 
  // vector<lower = 0>[R_2_cat3_num] gamma_M_R_2_cat3_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_R_2_cat3_mean);
  // vector<lower = 0>[R_2_cat3_num] gamma_M_R_2_cat3_beta  = pow(gamma_sd_prior, -2)*gamma_M_R_2_cat3_mean;
  // 
  // vector<lower = 0>[R_2_cat5_num] gamma_M_R_2_cat5_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_R_2_cat5_mean);
  // vector<lower = 0>[R_2_cat5_num] gamma_M_R_2_cat5_beta  = pow(gamma_sd_prior, -2)*gamma_M_R_2_cat5_mean;
  // 
  // vector<lower = 0>[N_2_cat3_num] gamma_M_N_2_cat3_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_N_2_cat3_mean);
  // vector<lower = 0>[N_2_cat3_num] gamma_M_N_2_cat3_beta  = pow(gamma_sd_prior, -2)*gamma_M_N_2_cat3_mean;
  // 
  // vector<lower = 0>[C_2_num] sigma_M_C_2_alpha = pow(gamma_sd_prior, -2)*square(sigma_M_C_2_mean);
  // vector<lower = 0>[C_2_num] sigma_M_C_2_beta  = pow(gamma_sd_prior, -2)*sigma_M_C_2_mean;
  // 
  // vector<lower = 0>[R_3_cat3_num] gamma_M_R_3_cat3_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_R_3_cat3_mean);
  // vector<lower = 0>[R_3_cat3_num] gamma_M_R_3_cat3_beta  = pow(gamma_sd_prior, -2)*gamma_M_R_3_cat3_mean;
  // 
  // vector<lower = 0>[R_3_cat5_num] gamma_M_R_3_cat5_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_R_3_cat5_mean);
  // vector<lower = 0>[R_3_cat5_num] gamma_M_R_3_cat5_beta  = pow(gamma_sd_prior, -2)*gamma_M_R_3_cat5_mean;
  // 
  // vector<lower = 0>[N_3_cat3_num] gamma_M_N_3_cat3_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_N_3_cat3_mean);
  // vector<lower = 0>[N_3_cat3_num] gamma_M_N_3_cat3_beta  = pow(gamma_sd_prior, -2)*gamma_M_N_3_cat3_mean;
  // 
  // real<lower = 0> gamma_M_C_3_2_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_C_3_2_mean);
  // real<lower = 0> gamma_M_C_3_2_beta  = pow(gamma_sd_prior, -2)*gamma_M_C_3_2_mean;
  // 
  // vector<lower = 0>[C_3_num] sigma_M_C_3_alpha = pow(gamma_sd_prior, -2)*square(sigma_M_C_3_mean);
  // vector<lower = 0>[C_3_num] sigma_M_C_3_beta  = pow(gamma_sd_prior, -2)*sigma_M_C_3_mean;
  // 
  // vector<lower = 0>[R_4_cat3_num] gamma_M_R_4_cat3_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_R_4_cat3_mean);
  // vector<lower = 0>[R_4_cat3_num] gamma_M_R_4_cat3_beta  = pow(gamma_sd_prior, -2)*gamma_M_R_4_cat3_mean;
  // 
  // vector<lower = 0>[R_4_cat5_num] gamma_M_R_4_cat5_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_R_4_cat5_mean);
  // vector<lower = 0>[R_4_cat5_num] gamma_M_R_4_cat5_beta  = pow(gamma_sd_prior, -2)*gamma_M_R_4_cat5_mean;
  // 
  // vector<lower = 0>[N_4_cat3_num] gamma_M_N_4_cat3_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_N_4_cat3_mean);
  // vector<lower = 0>[N_4_cat3_num] gamma_M_N_4_cat3_beta  = pow(gamma_sd_prior, -2)*gamma_M_N_4_cat3_mean;
  // 
  // vector<lower = 0>[C_4_num] gamma_M_C_4_alpha = pow(gamma_sd_prior, -2)*square(gamma_M_C_4_mean);
  // vector<lower = 0>[C_4_num] gamma_M_C_4_beta  = pow(gamma_sd_prior, -2)*gamma_M_C_4_mean;
  // 
  // vector<lower = 0>[C_4_num] sigma_M_C_4_alpha = pow(gamma_sd_prior, -2)*square(sigma_M_C_4_mean);
  // vector<lower = 0>[C_4_num] sigma_M_C_4_beta  = pow(gamma_sd_prior, -2)*sigma_M_C_4_mean;

  /*** assign X_R and X_Q ***/
    
    X_R = qr_R(X)[1:X_num,] / sqrt(N - 1);
    X_Q = qr_Q(X)[, 1:X_num] * sqrt(N - 1); 
    X_Q_nocons = X_Q[,2:X_num];

  /*** assign indexes ***/  
  
    RN_ind[1] = 1;
    RN_ind[2] = 2;
    RC_ind[1] = 1;
    RC_ind[2] = 3;
    NC_ind[1] = 2;
    NC_ind[2] = 3;

  /*** assign relationship indicators ***/

    // R_0_full           = to_vector(R_0);
    // R_1_full[R_0_ind0] = rep_vector(0, R_0_N0);
    // R_1_full[R_1_ind]  = to_vector(R_1);
    // R_2_full[R_1_ind0] = rep_vector(0, R_1_N0);
    // R_2_full[R_2_ind]  = to_vector(R_2);
    // R_3_full[R_2_ind0] = rep_vector(0, R_2_N0);
    // R_3_full[R_3_ind]  = to_vector(R_3);
    // R_4_full[R_3_ind0] = rep_vector(0, R_3_N0);
    // R_4_full[R_4_ind]  = to_vector(R_4);

}
parameters {

/*** lambda ***/

  // matrix[N,3] lambda_raw;
  // cholesky_factor_corr[3] L_corr_lambda;
  // vector[3] c;
  // vector[4] c_p;

/*** theta_0 ***/

  matrix[X_num-1,1] alpha_0_tilde_raw;
  matrix[R_0_N1,1] epsilon_0;

/*** theta_1 ***/

  // matrix[X_num-1,2] alpha_1_tilde_raw;
  // matrix[X_num,1] beta_1_raw;
  // row_vector[2]   gamma_1_raw;
  // row_vector[1]   xi_1_raw;
  // matrix[X_num, 1] delta_1_raw;
  // cholesky_factor_corr[2] L_corr_1;
  // 
  // matrix[R_1_N1, 2] epsilon_1;
  // vector[R_1_N0]    epsilon_N_1_R1eq0;

/*** theta_2 ***/

  // matrix[X_num-1,3] alpha_2_tilde_raw;
  // matrix[X_num,2] beta_2_raw;
  // row_vector[5]   gamma_2_raw;
  // row_vector[2]   xi_2_raw;
  // matrix[X_num, 2] delta_2_raw;
  // cholesky_factor_corr[3] L_corr_2; 
  // 
  // matrix[R_2_N1, 3] epsilon_2;
  // matrix[R_2_N0, 2] epsilon_NC_2_R2eq0; 

/*** theta_3 ***/

  // matrix[X_num-1,3] alpha_3_tilde_raw;
  // matrix[X_num,2] beta_3_raw;
  // row_vector[7]   gamma_3_raw;
  // row_vector[2]   xi_3_raw;
  // matrix[X_num, 2] delta_3_raw;
  // cholesky_factor_corr[3] L_corr_3; 
  // 
  // matrix[R_3_N1, 3] epsilon_3;
  // matrix[R_3_N0, 2] epsilon_NC_3_R3eq0; 

/*** theta_4 ***/
  
  // matrix[X_num-1,3] alpha_4_tilde_raw;
  // matrix[X_num,2] beta_4_raw;
  // row_vector[7]     gamma_4_raw;
  // row_vector[2]     xi_4_raw;
  // matrix[X_num,2] delta_4_raw;
  // cholesky_factor_corr[3] L_corr_4; 
  // 
  // matrix[R_4_N1, 3] epsilon_4;
  // matrix[R_4_N0, 2] epsilon_NC_4_R4eq0;

/*** M_R_0 ***/

  vector<lower = 0>[R_0_cat3_num] gamma_M_R_0_cat3;
  ordered[2] c_M_R_0_cat3[R_0_cat3_num];
  
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

  // vector[C_2_num] mu_M_C_2;
  // vector<lower = 0>[C_2_num] sigma_M_C_2;

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
  // vector<lower = 0>[C_3_num] sigma_M_C_3;
  // 
  // real<lower = 0> gamma_M_C_3_2;

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

  // vector<lower = 0>[C_4_num] gamma_M_C_4;
  // vector[C_4_num] mu_M_C_4;
  // vector<lower = 0>[C_4_num] sigma_M_C_4;

/*** relationship indicators ***/

  matrix[X_num,1] alpha_p_tilde;
  matrix[2,1] gamma_p_;

/*** anchors ***/
  
  // matrix[X_num, anchor_num] alpha_anchor_tilde; 
  // matrix[2, anchor_num] gamma_anchor;

}
transformed parameters {

/*** declare lambda ***/

//  matrix[N,3] lambda;

/*** declare structural parameters ***/

//  matrix[2,2] c_NC_diag = diag_matrix(c[2:3]);

  matrix[X_num,1] alpha_0_tilde;
  row_vector[1] sigma_0;
//  vector[1] c_0;
  
// matrix[X_num,2] alpha_1_tilde;
// matrix[X_num,1] beta_1_tilde;
// matrix[1,2] gamma_1;
// row_vector[1] xi_1;
// matrix[X_num,1] delta_1_tilde;
// vector[2] c_1;

  // matrix[X_num,3] alpha_2_tilde;
  // matrix[X_num,2] beta_2_tilde;
  // matrix[2,3] gamma_2; 
  // row_vector[2] xi_2;
  // matrix[X_num,2] delta_2_tilde;
  // vector[3] c_2;

  // matrix[X_num,3] alpha_3_tilde;
  // matrix[X_num,2] beta_3_tilde;
  // matrix[3,3] gamma_3; 
  // row_vector[2] xi_3;
  // matrix[X_num,2] delta_3_tilde;
  // vector[3] c_3;

  // matrix[X_num,3] alpha_4_tilde;
  // matrix[X_num,2] beta_4_tilde;
  // matrix[3,3] gamma_4; 
  // row_vector[2] xi_4;
  // matrix[X_num,2] delta_4_tilde;
  // vector[3] c_4;

/*** declare thetas ***/ 

  matrix[N, 1] theta_0 = rep_matrix(0, N, 1);
  // matrix[N, 2] theta_1 = rep_matrix(0, N, 2);
  // matrix[N, 3] theta_2 = rep_matrix(0, N, 3);
  // matrix[N, 3] theta_3 = rep_matrix(0, N, 3);
  // matrix[N, 3] theta_4 = rep_matrix(0, N, 3);

/*** declare measurement parameters ***/

  // vector<lower = 0>[C_2_num] gamma_M_C_2;
  // vector<lower = 0>[C_3_num] gamma_M_C_3;

/*** assign lambda ***/
{
  
  // matrix[N,3] lambda_ = lambda_raw * L_corr_lambda';
  // 
  // for (i in 1:3){
  //   
  //   lambda[,i] = standardize(lambda_[,i]);
  // 
  // }
  
}
/*** assign theta_0 ***/ 
{
    vector[R_0_N1] theta_R_0 = rep_vector(0,R_0_N1);

    real theta_R_0_mean;
    real theta_R_0_sd;

  /* generate unnormalized latent variables */

    theta_R_0 = 
      X_Q_nocons[R_0_ind1,] * alpha_0_tilde_raw[,1] + 
//      lambda[R_0_ind1,1]*c[1] + 
      epsilon_0[,1];
    
  /* normalize latent variable */
    
    theta_R_0_mean = mean(theta_R_0);
    theta_R_0_sd   = sd(theta_R_0);

    theta_0[R_0_ind1,1] = (theta_R_0 - theta_R_0_mean)/theta_R_0_sd;
    
  /* normalize parameters */

    alpha_0_tilde[1,1]       = -theta_R_0_mean/theta_R_0_sd;
    alpha_0_tilde[2:X_num,] = alpha_0_tilde_raw/theta_R_0_sd;
    
//    c_0[1] = c[1]/theta_R_0_sd;
    sigma_0[1] = 1/theta_R_0_sd;

}
/*** assign theta_1 ***/ 
{
//     vector[R_1_N1] theta_R_1 = rep_vector(0,R_1_N1);
//     vector[N]      theta_N_1 = rep_vector(0,N);
// 
//     row_vector[2] theta_1_mean;
//     row_vector[2] theta_1_sd;
//     
//   /* generate unnormalized latent variables */
// 
//     theta_R_1  =
//       X_Q_nocons[R_1_ind1,] * alpha_1_tilde_raw[,1] +
//       theta_0[R_1_ind1,1] * gamma_1_raw[1] +
// //      lambda[R_1_ind1,1] * c[1] +
//       epsilon_1[,1];
// 
//     theta_N_1[R_1_ind1]  = // theta_N_1 if R_1 = 1
//       X_Q_nocons[R_1_ind1,] * alpha_1_tilde_raw[,2] + 
//       X_Q[R_1_ind1,] * (beta_1_raw[,1] + delta_1_raw[,1]) +
//       theta_0[R_1_ind1,1] * (gamma_1_raw[2] + xi_1_raw[1]) +
// //      lambda[R_1_ind1,2] * c[2] +
//       epsilon_1 * L_corr_1[2,]';
// 
//     theta_N_1[R_1_ind0] = // theta_N_1 if R_1 = 0
//       X_Q_nocons[R_1_ind0,] * alpha_1_tilde_raw[,2] +
//       R_0_full[R_1_ind0] .* (X_Q[R_1_ind0,] * beta_1_raw[,1]) + 
//       theta_0[R_1_ind0,1] * gamma_1_raw[2] +
// //      lambda[R_1_ind0,2] * c[2] +
//       epsilon_N_1_R1eq0;
// 
//   /* normalize latent variable */
// 
//     theta_1_mean[1] = mean(theta_R_1);
//     theta_1_mean[2] = mean(theta_N_1);
//     
//     theta_1_sd[1]   = sd(theta_R_1);
//     theta_1_sd[2]   = sd(theta_N_1);
// 
//     theta_1[R_1_ind1,1] = (theta_R_1 - theta_1_mean[1])/theta_1_sd[1];
//     theta_1[,2]         = (theta_N_1 - theta_1_mean[2])/theta_1_sd[2];
// 
//   /* normalize parameters */
// 
//     alpha_1_tilde[1,]        = -theta_1_mean ./ theta_1_sd;
//     alpha_1_tilde[2:X_num,]  = alpha_1_tilde_raw ./ (rep_vector(1, X_num - 1) * theta_1_sd);
//     
//     
//     beta_1_tilde = beta_1_raw / theta_1_sd[2];
//     gamma_1[1,] = gamma_1_raw ./ theta_1_sd;
//     xi_1        = xi_1_raw / theta_1_sd[2];
//     
//     delta_1_tilde = delta_1_raw / theta_1_sd[2];
// 
//  //   c_1 = c[1:2] ./ theta_1_sd';

  } 
/*** assign theta_2 ***/ 
{
//   vector[R_2_N1] theta_R_2 = rep_vector(0,R_2_N1);
//   matrix[N,2]    theta_NC_2 = rep_matrix(0,N,2);
// 
//   row_vector[3] theta_2_mean;
//   row_vector[3] theta_2_sd;
//   
//   /* place gamma and xi into matricies for easier manipulation */
//   
//   matrix[2,3] gamma_2_ = rep_matrix(0.,2,3); 
//   matrix[2,2] xi_2_ = rep_matrix(0.,2,2);
//   matrix[2,2] L_corr_2_R2eq0 = cholesky_decompose(tcrossprod(L_corr_2[2:3,]));
// 
//   gamma_2_[1,1]      = gamma_2_raw[1];
//   gamma_2_[1,2:3]    = gamma_2_raw[2:3];
//   gamma_2_[2,2:3]    = gamma_2_raw[4:5];
//   xi_2_[1,] = xi_2_raw;
// 
//   /* generate unnormalized latent variables */
// 
//   theta_R_2 = 
//     X_Q_nocons[R_2_ind1,] * alpha_2_tilde_raw[,1] + 
//     theta_1[R_2_ind1,1] * gamma_2_raw[1] + 
// //    lambda[R_2_ind1,1] * c[1] +
//     epsilon_2[,1];
// 
//   theta_NC_2[R_2_ind1,] = // theta_NC_2 if R_2 = 1
//     X_Q_nocons[R_2_ind1,] * alpha_2_tilde_raw[,2:3] + 
//     X_Q[R_2_ind1,] * (beta_2_raw + delta_2_raw) + 
//     theta_1[R_2_ind1,] * (gamma_2_[,2:3] + xi_2_) + 
// //    lambda[R_2_ind1,2:3] * c_NC_diag +
//     epsilon_2 * L_corr_2[2:3,]';
// 
//   theta_NC_2[R_2_ind0,] = // theta_NC_2 if R_2 = 0
//     X_Q_nocons[R_2_ind0,] * alpha_2_tilde_raw[,2:3] + 
//     (R_1_full[R_2_ind0] * rep_row_vector(1, 2)) .* (X_Q[R_2_ind0,] * beta_2_raw) + 
//     theta_1[R_2_ind0,] * gamma_2_[,2:3] + 
// //    lambda[R_2_ind0,2:3] * c_NC_diag +
//     epsilon_NC_2_R2eq0 * L_corr_2_R2eq0';
// 
//   /* normalize latent variables */
// 
//   theta_2_mean[1] = mean(theta_R_2);
//   theta_2_mean[2] = mean(theta_NC_2[,1]);
//   theta_2_mean[3] = mean(theta_NC_2[,2]);
// 
//   theta_2_sd[1] = sd(theta_R_2);
//   theta_2_sd[2] = sd(theta_NC_2[,1]);
//   theta_2_sd[3] = sd(theta_NC_2[,2]);
// 
//   theta_2[R_2_ind1,1] = (theta_R_2 - theta_2_mean[1])/theta_2_sd[1];
//   theta_2[,2]         = (theta_NC_2[,1] - theta_2_mean[2])/theta_2_sd[2];
//   theta_2[,3]         = (theta_NC_2[,2] - theta_2_mean[3])/theta_2_sd[3];
// 
//   /* normalize parameters */
// 
//   alpha_2_tilde[1,] = -theta_2_mean ./ theta_2_sd;
//   alpha_2_tilde[2:X_num,] = alpha_2_tilde_raw ./ (rep_vector(1,X_num - 1) * theta_2_sd);
//   
//   beta_2_tilde = beta_2_raw ./ (rep_vector(1, X_num) * theta_2_sd[2:3]);
// 
//   gamma_2 = gamma_2_ ./ (rep_vector(1,2) * theta_2_sd);
// 
//   xi_2    = xi_2_raw ./ theta_2_sd[2:3];
// 
//   delta_2_tilde = delta_2_raw ./ (rep_vector(1, X_num) * theta_2_sd[2:3]);
//   
// //  c_2 = c[1:3] ./ theta_2_sd';

  }
/*** assign theta_3 ***/ 
{
//   vector[R_3_N1] theta_R_3  = rep_vector(0,R_3_N1);
//   matrix[N,2]    theta_NC_3 = rep_matrix(0,N,2);
// 
//   row_vector[3] theta_3_mean = rep_row_vector(0, 3);
//   row_vector[3] theta_3_sd = rep_row_vector(0, 3);
// 
//   matrix[2,2] L_corr_3_R3eq0 = cholesky_decompose(tcrossprod(L_corr_3)[2:3,2:3]);
// 
//   /* place gamma and xi into matricies for easier manipulation */
// 
//   matrix[3,3] gamma_3_ = rep_matrix(0.,3,3); 
//   matrix[3,2] xi_3_ = rep_matrix(0.,3,2);
// 
//   gamma_3_[1,]      = gamma_3_raw[1:3];
//   gamma_3_[2,2:3]   = gamma_3_raw[4:5];
//   gamma_3_[3,2:3]   = gamma_3_raw[6:7];
//   
//   xi_3_[1,] = xi_3_raw;
// 
//   /* generate unnormalized latent variables */
// 
//   theta_R_3 = 
//     X_Q_nocons[R_3_ind1,] * alpha_3_tilde_raw[,1] +
//     theta_2[R_3_ind1,1] * gamma_3_[1,1] + 
// //    lambda[R_3_ind1,1] * c[1] +
//     epsilon_3[,1];
// 
//   theta_NC_3[R_3_ind1,] = // theta_NC_3 if R_3 = 1
//     X_Q_nocons[R_3_ind1,] * alpha_3_tilde_raw[,2:3] + 
//     X_Q[R_3_ind1,] * (beta_3_raw[,1:2] + delta_3_raw[,1:2]) + 
//     theta_2[R_3_ind1,] * (gamma_3_[,2:3] + xi_3_) + 
// //  lambda[R_3_ind1,2:3] * c_NC_diag +
//     epsilon_3*L_corr_3[2:3,]';
// 
//   theta_NC_3[R_3_ind0,] = // theta_NC_3 if R_3 = 0
//     X_Q_nocons[R_3_ind0,] * alpha_3_tilde_raw[,2:3] + 
//     (R_2_full[R_3_ind0] * rep_row_vector(1,2)) .* (X_Q[R_3_ind0,] * beta_3_raw[,1:2]) +
//     theta_2[R_3_ind0,] * gamma_3_[,2:3] + 
// //    lambda[R_3_ind0,2:3] * c_NC_diag +
//     epsilon_NC_3_R3eq0*L_corr_3_R3eq0';
//     
//   /* normalize latent variables */
//     
//   theta_3_mean[1] = mean(theta_R_3);
//   theta_3_mean[2] = mean(theta_NC_3[,1]);
//   theta_3_mean[3] = mean(theta_NC_3[,2]);
//   
//   theta_3_sd[1] = sd(theta_R_3);
//   theta_3_sd[2] = sd(theta_NC_3[,1]);
//   theta_3_sd[3] = sd(theta_NC_3[,2]);
//   
//   theta_3[R_3_ind1,1] = (theta_R_3 - theta_3_mean[1])/theta_3_sd[1];
//   theta_3[,2]         = (theta_NC_3[,1] - theta_3_mean[2])/theta_3_sd[2];
//   theta_3[,3]         = (theta_NC_3[,2] - theta_3_mean[3])/theta_3_sd[3];
// 
//   /* normalize parameters */
// 
//   alpha_3_tilde[1,] = -theta_3_mean ./ theta_3_sd;
//   alpha_3_tilde[2:X_num,] = alpha_3_tilde_raw ./ (rep_vector(1,X_num - 1) * theta_3_sd);
// 
//   beta_3_tilde = beta_3_raw ./ (rep_vector(1, X_num) * theta_3_sd[2:3]);
// 
//   gamma_3 = gamma_3_ ./ (rep_vector(1,3) * theta_3_sd);
//   xi_3    = xi_3_raw ./ theta_3_sd[2:3];
// 
//   delta_3_tilde = delta_3_raw ./ (rep_vector(1, X_num) * theta_3_sd[2:3]);
// 
// //  c_3 = c[1:3] ./ theta_3_sd';

} 
/*** assign theta_4 ***/ 
{
//   vector[R_4_N1] theta_R_4  = rep_vector(0,R_4_N1);
//   matrix[N,2]    theta_NC_4 = rep_matrix(0,N,2);
// 
//   row_vector[3] theta_4_mean;
//   row_vector[3] theta_4_sd;
// 
//   matrix[2,2] L_corr_4_R4eq0 = cholesky_decompose(tcrossprod(L_corr_4[2:3,]));
// 
//   /* place gamma and xi into matricies for easier manipulation */
// 
//   matrix[3,3] gamma_4_ = rep_matrix(0.,3,3); 
//   matrix[3,2] xi_4_ = rep_matrix(0.,3,2);
// 
//   gamma_4_[1,]      = gamma_4_raw[1:3];
//   gamma_4_[2,2:3]   = gamma_4_raw[4:5];
//   gamma_4_[3,2:3]   = gamma_4_raw[6:7];
//   
//   xi_4_[1,] = xi_4_raw;
// 
//   /* generate unnormalized latent variables */
// 
//   theta_R_4 = 
//     X_Q_nocons[R_4_ind1,] * alpha_4_tilde_raw[,1] +
//     theta_3[R_4_ind1,1] * gamma_4_[1,1] + 
// //    lambda[R_4_ind1,1] * c[1] +
//     epsilon_4[,1];
// 
//   theta_NC_4[R_4_ind1,] = // theta_NC_4 if R_4 = 1
//     X_Q_nocons[R_4_ind1,] * alpha_4_tilde_raw[,2:3] +
//     X_Q[R_4_ind1,] *(beta_4_raw + delta_4_raw) + 
//     theta_3[R_4_ind1,] * (gamma_4_[,2:3] + xi_4_) + 
// //    lambda[R_4_ind1,2:3] * c_NC_diag +
//     epsilon_4* L_corr_4[2:3,]';
// 
//   theta_NC_4[R_4_ind0,] = // theta_NC_4 if R_4 = 0
//     X_Q_nocons[R_4_ind0,] * alpha_4_tilde_raw[,2:3] + 
//     (R_3_full[R_4_ind0] * rep_row_vector(1,2)) .* (X_Q[R_4_ind0,] * beta_4_raw) +
//     theta_3[R_4_ind0,] * gamma_4_[,2:3] + 
// //    lambda[R_4_ind0,2:3] * c_NC_diag +
//     epsilon_NC_4_R4eq0*L_corr_4_R4eq0';
// 
//   /* normalize latent variables */
//     
//   theta_4_mean[1] = mean(theta_R_4);
//   theta_4_mean[2] = mean(theta_NC_4[,1]);
//   theta_4_mean[3] = mean(theta_NC_4[,2]);
//   
//   theta_4_sd[1] = sd(theta_R_4);
//   theta_4_sd[2] = sd(theta_NC_4[,1]);
//   theta_4_sd[3] = sd(theta_NC_4[,2]);
// 
//   theta_4[R_4_ind1,1] = (theta_R_4 - theta_4_mean[1])/theta_4_sd[1];
//   theta_4[,2]         = (theta_NC_4[,1] - theta_4_mean[2])/theta_4_sd[2];
//   theta_4[,3]         = (theta_NC_4[,2] - theta_4_mean[3])/theta_4_sd[3];
// 
//   /* normalize parameters */
//   
//   alpha_4_tilde[1,] = -theta_4_mean ./ theta_4_sd;
//   alpha_4_tilde[2:X_num,] = alpha_4_tilde_raw ./ (rep_vector(1,X_num - 1) * theta_4_sd);
// 
//   beta_4_tilde = beta_4_raw ./ (rep_vector(1, X_num) * theta_4_sd[2:3]);
// 
//   gamma_4 = gamma_4_ ./ (rep_vector(1,3) * theta_4_sd);
//   xi_4    = xi_4_raw ./ theta_4_sd[2:3];
// 
//   delta_4_tilde = delta_4_raw ./ (rep_vector(1, X_num) * theta_4_sd[2:3]);
// 
// //  c_4 = c[1:3] ./ theta_4_sd';

}
/*** assign measurement parameters ***/

  // gamma_M_C_2[1] = gamma_M_C_4[1];
  // gamma_M_C_3[1] = gamma_M_C_4[1];
  // gamma_M_C_3[2] = gamma_M_C_3_2;

}
model {

/*** priors ***/

  /*** lambda coefficients ***/

  // c ~ gamma(4,4);
  // c_p ~ normal(normal_mu_prior, normal_sigma_prior);
  
  /*** indicators ***/

  to_vector(alpha_p_tilde)
    ~ normal(normal_mu_prior, normal_sigma_prior);
  to_vector(gamma_p_) ~ normal(normal_mu_prior, normal_sigma_prior);

  /*** theta_0 ***/
  
  to_vector(alpha_0_tilde_raw) ~ normal(normal_mu_prior, normal_sigma_prior);

  /*** theta_1 ***/

  // to_vector(alpha_1_tilde_raw) 
  //             ~ normal(normal_mu_prior, normal_sigma_prior);
  // to_vector(beta_1_raw) 
  //             ~ normal(normal_mu_prior, normal_sigma_prior);
  // gamma_1_raw ~ normal(normal_mu_prior, normal_sigma_prior);
  // xi_1_raw    ~ normal(normal_mu_prior, normal_sigma_prior);
  // to_vector(delta_1_raw)
  //             ~ normal(normal_mu_prior, normal_sigma_prior);
  // L_corr_1    ~ lkj_corr_cholesky(lkj_eta_prior_2); 

  /*** theta_2 ***/

  // to_vector(alpha_2_tilde_raw)  
  //             ~ normal(normal_mu_prior, normal_sigma_prior);
  // to_vector(beta_2_raw)
  //             ~ normal(normal_mu_prior, normal_sigma_prior);
  // gamma_2_raw ~ normal(normal_mu_prior, normal_sigma_prior);
  // xi_2_raw    ~ normal(normal_mu_prior, normal_sigma_prior);
  // to_vector(delta_2_raw)
  //             ~ normal(normal_mu_prior, normal_sigma_prior);
  // L_corr_2    ~ lkj_corr_cholesky(lkj_eta_prior_3); 

  /*** theta_3 ***/

  // to_vector(alpha_3_tilde_raw)  
  //             ~ normal(normal_mu_prior, normal_sigma_prior);
  // to_vector(beta_3_raw)
  //             ~ normal(normal_mu_prior, normal_sigma_prior);
  // gamma_3_raw ~ normal(normal_mu_prior, normal_sigma_prior);
  // xi_3_raw    ~ normal(normal_mu_prior, normal_sigma_prior);
  // to_vector(delta_3_raw)
  //             ~ normal(normal_mu_prior, normal_sigma_prior);
  // L_corr_3    ~ lkj_corr_cholesky(lkj_eta_prior_3); 

  /*** theta_4 ***/

  // to_vector(alpha_4_tilde_raw)  
  //             ~ normal(normal_mu_prior, normal_sigma_prior);
  // to_vector(beta_4_raw)
  //             ~ normal(normal_mu_prior, normal_sigma_prior);
  // gamma_4_raw ~ normal(normal_mu_prior, normal_sigma_prior);
  // xi_4_raw    ~ normal(normal_mu_prior, normal_sigma_prior);
  // to_vector(delta_4_raw)
  //             ~ normal(normal_mu_prior, normal_sigma_prior);
  // L_corr_4    ~ lkj_corr_cholesky(lkj_eta_prior_3); 

  /*** anchors ***/
  
  // to_vector(alpha_anchor_tilde) ~ normal(normal_mu_prior, normal_sigma_prior);
  // to_vector(gamma_anchor) ~ normal(normal_mu_prior, normal_sigma_prior);

  /*** M_R_0 ***/  
  
  gamma_M_R_0_cat3 ~ gamma(gamma_M_R_0_cat3_alpha, gamma_M_R_0_cat3_beta);
  
  for (m in 1:R_0_cat3_num){
    c_M_R_0_cat3[m] ~ normal(c_M_R_0_cat3_mean[m], normal_sigma_prior);
  }

  /*** M_R_1 ***/  
  
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

  /*** M_N_1 ***/  
  
  // gamma_M_N_1_cat5 ~ gamma(gamma_M_N_1_cat5_alpha, gamma_M_N_1_cat5_beta);
  // 
  // for (m in 1:N_1_cat5_num){
  //   c_M_N_1_cat5[m] ~ normal(c_M_N_1_cat5_mean[m], normal_sigma_prior);
  // }
  
  /*** M_R_2 ***/  
  
  // gamma_M_R_2_cat3 ~ gamma(gamma_M_R_2_cat3_alpha, gamma_M_R_2_cat3_beta);
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
  
  // gamma_M_N_2_cat3 ~ gamma(gamma_M_N_2_cat3_alpha, gamma_M_N_2_cat3_beta);
  // 
  // for (m in 1:N_2_cat3_num){
  //   c_M_N_2_cat3[m] ~ normal(c_M_N_2_cat3_mean[m], normal_sigma_prior);
  // }

  /*** M_C_2 ***/  
  
  // mu_M_C_2 ~ normal(mu_M_C_2_mean, normal_sigma_prior);  
  // sigma_M_C_2 ~ gamma(sigma_M_C_2_alpha, sigma_M_C_2_beta); 

  /*** M_R_3 ***/  
  
  // gamma_M_R_3_cat3 ~ gamma(gamma_M_R_3_cat3_alpha, gamma_M_R_3_cat3_beta);
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
  // /*** M_N_3 ***/  
  // 
  // gamma_M_N_3_cat3 ~ gamma(gamma_M_N_3_cat3_alpha, gamma_M_N_3_cat3_beta);
  // 
  // for (m in 1:N_3_cat3_num){
  //   c_M_N_3_cat3[m] ~ normal(c_M_N_3_cat3_mean[m], normal_sigma_prior);
  // }

  /*** M_C_3 ***/  
  
  // mu_M_C_3 ~ normal(mu_M_C_3_mean, normal_sigma_prior);  
  // sigma_M_C_3 ~ gamma(sigma_M_C_3_alpha, sigma_M_C_3_beta); 
  // 
  // gamma_M_C_3_2 ~ gamma(gamma_M_C_3_2_alpha, gamma_M_C_3_2_beta);

  /*** M_R_4 ***/  

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

  /*** M_N_4 ***/  
  
  // gamma_M_N_4_cat3 ~ gamma(gamma_M_N_4_cat3_alpha, gamma_M_N_4_cat3_beta);
  // 
  // for (m in 1:N_4_cat3_num){
  //   c_M_N_4_cat3[m] ~ normal(c_M_N_4_cat3_mean[m], normal_sigma_prior);
  // }

  /*** M_C_4 ***/  
  
  // mu_M_C_4 ~ normal(mu_M_C_4_mean, normal_sigma_prior);  
  // sigma_M_C_4 ~ gamma(sigma_M_C_4_alpha, sigma_M_C_4_beta); 
  // 
  // gamma_M_C_4 ~ gamma(gamma_M_C_4_alpha, gamma_M_C_4_beta);

/*** state variables ***/

  /*** lambda ***/

  // to_vector(lambda_raw)  ~ normal(0,1);
  // L_corr_lambda  ~ lkj_corr_cholesky(lkj_eta_prior_3);

  /*** theta_0 ***/

  to_vector(epsilon_0) ~ normal(0,1);

  /*** theta_1 ***/

  // to_vector(epsilon_1) ~ normal(0,1);
  // epsilon_N_1_R1eq0    ~ normal(0,1);

  /*** theta_2 ***/

  // to_vector(epsilon_2) ~ normal(0,1);
  // to_vector(epsilon_NC_2_R2eq0) 
  //                      ~ normal(0,1);

  /*** theta_3 ***/

  // to_vector(epsilon_3) ~ normal(0,1);
  // to_vector(epsilon_NC_3_R3eq0) 
  //                      ~ normal(0,1);

  /*** theta_4 ***/

  // to_vector(epsilon_4) ~ normal(0,1);
  // to_vector(epsilon_NC_4_R4eq0) 
  //                      ~ normal(0,1);

/*** relationship indicators ***/

  R_1 ~
    bernoulli(
      Phi_approx(
        X_Q[R_1_ind] * alpha_p_tilde[,1] +
        theta_0[R_1_ind,1] * gamma_p_[1,1] +
        square(theta_0[R_1_ind,1]) * gamma_p_[2,1] // +
//        lambda[R_1_ind,1] * c_p[1] // +
//        lambda[,4] * c[4]
        )
      );
//   
//   R_2 ~ 
//     bernoulli(
//       Phi_approx(
//         X_Q[R_2_ind,] * alpha_p_tilde[,2] +
//         theta_1[R_2_ind,1] * gamma_p_[1,2] + 
//         square(theta_1[R_2_ind,1]) * gamma_p_[2,2] // + 
// //        lambda[R_2_ind,1] * c_p[2] // +
// //        lambda[R_2_ind,4] * c[4]
//       ));
// 
//   R_3 ~ 
//     bernoulli(
//       Phi_approx(
//         X_Q[R_3_ind,] * alpha_p_tilde[,3] +
//         theta_2[R_3_ind,1] * gamma_p_[1,3] +
//         square(theta_2[R_3_ind,1]) * gamma_p_[2,3] // +
// //        lambda[R_3_ind,1] * c_p[3] // + 
// //        lambda[R_3_ind,4] * c[4]
//       )
//     );
// 
//   R_4 ~ 
//     bernoulli(
//       Phi_approx(
//         X_Q[R_4_ind,] * alpha_p_tilde[,4] +
//         theta_3[R_4_ind,1] * gamma_p_[1,4] +
//         square(theta_3[R_4_ind,1]) * gamma_p_[2,4] // +
// //        lambda[R_4_ind,1] * c_p[4] // + 
// //        lambda[R_4_ind,4] * c[4]
//       )
//     );
    
/*** measurements ***/

  /*** theta_R_0 ***/
  {
      int pos = 1;
      for (m in 1:R_0_cat3_num){
        for (n in pos:(pos + I_R_0_cat3_num[m] - 1)){
          int ind_ = I_R_0_cat3_ind[n];
          M_R_0_cat3[n] ~ ordered_logistic(gamma_M_R_0_cat3[m]*theta_0[ind_,1], c_M_R_0_cat3[m]);
        }
        pos = pos + I_R_0_cat3_num[m];
      }
  } 
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
  // {
  //     int pos = 1;
  //     for (m in 1:R_2_cat3_num){
  //       for (n in pos:(pos + I_R_2_cat3_num[m] - 1)){
  //         int ind_ = I_R_2_cat3_ind[n];
  //         M_R_2_cat3[n] ~ ordered_logistic(gamma_M_R_2_cat3[m]*theta_2[ind_,1], c_M_R_2_cat3[m]);
  //       }
  //       pos = pos + I_R_2_cat3_num[m];
  //     }
  // }
  // {
  //     int pos = 1;
  //     for (m in 1:R_2_cat5_num){
  //       for (n in pos:(pos + I_R_2_cat5_num[m] - 1)){
  //         int ind_ = I_R_2_cat5_ind[n];
  //         M_R_2_cat5[n] ~ ordered_logistic(gamma_M_R_2_cat5[m]*theta_2[ind_,1], c_M_R_2_cat5[m]);
  //       }
  //       pos = pos + I_R_2_cat5_num[m];
  //     }
  // } 
  /*** theta_N_2 ***/
  // {
  //     int pos = 1;
  //     for (m in 1:N_2_cat3_num){
  //       for (n in pos:(pos + I_N_2_cat3_num[m] - 1)){
  //         int ind_ = I_N_2_cat3_ind[n];
  //         M_N_2_cat3[n] ~ ordered_logistic(gamma_M_N_2_cat3[m]*theta_2[ind_,2], c_M_N_2_cat3[m]);
  //       }
  //       pos = pos + I_N_2_cat3_num[m];
  //     }
  // } 
  /*** theta_C_2 ***/
  // {
  //     int pos = 1;
  //     for (m in 1:C_2_num){
  //       M_C_2[pos:(pos + I_C_2_num[m] - 1)] ~ 
  //           normal(
  //             mu_M_C_2[m] + gamma_M_C_2[m] * theta_2[I_C_2_ind[pos:(pos + I_C_2_num[m] - 1)],3],
  //             sigma_M_C_2[m]
  //         );
  //       pos = pos + I_C_2_num[m];
  //       }
  // } 
  /*** theta_R_3 ***/
  // {
  //     int pos = 1;
  //     for (m in 1:R_3_cat3_num){
  //       for (n in pos:(pos + I_R_3_cat3_num[m] - 1)){
  //         int ind_ = I_R_3_cat3_ind[n];
  //         M_R_3_cat3[n] ~ ordered_logistic(gamma_M_R_3_cat3[m]*theta_3[ind_,1], c_M_R_3_cat3[m]);
  //       }
  //       pos = pos + I_R_3_cat3_num[m];
  //     }
  // }
  // {
  //     int pos = 1;
  //     for (m in 1:R_3_cat5_num){
  //       for (n in pos:(pos + I_R_3_cat5_num[m] - 1)){
  //         int ind_ = I_R_3_cat5_ind[n];
  //         M_R_3_cat5[n] ~ ordered_logistic(gamma_M_R_3_cat5[m]*theta_3[ind_,1], c_M_R_3_cat5[m]);
  //       }
  //       pos = pos + I_R_3_cat5_num[m];
  //     }
  // } 
  /*** theta_N_3 ***/
  // {
  //     int pos = 1;
  //     for (m in 1:N_3_cat3_num){
  //       for (n in pos:(pos + I_N_3_cat3_num[m] - 1)){
  //         int ind_ = I_N_3_cat3_ind[n];
  //         M_N_3_cat3[n] ~ ordered_logistic(gamma_M_N_3_cat3[m]*theta_3[ind_,2], c_M_N_3_cat3[m]);
  //       }
  //       pos = pos + I_N_3_cat3_num[m];
  //     }
  // } 
  /*** theta_C_3 ***/
  // {
  //     int pos = 1;
  //     for (m in 1:C_3_num){
  //       M_C_3[pos:(pos + I_C_3_num[m] - 1)] ~ 
  //           normal(
  //             mu_M_C_3[m] + gamma_M_C_3[m] * theta_3[I_C_3_ind[pos:(pos + I_C_3_num[m] - 1)],3],
  //             sigma_M_C_3[m]
  //         );
  //       pos = pos + I_C_3_num[m];
  //       }
  // } 
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
  
  matrix[X_num,1] alpha_0 = X_R\alpha_0_tilde;
  // matrix[X_num,2] alpha_1 = X_R\alpha_1_tilde;
  // matrix[X_num,3] alpha_2 = X_R\alpha_2_tilde;
  // matrix[X_num,3] alpha_3 = X_R\alpha_3_tilde;
  // matrix[X_num,3] alpha_4 = X_R\alpha_4_tilde;
  // 
  // matrix[X_num,1] delta_1 = X_R\delta_1_tilde;
  // matrix[X_num,2] delta_2 = X_R\delta_2_tilde;
  // matrix[X_num,2] delta_3 = X_R\delta_3_tilde;
  // matrix[X_num,2] delta_4 = X_R\delta_4_tilde;
  // 
  // matrix[X_num,1] beta_1 = X_R\beta_1_tilde;
  // matrix[X_num,2] beta_2 = X_R\beta_2_tilde;
  // matrix[X_num,2] beta_3 = X_R\beta_3_tilde;
  // matrix[X_num,2] beta_4 = X_R\beta_4_tilde;

  matrix[X_num,1] alpha_p = X_R\alpha_p_tilde;

//  matrix[X_num,anchor_num] alpha_anchor = X_R\alpha_anchor_tilde;

//  corr_matrix[3] corr_lambda = tcrossprod(L_corr_lambda);

  // corr_matrix[2] corr_1 = tcrossprod(L_corr_1);
  // corr_matrix[3] corr_2 = tcrossprod(L_corr_2);
  // corr_matrix[3] corr_3 = tcrossprod(L_corr_3);
  // corr_matrix[3] corr_4 = tcrossprod(L_corr_4);
  
}

