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
  
  void add_logit_outcome_lp(
    int num, 
    int[] I_num, 
    int[] I_ind, 
    int[] outcome,
    matrix alpha,
    matrix gamma,
    vector[] c,
    matrix theta,
    matrix X
    )
    {
    int pos = 1;
      for (m in 1:num)
      {
        int ind[I_num[m]] = I_ind[pos:(pos + I_num[m] - 1)];
        outcome[pos:(pos + I_num[m] - 1)] ~
          bernoulli_logit(
            c[m][1] + X[ind,]*alpha[,m] +
              gamma[1,m]*theta[ind,2] + gamma[2,m]*theta[ind,3]
          );
        pos = pos + I_num[m];
      }
    }
    
  void add_orderedLogit_outcome_lp(
    int num, 
    int[] I_num, 
    int[] I_ind, 
    int[] outcome,
    matrix alpha,
    matrix gamma,
    vector[] c,
    matrix theta,
    matrix X
    )
    {
      int pos = 1;
        for (m in 1:num)
        {
          for (n in pos:(pos + I_num[m] - 1))
          {
            int ind = I_ind[n];
            outcome[n] ~ 
              ordered_logistic(
                X[ind,]*alpha[,m] +
                gamma[1,m]*theta[ind,2] + gamma[2,m]*theta[ind,3], 
                c[m]
                );
          }
          pos = pos + I_num[m];
        }
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
       *_num:   number of measurements
       I_*_num: number of non-missing observations for each measurement
       I_*_ind: indices of non-missing observations for each measurement
       M_*:     measurement vector
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

    /*** R_3 ***/

    int<lower = 0, upper = N> R_3_N;
    int<lower = 0, upper = N> R_3_ind[R_3_N];
    int<lower = 0, upper = 1> R_3_[R_3_N];
    int<lower = 0, upper = N> R_3_N0;
    int<lower = 0, upper = N> R_3_ind0[R_3_N0];
    int<lower = 0, upper = N> R_3_N1;
    int<lower = 0, upper = N> R_3_ind1[R_3_N1];

    /*** R_4 ***/
    
    int<lower = 0, upper = N> R_4_N;
    int<lower = 0, upper = N> R_4_ind[R_4_N];
    int<lower = 0, upper = 1> R_4_[R_4_N];
    int<lower = 0, upper = N> R_4_N0;
    int<lower = 0, upper = N> R_4_ind0[R_4_N0];
    int<lower = 0, upper = N> R_4_N1;
    int<lower = 0, upper = N> R_4_ind1[R_4_N1];
    
    /*** R_5 ***/
    
    int<lower = 0, upper = N> R_5_N;
    int<lower = 0, upper = N> R_5_ind[R_5_N];
    int<lower = 0, upper = 1> R_5_[R_5_N];
    int<lower = 0, upper = N> R_5_N0;
    int<lower = 0, upper = N> R_5_ind0[R_5_N0];
    int<lower = 0, upper = N> R_5_N1;
    int<lower = 0, upper = N> R_5_ind1[R_5_N1];

  /*** outcomes ***/
  
    /*** Year 9 ***/
    
    int<lower = 1> outcome_yr9_cat2_num;
    int<lower = 1, upper = N> I_outcome_yr9_cat2_num[outcome_yr9_cat2_num];
    int<lower = 1, upper = N> I_outcome_yr9_cat2_ind[sum(I_outcome_yr9_cat2_num)];
    int<lower = 0, upper = 1> outcome_yr9_cat2[sum(I_outcome_yr9_cat2_num)];
    
    int<lower = 1> outcome_yr9_cat5_num;
    int<lower = 1, upper = N> I_outcome_yr9_cat5_num[outcome_yr9_cat5_num];
    int<lower = 1, upper = N> I_outcome_yr9_cat5_ind[sum(I_outcome_yr9_cat5_num)];
    int<lower = 1, upper = 5> outcome_yr9_cat5[sum(I_outcome_yr9_cat5_num)];

    /*** Year 15 ***/
    
    int<lower = 1> outcome_yr15_cat2_num;
    int<lower = 1, upper = N> I_outcome_yr15_cat2_num[outcome_yr15_cat2_num];
    int<lower = 1, upper = N> I_outcome_yr15_cat2_ind[sum(I_outcome_yr15_cat2_num)];
    int<lower = 0, upper = 1> outcome_yr15_cat2[sum(I_outcome_yr15_cat2_num)];
    
    int<lower = 1> outcome_yr15_cat4_num;
    int<lower = 1, upper = N> I_outcome_yr15_cat4_num[outcome_yr15_cat4_num];
    int<lower = 1, upper = N> I_outcome_yr15_cat4_ind[sum(I_outcome_yr15_cat4_num)];
    int<lower = 1, upper = 4> outcome_yr15_cat4[sum(I_outcome_yr15_cat4_num)];

  /*** measurement parameters ***/
  
    /*** theta_0 ***/
    
    vector<lower = 0>[R_0_cat3_num] gamma_M_R_0_cat3;
    ordered[2] c_M_R_0_cat3[R_0_cat3_num];
  
    /*** theta_1 ***/
  
    vector<lower = 0>[R_1_cat3_num] gamma_M_R_1_cat3;
    ordered[2] c_M_R_1_cat3[R_1_cat3_num];

    vector<lower = 0>[R_1_cat5_num] gamma_M_R_1_cat5;
    ordered[4] c_M_R_1_cat5[R_1_cat5_num];

    vector<lower = 0>[N_1_cat5_num] gamma_M_N_1_cat5;
    ordered[4] c_M_N_1_cat5[N_1_cat5_num];
    
    /*** theta_2 ***/
   
    vector<lower = 0>[R_2_cat3_num] gamma_M_R_2_cat3;
    ordered[2] c_M_R_2_cat3[R_2_cat3_num];

    vector<lower = 0>[R_2_cat5_num] gamma_M_R_2_cat5;
    ordered[4] c_M_R_2_cat5[R_2_cat5_num];

    vector<lower = 0>[N_2_cat3_num] gamma_M_N_2_cat3;
    ordered[2] c_M_N_2_cat3[N_2_cat3_num];

    vector[C_2_num] mu_M_C_2;
    vector<lower = 0>[C_2_num] gamma_M_C_2;
    vector<lower = 0>[C_2_num] sigma_M_C_2;

    /*** theta_3 ***/

    vector<lower = 0>[R_3_cat3_num] gamma_M_R_3_cat3;
    ordered[2] c_M_R_3_cat3[R_3_cat3_num];

    vector<lower = 0>[R_3_cat5_num] gamma_M_R_3_cat5;
    ordered[4] c_M_R_3_cat5[R_3_cat5_num];

    vector<lower = 0>[N_3_cat3_num] gamma_M_N_3_cat3;
    ordered[2] c_M_N_3_cat3[N_3_cat3_num];

    vector[C_3_num] mu_M_C_3;
    vector<lower = 0>[C_3_num] gamma_M_C_3;
    vector<lower = 0>[C_3_num] sigma_M_C_3;

    /*** theta_4 ***/
    
    vector<lower = 0>[R_4_cat3_num] gamma_M_R_4_cat3;
    ordered[2] c_M_R_4_cat3[R_4_cat3_num];

    vector<lower = 0>[R_4_cat5_num] gamma_M_R_4_cat5;
    ordered[4] c_M_R_4_cat5[R_4_cat5_num];

    vector<lower = 0>[N_4_cat3_num] gamma_M_N_4_cat3;
    ordered[2] c_M_N_4_cat3[N_4_cat3_num];

    vector[C_4_num] mu_M_C_4;
    vector<lower = 0>[C_4_num] gamma_M_C_4;
    vector<lower = 0>[C_4_num] sigma_M_C_4;

}
transformed data {

  /*** declare X_R, X_Q ***/

  // standardized covariate matrix (orthonormal, mean-zero columns)

  matrix[X_num, X_num] X_R;
  matrix[N, X_num] X_Q; 

  /*** declare relationship indicator vectors for full sample ***/

  // need to assign these because Stan can't read in NAs
  // NAs set to 0 (do not enter likelihood)

  vector[N] R_0 = rep_vector(0, N);
  vector[N] R_1 = rep_vector(0, N); 
  vector[N] R_2 = rep_vector(0, N);
  vector[N] R_3 = rep_vector(0, N);
  vector[N] R_4 = rep_vector(0, N);
  vector[N] R_5 = rep_vector(0, N);

  /*** prior parameters ***/

  // prior mean for standardized coefficients 
  real normal_mu_prior = 0; 
  // prior variance for standardized coefficients and threshold parameters
  real<lower = 0> normal_sigma_prior = 1; 
  // prior standard deviation for correlation parameters
  real lkj_sd_prior = .4; 
  // prior standard deviation for factor loading and st dev parameters
  real gamma_sd_prior = 1; 
  
  // convert lkj sd to lkj eta parameter
  
  real lkj_eta_prior_2 = .5/square(lkj_sd_prior)*(1 - square(lkj_sd_prior)); 
  real lkj_eta_prior_3 = .5/square(lkj_sd_prior)*(1 - 2*square(lkj_sd_prior)); 
  real lkj_eta_prior_4 = .5/square(lkj_sd_prior)*(1 - 3*square(lkj_sd_prior));   
  
  /*** assign X_R and X_Q ***/
    
    X_R = qr_R(X)[1:X_num,] / sqrt(N - 1);
    X_Q = qr_Q(X)[, 1:X_num] * sqrt(N - 1); 

  /*** assign relationship indicators ***/
  
    R_0           = to_vector(R_0_);
    R_1[R_1_ind]  = to_vector(R_1_);
    R_2[R_2_ind]  = to_vector(R_2_);
    R_3[R_3_ind]  = to_vector(R_3_);
    R_4[R_4_ind]  = to_vector(R_4_);
    R_5[R_5_ind]  = to_vector(R_5_);

}
parameters {

/*** lambda ***/

  matrix[N,3] lambda_raw;
  cholesky_factor_corr[3] L_corr_lambda;
  vector<lower = 0, upper = 1>[3] sigma_lambda;

/*** theta_0 ***/

  matrix[X_num,1]  alpha_0_tilde;
  vector<lower = 0, upper = 1>[1] sigma_0;
  
  matrix[R_0_N1,1] epsilon_0_R0eq1;

/*** theta_1 ***/

  matrix[X_num,2] alpha_1_tilde;
  row_vector[1] beta_1_;
  real<lower = 0> gamma_1_11_;
  row_vector[1] gamma_1_;
  row_vector[1] xi_1_;
  row_vector[1] delta_1_;
  cholesky_factor_corr[2] L_corr_1;
  vector<lower = 0, upper = 1>[2] sigma_1;

  matrix[R_1_N1, 2] epsilon_1_R1eq1;
  matrix[R_1_N0, 1] epsilon_N_1_R1eq0;

/*** theta_2 ***/

  matrix[X_num,3] alpha_2_tilde;
  row_vector[2] beta_2_;
  real<lower = 0> gamma_2_11_;
  real<lower = 0> gamma_2_22_;
  row_vector[3] gamma_2_;
  row_vector[2] delta_2_;
  row_vector[2] xi_2_;
  cholesky_factor_corr[3] L_corr_2;
  vector<lower = 0, upper = 1>[3] sigma_2;

  matrix[R_2_N1, 3] epsilon_2_R2eq1;
  matrix[R_2_N0, 2] epsilon_NC_2_R2eq0;
  
/*** theta_3 ***/

  matrix[X_num,3] alpha_3_tilde;
  row_vector[2] beta_3_;
  real<lower = 0> gamma_3_11_;
  real<lower = 0> gamma_3_22_;
  real<lower = 0> gamma_3_33_;
  row_vector[4] gamma_3_;
  row_vector[2] delta_3_;
  row_vector[2] xi_3_;
  cholesky_factor_corr[3] L_corr_3;
  vector<lower = 0, upper = 1>[3] sigma_3;

  matrix[R_3_N1, 3] epsilon_3_R3eq1;
  matrix[R_3_N0, 2] epsilon_NC_3_R3eq0;

/*** theta_4 ***/

  matrix[X_num,3] alpha_4_tilde;
  row_vector[2] beta_4_;
  real<lower = 0> gamma_4_11_;
  real<lower = 0> gamma_4_22_;
  real<lower = 0> gamma_4_33_;
  row_vector[4] gamma_4_;
  row_vector[2] delta_4_;
  row_vector[2] xi_4_;
  cholesky_factor_corr[3] L_corr_4;
  vector<lower = 0, upper = 1>[3] sigma_4;

  matrix[R_4_N1, 3] epsilon_4_R4eq1;
  matrix[R_4_N0, 2] epsilon_NC_4_R4eq0;

/*** relationship indicators ***/

  matrix[X_num,5] alpha_p_tilde;
  vector[5] gamma_p_; // (extra undercore at end because gamma_p is a protected function)
  vector[5] c_p;

/*** outcomes ***/
  
  matrix[X_num, outcome_yr9_cat2_num]  alpha_outcome_yr9_cat2_tilde;
  matrix[2,     outcome_yr9_cat2_num]  gamma_outcome_yr9_cat2;
  vector[1]  c_outcome_yr9_cat2[outcome_yr9_cat2_num];
  
  matrix[X_num, outcome_yr9_cat5_num]  alpha_outcome_yr9_cat5_tilde;
  matrix[2,     outcome_yr9_cat5_num]  gamma_outcome_yr9_cat5;
  ordered[4] c_outcome_yr9_cat5[outcome_yr9_cat5_num];

  matrix[X_num, outcome_yr15_cat2_num] alpha_outcome_yr15_cat2_tilde;
  matrix[2,     outcome_yr15_cat2_num] gamma_outcome_yr15_cat2;
  vector[1]  c_outcome_yr15_cat2[outcome_yr15_cat2_num];
  
  matrix[X_num, outcome_yr15_cat4_num] alpha_outcome_yr15_cat4_tilde;
  matrix[2,     outcome_yr15_cat4_num] gamma_outcome_yr15_cat4;
  ordered[3] c_outcome_yr15_cat4[outcome_yr15_cat4_num];

}
transformed parameters {

/*** declare lambda ***/

  matrix[N,3] lambda;

/*** declare structural parameters ***/

  /*** theta_0 ***/


  /*** theta_1 ***/

  row_vector[2] beta_1  = rep_row_vector(0,2);
  matrix[1,2]   gamma_1 = rep_matrix(0,1,2);
  row_vector[2] delta_1 = rep_row_vector(0,2);
  matrix[1,2]   xi_1    = rep_matrix(0,1,2);

  /*** theta_2 ***/

  row_vector[3] beta_2  = rep_row_vector(0,3);
  matrix[2,3]   gamma_2 = rep_matrix(0,2,3);
  row_vector[3] delta_2 = rep_row_vector(0,3);
  matrix[2,3]   xi_2    = rep_matrix(0,2,3);

  /*** theta_3 ***/

  row_vector[3]   beta_3  = rep_row_vector(0,3);
  matrix[3,3]     gamma_3 = rep_matrix(0,3,3);
  row_vector[3]   delta_3 = rep_row_vector(0,3);
  matrix[3,3]     xi_3    = rep_matrix(0,3,3);

  /*** theta_4 ***/

  row_vector[3]   beta_4  = rep_row_vector(0,3);
  matrix[3,3]     gamma_4 = rep_matrix(0,3,3);
  row_vector[3]   delta_4 = rep_row_vector(0,3);
  matrix[3,3]     xi_4    = rep_matrix(0,3,3);

/*** declare thetas ***/ 

  matrix[N, 1] theta_0 = rep_matrix(0, N, 1);
  matrix[N, 2] theta_1 = rep_matrix(0, N, 2);
  matrix[N, 3] theta_2 = rep_matrix(0, N, 3);
  matrix[N, 3] theta_3 = rep_matrix(0, N, 3);
  matrix[N, 3] theta_4 = rep_matrix(0, N, 3);
 
/*** assign transformed parameters ***/  
{
  /*** declare and assign epsilons ***/ 
  
    matrix[N,1] epsilon_0 = rep_matrix(0,N,1);
    matrix[N,2] epsilon_1 = rep_matrix(0,N,2);
    matrix[N,3] epsilon_2 = rep_matrix(0,N,3);
    matrix[N,3] epsilon_3 = rep_matrix(0,N,3);
    matrix[N,3] epsilon_4 = rep_matrix(0,N,3);

    epsilon_0[R_0_ind1,1] = epsilon_0_R0eq1 * sigma_0;
    
    epsilon_1[R_1_ind1,]  = epsilon_1_R1eq1 * L_corr_1' * diag_matrix(sigma_1);
    epsilon_1[R_1_ind0,2] = epsilon_N_1_R1eq0[,1] * sigma_1[2];

    epsilon_2[R_2_ind1,] = epsilon_2_R2eq1 * L_corr_2' * diag_matrix(sigma_2);
    epsilon_2[R_2_ind0,2:3] =
      epsilon_NC_2_R2eq0 * cholesky_decompose(tcrossprod(L_corr_2[2:3,])) * diag_matrix(sigma_2[2:3]);

    epsilon_3[R_3_ind1,] = epsilon_3_R3eq1 * L_corr_3' * diag_matrix(sigma_3);
    epsilon_3[R_3_ind0,2:3] =
      epsilon_NC_3_R3eq0 * cholesky_decompose(tcrossprod(L_corr_3[2:3,])) * diag_matrix(sigma_3[2:3]);

    epsilon_4[R_4_ind1,] = epsilon_4_R4eq1 * L_corr_4' * diag_matrix(sigma_4);
    epsilon_4[R_4_ind0,2:3] =
      epsilon_NC_4_R4eq0 * cholesky_decompose(tcrossprod(L_corr_4[2:3,])) * diag_matrix(sigma_4[2:3]);
  /*** assign lambda ***/ 
    lambda = lambda_raw * L_corr_lambda' * diag_matrix(sigma_lambda);
  /*** assign theta_0 ***/
  {
    vector[N] theta_R_0 = rep_vector(0,N);

    /* generate unnormalized latent variables */

    theta_R_0[R_0_ind1] =
      X_Q[R_0_ind1,]*alpha_0_tilde[,1] +
      lambda[R_0_ind1,1] + 
      epsilon_0[R_0_ind1,1];

    theta_0[,1] = theta_R_0;
  }
  /*** assign theta_1 ***/
  {
    vector[N] theta_R_1 = rep_vector(0,N);
    vector[N] theta_N_1 = rep_vector(0,N);

    beta_1[2:2]  = beta_1_;
    delta_1[2:2] = delta_1_;

    gamma_1[1,1] = gamma_1_11_;
    gamma_1[1,2] = gamma_1_[1];

    xi_1 = gamma_1;
    xi_1[1,2] = gamma_1_[1] + xi_1_[1];

    /* generate unnormalized latent variables */

    theta_R_1[R_1_ind1] =
      X_Q[R_1_ind1,]*alpha_1_tilde[,1] +
      lambda[R_1_ind1,1] + 
      epsilon_0[R_1_ind1,]*gamma_1[,1] +
      epsilon_1[R_1_ind1,1];

    theta_N_1[R_1_ind0] =
      X_Q[R_1_ind0,]*alpha_1_tilde[,2] +
      beta_1[2]*R_0[R_1_ind0] +
      lambda[R_1_ind0,2] + 
      epsilon_0[R_1_ind0,]*gamma_1[,2] +
      epsilon_1[R_1_ind0,2];

    theta_N_1[R_1_ind1] =
      X_Q[R_1_ind1,]*alpha_1_tilde[,2] +
      beta_1[2] + delta_1[2] +
      lambda[R_1_ind1,2] + 
      epsilon_0[R_1_ind1,]*xi_1[,2] +
      epsilon_1[R_1_ind1,2];

    theta_1[,1] = theta_R_1;
    theta_1[,2] = theta_N_1;
  }
  /*** assign theta_2 ***/
  {
    vector[N] theta_R_2 = rep_vector(0,N);
    vector[N] theta_N_2 = rep_vector(0,N);
    vector[N] theta_C_2 = rep_vector(0,N);

    /* transform parameters for easier manipulation */

    beta_2[2:3]  = beta_2_;
    delta_2[2:3] = delta_2_;

    gamma_2[1,1] = gamma_2_11_;
    gamma_2[1,2] = gamma_2_[1];
    gamma_2[2,2] = gamma_2_22_;
    gamma_2[1,3] = gamma_2_[2];
    gamma_2[2,3] = gamma_2_[3];

    xi_2 = gamma_2;
    xi_2[1,2] = gamma_2_[1] + xi_2_[1];
    xi_2[1,3] = gamma_2_[2] + xi_2_[2];

    /* generate unnormalized latent variables */

    theta_R_2[R_2_ind1] =
      X_Q[R_2_ind1,] * alpha_2_tilde[,1] +
      lambda[R_2_ind1,1] + 
      epsilon_1[R_2_ind1,] * gamma_2[,1] +
      epsilon_2[R_2_ind1,1];

    theta_N_2[R_2_ind0] =
      X_Q[R_2_ind0,] * alpha_2_tilde[,2] +
      beta_2[2] * R_1[R_2_ind0] +
      lambda[R_2_ind0,2] + 
      epsilon_1[R_2_ind0,] * gamma_2[,2] +
      epsilon_2[R_2_ind0,2];

    theta_N_2[R_2_ind1] =
      X_Q[R_2_ind1,] * alpha_2_tilde[,2] +
      beta_2[2] + delta_2[2] +
      lambda[R_2_ind1,2] + 
      epsilon_1[R_2_ind1,] * xi_2[,2] +
      epsilon_2[R_2_ind1,2];

    theta_C_2[R_2_ind0] =
      X_Q[R_2_ind0,] * alpha_2_tilde[,3] +
      beta_2[3]  * R_1[R_2_ind0] +
      lambda[R_2_ind0,3] + //* c_2[3] +
      // lambda[R_2_ind0]*c_2[3] +
      epsilon_1[R_2_ind0,] * gamma_2[,3] +
      epsilon_2[R_2_ind0,3];

    theta_C_2[R_2_ind1] =
      X_Q[R_2_ind1,] * alpha_2_tilde[,3] +
      beta_2[3] + delta_2[3] +
      lambda[R_2_ind1,3] + 
      epsilon_1[R_2_ind1,] * xi_2[,3] +
      epsilon_2[R_2_ind1,3];

    theta_2[,1] = theta_R_2;
    theta_2[,2] = theta_N_2;
    theta_2[,3] = theta_C_2;
  }
  /*** assign theta_3 ***/
  {
    vector[N] theta_R_3 = rep_vector(0,N);
    vector[N] theta_N_3 = rep_vector(0,N);
    vector[N] theta_C_3 = rep_vector(0,N);

    /* transform parameters for easier manipulation */

    beta_3[2:3] = beta_3_;
    delta_3[2:3] = delta_3_;

    gamma_3[1,1] = gamma_3_11_;
    gamma_3[1,2] = gamma_3_[1];
    gamma_3[2,2] = gamma_3_22_;
    gamma_3[3,2] = gamma_3_[2];
    gamma_3[1,3] = gamma_3_[3];
    gamma_3[2,3] = gamma_3_[4];
    gamma_3[3,3] = gamma_3_33_;

    xi_3 = gamma_3;
    xi_3[1,2] = gamma_3_[1] + xi_3_[1];
    xi_3[1,3] = gamma_3_[3] + xi_3_[2];

    /* generate unnormalized latent variables */

    theta_R_3[R_3_ind1] =
      X_Q[R_3_ind1,]*alpha_3_tilde[,1] +
      lambda[R_3_ind1,1] + 
      epsilon_2[R_3_ind1,] * gamma_3[,1] +
      epsilon_3[R_3_ind1,1];

    theta_N_3[R_3_ind0] =
      X_Q[R_3_ind0,]*alpha_3_tilde[,2] +
      beta_3[2]*R_2[R_3_ind0] +
      lambda[R_3_ind0,2] + 
      epsilon_2[R_3_ind0,]*gamma_3[,2] +
      epsilon_3[R_3_ind0,2];

    theta_N_3[R_3_ind1] =
      X_Q[R_3_ind1,]*alpha_3_tilde[,2] +
      beta_3[2] + delta_3[2] +
      lambda[R_3_ind1,2] + 
      epsilon_2[R_3_ind1,] * xi_3[,2] +
      epsilon_3[R_3_ind1,2];

    theta_C_3[R_3_ind0] =
      X_Q[R_3_ind0,]*alpha_3_tilde[,3] +
      beta_3[3]*R_2[R_3_ind0] +
      lambda[R_3_ind0,3] + 
      epsilon_2[R_3_ind0,]*gamma_3[,3] +
      epsilon_3[R_3_ind0,3];

    theta_C_3[R_3_ind1] =
      X_Q[R_3_ind1,]*alpha_3_tilde[,3] +
      beta_3[3] + delta_3[3] +
      lambda[R_3_ind1,3] + 
      epsilon_2[R_3_ind1,] * xi_3[,3] +
      epsilon_3[R_3_ind1,3];

    theta_3[,1] = theta_R_3;
    theta_3[,2] = theta_N_3;
    theta_3[,3] = theta_C_3;
  }
  /*** assign theta_4 ***/
  {
    vector[N] theta_R_4 = rep_vector(0,N);
    vector[N] theta_N_4 = rep_vector(0,N);
    vector[N] theta_C_4 = rep_vector(0,N);

    /* transform parametersfor easier manipulation */

    beta_4[2:3] = beta_4_;
    delta_4[2:3] = delta_4_;

    gamma_4[1,1] = gamma_4_11_;
    gamma_4[1,2] = gamma_4_[1];
    gamma_4[2,2] = gamma_4_22_;
    gamma_4[3,2] = gamma_4_[2];
    gamma_4[1,3] = gamma_4_[3];
    gamma_4[2,3] = gamma_4_[4];
    gamma_4[3,3] = gamma_4_33_;

    xi_4 = gamma_4;
    xi_4[1,2] = gamma_4_[1] + xi_4_[1];
    xi_4[1,3] = gamma_4_[3] + xi_4_[2];

    /* generate unnormalized latent variables */

    theta_R_4[R_4_ind1] =
      X_Q[R_4_ind1,]*alpha_4_tilde[,1] +
      lambda[R_4_ind1,1] + 
      epsilon_3[R_4_ind1,] * gamma_4[,1] +
      epsilon_4[R_4_ind1,1];

    theta_N_4[R_4_ind0] =
      X_Q[R_4_ind0,]*alpha_4_tilde[,2] +
      beta_4[2]*R_3[R_4_ind0] +
      lambda[R_4_ind0,2] + 
      epsilon_3[R_4_ind0,] * gamma_4[,2] +
      epsilon_4[R_4_ind0,2];

    theta_N_4[R_4_ind1] =
      X_Q[R_4_ind1,]*alpha_4_tilde[,2] +
      beta_4[2] + delta_4[2] +
      lambda[R_4_ind1,2] + 
      epsilon_3[R_4_ind1,] * xi_4[,2] +
      epsilon_4[R_4_ind1,2];

    theta_C_4[R_4_ind0] =
      X_Q[R_4_ind0,]*alpha_4_tilde[,3] +
      beta_4[3]*R_3[R_4_ind0] +
      lambda[R_4_ind0,3] + 
      epsilon_3[R_4_ind0,] * gamma_4[,3] +
      epsilon_4[R_4_ind0,3];

    theta_C_4[R_4_ind1] =
      X_Q[R_4_ind1,]*alpha_4_tilde[,3] +
      beta_4[3] + delta_4[3] +
      lambda[R_4_ind1,3] + 
      epsilon_3[R_4_ind1,] * xi_4[,3] +
      epsilon_4[R_4_ind1,3];

    theta_4[,1] = theta_R_4;
    theta_4[,2] = theta_N_4;
    theta_4[,3] = theta_C_4;
  }
}
}
model {

/*** priors ***/

  /*** indicators ***/

   to_vector(alpha_p_tilde)
     ~ normal(normal_mu_prior, normal_sigma_prior);
    gamma_p_ ~ normal(normal_mu_prior, normal_sigma_prior);
    c_p ~ normal(normal_mu_prior, normal_sigma_prior);

  /*** theta_0 ***/
  
  to_vector(alpha_0_tilde)
           ~ normal(normal_mu_prior, normal_sigma_prior);
  sigma_0  ~ beta(6,1.5);

  /*** theta_1 ***/
  
  to_vector(alpha_1_tilde)
              ~ normal(normal_mu_prior, normal_sigma_prior);
  beta_1_     ~ normal(normal_mu_prior, normal_sigma_prior);
  gamma_1_    ~ normal(normal_mu_prior, normal_sigma_prior);
  gamma_1_11_ ~ gamma(2,2);
  delta_1_    ~ normal(normal_mu_prior, normal_sigma_prior);
  xi_1_       ~ normal(normal_mu_prior, normal_sigma_prior);
  L_corr_1    ~ lkj_corr_cholesky(lkj_eta_prior_2);
  sigma_1[1]  ~ beta(1.5,1.5);
  sigma_1[2]  ~ beta(3,1.5);

  /*** theta_2 ***/

  to_vector(alpha_2_tilde)
              ~ normal(normal_mu_prior, normal_sigma_prior);
  beta_2_     ~ normal(normal_mu_prior, normal_sigma_prior);
  gamma_2_    ~ normal(normal_mu_prior, normal_sigma_prior);
  gamma_2_11_ ~ gamma(1.5,1.5);
  gamma_2_22_ ~ gamma(1.5,1.5);
  delta_2_    ~ normal(normal_mu_prior, normal_sigma_prior);
  xi_2_       ~ normal(normal_mu_prior, normal_sigma_prior);
  L_corr_2    ~ lkj_corr_cholesky(lkj_eta_prior_3);
  sigma_2[1:2] ~ beta(1.5,1.5);
  sigma_2[3]   ~ beta(3,1.5);

  /*** theta_3 ***/

  to_vector(alpha_3_tilde)
              ~ normal(normal_mu_prior, normal_sigma_prior);
  beta_3_     ~ normal(normal_mu_prior, normal_sigma_prior);
  gamma_3_    ~ normal(normal_mu_prior, normal_sigma_prior);
  gamma_3_11_ ~ gamma(1.5,1.5);
  gamma_3_22_ ~ gamma(1.5,1.5);
  gamma_3_33_ ~ gamma(1.5,1.5);
  delta_3_    ~ normal(normal_mu_prior, normal_sigma_prior);
  xi_3_       ~ normal(normal_mu_prior, normal_sigma_prior);
  L_corr_3    ~ lkj_corr_cholesky(lkj_eta_prior_3);
  sigma_3     ~ beta(1.5,1.5);

  /*** theta_4 ***/

  to_vector(alpha_4_tilde)
              ~ normal(normal_mu_prior, normal_sigma_prior);
  beta_4_     ~ normal(normal_mu_prior, normal_sigma_prior);
  gamma_4_    ~ normal(normal_mu_prior, normal_sigma_prior);
  gamma_4_11_ ~ gamma(1.5,1.5);
  gamma_4_22_ ~ gamma(1.5,1.5);
  gamma_4_33_ ~ gamma(1.5,1.5);
  delta_4_    ~ normal(normal_mu_prior, normal_sigma_prior);
  xi_4_       ~ normal(normal_mu_prior, normal_sigma_prior);
  L_corr_4    ~ lkj_corr_cholesky(lkj_eta_prior_3);
  sigma_4     ~ beta(1.5,1.5);
  
  /*** outcomes ***/
  
  to_vector(alpha_outcome_yr9_cat2_tilde) ~ normal(normal_mu_prior, normal_sigma_prior);
  to_vector(gamma_outcome_yr9_cat2)       ~ normal(normal_mu_prior, normal_sigma_prior);
  for (m in 1:outcome_yr9_cat2_num){
    c_outcome_yr9_cat2[m] ~ normal(normal_mu_prior, normal_sigma_prior);
  }

  to_vector(alpha_outcome_yr9_cat5_tilde) ~ normal(normal_mu_prior, normal_sigma_prior);
  to_vector(gamma_outcome_yr9_cat5)       ~ normal(normal_mu_prior, normal_sigma_prior);
  for (m in 1:outcome_yr9_cat5_num){
    c_outcome_yr9_cat5[m] ~ normal(normal_mu_prior, normal_sigma_prior);
  }

  to_vector(alpha_outcome_yr15_cat2_tilde) ~ normal(normal_mu_prior, normal_sigma_prior);
  to_vector(gamma_outcome_yr15_cat2)       ~ normal(normal_mu_prior, normal_sigma_prior);
  for (m in 1:outcome_yr15_cat2_num){
    c_outcome_yr15_cat2[m] ~ normal(normal_mu_prior, normal_sigma_prior);
  }

  to_vector(alpha_outcome_yr15_cat4_tilde) ~ normal(normal_mu_prior, normal_sigma_prior);
  to_vector(gamma_outcome_yr15_cat4)       ~ normal(normal_mu_prior, normal_sigma_prior);
  for (m in 1:outcome_yr15_cat4_num){
    c_outcome_yr15_cat4[m] ~ normal(normal_mu_prior, normal_sigma_prior);
  }

/*** state variables ***/

  /*** lambda ***/

   to_vector(lambda_raw)  ~ normal(0,1);
  L_corr_lambda  ~ lkj_corr_cholesky(lkj_eta_prior_3);
  sigma_lambda ~ beta(1.5,1.5);

  /*** theta_0 ***/

   to_vector(epsilon_0_R0eq1) ~ normal(0,1);

  /*** theta_1 ***/

  to_vector(epsilon_1_R1eq1)   ~ normal(0,1);
  to_vector(epsilon_N_1_R1eq0) ~ normal(0,1);

  /*** theta_2 ***/
  
  to_vector(epsilon_2_R2eq1) ~ normal(0,1);
  to_vector(epsilon_NC_2_R2eq0) ~ normal(0,1);

  /*** theta_3 ***/

  to_vector(epsilon_3_R3eq1) ~ normal(0,1);
  to_vector(epsilon_NC_3_R3eq0) ~ normal(0,1);

  /*** theta_4 ***/

  to_vector(epsilon_4_R4eq1) ~ normal(0,1);
  to_vector(epsilon_NC_4_R4eq0) ~ normal(0,1);

/*** relationship indicators ***/

  /*** R_1 ***/
  
  R_1_ ~
     bernoulli(
       Phi_approx(
        X_Q[R_1_ind,] * alpha_p_tilde[,1] +
        theta_0[R_1_ind,1] * gamma_p_[1] +
        lambda[R_1_ind,1] * c_p[1]
       )
     );

  /*** R_2 ***/

  R_2_ ~
    bernoulli(
      Phi_approx(
        X_Q[R_2_ind,] * alpha_p_tilde[,2] +
        theta_1[R_2_ind,1] * gamma_p_[2] +
        lambda[R_2_ind,1] * c_p[2]
        )
      );

  /*** R_3 ***/

  R_3_ ~
    bernoulli(
      Phi_approx(
        X_Q[R_3_ind,] * alpha_p_tilde[,3] +
        theta_2[R_3_ind,1] * gamma_p_[3] + 
       lambda[R_3_ind,1] * c_p[3]
      )
    );

  /*** R_4 ***/

  R_4_ ~
    bernoulli(
      Phi_approx(
        X_Q[R_4_ind,] * alpha_p_tilde[,4] +
        theta_3[R_4_ind,1] * gamma_p_[4] + 
        lambda[R_4_ind,1] * c_p[4]
        )
      );
    
  /*** R_5 ***/
    
  R_5_ ~
    bernoulli(
      Phi_approx(
        X_Q[R_5_ind,] * alpha_p_tilde[,5] +
        theta_4[R_5_ind,1] * gamma_p_[5] + 
        lambda[R_5_ind,1] * c_p[5]
        )
      );
    
/*** measurements ***/

  /*** theta_R_0 ***/
  
  add_R_measurement_lp(
    R_0_cat3_num,
    I_R_0_cat3_num,
    I_R_0_cat3_ind,
    M_R_0_cat3,
    gamma_M_R_0_cat3,
    c_M_R_0_cat3,
    theta_0[,1]
  )
  
  /*** theta_R_1 ***/
  
  add_R_measurement_lp(
    R_1_cat3_num,
    I_R_1_cat3_num,
    I_R_1_cat3_ind,
    M_R_1_cat3,
    gamma_M_R_1_cat3,
    c_M_R_1_cat3,
    theta_1[,1]
  )

  add_R_measurement_lp(
    R_1_cat5_num,
    I_R_1_cat5_num,
    I_R_1_cat5_ind,
    M_R_1_cat5,
    gamma_M_R_1_cat5,
    c_M_R_1_cat5,
    theta_1[,1]
  )

  /*** theta_N_1 ***/
  
  add_N_measurement_lp(
    N_1_cat5_num,
    I_N_1_cat5_num,
    I_N_1_cat5_ind,
    M_N_1_cat5,
    gamma_M_N_1_cat5,
    c_M_N_1_cat5,
    theta_1[,2]
  )
  
  /*** theta_R_2 ***/
  
  add_R_measurement_lp(
    R_2_cat3_num,
    I_R_2_cat3_num,
    I_R_2_cat3_ind,
    M_R_2_cat3,
    gamma_M_R_2_cat3,
    c_M_R_2_cat3,
    theta_2[,1]
  )

  add_R_measurement_lp(
    R_2_cat5_num,
    I_R_2_cat5_num,
    I_R_2_cat5_ind,
    M_R_2_cat5,
    gamma_M_R_2_cat5,
    c_M_R_2_cat5,
    theta_2[,1]
  )

  /*** theta_N_2 ***/
  
  add_N_measurement_lp(
    N_2_cat3_num,
    I_N_2_cat3_num,
    I_N_2_cat3_ind,
    M_N_2_cat3,
    gamma_M_N_2_cat3,
    c_M_N_2_cat3,
    theta_2[,2]
  )
  
  /*** theta_C_2 ***/
  
  add_C_measurement_lp(
    C_2_num,
    I_C_2_num,
    I_C_2_ind,
    M_C_2,
    mu_M_C_2,
    gamma_M_C_2,
    sigma_M_C_2,
    theta_2[,3]
  )
  
  /*** theta_R_3 ***/
  
  add_R_measurement_lp(
    R_3_cat3_num,
    I_R_3_cat3_num,
    I_R_3_cat3_ind,
    M_R_3_cat3,
    gamma_M_R_3_cat3,
    c_M_R_3_cat3,
    theta_3[,1]
  )

  add_R_measurement_lp(
    R_3_cat5_num,
    I_R_3_cat5_num,
    I_R_3_cat5_ind,
    M_R_3_cat5,
    gamma_M_R_3_cat5,
    c_M_R_3_cat5,
    theta_3[,1]
  )

  /*** theta_N_3 ***/
  
  add_N_measurement_lp(
    N_3_cat3_num,
    I_N_3_cat3_num,
    I_N_3_cat3_ind,
    M_N_3_cat3,
    gamma_M_N_3_cat3,
    c_M_N_3_cat3,
    theta_3[,2]
  )
  
  /*** theta_C_3 ***/
  
  add_C_measurement_lp(
    C_3_num,
    I_C_3_num,
    I_C_3_ind,
    M_C_3,
    mu_M_C_3,
    gamma_M_C_3,
    sigma_M_C_3,
    theta_3[,3]
  )
  
  /*** theta_R_4 ***/
  
  add_R_measurement_lp(
    R_4_cat3_num,
    I_R_4_cat3_num,
    I_R_4_cat3_ind,
    M_R_4_cat3,
    gamma_M_R_4_cat3,
    c_M_R_4_cat3,
    theta_4[,1]
  )

  add_R_measurement_lp(
    R_4_cat5_num,
    I_R_4_cat5_num,
    I_R_4_cat5_ind,
    M_R_4_cat5,
    gamma_M_R_4_cat5,
    c_M_R_4_cat5,
    theta_4[,1]
  )

  /*** theta_N_4 ***/
  
  add_N_measurement_lp(
    N_4_cat3_num,
    I_N_4_cat3_num,
    I_N_4_cat3_ind,
    M_N_4_cat3,
    gamma_M_N_4_cat3,
    c_M_N_4_cat3,
    theta_4[,2]
  )
  
  /*** theta_C_4 ***/
  
  add_C_measurement_lp(
    C_4_num,
    I_C_4_num,
    I_C_4_ind,
    M_C_4,
    mu_M_C_4,
    gamma_M_C_4,
    sigma_M_C_4,
    theta_4[,3]
  )
  
/*** outcomes ***/
  
  /*** Year 9 ***/
  
  add_logit_outcome_lp(
    outcome_yr9_cat2_num, 
    I_outcome_yr9_cat2_num, 
    I_outcome_yr9_cat2_ind, 
    outcome_yr9_cat2,
    alpha_outcome_yr9_cat2_tilde,
    gamma_outcome_yr9_cat2,
    c_outcome_yr9_cat2,
    theta_4,
    X_Q
    )
    
  add_orderedLogit_outcome_lp(
    outcome_yr9_cat5_num, 
    I_outcome_yr9_cat5_num, 
    I_outcome_yr9_cat5_ind, 
    outcome_yr9_cat5,
    alpha_outcome_yr9_cat5_tilde,
    gamma_outcome_yr9_cat5,
    c_outcome_yr9_cat5,
    theta_4,
    X_Q
    )
    
  /*** Year 15 ***/  
  
  add_logit_outcome_lp(
    outcome_yr15_cat2_num, 
    I_outcome_yr15_cat2_num, 
    I_outcome_yr15_cat2_ind, 
    outcome_yr15_cat2,
    alpha_outcome_yr15_cat2_tilde,
    gamma_outcome_yr15_cat2,
    c_outcome_yr15_cat2,
    theta_4,
    X_Q
    )
    
  add_orderedLogit_outcome_lp(
    outcome_yr15_cat4_num, 
    I_outcome_yr15_cat4_num, 
    I_outcome_yr15_cat4_ind, 
    outcome_yr15_cat4,
    alpha_outcome_yr15_cat4_tilde,
    gamma_outcome_yr15_cat4,
    c_outcome_yr15_cat4,
    theta_4,
    X_Q
    )

}
generated quantities {
  
  matrix[X_num,1] alpha_0 = X_R\alpha_0_tilde;
  matrix[X_num,2] alpha_1 = X_R\alpha_1_tilde;
  matrix[X_num,3] alpha_2 = X_R\alpha_2_tilde;
  matrix[X_num,3] alpha_3 = X_R\alpha_3_tilde;
  matrix[X_num,3] alpha_4 = X_R\alpha_4_tilde;
  matrix[X_num,5] alpha_p = X_R\alpha_p_tilde;

  matrix[X_num, outcome_yr9_cat2_num]  alpha_outcome_yr9_cat2  = X_R\alpha_outcome_yr9_cat2_tilde;
  matrix[X_num, outcome_yr9_cat5_num]  alpha_outcome_yr9_cat5  = X_R\alpha_outcome_yr9_cat5_tilde;
  matrix[X_num, outcome_yr15_cat2_num] alpha_outcome_yr15_cat2 = X_R\alpha_outcome_yr15_cat2_tilde;
  matrix[X_num, outcome_yr15_cat4_num] alpha_outcome_yr15_cat4 = X_R\alpha_outcome_yr15_cat4_tilde;

  corr_matrix[3] corr_lambda = tcrossprod(L_corr_lambda);
  corr_matrix[2] corr_1 = tcrossprod(L_corr_1);
  corr_matrix[3] corr_2 = tcrossprod(L_corr_2);
  corr_matrix[3] corr_3 = tcrossprod(L_corr_3);
  corr_matrix[3] corr_4 = tcrossprod(L_corr_4);
  
}

