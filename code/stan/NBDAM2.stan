data {
  int N;  // the number of observations
  int KX; // the number of columns in the X covariate matrix
  int KU; // the number of columns in the U covariate matrix
  int KT; // the number of columns in the time effect matrix
  int KS; // the number of states -- not used
  int KP; // the number of policy effects

  int y[N];      // the outcome
  vector[N] l_1; // lagged outcome, lag 1 on log scale
  vector[N] l_2; // lagged outcome, lag 2 on log scale
  
  matrix[N,KP] P;   // policy matrix
  matrix[N,KP] P_1; // lagged policy matrix, lag 1
  matrix[N,KP] P_2; // lagged policy matrix, lag 2
  
  vector[N] offset;   // offset -- log(population)
  vector[N] offset_1; // offset, lag 1
  vector[N] offset_2; // offset, lag 2
  
  matrix[N,KX] X;     //  covariate matrix not subject to shrinkage
  matrix[N,KU] U;     //  covariate matrix for lasso
  
  matrix[N,KT] T;     // time effects matrix
  matrix[N,KT] T_1;   // time effects matrix, lag 1
  matrix[N,KT] T_2;   // time effects matrix, lag 2
  
  int S[N]; // the state -- not used
}
parameters {
  real alpha;  // intercept
  real delta1; // lag 1 ar coefficient
  real delta2; // lag 2 ar coefficient
  
  vector[KP] beta; // policy parameters

  vector[KX] gamma_X; // covariate parameters with no shrinkage
  vector[KU] gamma_U; // covariate parameters with lasso
  real<lower=0> tau; // the lasso tuning parameter

  vector[KT] zeta; // time parameters
  
  real<lower=0> invphi; // the overdispersion parameter for neg bin
}
transformed parameters {
    real phi;
    phi = 1.0 / invphi;
}
model {  
  // prior for intercept -- noninformative
  alpha ~ normal(0,sqrt(10));
  
  // lasso prior for covariate set U
  gamma_U ~ double_exponential(0, tau);
  tau ~ cauchy(0, 1);
  
  // prior for NB dispersion
  invphi ~ normal(0, 0.1);

  // priors for ar coefficients
  delta1 ~ normal(.5,1); 
  delta2 ~ normal(0,1); 

  // priors for policy effects 
  beta ~ normal(0 , 0.071); 
  
  // priors for covaraites X
  gamma_X ~ normal(0,0.1);  
  zeta ~ normal(0,.2); 
  
  y ~ neg_binomial_2_log( alpha + ( l_1 - offset_1 - P_1*beta)*delta1 + ( l_2 - offset_2 - P_2*beta)*delta2 + P*beta + T*zeta + X*gamma_X + U*gamma_U + offset, phi);
}