// Log-Gaussian Cox Process hyperparameters sampling
// Based on:
// Stan Modeling Language. User's Gude and Reference Manual (Stan Version 2.11.0.)
//
// NOTES: - This code does not compute predictions of the GP;
//        - Covariance: RBF;
//        - Link function: log(1+exp(gp));
//        - No noise parameter;


data {
  
  int<lower=1> num_data;
  int<lower=1> num_pred;
  int<lower=1> input_dim;
  int<lower=0> Y_data[num_data];
  int<lower=0> T_data[num_data];
  int<lower=0> T_pred[num_pred];
  vector[num_data] M_prior_data;
  vector[num_pred] M_prior_pred;
  matrix[num_data, input_dim] X_data;
  matrix[num_pred, input_dim] X_pred;
  
}

transformed data {
  
  int<lower=1> num_star;
  int<lower=0> T_star[num_data+num_pred];
  matrix[num_data+num_pred, input_dim] X_star;
  
  num_star = num_data + num_pred;
  
  for (i in 1:num_data) X_star[i] = X_data[i];
  for (i in 1:num_pred) X_star[num_data + i] = X_pred[i];
  
  for (i in 1:num_data) T_star[i] = T_data[i];
  for (i in 1:num_pred) T_star[num_data + i] = T_pred[i];
  
}

parameters {
  
  real<lower=0> rbf_var_t;
  real<lower=0> rbf_var_ntl;
  real<lower=0> rbf_lengthscale_sq_t;
  real<lower=0> rbf_lengthscale_sq_ntl;
  //real<lower=0> rbf_lengthscale_sq_lit;
  vector[num_data] GP_data;
  vector[num_pred] GP_pred;
  vector[num_pred] Y_pred;
  
}

transformed parameters {
  
  cov_matrix[num_data+num_pred] K_star;
  
  
  // Diagonal elements
  for (i in 1:num_star) K_star[i,i] = rbf_var_ntl + rbf_var_t + 1e-6;
  
  // Off-diagonal elements
  for (i in 1:(num_star-1)) {
    for (j in (i+1):num_star) {
      K_star[i,j] = rbf_var_t*exp(-pow(X_star[i,1] - X_star[j,1], 2)/rbf_lengthscale_sq_t) +
                    rbf_var_ntl*exp(-pow(X_star[i,2] - X_star[j,2], 2)/rbf_lengthscale_sq_ntl); //- pow(X_star[i,3] - X_star[j,3], 2)/rbf_lengthscale_sq_lit);
                    
      K_star[j,i] = K_star[i,j];
    }  
  }
  
}

model {
  
  vector[num_star] M_star;
  vector[num_star] GP_star;
  
  for (i in 1:num_data) M_star[i] = M_prior_data[i];
  for (i in 1:num_pred) M_star[num_data + i] = M_prior_pred[i];
  
  for (i in 1:num_data) GP_star[i] = GP_data[i];
  for (i in 1:num_pred) GP_star[num_data + i] = GP_pred[i];
  
  rbf_var_t ~ gamma(8, 1);
  rbf_var_ntl ~ gamma(8, 1);
  rbf_lengthscale_sq_t ~ gamma(3, 1);
  //rbf_lengthscale_sq_lit ~ gamma(3, 1);
  
  GP_star ~ multi_normal(M_star, K_star);
  
  for (i in 1:num_data) Y_data[i] ~ binomial_logit(T_data[i], GP_data[i]);
  
}

