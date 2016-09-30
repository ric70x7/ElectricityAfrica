// Binomial sampler with GP
// ---------------------------
//
// Edited: September 26, 2016


data {
  
  int<lower=1> num_data;
  int<lower=1> num_pred;
  int<lower=1> input_dim;
  int<lower=0> Y_data;//[num_data];
  int<lower=0> T_data;//[num_data];
  int<lower=0> T_pred[num_pred];
  real<lower=0> rbf_var;
  real<lower=0> rbf_lengthscale_sq;
  real M_prior_data;
  vector[num_pred] M_prior_pred;
  real X_data;
  matrix[num_pred, input_dim] X_pred;
  
}

transformed data {
  
  int<lower=1> num_star;
  int<lower=0> T_star[num_data+num_pred];
  matrix[num_data+num_pred, input_dim] X_star;
  cov_matrix[num_data+num_pred] K_star;
  
  num_star = num_data + num_pred;
  
  X_star[num_data, 1] = X_data;
  for (i in 1:num_pred) X_star[num_data + i] = X_pred[i];
  
  for (i in 1:num_data) T_star[i] = T_data;
  for (i in 1:num_pred) T_star[num_data + i] = T_pred[i];
  
  
  // Diagonal elements
  for (i in 1:num_star) K_star[i,i] = rbf_var + 1e-8;
  
  // Off-diagonal elements
  for (i in 1:(num_star-1)) {
    for (j in (i+1):num_star) {
      K_star[i,j] = rbf_var * exp(-pow(X_star[i,1] - X_star[j,1], 2)/rbf_lengthscale_sq);
      K_star[j,i] = K_star[i,j];
    }  
  }
  
  
}

parameters {
  
  //vector[num_data] GP_data;
  real GP_data;
  vector[num_pred] GP_pred;
  
}

transformed parameters {
  
}

model {
  
  vector[num_star] M_star;
  vector[num_star] GP_star;
  
  for (i in 1:num_data) M_star[i] = M_prior_data;
  for (i in 1:num_pred) M_star[num_data + i] = M_prior_pred[i];
  
  for (i in 1:num_data) GP_star[i] = GP_data;
  for (i in 1:num_pred) GP_star[num_data + i] = GP_pred[i];
  
  GP_star ~ multi_normal(M_star, K_star);
  
  for (i in 1:num_data) Y_data ~ binomial_logit(T_data, GP_data);
  
}