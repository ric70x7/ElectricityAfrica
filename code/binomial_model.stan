// Binomial regression with GP
// ---------------------------
//
// Edited: September 26, 2016


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
  
  real<lower=0> rbf_var[input_dim];
  real<lower=0> rbf_lengthscale_sq[input_dim];
  vector[num_data] GP_data;
  vector[num_pred] GP_pred;
  
}

transformed parameters {
  
  cov_matrix[num_data+num_pred] K_star;
  
  
  // Diagonal elements
  for (i in 1:num_star) K_star[i,i] = sum(rbf_var) + 1e-8;
  
  // Off-diagonal elements
  for (i in 1:(num_star-1)) {
    for (j in (i+1):num_star) {
      K_star[i,j] = 0;
      for (d in 1:input_dim ) {
        K_star[i,j] = K_star[i,j] + rbf_var[d] * exp(-pow(X_star[i,d] - X_star[j,d], 2)/rbf_lengthscale_sq[d]);
      }
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
  
  for (i in 1:input_dim) rbf_var[i] ~ gamma(1, 1);
  for (i in 1:input_dim) rbf_lengthscale_sq[i] ~ gamma(1, 1);
  
  GP_star ~ multi_normal(M_star, K_star);
  
  for (i in 1:num_data) Y_data[i] ~ binomial_logit(T_data[i], GP_data[i]);
  
}
