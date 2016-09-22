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
  int<lower=1> input_dim;
  real<lower=0> Y_data[num_data];
  vector[num_data] M_prior_data;
  matrix[num_data, input_dim] X_data;
  
}

transformed data {
  
  vector[num_data] Z_data;
  
  for (i in 1:num_data) Z_data[i] = log(Y_data[i]/(1 - Y_data[i]));
  
}

parameters {
  
  real<lower=0> rbf_var;
  real<lower=0> rbf_lengthscale_sq;
  vector[num_data] GP_data;
  
}

transformed parameters {
  
  cov_matrix[num_data] K_data;
  
  
  // Diagonal elements
  for (i in 1:num_data) K_data[i,i] = rbf_var + 1e-6;
  
  // Off-diagonal elements
  for (i in 1:(num_data-1)) {
    for (j in (i+1):num_data) {
      K_data[i,j] = rbf_var*exp(-dot_self(X_data[i] - X_data[j])/rbf_lengthscale_sq);
      K_data[j,i] = K_data[i,j];
    }  
  }
  
}

model {
  
  Z_data ~ multi_normal(M_prior_data,K_data);
  rbf_var ~ gamma(8, 1);
  rbf_lengthscale_sq ~ gamma(3, 1);
  
}
