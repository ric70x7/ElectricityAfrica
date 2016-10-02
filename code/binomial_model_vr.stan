// Binomial regression with GP
// ---------------------------
//
// Edited: October 2, 2016


data {
  
  int<lower=1> input_dim;
  int<lower=1> output_dim;
  int<lower=1> num_data[output_dim];
  int<lower=0> num_pred[output_dim];
  int<lower=0> Y_data[num_data[1]];
  int<lower=0> T_data[num_data[1]];
  //int<lower=0> T_pred[num_pred[1]];
  real Z_data[sum(num_data) - num_data[1]];
  vector[sum(num_data)] M_prior_data;
  vector[sum(num_pred)] M_prior_pred;
  matrix[sum(num_data), input_dim] X_data;
  matrix[sum(num_pred), input_dim] X_pred;
  
}

transformed data {
  
  int accum_data[1+output_dim];
  int accum_pred[1+output_dim];
  int accum_star[1+output_dim];
  //int<lower=0> T_star[num_data[1]+num_pred[1]];
  matrix[sum(num_data)+sum(num_pred), input_dim] X_star;
  
  accum_data[1] = 0;
  accum_pred[1] = 0;
  accum_star[1] = 0;
  for (n in 1:output_dim) {
    accum_data[n+1] = num_data[n] + accum_data[n];
    accum_pred[n+1] = num_pred[n] + accum_pred[n];
    accum_star[n+1] = num_data[n] + num_pred[n] + accum_star[n];
  }
  
  for (n in 1:output_dim) {
    for (i in 1:num_data[n]) {
      X_star[i + accum_data[n] + accum_pred[n]] = X_data[i];
    }
    for (i in 1:num_pred[n]) {
      X_star[i + accum_data[n+1] + accum_pred[n]] = X_pred[i];
    }
  }
  
  //for (i in 1:num_data[1]) T_star[i] = T_data[i];
  //for (i in 1:num_pred[n])  T_star[i + accum_data[2]] = T_pred[i];
  
}

parameters {
  
  real<lower=0> rbf_var[input_dim];
  real<lower=0> rbf_lengthscale_sq[input_dim];
  real<lower=0, upper=1> rho[output_dim - 1];
  real<lower=0> noise_var;
  vector[sum(num_data)] GP_data;
  vector[sum(num_pred)] GP_pred;
  
}

transformed parameters {
  
  cov_matrix[accum_star[output_dim+1]] K_star;
  
  
  // Diagonal blocks
  for(n in 1:output_dim) {
    // Main diagonal 
    for(i in (1+accum_star[n]):accum_star[n+1])  K_star[i, i] = sum(rbf_var) + 1e-8;
    // Off-diagonal 
    for(i in (accum_star[n]+1):(accum_star[n+1]-1)) {
      for(j in (i+1):(accum_star[n+1])) {
        K_star[i,j] = 0;
        for (d in 1:input_dim) {
          K_star[i,j] = K_star[i,j] + rbf_var[d] * exp(-pow(X_star[i,d] - X_star[j,d], 2)/rbf_lengthscale_sq[d]);
        }
        K_star[j,i] = K_star[i,j];
      }
    }
  }
  
  // Off-diagonal blocks
  for (n in 1:(output_dim-1)) {
    // Diagonals
    for(i in (1+accum_star[n]):accum_star[n+1]) {
      K_star[i, i + accum_star[n+1]] = rho[n] * sum(rbf_var) + 1e-8;
      K_star[i + accum_star[n+1], i] = rho[n] * sum(rbf_var) + 1e-8;
    }
    // Off-diagonals 
    for(i in (accum_star[n]+1):(accum_star[n+1]-1)) {
      for(j in (i+1+accum_star[n+1]):(accum_star[n+2])) {
        K_star[i,j] = 0;
        for (d in 1:input_dim) {
          K_star[i,j] = K_star[i,j] + rho[n] * rbf_var[d] * exp(-pow(X_star[i,d] - X_star[j,d], 2)/rbf_lengthscale_sq[d]);
        }
        K_star[j,i] = K_star[i,j];
      }
    }
  }
  
  // Non-correlated blocks 
  if (output_dim > 2) {
    for (n in (1+1):(output_dim-1)) {
      for(i in (1+accum_star[n+1]):accum_star[n+2]) K_star[i,i] = 0;
        
      for(i in (accum_star[n]+1):(accum_star[n+1]-1)) {
        for(j in (i+1+accum_star[n+1]):(accum_star[n+2])) {
          K_star[i,j] = 0;
          K_star[j,i] = 0;
        }
      }
    }
  }
  
}

model {
  
  vector[sum(num_data) + sum(num_pred)] M_star;
  vector[sum(num_data) + sum(num_pred)] GP_star;
  
  
  for (n in 1:output_dim) {
    for (i in 1:num_data[n]) M_star[i + accum_data[n] + accum_pred[n]] = M_prior_data[i];
    for (i in 1:num_pred[n]) M_star[i + accum_data[n+1] + accum_pred[n]] = M_prior_pred[i];
    
  }
  
  for (n in 1:output_dim) {
    for (i in 1:num_data[n]) GP_star[i + accum_data[n] + accum_pred[n]] = GP_data[i];
    for (i in 1:num_pred[n]) GP_star[i + accum_data[n+1] + accum_pred[n]] = GP_pred[i];
    
  }
  
  noise_var ~ gamma(1, 1);
  for (i in 1:input_dim) rbf_var[i] ~ gamma(1, 1);
  for (i in 1:input_dim) rbf_lengthscale_sq[i] ~ gamma(1, 1);
  for (i in 1:output_dim) rho[i] ~ beta(1, 1);
  
  GP_star ~ multi_normal(M_star, K_star);
  
  for (i in 1:num_data[1]) {
    Y_data[i] ~ binomial_logit(T_data[i], GP_data[i]);
  }
  
  for (n in 2:output_dim) {
    for (i in 1:num_data[n]) {
      Z_data[i + accum_data[n-1]] ~ normal(GP_data[i + accum_data[n]], noise_var);
    }
  }
  
}
