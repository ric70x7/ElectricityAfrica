// Binomial regression with GP
// ---------------------------
//
// Edited: October 5, 2016


data {
  
  int<lower=1> num_countries;
  int<lower=1> num_years;
  int<lower=1> output_dim;
  int<lower=1> rho_dim;
  int<lower=1> num_data[num_countries];
  int<lower=0> N[num_countries, num_years];
  int Y[num_countries, num_years];
  real Z[num_countries, (output_dim-1)*num_years];
  int ix_data[num_countries, num_years];
  vector[num_years] MY_prior[num_countries];
  vector[(output_dim-1)*num_years] MZ_prior[num_countries];
  vector[num_years] X;
  
}

transformed data {
  
  int num_z;
  int num_star;
  matrix[num_years, num_years] K_core;
  
  num_star = output_dim * num_years;
  num_z = (output_dim-1) * num_years;
  
  // K_core
  for (i in 1:num_years) K_core[i,i] = 0;
  for (i in 1:(num_years-1)) {
    for (j in (i+1):num_years) {
      K_core[i,j] = pow(X[i] - X[j], 2)/2;
      K_core[j,i] = K_core[i,j];
    }  
  }
  
}

parameters {
  
  real<lower=0> rbf_var;
  real<lower=0> rbf_lengthscale_sq;
  real<lower=0> noise_var;
  real<lower=0, upper=1> rho[output_dim];
  vector[num_years] GPY[num_countries];
  vector[num_z] GPZ[num_countries];
  
}

transformed parameters {
  
  cov_matrix[num_star] K_star;
  
  for (i in 1:num_years) {
    for (j in 1:num_years) {
      
      // Diagonal blocks
      for (n in 1:output_dim) {
        K_star[i+(n-1)*num_years,j+(n-1)*num_years] = rbf_var*exp(-K_core[i,j]/rbf_lengthscale_sq);
      }
      
      // Off-diagonal blocks
      for (n in 1:(output_dim-1)) {
        K_star[i,j+n*num_years] = rho[n]*K_star[i,j];
        K_star[i+n*num_years,j] = rho[n]*K_star[i,j];
      }
      K_star[i+num_years,j+2*num_years] = rho[3]*K_star[i,j];
      K_star[i+2*num_years,j+num_years] = rho[3]*K_star[i,j];
      
    }
  } 
  
}

model {
  
  vector[num_star] M_star[num_countries];
  vector[num_star] GP_star[num_countries];
  
  rbf_var ~ gamma(1, 1);
  rbf_lengthscale_sq ~ gamma(2, 2);
  noise_var ~ gamma(1, 1);
  rho[1] ~ beta(2, 5);
  rho[2] ~ beta(2, 5);
  rho[3] ~ beta(1, 3);
  
  for (q in 1:num_countries) {
    // Prior mean
    for (i in 1:num_years) M_star[q,i] = MY_prior[q,i];
    for (i in 1:num_z) M_star[q,i + num_years] = MZ_prior[q,i];
    
    // Latent variable 
    for (i in 1:num_years) GP_star[q,i] = GPY[q,i];
    for (i in 1:num_z) GP_star[q,i+num_years] = GPZ[q,i];
    
    GP_star[q] ~ multi_normal(M_star[q], K_star);
    
    for (i in 1:num_data[q]) Y[q,ix_data[q,i]] ~ binomial_logit(N[q,ix_data[q,i]], GPY[q,ix_data[q,i]]);
    
    //NOTE the last two years of NTL data are missing
    for (i in 1:(num_years-2)) Z[q,i] ~ normal(GPZ[q,i], sqrt(noise_var));
    for (i in 1:(num_years-2)) Z[q,i+num_years] ~ normal(GPZ[q,i+num_years], sqrt(noise_var));
    
  }
  
}

generated quantities {
  
  //int W[2, num_years];
  //
  //for(i in 1:2){
  //  for(j in 1:num_years) W[i,j] <- Q[i,j];
  //}
  
  //cov_matrix[num_star] K_new;
  //for(i in 1:num_star){
  //  for(j in 1:num_star){
  //    K_new[i,j] <- K_star[i,j];
  //  }
  //}
  
}
