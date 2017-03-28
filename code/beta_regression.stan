// Beta regression with GP
// -----------------------
//
// Edited: March 14, 2017


data {
  
  int<lower=1> num_obsv;
  int<lower=1> num_pred;
  
  real y_obsv[num_obsv];
  real x_obsv[num_obsv];
  real x_pred[num_pred];
  
  vector[num_obsv] m_obsv;
  vector[num_pred] m_pred;
  
}

transformed data {
  
  int num_star;
  real x_star[num_obsv+num_pred];
  vector[num_obsv+num_pred] m_star;
  matrix[num_obsv+num_pred, num_obsv+num_pred] kern_core;
  
  num_star = num_obsv + num_pred; 
  
  for (i in 1:num_obsv) {
    x_star[i] = x_obsv[i];
    m_star[i] = m_obsv[i];
  }
  
  for (i in 1:num_pred) {
    x_star[num_obsv+i] = x_pred[i];
    m_star[num_obsv+i] = m_pred[i];
  }
  
  // kern_core: linear kernel with offset
  for (i in 1:num_star) kern_core[i,i] = 1 + pow(x_star[i],2) + 1e-6;
  for (i in 1:(num_star-1)) {
    for (j in (i+1):num_star) {
      kern_core[i,j] = 1+x_star[i]*x_star[j];
      kern_core[j,i] = kern_core[i,j];
    }  
  }
  
}

parameters {
  
  real<lower=0> var_kern;
  real<lower=0> phi_beta;
  vector[num_star] f_star;
  
}

transformed parameters {
  
  vector[num_star] phi_star;
  cov_matrix[num_star] kern_star;
  
  for (i in 1:num_star) phi_star[i] = inv_logit(f_star[i]);
  
  kern_star = var_kern * kern_core;
  
}

model {
  
  var_kern ~ chi_square(.1);
  phi_beta ~ chi_square(1500);
  f_star ~ multi_normal(m_star, kern_star);
    
  for (i in 1:num_obsv) y_obsv[i] ~  beta(phi_star[i]*phi_beta, (1-phi_star[i])*phi_beta);
  
  
}

generated quantities {

  real y_pred[num_pred];
  
  for(i in 1:num_pred) y_pred[i] = beta_rng(phi_star[num_obsv+i]*phi_beta, (1-phi_star[num_obsv+i])*phi_beta);
    
}
