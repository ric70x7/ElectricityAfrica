# Beta regression models to fit WDI estimates of electricity access
# -----------------------------------------------------------------
#
# Edited: September 26, 2016
# This is the core file (train and test with non obfuscated data)

library(betareg)
library(rstan)
rstan_options(aut_write = TRUE)
options(mc.cores = 10)


graphics.off()
rm(list = ls())
load("code_output/country_stats.RData")


# Functions to process samples
mean_inv_logit <- function(x){
  return(apply(1/(1+exp(-x)), 2, FUN = mean, na.rm = TRUE))
} 

ciup_inv_logit <- function(x){
  return(apply(1/(1+exp(-x)), 2, FUN = quantile, na.rm = TRUE, probs = .975))
} 

cilo_inv_logit <- function(x){
  return(apply(1/(1+exp(-x)), 2, FUN = quantile, na.rm = TRUE, probs = .025))
} 

mean_f <- function(x){
  return(apply(x, 2, FUN = mean, na.rm = TRUE))
} 

sd_f <- function(x){
  return(apply(x, 2, FUN = sd, na.rm = TRUE))
} 


country_stats$iso3
country_stats$lh_h <- country_stats$num_lithouseholds/country_stats$num_households

# New data to store
country_stats$f_mean <- NA
country_stats$f_sd <- NA
country_stats$r_mean <- NA
country_stats$r_low <- NA
country_stats$r_upp <- NA
country_stats$rbf_var <- NA
country_stats$rbf_lengthscale_sq <- NA


# GP model per country
missing_countries <- c()
for(iso3i in unique(country_stats$iso3)){
  
  # Dataframe for each country
  df <- subset(country_stats, iso3 == iso3i)
  df$r_obsv <- subset(raw_country_stats, iso3 == iso3i)$r
  df$r_obsv[df$r_obsv == 1] <- .999
  df$r_obsv[df$r_obsv == 0] <- .001
  
  # Data observed and data to be predicted
  mask_data <- !is.na(df$r_obsv)
  mask_pred <- is.na(df$r_obsv)
  obsv_pred <- c(df$year[mask_data], df$year[mask_pred])
  ix.order <- sort.list(obsv_pred)
  
  if(sum(mask_data)>1){
  
    if(sum(mask_data) ==  2){
      
      beta_pred <- df$r_obsv
      r1 <- df$r_obsv[mask_data][1] 
      r2 <- df$r_obsv[mask_data][2]
      y1 <- df$year[mask_data][1]
      y2 <- df$year[mask_data][2]  
      slope <- (r2-r1)/(y2-y1)
      beta_pred[df$year < y1] <- r1
      beta_pred[df$year > y2] <- r2
      if(iso3i != "MRT"){
        for(j in (sum(df$year<=y1) + 1):sum(df$year<y2)){
          beta_pred[j] <-beta_pred[j-1] + slope
        }
        logit_beta <- log(beta_pred/(1-beta_pred))
        
      }
    }else{
  
      # Beta regression: this is our mean prior
      predictor <- r_obsv ~ 1 +  year
      beta_model <- betareg(predictor, data = subset(df, !is.na(df$r_obsv)))
      beta_pred <- predict(beta_model, newdata = df)
      logit_beta <- log(beta_pred/(1-beta_pred))
      
    }
    
    # Input matrix
    X <- matrix(cbind(df$year, df$lh_h, df$num_litpix), ncol = 3)
    X[,1] <- scale(X[,1], center = mean(X[,1]), scale = sd(X[,1]))
    X[,2] <- scale(X[,2], center = mean(X[,2]), scale = sd(X[,2]))
    X[,3] <- scale(X[,3], center = mean(X[,3]), scale = sd(X[,3]))
    
    # GP regression 1D 
    input_dim <- 1 
    mb1 <- stan(file="code/binomial_model.stan",
                data=list(X_data = matrix(X[mask_data, 1:input_dim], ncol = input_dim),
                          X_pred = matrix(X[mask_pred, 1:input_dim], ncol = input_dim),
                          Y_data = floor(df$num_households[mask_data] * df$r_obsv[mask_data]),
                          T_data = df$num_households[mask_data],
                          T_pred = df$num_households[mask_pred],
                          input_dim = input_dim,
                          num_data = sum(mask_data),
                          num_pred = nrow(df) - sum(mask_data),
                          M_prior_data = logit_beta[mask_data],
                          M_prior_pred = logit_beta[mask_pred]),
                warmup = 3000, iter = 4000, chains = 10)#, verbose = TRUE)
    
    # Process samples
    mb1_samples <- extract(mb1, permuted = TRUE)
    mb1_f_mean <- c(mean_f(mb1_samples$GP_data), mean_f(mb1_samples$GP_pred))[ix.order]
    mb1_f_sd <- c(sd_f(mb1_samples$GP_data), sd_f(mb1_samples$GP_pred))[ix.order]
    mb1_e_mean <- c(mean_inv_logit(mb1_samples$GP_data), mean_inv_logit(mb1_samples$GP_pred))[ix.order]
    mb1_e_ciup <- c(ciup_inv_logit(mb1_samples$GP_data), ciup_inv_logit(mb1_samples$GP_pred))[ix.order]
    mb1_e_cilo <- c(cilo_inv_logit(mb1_samples$GP_data), cilo_inv_logit(mb1_samples$GP_pred))[ix.order]
    
    # Store data 
    csix <- country_stats$iso3 == iso3i
    country_stats$r_mean[csix] <- mb1_e_mean
    country_stats$r_low[csix] <- mb1_e_cilo
    country_stats$r_upp[csix] <- mb1_e_ciup
    country_stats$f_mean[csix] <- mb1_f_mean
    country_stats$f_sd[csix] <- mb1_f_sd
    country_stats$rbf_var[csix] <- ifelse(input_dim == 1, mean(mb1_samples$rbf_var), apply(mb1_samples$rbf_var, 2, mean)[1])
    country_stats$rbf_lengthscale_sq[csix] <- mean(mb1_samples$rbf_lengthscale_sq)
    
    plot(df$year[mask_data], df$r_obsv[mask_data], xlim = c(2000,2015), ylim = c(0,1), pch = 16, col = "black", main = iso3i)
    lines(df$year, beta_pred, col = "black")
    lines(df$year, mb1_e_mean, col = "blue")
    lines(df$year, mb1_e_ciup, col = "blue", lty = 2)
    lines(df$year, mb1_e_cilo, col = "blue", lty = 2)
    
    
  }else{
    
    missing_countries <- c(missing_countries, iso3i)
    
  }
  
}


# GP samples for countries with single observations
mean_rbf <- mean(country_stats$rbf_var, na.rm = TRUE)
mean_lengthscale_sq <- mean(country_stats$rbf_lengthscale_sq, na.rm = TRUE)
for(iso3i in missing_countries){
  # Dataframe for each country
  df <- subset(country_stats, iso3 == iso3i)
  df$r_obsv <- subset(raw_country_stats, iso3 == iso3i)
  df$r_obsv[df$r_obsv == 1] <- .999
  df$r_obsv[df$r_obsv == 0] <- .001
  
  # Data observed and data to be predicted
  mask_data <- !is.na(df$r_obsv)
  mask_pred <- is.na(df$r_obsv)
  obsv_pred <- c(df$year[mask_data], df$year[mask_pred])
  ix.order <- sort.list(obsv_pred)
  
  if(sum(mask_data)==1){
    
    beta_pred <- rep(df$r_obsv[mask_data], nrow(df)) 
    logit_beta <- log(beta_pred/(1-beta_pred))
    
    # Input matrix
    X <- matrix(cbind(df$year, df$lh_h, df$num_litpix), ncol = 3)
    X[,1] <- scale(X[,1], center = mean(X[,1]), scale = sd(X[,1]))
    X[,2] <- scale(X[,2], center = mean(X[,2]), scale = sd(X[,2]))
    X[,3] <- scale(X[,3], center = mean(X[,3]), scale = sd(X[,3]))
    
    # GP simulation
    input_dim <- 1 
    mb1 <- stan(file="code/binomial_sampler.stan",
                data=list(X_data = X[mask_data, 1:input_dim],
                          X_pred = matrix(X[mask_pred, 1:input_dim], ncol = input_dim),
                          Y_data = floor(df$num_households[mask_data] * df$r[mask_data]),
                          T_data = df$num_households[mask_data],
                          T_pred = df$num_households[mask_pred],
                          input_dim = input_dim,
                          num_data = sum(mask_data),
                          num_pred = nrow(df) - sum(mask_data),
                          M_prior_data = logit_beta[mask_data],
                          M_prior_pred = logit_beta[mask_pred],
                          rbf_var = mean_rbf,
                          rbf_lengthscale_sq = mean_lengthscale_sq),
                warmup = 1000, iter = 2000, chains = 10)
    
    
    # Process samples
    mb1_samples <- extract(mb1, permuted = TRUE)
    mb1_f_mean <- c(mean(mb1_samples$GP_data), mean_f(mb1_samples$GP_pred))[ix.order]
    mb1_f_sd <- c(sd(mb1_samples$GP_data), sd_f(mb1_samples$GP_pred))[ix.order]
    mb1_e_mean <- c(mean(1/(1+exp(-mb1_samples$GP_data))), mean_inv_logit(mb1_samples$GP_pred))[ix.order]
    mb1_e_ciup <- c(quantile(1/(1+exp(-mb1_samples$GP_data)), probs = .975), ciup_inv_logit(mb1_samples$GP_pred))[ix.order]
    mb1_e_cilo <- c(quantile(1/(1+exp(-mb1_samples$GP_data)), probs = .025), cilo_inv_logit(mb1_samples$GP_pred))[ix.order]
    
    # Store data 
    csix <- country_stats$iso3 == iso3i
    country_stats$r_mean[csix] <- mb1_e_mean
    country_stats$r_low[csix] <- mb1_e_cilo
    country_stats$r_upp[csix] <- mb1_e_ciup
    country_stats$f_mean[csix] <- mb1_f_mean
    country_stats$f_sd[csix] <- mb1_f_sd
    country_stats$rbf_var[csix] <- mean_rbf
    country_stats$rbf_lengthscale_sq[csix] <- mean_lengthscale_sq
    
    #plot(df$year[mask_data], df$r[mask_data], xlim = c(2000,2015))
    plot(df$year[mask_data], df$r[mask_data], xlim = c(2000,2015), ylim = c(0,1), pch = 16, col = "black", main = iso3i)
    lines(df$year, beta_pred, col = "black")
    lines(df$year, mb1_e_mean, col = "blue")
    lines(df$year, mb1_e_ciup, col = "blue", lty = 2)
    lines(df$year, mb1_e_cilo, col = "blue", lty = 2)
    
  }
  
}
    
# Use data of Morocco on Western Sahara Territory
ixesh <- country_stats$iso3 == "ESH"
ixmar <- country_stats$iso3 == "MAR"
country_stats[ixesh, 13:ncol(country_stats)] <- country_stats[ixmar, 13:ncol(country_stats)]


save(country_stats, file = "code_output/country_annual_estimates.RData")
