# Beta regression models to fit WDI estimates of electricity access
# -----------------------------------------------------------------
#
# Edited: October 3, 2016
# This is the core file (train and test with non obfuscated data)

library(betareg)
library(rstan)
rstan_options(aut_write = TRUE)
options(mc.cores = 10)

graphics.off()
rm(list = ls())

logit <- function(x){
  y <- ifelse(x == 0 | x == 1, -1 + (.001)^(1-x) + (1-.001)^(x), x)
  return(log(y/(1-y)))
}

load("code_output/country_stats.RData")
solkm <- read.csv("data/sol_km_w_iso3.csv")
#iso3i <- "UGA"
iso3i <- "KEN"
k_params <- data.frame(iso3 = paste(sort(unique(country_stats$iso3))))
z_country_stats <- country_stats[,c("iso3", "year", "num_households", "num_litpix",  "num_lithouseholds", "ntl_pkh")]
z_country_stats$sol_km2 <- NA
z_country_stats$r <- NA

for(iso3i in unique(country_stats$iso3)){
  
  zix <- z_country_stats$iso3 == iso3i
  rix <- z_country_stats$iso3 == iso3i
  z_country_stats$r[zix] <- raw_country_stats$r[rix]
  z_country_stats$logit_r[zix] <- logit(raw_country_stats$r[rix])
  
  sdf1 <- z_country_stats[zix,]
  z_country_stats$num_obsv[zix] <- sum(!is.na(sdf1$r))
  #sdf2 <- subset(z_stats, iso3 == iso3i)
  
  if(sum(!is.na(sdf1$r)) >= 2){
    mean1 <- mean(sdf1$logit_r, na.rm = TRUE) 
    sd1 <- sd(sdf1$logit_r, na.rm = TRUE) 
  }else{
    mean1 <- mean(z_country_stats$logit_r, na.rm = TRUE)
    sd1 <- sd(z_country_stats$logit_r, na.rm = TRUE)
  }
  
  z_country_stats$num_litpix[zix] <- scale(sdf1$num_litpix) * sd1 + mean1
  z_country_stats$num_lithouseholds[zix] <- scale(sdf1$num_lithouseholds) * sd1 + mean1
  z_country_stats$ntl_pkh[zix] <- scale(sdf1$ntl_pkh) * sd1 + mean1
  z_country_stats$sol_km2[zix] <- scale(as.numeric(solkm[solkm$iso3 == iso3i,2:17])) * sd1 + mean1
  
}
z_country_stats$y <- z_country_stats$r * z_country_stats$num_households
z_country_stats$y <- sapply(z_country_stats$y, function(x) max(1, floor(x)))
                           

df <- subset(z_country_stats, iso3 == "SEN")
plot(df$year, logit(df$r), col = "red", pch = 16)
lines(df$year, df$num_lithouseholds, col = "blue")
lines(df$year, df$sol_km2, col = "green")

plot(z_country_stats$num_litpix[z_country_stats$num_obsv > 2], z_country_stats$logit_r[z_country_stats$num_obsv > 2], col = "red")

# Functions to process samples
mean_inv_logit <- function(x){
  return(apply(1/(1+exp(-x)), 3, FUN = mean, na.rm = TRUE))
} 

ciup_inv_logit <- function(x){
  return(apply(1/(1+exp(-x)), 3, FUN = quantile, na.rm = TRUE, probs = .975))
} 

cilo_inv_logit <- function(x){
  return(apply(1/(1+exp(-x)), 3, FUN = quantile, na.rm = TRUE, probs = .025))
} 

mean_f <- function(x){
  return(apply(x, 3, FUN = mean, na.rm = TRUE))
} 

sd_f <- function(x){
  return(apply(x, 3, FUN = sd, na.rm = TRUE))
} 



# New data to store
iso3list <- unique(country_stats$iso3)
num_countries <- length(iso3list)
X <- 2000:2016
num_years <- length(X)
Y <- matrix(NA, ncol = num_countries, nrow = num_years)
MY_pior <- Y
Z1 <- matrix(NA, ncol = num_countries, nrow = num_years)
Z2 <- matrix(NA, ncol = num_countries, nrow = num_years)
MZ1_prior <- Z1
MZ2_prior <- Z2
num_data <- rep(NA, num_years)

for(i in 1:num_countries){
  iso3i <- iso3list[i]
  
  # Dataframe for each country
  df <- subset(z_country_stats, iso3 == iso3i)
  df$r[df$r == 1] <- .999
  df$r[df$r == 0] <- .001
  
}


#country_stats$f_mean <- NA
#country_stats$f_sd <- NA
#country_stats$r_mean <- NA
#country_stats$r_low <- NA
#country_stats$r_upp <- NA
#country_stats$rbf_var <- NA
#country_stats$rbf_lengthscale_sq <- NA
iso3i <- "ZMB"
# GP model per country
missing_countries <- c()
for(iso3i in unique(country_stats$iso3)){
  
  # Dataframe for each country
  df <- subset(z_country_stats, iso3 == iso3i)
  df$r[df$r == 1] <- .999
  df$r[df$r == 0] <- .001
  
  # Data observed and data to be predicted
  mask_data <- !is.na(df$r)
  mask_pred <- is.na(df$r)
  obsv_pred <- c(df$year[mask_data], df$year[mask_pred])
  ix.order <- sort.list(obsv_pred)
  
  if(sum(mask_data)>1){
  
    if(sum(mask_data) ==  2){
      
      beta_pred <- df$r
      r1 <- df$r[mask_data][1] 
      r2 <- df$r[mask_data][2]
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
      predictor <- r ~ 1 +  year
      beta_model <- betareg(predictor, data = subset(df, !is.na(df$r)))
      beta_pred <- predict(beta_model, newdata = df)
      logit_beta <- log(beta_pred/(1-beta_pred))
    }
    
    z1_predictor <- num_lithouseholds ~ 1 + year
    z1_model <- lm(z1_predictor, data = df)
    z1_pred <- predict(z1_model, df)
    
    z2_predictor <- sol_km2 ~ 1 + year
    z2_model <- lm(z2_predictor, data = df)
    z2_pred <- predict(z2_model, df)
    
    graphics.off() 
    plot(df$year, df$num_lithouseholds, col = "blue", pch = 16)
    lines(df$year, z1_pred, col = "blue")
    points(df$year, df$sol_km2, col = "green", pch = 16)
    lines(df$year, z2_pred, col = "green")
    points(df$year, df$logit_r, col = "red", pch = 16)
    lines(df$year, logit_beta, col = "red")
    
    
    # Organize data
    output_dim <- 3
    rho_dim <- 3
    num_countries <- 1 
    num_years <- 16
    num_data <- sum(mask_data); dim(num_data) <- 1
    N <- sapply(df$num_households, function(x) ifelse(is.na(x), 0, x)); dim(N) <- c(1, num_years)
    Y <- sapply(df$y, function(x) ifelse(is.na(x), -1, x)); dim(Y) <- c(1, num_years)
    ix_data <- c(seq(df$r)[mask_data], rep(-1, sum(mask_pred))); dim(ix_data) <- c(1, num_years)
    ix_pred <- c(seq(df$r)[mask_pred], rep(-1, sum(mask_data))); dim(ix_pred) <- c(1, num_years)
    X <- df$year; 
    MY_prior <- logit_beta; dim(MY_prior) <- c(1, num_years)
    MZ_prior <- c(z1_pred, z2_pred); dim(MZ_prior) <- c(1, 2*num_years)
    Z <- c(df$num_lithouseholds, df$sol_km2); dim(Z) <- c(1,length(Z))
    
    # GP regression 1D 
    mb1 <- stan(file="code/vector_gp_mixed_noise.stan",
                data=list(num_countries = num_countries,
                          num_years = num_years,
                          output_dim = output_dim,
                          rho_dim = rho_dim,
                          num_data = num_data,
                          ix_data = ix_data,
                          N = N,
                          X = X,
                          Y = Y,
                          Z = Z,
                          MY_prior = MY_prior,
                          MZ_prior = MZ_prior),
                warmup = 100, iter = 200, chains = 1, verbose = TRUE)
                #warmup = 3000, iter = 4000, chains = 10)#, verbose = TRUE)
    
    # Process samples
    mb1_samples <- extract(mb1, permuted = TRUE)
    
    
    f1_mean <- mean_f(mb1_samples$GPY)
    f1_sd <- sd_f(mb1_samples$GPY)
    z_mean <- mean_f(mb1_samples$GPZ)
    z_sd <- sd_f(mb1_samples$GPZ)
    f2_mean <- z_mean[1:16]; f3_mean <- z_mean[17:32]
    f2_sd <- z_sd[1:16]; f3_sd <- z_sd[17:32]
    
    
    r_mean <- c(mean_inv_logit(mb1_samples$GPY))
    r_ciup <- c(ciup_inv_logit(mb1_samples$GPY))
    r_cilo <- c(cilo_inv_logit(mb1_samples$GPY))
    
    graphics.off()
    plot(df$year, f1_mean, col = "red")
    lines(df$year, f1_mean + 1.96 * f1_sd, col = "red", lty = 2)
    lines(df$year, f1_mean - 1.96 * f1_sd, col = "red", lty = 2)
    points(df$year, df$logit_r, col = "red", pch = 16)
    lines(df$year, MY_prior, col = "red")
    
    plot(df$year, f2_mean, col = "blue")
    lines(df$year, f2_mean + 1.96 * f2_sd, col = "blue", lty = 2)
    lines(df$year, f2_mean - 1.96 * f2_sd, col = "blue", lty = 2)
    points(df$year[1:14], Z[1:14], col = "blue", pch = 16)
    lines(df$year, MZ_prior[1:16], col = "blue")
    points(df$year, f1_mean, col = "red")
    
    points(df$year, f3_mean, col = "green")
    lines(df$year, f3_mean + 1.96 * f3_sd, col = "green", lty = 2)
    lines(df$year, f3_mean - 1.96 * f3_sd, col = "green", lty = 2)
    points(df$year[1:14], Z[17:30], col = "green", pch = 16)
    lines(df$year, z2_pred, col = "green")
    
    
    plot(df$year, r_mean, col = "red", ylim = c(0,1))
    lines(df$year, r_ciup, col = "red", lty = 2)
    lines(df$year, r_cilo, col = "red", lty = 2)
    points(df$year, df$r, col = "red", pch = 16)
    lines(df$year, beta_pred, col = "red")
    
    
    
    hist(mb1_samples$noise_var)
    hist(mb1_samples$rbf_var)
    hist(mb1_samples$rbf_lengthscale_sq)
    hist(mb1_samples$rho[,1], xlim = c(0,1))
    hist(mb1_samples$rho[,2], xlim = c(0,1))
    hist(mb1_samples$rho[,3], xlim = c(0,1))
    
    
    
    
    # Store data 
    csix <- country_stats$iso3 == iso3i
    country_stats$r_mean[csix] <- mb1_e_mean
    country_stats$r_low[csix] <- mb1_e_cilo
    country_stats$r_upp[csix] <- mb1_e_ciup
    country_stats$f_mean[csix] <- mb1_f_mean
    country_stats$f_sd[csix] <- mb1_f_sd
    country_stats$rbf_var[csix] <- ifelse(input_dim == 1, mean(mb1_samples$rbf_var), apply(mb1_samples$rbf_var, 2, mean)[1])
    country_stats$rbf_lengthscale_sq[csix] <- mean(mb1_samples$rbf_lengthscale_sq)
    
    plot(df$year[mask_data], df$r[mask_data], xlim = c(2000,2015), ylim = c(0,1), pch = 16, col = "black", main = iso3i)
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
  df$r <- subset(raw_country_stats, iso3 == iso3i)
  df$r[df$r == 1] <- .999
  df$r[df$r == 0] <- .001
  
  # Data observed and data to be predicted
  mask_data <- !is.na(df$r)
  mask_pred <- is.na(df$r)
  obsv_pred <- c(df$year[mask_data], df$year[mask_pred])
  ix.order <- sort.list(obsv_pred)
  
  if(sum(mask_data)==1){
    
    beta_pred <- rep(df$r[mask_data], nrow(df)) 
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