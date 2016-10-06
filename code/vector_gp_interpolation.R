# Annual estimates of household electricity access per country
# -----------------------------------------------------------------
#
# Edited: October 5, 2016

library(betareg)
library(rstan)
rstan_options(aut_write = TRUE)
options(mc.cores = 10)

graphics.off()
rm(list = ls())


# Link functions function (transforms offests values 0 and 1)
logit <- function(x){
  y <- ifelse(x == 0 | x == 1, -1 + (.001)^(1-x) + (1-.001)^(x), x)
  return(log(y/(1-y)))
}
inv_logit <- function(x) 1/(1+exp(-x))


# Functions to be applied on stans's samples
mean_f <- function(x) apply(x, c(2,3), FUN = mean, na.rm = TRUE)
sd_f <- function(x) apply(x, c(2,3), FUN = sd, na.rm = TRUE)
mean_inv_logit <- function(x) apply(inv_logit(x), c(2,3), FUN = mean, na.rm = TRUE)
sd_inv_logit <- function(x) apply(inv_logit(x), c(2,3), FUN = sd, na.rm = TRUE)
ciup_inv_logit <- function(x) apply(inv_logit(x), c(2,3), FUN = quantile, na.rm = TRUE, probs = .975)
cilo_inv_logit <- function(x) apply(inv_logit(x), c(2,3), FUN = quantile, na.rm = TRUE, probs = .025)


# Load data
load("code_output/country_stats.RData")
solkm <- read.csv("data/sol_km_w_iso3.csv")

# Create data frame that will contain the results
annual_data <- merge(raw_country_stats[,c("iso3", "year", "r")],
                     country_stats[,c("iso3", "year", "num_households", "num_lithouseholds")],
                     by = c("year", "iso3"))
colnames(annual_data)[3] <- "reported_r"

# Add sum of lights per km^2
annual_data$sol_km2 <- NA
for(iso3i in unique(annual_data$iso3)){
  ixsl <- solkm$iso3 == iso3i
  ixad <- annual_data$iso3 == iso3i
  annual_data$sol_km2[ixad] <- as.numeric(solkm[ixsl,2:17])
}

# Remove NTL related data of 2014 and 2015
# NOTE: NTL values of these years is not available, 2013 was used
annual_data[annual_data$year %in% c(2014,2015), c("sol_km2","num_lithouseholds")] <- NA


# Define objects that will be passed to rstan
X <- 2000:2015
iso3list <- paste(unique(country_stats$iso3))
num_countries <- length(iso3list)
num_years <- length(X)
num_data <- matrix(rep(NA, num_countries), dimnames = list(iso3list))
ix_data <- matrix(-1, nrow = num_countries, ncol = num_years, dimnames = list(iso3list))
N <- matrix(NA, nrow = num_countries, ncol = num_years, dimnames = list(iso3list))
Y <- matrix(NA, nrow = num_countries, ncol = num_years, dimnames = list(iso3list))
Z1 <- matrix(NA, nrow = num_countries, ncol = num_years, dimnames = list(iso3list))
Z2 <- matrix(NA, nrow = num_countries, ncol = num_years, dimnames = list(iso3list))
MR_prior <- Y
MY_prior <- Y
MZ1_prior <- Z1
MZ2_prior <- Z2

for(iso3i in iso3list){
  
  ixad <- annual_data$iso3 == iso3i
  ixmt <- dimnames(Y)[[1]] == iso3i
  df <- annual_data[ixad,]
  
  # Use data of Morocco on Western Sahara Territory
  if(iso3i == "ESH"){
    df$reported_r <- annual_data$reported_r[annual_data$iso3 == "MAR"]
  }
  
  # Observed points mask
  mask_obsv <- !is.na(df$reported_r)
  ix_data[ixmt,seq(sum(mask_obsv))] <- seq(num_years)[mask_obsv]
  num_data[ixmt] <- sum(mask_obsv)
  
  offset_param <- mean(logit(df$reported_r), na.rm = TRUE)
  if(sum(mask_obsv)==1){
    scale_param <- sd(logit(annual_data$reported_r), na.rm = TRUE)
  }else{
    scale_param <- sd(logit(df$reported_r), na.rm = TRUE)
  }
  
  N[ixmt,] <- df$num_households
  Y[ixmt,] <- floor(df$num_households * df$reported_r)
  Z1[ixmt,] <- (scale(df$sol_km2) * scale_param + offset_param)
  Z2[ixmt,] <- (scale(df$num_lithouseholds) * scale_param + offset_param)
  
  
  # Priors for the rate
  if(sum(mask_obsv) > 2){
    
    # Beta regression
    r_predictor <- reported_r ~ 1 +  year
    r_model <- betareg(r_predictor, data = subset(df, mask_obsv))
    beta_pred <- predict(r_model, newdata = df)
    logit_beta <- logit(beta_pred)
    
  }else{
    
    if(sum(mask_obsv) ==  2){
      
      # Linear interpolation between observations,
      # plateau before/after first/last observations
      beta_pred <- df$reported_r
      r1 <- beta_pred[mask_obsv][1]
      r2 <- beta_pred[mask_obsv][2]
      y1 <- X[mask_obsv][1]
      y2 <- X[mask_obsv][2]  
      slope <- (r2-r1)/(y2-y1)
      beta_pred[X < y1] <- r1
      beta_pred[X > y2] <- r2
      if(y2-y1 > 1){
        for(j in (sum(X<=y1) + 1):sum(X<y2)){
          beta_pred[j] <-beta_pred[j-1] + slope
        }
      }
      
    }else{
      
      beta_pred <- rep(df$reported_r[mask_obsv], num_years)
      
    }
  }
  
  MR_prior[ixmt,] <- beta_pred
  MY_prior[ixmt,] <- logit(beta_pred)
  
  # Priors for Z1
  z1_predictor <- sol_km2 ~ 1 + year
  z1_model <- lm(z1_predictor, data = data.frame(year = X, sol_km2 = Z1[ixmt,]))
  z1_pred <- predict(z1_model, df)
  MZ1_prior[ixmt,] <- z1_pred
  
  # Priors for Z2
  z2_predictor <- num_lithouseholds ~ 1 + year
  z2_model <- lm(z2_predictor, data = data.frame(year = X, num_lithouseholds = Z2[ixmt,]))
  z2_pred <- predict(z2_model, df)
  MZ2_prior[ixmt,] <- z2_pred
  
  # Enconde NA values
  Y[ixmt,] <- sapply(Y[ixmt,], function(x) ifelse(is.na(x), -100, x))
  Z1[ixmt,] <- sapply(Z1[ixmt,], function(x) ifelse(is.na(x), -100, x))
  Z2[ixmt,] <- sapply(Z2[ixmt,], function(x) ifelse(is.na(x), -100, x))
  
}


# This is just to run the model on a subset of countries
items <- 1:length(num_countries)
num_countries_ <- length(items)
output_dim <- 3
rho_dim <- 3
num_data_ <- num_data[items]; dim(num_data_) <- num_countries_
ix_data_ = ix_data[items,]; dim(ix_data_) <- c(num_countries_, num_years)
N_ <- N[items,]; dim(N_) <- c(num_countries_, num_years)
X
Y_ <- Y[items,]; dim(Y_) <- c(num_countries_, num_years)
Z_ <- cbind(Z1,Z2)[items,]; dim(Z_) <- c(num_countries_, 2*num_years)
MY_prior_ <- MY_prior[items,]; dim(MY_prior_) <- c(num_countries_, num_years)
MZ_prior_ <- cbind(MZ1_prior,MZ2_prior)[items,]; dim(MZ_prior_) <- c(num_countries_, 2*num_years)


# Run stan model
vgpm <- stan(file="code/vector_gp_mixed_noise.stan",
             data=list(num_countries = num_countries_,
                       num_years = num_years,
                       output_dim = output_dim,
                       rho_dim = rho_dim,
                       num_data = num_data_,
                       ix_data = ix_data_,
                       N = N_,
                       X = X,
                       Y = Y_,
                       Z = Z_,
                       MY_prior = MY_prior_,
                       MZ_prior = MZ_prior_),
             warmup = 2500, iter = 5000, chains = 1, verbose = TRUE)


# Extract samples
vgpm_samples <- extract(vgpm, permuted = TRUE)


# Compute mean, sd and credible intervals
f1_mean <- mean_f(vgpm_samples$GPY)
f1_sd <- sd_f(vgpm_samples$GPY)
z_mean <- mean_f(vgpm_samples$GPZ)
z_sd <- sd_f(vgpm_samples$GPZ)
f2_mean <- z_mean[,1:16]; f3_mean <- z_mean[,17:32]
f2_sd <- z_sd[,1:16]; f3_sd <- z_sd[,17:32]
r_mean <- mean_inv_logit(vgpm_samples$GPY)
r_sd <- sd_inv_logit(vgpm_samples$GPY)
r_cilo <- cilo_inv_logit(vgpm_samples$GPY)
r_ciup <- ciup_inv_logit(vgpm_samples$GPY)

# Add results to annual_data
for(iso3i in iso3list){
  ixad <- annual_data$iso3 == iso3i
  
  # Electricity access
  annual_data$r_mean[ixad] <- r_mean[ixad,]
  annual_data$r_sd[ixad] <- r_sd[ixad,]
  annual_data$r_cilo[ixad] <- r_cilo[ixad,]
  annual_data$r_ciup[ixad] <- r_ciup[ixad,]
  
  # Latent variable
  annual_data$f_mean[ixad] <- f1_mean[ixad,]
  annual_data$f_sd[ixad] <- f1_sd[ixad,]
  
  # Sum of lights
  annual_data$sol_mean[ixad] <- f2_mean[ixad,]
  annual_data$sol_sd[ixad] <- f2_sd[ixad,]
  
  # Lit households
  annual_data$num_lithouseholds_mean[ixad] <- f3_mean[ixad,]
  annual_data$num_lithouseholds_sd[ixad] <- f3_sd[ixad,]
  
  
}


graphics.off()

item <- 5; mask_item <- ix_data[item, 1:num_data[item]]

plot(X, f1_mean[item,], col = "red", ylim = c(min(z_mean[item,] - 2*z_sd[item,]),max(z_mean[item,] + 2*z_sd[item,]) ) )
lines(X, f1_mean[item,] + 1.96 * f1_sd[item,], col = "red", lty = 2)
lines(X, f1_mean[item,] - 1.96 * f1_sd[item,], col = "red", lty = 2)
lines(X, MY_prior[item,], col = "red")
points(X[mask_item], logit(Y[item,mask_item]/N[item,mask_item]), col = "red", pch = 16)

#plot(X, f2_mean, col = "blue")
points(X, f2_mean[item,], col = "blue")
lines(X, f2_mean[item,] + 1.96 * f2_sd[item,], col = "blue", lty = 2)
lines(X, f2_mean[item,] - 1.96 * f2_sd[item,], col = "blue", lty = 2)
lines(X, MZ1_prior[item,], col = "blue")
points(X[1:14], Z1[item,1:14], col = "blue", pch = 16)

points(X, f3_mean[item,], col = "green")
lines(X, f3_mean[item,] + 1.96 * f3_sd[item,], col = "green", lty = 2)
lines(X, f3_mean[item,] - 1.96 * f3_sd[item,], col = "green", lty = 2)
lines(X, MZ2_prior[item,], col = "green")
points(X[1:14], Z2[item, 1:14], col = "green", pch = 16)


plot(X, r_mean[item,], col = "red", ylim = c(0,1))
lines(X, r_ciup[item,], col = "red", lty = 2)
lines(X, r_cilo[item,], col = "red", lty = 2)
lines(X, MR_prior[item,], col = "red")
points(X[mask_item], Y[item,mask_item]/N[item,mask_item], col = "red", pch = 16)



hist(vgpm_samples$noise_var)
hist(vgpm_samples$rbf_var)
hist(vgpm_samples$rbf_lengthscale_sq)
hist(vgpm_samples$rho[,1], xlim = c(0,1))
hist(vgpm_samples$rho[,2], xlim = c(0,1))
hist(vgpm_samples$rho[,3], xlim = c(0,1))



# Save data
save(annual_data, file = "code_output/country_annual_estimates.RData")
save(vgpm_samples, file = "code_output/country_annual_estimates.RData")