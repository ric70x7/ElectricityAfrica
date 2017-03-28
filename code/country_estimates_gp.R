# Annual estimates of household electricity access per country
# -----------------------------------------------------------------
#
# Edited: March 13, 2017

library(rstan)
library(betareg)

graphics.off()
rm(list = ls())

rstan_options(aut_write = TRUE)
options(mc.cores = 20)

# Functions to implement in stan samples
stan_mean <- function(x) apply(x, c(2), FUN = mean, na.rm = TRUE)
stan_lbou <- function(x) apply(x, c(2), FUN = quantile, na.rm = TRUE, probs = .975)
stan_ubou <- function(x) apply(x, c(2), FUN = quantile, na.rm = TRUE, probs = .025)
stan_sdev <- function(x) apply(x, c(2), FUN = sd, na.rm = TRUE)

# Load data
load("code_output/country_stats.RData")


# Create data frame that will contain the results
annual_data <- merge(raw_country_stats[,c("iso3", "year", "r")],
                     country_stats[,c("iso3", "year")], by = c("year", "iso3"))
colnames(annual_data)[3] <- "reported_r"


# Define objects that will be passed to rstan
iso3list <- paste(unique(country_stats$iso3))

# Compute beta regressions for every country
prev_model <- NA
for(iso3i in iso3list){
  
  ixad <- annual_data$iso3 == iso3i
  dfi <- annual_data[ixad,]
  
  # Use data of Morocco on Western Sahara Territory
  if(iso3i == "ESH"){
    dfi$reported_r <- annual_data$reported_r[annual_data$iso3 == "MAR"]
  }
  
  dfi$reported_r[dfi$reported_r >= 1] <- 1 - 1e-4;
  dfi$reported_r[dfi$reported_r <= 0] <- 1e-4;
  mask_obsv <- !is.na(dfi$reported_r)
  
  # Priors for the rate
  if(sum(mask_obsv) > 2){
    
    # Beta regression
    df4beta <- subset(dfi, mask_obsv)
    lm_beta <- betareg(reported_r ~ 1 +  year, data = df4beta)
    beta_prior <- predict(lm_beta, newdata = dfi)
    
    
  }else{
    
    beta_prior <- rep(mean(dfi$reported_r, na.rm = TRUE), nrow(dfi))
    
  }
  
  gp_prior <- log(beta_prior/(1-beta_prior))
  
  gp_beta <- stan(file="code/beta_regression.stan",
             data=list(num_obsv = sum(mask_obsv),
                       num_pred = nrow(dfi),
                       x_obsv = dfi$year[mask_obsv] - 2000,
                       x_pred = dfi$year - 2000,
                       y_obsv = dfi$reported_r[mask_obsv],
                       m_obsv = gp_prior[mask_obsv],
                       m_pred = gp_prior),
             pars = c("f_star", "phi_star", "var_kern", "phi_beta", "y_pred"),
             fit = prev_model,
             warmup = 1500, iter = 51500, chains = 2, verbose = FALSE)
  
  prev_model <- gp_beta
  
  stan_samp <- extract(gp_beta, permuted = TRUE)
  stan_mean <- function(x) apply(x, c(2), FUN = mean, na.rm = TRUE)
  stan_lbou <- function(x) apply(x, c(2), FUN = quantile, na.rm = TRUE, probs = .975)
  stan_ubou <- function(x) apply(x, c(2), FUN = quantile, na.rm = TRUE, probs = .025)
  y_mean <- stan_mean(stan_samp$y_pred)
  y_lbou <- stan_lbou(stan_samp$y_pred)
  y_ubou <- stan_ubou(stan_samp$y_pred)
  f_mean <- stan_mean(stan_samp$f_star[, sum(mask_obsv)+1:16])
  f_sdev <- stan_ubou(stan_samp$f_star[, sum(mask_obsv)+1:16])
  
  plot(dfi$year, dfi$reported_r, ylim = c(0,1), xlim=c(2000,2015), pch = 16)
  lines(dfi$year, beta_prior, col = "gray", lw = 2)
  lines(dfi$year, y_mean, col = "red")
  lines(dfi$year, y_lbou, col = "red", lty = 2)
  lines(dfi$year, y_ubou, col = "red", lty = 2)
  
  
  # Add results to annual_data
  annual_data$r_mean[ixad] <- y_mean
  annual_data$r_lbou[ixad] <- y_lbou
  annual_data$r_ubou[ixad] <- y_ubou
  
  # Latent variable
  annual_data$f_mean[ixad] <- f_mean
  annual_data$f_sdev[ixad] <- f_sdev
  
}

# Save data
save(annual_data, file = "code_output/country_annual_estimates.RData")
