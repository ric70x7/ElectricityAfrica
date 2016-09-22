# Beta regression models to fit WDI estimates of electricity access
# -----------------------------------------------------------------
#
# Edited: September 19, 2016
# This is the core file (train and test with non obfuscated data)

library(betareg)
library(rstan)

rm(list = ls())
load("code_output/country_stats.RData")

# Beta regressions
for(iso3i in unique(country_stats$iso3)){
  
  # Dataframe for each country
  df <- subset(country_stats, iso3 == iso3i)
  df$r <- subset(raw_country_stats, iso3 == iso3i)$r
  df$r[df$r == 1] <- .999
  df$r[df$r == 0] <- .001
  
  # Beta regression: this is our mean prior
  predictor <- r ~ 1 +  ntl_pkh
  beta_model <- betareg(predictor, data = subset(df, !is.na(df$r)))
  beta_pred <- predict(beta_model, newdata = df)
  
  # Stan model
  input_dim <- 2 
  mask_data <- !is.na(df$r)
  mask_pred <- is.na(df$r)
  #X_data <- matrix(cbind((df$year[mask_data]-2000)/10, df$ntl_pkh[mask_data], (df$num_litpix[mask_data]-mean(df$num_litpix))/sd(df$num_litpix)), ncol = input_dim)
  #X_pred <- matrix(cbind((df$year[mask_pred]-2000)/10, df$ntl_pkh[mask_pred], (df$num_litpix[mask_pred]-mean(df$num_litpix))/sd(df$num_litpix)), ncol = input_dim)
  X_data <- matrix(cbind((df$year[mask_data]-2000)/10, df$ntl_pkh[mask_data]), ncol = input_dim)
  X_pred <- matrix(cbind((df$year[mask_pred]-2000)/10, df$ntl_pkh[mask_pred]), ncol = input_dim)
  hyper.stan <- stan(file="code/binomial_model.stan",
                     data=list(X_data = X_data,
                               X_pred = X_pred,
                               Y_data = floor(df$num_households[mask_data] * df$r[mask_data]),
                               T_data = df$num_households[mask_data],
                               T_pred = df$num_households[mask_pred],
                               input_dim = input_dim,
                               num_data = sum(mask_data),
                               num_pred = nrow(df) - sum(mask_data),
                               M_prior_data = beta_pred[mask_data],
                               M_prior_pred = beta_pred[mask_pred]),
                     warmup = 1000, iter = 2000, chains = 1)
  
  
  gp_samples <- extract(hyper.stan, permuted = TRUE)
  
  beta_mean_data <- apply(1/(1+exp(-gp_samples$GP_data)), 2, FUN = mean, na.rm = TRUE)
  beta_mean_pred <- apply(1/(1+exp(-gp_samples$GP_pred)), 2, FUN = mean, na.rm = TRUE)
  
  plot(df$year[mask_data], df$r[mask_data], ylim = c(0,1), pch = 16, col = "grey")
  plot(df$year[mask_data], beta_mean_data, ylim = c(0,1), pch = 16, col = "black")
  points(df$year[mask_pred], beta_mean_pred, pch = 16, col = "red")
  
  plot(df$ntl_pkh[mask_data], df$r[mask_data], ylim = c(0,1), pch = 16, col = "grey")
  plot(df$ntl_pkh[mask_data], beta_mean_data, ylim = c(0,1), pch = 16, col = "black")
  points(df$ntl_pkh[mask_pred], beta_mean_pred, pch = 16, col = "red")
  
}

iso3i <- "ZMB"
iso3i <- "ZWE"

graphics.off()

save(wdi.estimates, file = "code_output/wdi_factors.RData")
