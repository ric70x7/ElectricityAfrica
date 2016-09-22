# Beta regression models to fit WDI estimates of electricity access
# -----------------------------------------------------------------
#
# Edited: September 19, 2016
# This is the core file (train and test with non obfuscated data)

library(betareg)
library(rstan)

rm(list=ls())

# WDI data
wdi <- read.csv("data/World_Development_Indicators/Data2.csv")
#wdi <- wdi[complete.cases(wdi),]
wdi$Country.Name <- paste(wdi$Country.Name)
wdi$Country.Code <- paste(wdi$Country.Code)

# Sum of lights related fles
load("code_output/country_stats.RData")

# DHS data
load("code_output/electricity_dhs.RData")

# Add WDI to country_stats
country_stats$wdi <- NA
for(iso3i in country_stats$iso3){
  iso3x <- ifelse(iso3i == "ESH", "MAR", iso3i) # Western Sahara is a disputed territory, there is no WDI data
  for(yj in c(2000,2010,2012)){
    varj <- paste("YR", yj, sep = "")
    ix <- country_stats$iso3 == iso3i & country_stats$year == yj
    country_stats$wdi[ix] <-  wdi[wdi$Country.Code == iso3x, varj]/100
  }
}

# Add DHS to country_stats
country_stats$dhs_r <- NA
for(iso3i in unique(survey.data.agg$iso3)){
  dfi <- subset(survey.data.agg, iso3 == iso3i)
  for(yj in unique(dfi$year)){
    dfij <- subset(dfi, year == yj)
    num_pos <- sum(dfij$has_electricity)
    num_tot <- sum(dfij$total)
    ix <- country_stats$iso3 == iso3i & country_stats$year == yj
    country_stats$dhs_r[ix] <- num_pos/num_tot
  }
}


# Beta regressions
country_stats$wdi_estimate <- NA
for(iso3i in unique(country_stats$iso3)){
  
  # Dataframe for each country
  df <- subset(country_stats, iso3 == iso3i)
  df$wdi[df$wdi == 1] <- .999
  df$wdi[df$wdi == 0] <- .001
  df$X <- 1000*df$ntl/df$pop
  
#  predictor <- wdi ~ 1 +  X
#  beta_model <- betareg(predictor, data = subset(df, !is.na(df$wdi)))
#  beta_pred <- predict(beta_model, newdata = df)
#  
#  country_stats$wdi_estimate[country_stats$iso3 == iso3i] <- beta_pred
#  
#  #plot(df$X, df$wdi, col = "black", pch = 16)
#  #points(df$X, beta_pred, col = "red", pch = 16)
#  
#  plot(df$year, df$wdi, col = "black", pch = 16, ylim = c(0,1), main = iso3i)
#  lines(df$year, beta_pred, col = "red")
#  points(df$year, df$dhs_r, pch = 16, col = "gray")
  
  
  gp_df <- data.frame(year = 2000:2015,
                      r = NA,
                      x = 1000*df$ntl/df$pop)
  for(yj in gp_df$year){
    gp_r <- df$wdi[df$year == yj]
    if(!is.na(gp_r)){
      gp_df$r[gp_df$year==yj] <- gp_r
    }else{
      gp_r <- df$dhs_r[df$year == yj]
      if(!is.na(gp_r)){
        gp_df$r[gp_df$year==yj] <- gp_r
      }
    }
  }
  
  input_dim <- 1 
  data_mask <- !is.na(gp_df$r)
  hyper.stan <- stan(file="code/logistic_hyper.stan",
                     data=list(X_data = matrix(gp_df$x[data_mask] ,ncol = input_dim),
                               Y_data = gp_df$r[data_mask],
                               input_dim = input_dim,
                               num_data = sum(data_mask),
                               M_prior_data = log(gp_df$r[data_mask]/(1-gp_df$r[data_mask]))  ),
                     warmup = 300, iter = 1000, chains = 1)
  
  
  
  gp_samples <- extract(hyper.stan, permuted = TRUE)
  
  beta_mean <- apply(1/(1+exp(-gp_samples$GP_data)), 2, FUN = mean, na.rm = TRUE)
  
}




graphics.off()

save(wdi.estimates, file = "code_output/wdi_factors.RData")
