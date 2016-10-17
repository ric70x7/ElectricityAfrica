# Add covariates to set3
# ----------------------------
#
# Edited: October 15, 2016
# Covariates are also standardized/transformend


rm(list = ls())
library(raster)


load("code_output/electricity_dhs_third.RData")
df3 <- set3[, c("country", "iso3", "lon", "lat", "year", "has_electricity", "total", "r")]
load("code_output/country_annual_estimates.RData")


# Add pixel locations
filename <- paste("data/ntl/Inland_water_masked_5k/ts2010W_template.tif") #5Km resolution
afr <- raster(filename)
df3$pixel <- cellFromXY(afr, df3[, c("lon", "lat")])


# Time points in database
years <- sort(unique(df3$year))


# Add NTL
# NOTE: Only data from 2014 and 2015, NTL used is form 2013
df3$ntl <- NA
yi <- 2013
filename <- paste("data/ntl/Inland_water_masked_5k/ts", yi, "W_template.tif", sep = "") #5Km resolution
afr <- raster(filename)
pixels <- cellFromXY(afr, df3[, c("lon", "lat")])
df3$ntl <- afr[pixels]


# Add population data to the dataframe
df3$pop <- NA
for(yi in years){
  mask <- df3$year == yi
  filename <- paste("code_output/Population/GPW4_", yi, ".tif", sep = "") #5Km resolution
  afr <- raster(filename)
  pixels <- cellFromXY(afr, df3[mask, c("lon", "lat")])
  df3$pop[mask] <- afr[pixels]
}


# Add households data to the dataframe
df3$house <- NA
for(yi in years){
  mask <- df3$year == yi
  filename <- paste("code_output/Households/HHW4_", yi, ".tif", sep = "")
  afr <- raster(filename)
  pixels <- cellFromXY(afr, df3[mask, c("lon", "lat")])
  df3$house[mask] <- afr[pixels]
}


# Add country stats
df3$country_r_mean <- NA
df3$country_r_low <- NA
df3$country_r_upp <- NA
df3$country_f_mean <- NA
df3$country_f_sd <- NA
df3 <- subset(df3, iso3 != "COM") #FIXME
for(iso3j in unique(df3$iso3)){
  years_j <- unique(subset(df3, iso3 == iso3j)$year)
  for(yj in years_j){
    csix <- annual_data$year == yj & annual_data$iso3 == iso3j
    dfix <- df3$year == yj & df3$iso3 == iso3j
    df3$country_r_mean[dfix] <- annual_data$r_mean[csix]
    df3$country_r_low[dfix] <- annual_data$r_cilo[csix]
    df3$country_r_upp[dfix] <- annual_data$r_ciup[csix]
    df3$country_f_mean[dfix] <- annual_data$f_mean[csix]
    df3$country_f_sd[dfix] <- annual_data$f_sd[csix]
  }
}


# Remove observations with no population
df3 <- subset(df3, !(is.na(df3$pop) | df3$pop == 0))


## Standardize/transform values
#load("code_output/z_params.RData")
#df3$z.year <- scale(df3$year, center = center.year, scale = scale.year)
#df3$z.pop <- log(1+df3$pop)
#df3$z.ntl <- log(1+df3$ntl)
#df3$z.house <- log(1+df3$house)


save(df3, file = "code_output/set3_w_covariates.RData")
