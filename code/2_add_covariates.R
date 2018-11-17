# Add covariates from raster files
# --------------------------------
# Last edited: November 5, 2018

rm(list = ls())
library(raster)
library(maptools)

mode <- function(x) {
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
}

add_covariates <- function(df) {
  # Add covariates to a dataframe with columns ("lon", "lat")
  years <- 2000:2015
  
  # Add NTL data to the dataframe
  print("ntl")
  df$ntl <- NA
  for(i in seq(years)){
    print(i)
    yi <- years[i]
    mask <- df$year == yi
    if (sum(mask) > 0) {
      filename <- paste("data/ntl/GP2_Africa_5k/GP2_Africa_", min(yi, 2013), ".tif", sep = "") #5Km resolution
      afr <- raster(filename)
      df$ntl[mask] <- extract(afr, df[mask, c("lon", "lat")], buffer=5000, fun = function(x) {mean(x[x<100], na.rm=TRUE)})
    }
  }
  
  # Add population data to the dataframe
  print("pop")
  df$pop <- NA
  for(i in seq(years)){
    yi <- years[i]
    mask <- df$year == yi
    if (sum(mask) > 0) {
      filename <- paste("code_output/Population/GPW4_", yi, ".tif", sep = "") #5Km resolution
      afr <- raster(filename)
      df$pop[mask] <- extract(afr, df[mask, c("lon", "lat")], buffer=5000, fun = function(x) {mean(x, na.rm=TRUE)})
    }
  }
  
  # Add other covariates to the dataframe
  print("impervious")
  for(i in seq(years)){
    yi <- years[i]
    mask <- df$year == yi
    if (sum(mask) > 0) {
      filename <- paste("data/lulc_resampled/impervious_dist_", yi, ".tif", sep = "") #5Km resolution
      afr <- raster(filename)
      df$dist2impervious[mask] <- extract(afr, df[mask, c("lon", "lat")], buffer=5000, fun = function(x) {mean(x, na.rm=TRUE)})
    }
  }
    
  print("lctype")
  df$lctype <- NA
  for(i in seq(years)){
    yi <- years[i]
    mask <- df$year == yi
    if (sum(mask) > 0) {
      filename <- paste("data/lulc_resampled/7class", yi, ".tif", sep = "") #5Km resolution
      afr <- raster(filename)
      df$lctype[mask] <- extract(afr, df[mask, c("lon", "lat")], buffer=5000, fun = mode)
    }
  }
  
  print("prob")
  df$freq_impervious <- NA
  for(i in seq(years)){
    yi <- years[i]
    mask <- df$year == yi
    if (sum(mask) > 0) {
      filename <- paste("data/lulc_resampled/prob", yi, ".tif", sep = "") #5Km resolution
      afr <- raster(filename)
      df$freq_impervious[mask] <- extract(afr, df[mask, c("lon", "lat")], buffer=5000, fun = function(x) {mean(x, na.rm=TRUE)})
    }
  }
  
  # Transformed distance
  df$kz <- exp(- df$dist2impervious / sd(df$dist2impervious, na.rm = TRUE))
  df$kz[is.na(df$kz)] <- 0
  
  # Standardize covariates
  df$z.year <- df$year - 1999
  
  return(df)
}

