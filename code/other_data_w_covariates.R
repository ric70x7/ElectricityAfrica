# Add covariates to other.data
# ----------------------------
#
# Edited: July 25, 2016
# Covariates are also standardized/transformend


rm(list = ls())
library(raster)


load("code_output/other_data_preprocess.RData")
df.test <- other.data[, c("lon", "lat", "year", "electricity")]


# Add pixel locations
filename <- paste("data/ntl/Inland_water_masked_5k/ts2010W_template.tif") #5Km resolution
afr <- raster(filename)
df.test$pixel <- cellFromXY(afr, other.data[, c("lon", "lat")])


# Time points in database
years <- sort(unique(df.test$year))


# Add NTL
df.test$ntl <- NA
for(yi in years){ #NOTE years < 2013
  mask <- df.test$year == yi
  filename <- paste("data/ntl/Inland_water_masked_5k/ts", yi, "W_template.tif", sep = "") #5Km resolution
  afr <- raster(filename)
  pixels <- cellFromXY(afr, df.test[mask, c("lon", "lat")])
  df.test$ntl[mask] <- afr[pixels]
}


# Add population data to the dataframe
df.test$pop <- NA
for(yi in years){ #NOTE years < 2013
  mask <- df.test$year == yi
  filename <- paste("code_output/Population/GPW3_", yi, ".tif", sep = "") #5Km resolution
  afr <- raster(filename)
  pixels <- cellFromXY(afr, df.test[mask, c("lon", "lat")])
  df.test$pop[mask] <- afr[pixels]
}


# Standardize/transform values
load("code_output/z_params.RData")
df.test$z.year <- scale(df.test$year, center = center.year, scale = scale.year)
df.test$z.pop <- log(1+df.test$pop)
df.test$z.ntl <- log(1+df.test$ntl)


save(df.test, file = "code_output/other_data_w_covariates.RData")
