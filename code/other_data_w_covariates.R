# Add covariates to other.data
# ----------------------------
#
# Edited: August 26, 2016
# Covariates are also standardized/transformend


rm(list = ls())
library(raster)


load("code_output/other_data_preprocess.RData")
df2 <- other.data[, c("lon", "lat", "year", "electricity")]


# Add pixel locations
filename <- paste("data/ntl/Inland_water_masked_5k/ts2010W_template.tif") #5Km resolution
afr <- raster(filename)
df2$pixel <- cellFromXY(afr, other.data[, c("lon", "lat")])


# Time points in database
years <- sort(unique(df2$year))


# Add NTL
df2$ntl <- NA
for(yi in years){ #NOTE years < 2013
  mask <- df2$year == yi
  filename <- paste("data/ntl/Inland_water_masked_5k/ts", yi, "W_template.tif", sep = "") #5Km resolution
  afr <- raster(filename)
  pixels <- cellFromXY(afr, df2[mask, c("lon", "lat")])
  df2$ntl[mask] <- afr[pixels]
}


# Add population data to the dataframe
df2$pop <- NA
for(yi in years){ #NOTE years < 2013
  mask <- df2$year == yi
  filename <- paste("code_output/Population/GPW3_", yi, ".tif", sep = "") #5Km resolution
  afr <- raster(filename)
  pixels <- cellFromXY(afr, df2[mask, c("lon", "lat")])
  df2$pop[mask] <- afr[pixels]
}


# Standardize/transform values
load("code_output/z_params.RData")
df2$z.year <- scale(df2$year, center = center.year, scale = scale.year)
df2$z.pop <- log(1+df2$pop)
df2$z.ntl <- log(1+df2$ntl)


save(df2, file = "code_output/other_data_w_covariates.RData")
