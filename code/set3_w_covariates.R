# Add covariates to set3
# ----------------------------
#
# Edited: August 26, 2016
# Covariates are also standardized/transformend


rm(list = ls())
library(raster)


load("code_output/electricity_dhs_third.RData")
df3 <- set3[, c("ID", "lon", "lat", "year", "has_electricity", "total", "r")]


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
  filename <- paste("code_output/Population/GPW3_", yi, ".tif", sep = "") #5Km resolution
  afr <- raster(filename)
  pixels <- cellFromXY(afr, df3[mask, c("lon", "lat")])
  df3$pop[mask] <- afr[pixels]
}


# Standardize/transform values
load("code_output/z_params.RData")
df3$z.year <- scale(df3$year, center = center.year, scale = scale.year)
df3$z.pop <- log(1+df3$pop)
df3$z.ntl <- log(1+df3$ntl)

# Remove points with NTL > 100
df3 <- subset(df3, ntl < 100)


save(df3, file = "code_output/set3_w_covariates.RData")
