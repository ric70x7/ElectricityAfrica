# Resample rasters to make predictions
# ------------------------------------

rm(list = ls())
library(raster)
library(maptools)

filename <- paste("data/ntl/GP2_Africa_5k/GP2_Africa_2010.tif") #5Km resolution
afr0 <- raster(filename)
years <- 2000:2015


for(i in seq(years)){
  yi <- years[i]
  filename <- paste("data/Impervious Distance/", yi, "_Imp_Dist.tif", sep = "") #5Km resolution
  afr <- raster(filename)
  namei <- paste("data/lulc_resampled/impervious_dist_", yi, ".tif", sep='')
  new_afr <- raster::resample(afr, afr0, filename=namei, overwrite=TRUE)
}

for(i in seq(years)){
  yi <- years[i]
  filename <- paste("data/LULC Class/", yi, "_7Class.tif", sep = "") #5Km resolution
  afr <- raster(filename)
  namei <- paste("data/lulc_resampled/7class", yi, ".tif", sep='')
  new_afr <- raster::resample(afr, afr0, method="ngb", filename=namei, overwrite=TRUE)
}

for(i in seq(years)){
  yi <- years[i]
  filename <- paste("data/Probability/", yi, "_Prob.tif", sep = "") #5Km resolution
  afr <- raster(filename)
  namei <- paste("data/lulc_resampled/prob", yi, ".tif", sep='')
  new_afr <- raster::resample(afr, afr0, filename=namei, overwrite=TRUE)
}
