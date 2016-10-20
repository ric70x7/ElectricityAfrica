# Create layers to offset logit_r
# -------------------------------
#
# Edited: October 19, 2016

rm(list = ls())
library(raster)
library(maptools)

load("code_output/country_annual_estimates.RData")
load("code_output/merged_data.RData")
years <- 2000:2015


# Create layer of offsets
ntl_x <- raster(paste("data/ntl/Inland_water_masked_5k/ts2010W_template.tif", sep = ""))
ntl_x[ntl_x[]==128] <- NA # Value 128 is NA

# Function to define new raster object
make_raster <- function(x, ref_raster = ntl_x){
  new_obj <- matrix(x, ncol = ref_raster@ncols, nrow = ref_raster@nrows, byrow = TRUE)
  new_obj <- raster(new_obj,
                    xmn = ref_raster@extent@xmin,
                    xmx = ref_raster@extent@xmax,
                    ymn = ref_raster@extent@ymin,
                    ymx = ref_raster@extent@ymax,
                    crs = ref_raster@crs)
  return(new_obj)
}

afri_main <- readShapePoly("data/Africa_main_country/Africa_main_country.shp")
afri_countries <- unique(annual_data$iso3)
for(i in seq(years)){
  yi <- 1999 + i
  country_mean_layer <- rep(NA, length(ntl_x[]))
  lit_layer <- rep(NA, length(ntl_x[]))
  ntl.i <- raster(paste("data/ntl/Inland_water_masked_5k/ts", min(2013,yi), "W_template.tif", sep = ""))
  for(iso3j in afri_countries){ 
    shp_boundary <- afri_main[afri_main$ISO3 == iso3j,]
    raster_mask <- mask(ntl_x, shp_boundary)
    cells_mask <- !is.na(raster_mask[])
    
    iso3j_offset <- annual_data$r_mean[annual_data$iso3 == iso3j & annual_data$year == yi]
    country_mean_layer[cells_mask] <- log(iso3j_offset/(1-iso3j_offset)) 
    lit_layer[cells_mask] <- sapply(ntl.i[cells_mask], FUN = function(x) min(1, x))
  }  
  
  country_mean_layer <- make_raster(country_mean_layer)
  lit_layer <- make_raster(lit_layer)
  
  annual_min_layer <- country_mean_layer
  annual_min_layer[lit_layer[]==0] <- min(country_mean_layer[], na.rm = TRUE)
  logit_min_offset <- country_mean_layer
  logit_min_offset[ntl.i[]==0] <- annual_min_layer[ntl.i[]==0]
  logit_min_offset <- make_raster(logit_min_offset)
  
  zero_mean_layer <- country_mean_layer
  zero_mean_pi <- mean(df$r[df$obfuscated & df$ntl == 0], na.rm = TRUE)
  logit_zero_offset <- country_mean_layer
  logit_zero_offset[ntl.i[]==0] <- log(zero_mean_pi/(1-zero_mean_pi))
  logit_zero_offset <- make_raster(logit_zero_offset)
  
  country_mean_file <- paste("code_output/z_covariates/logit_r_", yi, sep = "")
  lit_file <- paste("code_output/z_covariates/lit_", yi, sep = "")
  min_file <- paste("code_output/z_covariates/logit_min_offset", yi, sep = "")
  zero_file <- paste("code_output/z_covariates/logit_zero_offset", yi, sep = "")
  
  writeRaster(country_mean_layer, country_mean_file, format = "GTiff", overwrite = TRUE)
  writeRaster(lit_layer, lit_file, format = "GTiff", overwrite = TRUE)
  writeRaster(logit_min_offset, min_file, format = "GTiff", overwrite = TRUE)
  writeRaster(logit_zero_offset, zero_file, format = "GTiff", overwrite = TRUE)
}

