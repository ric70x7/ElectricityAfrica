# Standardized covariates
# -----------------------------------
#
# Edited October 19, 2016
#
# Standardization parameters are computed for each country

rm(list = ls())
graphics.off()
library(raster)

load("code_output/country_annual_estimates.RData")
load("code_output/z_params.RData")
afri_countries <- unique(country_stats$iso3)

# Raster files
years <- 2000:2015

# Shape file of Africa
afri_main <- shapefile("data/Africa_main_country/Africa_main_country.shp")

# NTL template
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


# Store standardization parameters
df.z_factors <- data.frame(iso3 = sort(unique(country_stats$iso3)))
z_year_mean <- mean(2000:2015)
z_year_sd <- sd(2000:2015)


for(i in c(10, 1:9, 11:16)){
  
  yi <- years[i]
  offset_layer <- rep(NA, length(ntl_x[]))
  lit_layer <- rep(NA, length(ntl_x[]))
  z.ntl_layer <- rep(NA, length(ntl_x[]))
  z.house_layer <- rep(NA, length(ntl_x[]))
  z.pop_layer <- rep(NA, length(ntl_x[]))
  
  ntl.i <- raster(paste("data/ntl/Inland_water_masked_5k/ts", min(2013,yi), "W_template.tif", sep = ""))
  pop.i <- raster(paste("code_output/Population/GPW4_", yi, ".tif", sep = ""))
  house.i <- raster(paste("code_output/Households/HHW4_", yi, ".tif", sep = ""))
  

  for(iso3j in afri_countries){ 
    shp_boundary <- afri_main[afri_main$ISO3 == iso3j,]
    raster_mask <- mask(ntl_x, shp_boundary)
    cells_mask <- !is.na(raster_mask[])
    zix <- df.z_factors$iso3 == iso3j
    
    iso3j_offset <- country_stats$r_mean[country_stats$iso3 == iso3j & country_stats$year == yi]
    offset_layer[cells_mask] <- log(iso3j_offset/(1-iso3j_offset)) 
    
    lit_layer[cells_mask] <- sapply(ntl.i[cells_mask], FUN = function(x) min(1, x))
    
    # Compute standardization parameters just for year 2000
    if(i == 10){
      df.z_factors$ntl_mean[zix] <- mean((ntl.i[cells_mask])[ntl.i[cells_mask]>0])
      df.z_factors$ntl_sd[zix] <- sd((ntl.i[cells_mask])[ntl.i[cells_mask]>0])
      
      df.z_factors$pop_mean[zix] <- mean((pop.i[cells_mask])[pop.i[cells_mask]>0])
      df.z_factors$pop_sd[zix] <- sd((pop.i[cells_mask])[pop.i[cells_mask]>0])
      
      df.z_factors$house_mean[zix] <- mean((house.i[cells_mask])[house.i[cells_mask]>0])
      df.z_factors$house_sd[zix] <- sd((house.i[cells_mask])[house.i[cells_mask]>0])
      
    }
    
    z.ntl_layer[cells_mask] <- (ntl.i[cells_mask] - df.z_factors$ntl_mean[zix])/df.z_factors$ntl_sd[zix]
    z.pop_layer[cells_mask] <- (pop.i[cells_mask] - df.z_factors$pop_mean[zix])/df.z_factors$pop_sd[zix]
    z.house_layer[cells_mask] <- (house.i[cells_mask] - df.z_factors$house_mean[zix])/df.z_factors$house_sd[zix]
    
  }
  
  offset_layer <- make_raster(offset_layer)
  lit_layer <- make_raster(lit_layer)
  z.ntl_layer <- make_raster(z.ntl_layer)
  z.pop_layer <- make_raster(z.pop_layer)
  z.house_layer <- make_raster(z.house_layer)
  
  offset_file <- paste("code_output/z_covariates/logitr_", yi, sep = "")
  lit_file <- paste("code_output/z_covariates/lit_", yi, sep = "")
  ntl_file <- paste("code_output/z_covariates/zntl_", yi, sep = "")
  pop_file <- paste("code_output/z_covariates/zpop_", yi, sep = "")
  house_file <- paste("code_output/z_covariates/zhouse_", yi, sep = "")
  
  writeRaster(offset_layer, offset_file, format = "GTiff", overwrite = TRUE)
  writeRaster(lit_layer, lit_file, format = "GTiff", overwrite = TRUE)
  writeRaster(z.ntl_layer, ntl_file, format = "GTiff", overwrite = TRUE)
  writeRaster(z.pop_layer, pop_file, format = "GTiff", overwrite = TRUE)
  writeRaster(z.house_layer, house_file, format = "GTiff", overwrite = TRUE)
  
  # Save z_params
  if(i == 10){
    save(z_year_mean, z_year_sd, df.z_factors, file = "code_output/z_params.RData")
  }
} 

