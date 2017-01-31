# Standardized NTL
# -----------------------------------
#
# Edited January 23, 2016
#
# Standardization is done on the log scale
# Standardization parameters are computed for each country
# NTL pixels with zero light have also zero in the standardized log scale

rm(list = ls())
graphics.off()
library(raster)

load("code_output/country_annual_estimates.RData")
load("code_output/z_params.RData")
afri_countries <- unique(annual_data$iso3)

# Raster files
years <- 2000:2015

# Shape file of Africa
afri_main <- shapefile("data/Africa_main_country/Africa_main_country.shp")

# NTL template
ntl_x <- raster(paste("data/ntl/GP2_Africa_5k/GP2_Africa_2010.tif", sep = ""))
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
df.z_factors <- data.frame(iso3 = sort(afri_countries))


for(i in c(10, 1:9, 11:16)){
  
  yi <- years[i]
  ntl.i <- raster(paste("data/ntl/GP2_Africa_5k/GP2_Africa_", min(2013,yi), ".tif", sep = ""))
  ntl.i[ntl.i>64] <- NA
  
  lit_layer <- sapply(ntl.i[], FUN = function(x) min(1, x))
  ntl.i[] <- log(1+ntl.i[])
  
  z.ntl_layer <- rep(NA, length(ntl.i[]))
  
  pop.i <- raster(paste("code_output/Population/GPW4_", yi, ".tif", sep = ""))
  pop.i <- log(1+pop.i[])
  
  z.pop_p_layer <- rep(NA, length(ntl.i[]))
  z.pop_n_layer <- rep(NA, length(ntl.i[]))
  

  for(iso3j in afri_countries){ 
    shp_boundary <- afri_main[afri_main$ISO3 == iso3j,]
    raster_mask <- mask(ntl.i, shp_boundary)
    cells_mask <- !is.na(raster_mask[])
    zix <- df.z_factors$iso3 == iso3j
    
    # Compute standardization parameters just for year 2000
    if(i == 10){
      df.z_factors$ntl_mean[zix] <- mean(ntl.i[cells_mask & lit_layer == 1])
      df.z_factors$ntl_sd[zix] <- sd(ntl.i[cells_mask & lit_layer == 1])
      
      df.z_factors$pop_p_mean[zix] <- mean(pop.i[cells_mask & lit_layer == 1])
      df.z_factors$pop_p_sd[zix] <- sd(pop.i[cells_mask & lit_layer == 1])
      
      df.z_factors$pop_n_mean[zix] <- mean(pop.i[cells_mask & lit_layer == 0])
      df.z_factors$pop_n_sd[zix] <- sd(pop.i[cells_mask & lit_layer == 0])
    }
    
    z.ntl_layer[cells_mask] <- (ntl.i[cells_mask] - df.z_factors$ntl_mean[zix])/df.z_factors$ntl_sd[zix]
    z.pop_p_layer[cells_mask] <- (pop.i[cells_mask] - df.z_factors$pop_p_mean[zix])/df.z_factors$pop_p_sd[zix]
    z.pop_n_layer[cells_mask] <- (pop.i[cells_mask] - df.z_factors$pop_n_mean[zix])/df.z_factors$pop_n_sd[zix]
  }
  
  z.ntl_layer[lit_layer == 0] <- 0
  z.pop_p_layer[lit_layer == 0] <- 0
  z.pop_n_layer[lit_layer == 1] <- 0
  
  # A few pixels are not catched by the shp polygons we define them as zero
  aux1 <- sum(!is.na(ntl.i[]) & is.na(z.ntl_layer[]))
  aux2 <- sum(!is.na(ntl.i[]) & is.na(z.pop_p_layer[]))
  aux3 <- sum(!is.na(ntl.i[]) & is.na(z.pop_n_layer[]))
  if(aux1 > 0 | aux2 > 0 | aux3 > 0){
    print(paste(iso3j, " ", aux1, aux2, aux3))
  }
  z.ntl_layer[!is.na(ntl.i[]) & is.na(z.ntl_layer[])] <- 0
  z.pop_p_layer[!is.na(ntl.i[]) & is.na(z.pop_p_layer[])] <- 0
  z.pop_n_layer[!is.na(ntl.i[]) & is.na(z.pop_n_layer[])] <- 0
  
  
  z.ntl_layer <- make_raster(z.ntl_layer)
  z.pop_p_layer <- make_raster(z.pop_p_layer)
  z.pop_n_layer <- make_raster(z.pop_n_layer)
  
  z.ntl_file <- paste("code_output/z_covariates/zpositive_ntl_", yi, sep = "")
  pop_p_file <- paste("code_output/z_covariates/zpositive_pop_", yi, sep = "")
  pop_n_file <- paste("code_output/z_covariates/zzero_pop_", yi, sep = "")
  
  writeRaster(z.ntl_layer, z.ntl_file, format = "GTiff", overwrite = TRUE)
  writeRaster(z.pop_p_layer, pop_p_file, format = "GTiff", overwrite = TRUE)
  writeRaster(z.pop_n_layer, pop_n_file, format = "GTiff", overwrite = TRUE)
} 
