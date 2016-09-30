# Estimate number of households per pixel
# ----------------------------------------
#
# Edited September 30, 2016

rm(list = ls())
graphics.off()
library(raster)

load("code_output/country_annual_estimates.RData")
afri_countries <- unique(country_stats$iso3)

# Raster files
pop.years <- 2000:2015

# Shape file of Africa
afri_main <- shapefile("data/Africa_main_country/Africa_main_country.shp")

# NTL template
ntl_x <- raster(paste("data/ntl/Inland_water_masked_5k/ts2010W_template.tif", sep = ""))


for(i in seq(pop.years)){
  yi <- pop.years[i]
  pop <- raster(paste("code_output/Population/GPW4_", yi, ".tif", sep = ""))
  num_h <- rep(NA, length(pop[]))
  inshp <- rep(FALSE, length(pop[]))
  
  for(iso3j in afri_countries){ 
    shp_boundary <- afri_main[afri_main$ISO3 == iso3j,]
    raster_mask <- mask(ntl_x, shp_boundary)
    cells_mask <- !is.na(raster_mask[])
    
    hfactor <- country_stats$household_members[country_stats$year == yi & country_stats$iso3 == iso3j]
    num_h[cells_mask] <-  sapply(pop[cells_mask], FUN = function(x) ifelse(x == 0, 0, max(1, floor(x/hfactor))))
    inshp[cells_mask] <- TRUE
  }
  
  num_h <- matrix(num_h,
                  ncol = pop@ncols,
                  nrow = pop@nrows,
                  byrow = TRUE)
  
  num_h <- raster(num_h,
                  xmn = pop[[1]]@extent@xmin,
                  xmx = pop[[1]]@extent@xmax,
                  ymn = pop[[1]]@extent@ymin,
                  ymx = pop[[1]]@extent@ymax,
                  crs = pop[[1]]@crs)
  
  
  filename <- paste("code_output/Households/HHW4_", yi, sep = "")
  writeRaster(num_h, filename, format = "GTiff", overwrite = TRUE)
  
} 
  
graphics.off()
#plot(log(1+raster("code_output/Households/HHW4_2000.tif")), col = terrain.colors(16), breaks = c(seq(0,10,2)))
#plot(log(1+raster("code_output/Households/HHW4_2005.tif")), col = terrain.colors(16), breaks = c(seq(0,10,2)))
#plot(log(1+raster("code_output/Households/HHW4_2010.tif")), col = terrain.colors(16), breaks = c(seq(0,10,2)))
#plot(log(1+raster("code_output/Households/HHW4_2015.tif")), col = terrain.colors(16), breaks = c(seq(0,10,2)))