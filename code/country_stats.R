# Compute ntl and pop statistics per country
# ------------------------------------------
#
# Edited: September 21, 2016


library(raster)

afri_main <- shapefile("data/Africa_main_country/Africa_main_country.shp")

country_stats <- data.frame(year = sort(rep(2000:2015, length(afri_main$ISO3))),
                            iso3 = rep(afri_main$ISO3, 16),
                            ntl = NA, pop = NA, ntl_pkh = NA, num_ntlpix = NA)


for(iso3j in afri_main$ISO3){
  # Base layers
  ntl_x <- raster(paste("data/ntl/Inland_water_masked_5k/ts2010W_template.tif", sep = ""))
  ntl_x[ntl_x[]==128] <- NA # Value 128 is NA
  # Masks
  shp_boundary <- afri_main[afri_main$ISO3 == iso3j,]
  raster_mask <- mask(ntl_x, shp_boundary)
  cells_mask <- !is.na(raster_mask[])
  for(i in 1:16){
  
    yi <- 1999 + i
    ix <- country_stats$year == yi & country_stats$iso3 == iso3j
  
    ntl <- raster(paste("data/ntl/Inland_water_masked_5k/ts", min(yi, 2013) , "W_template.tif", sep = ""))
    ntl[ntl[]==128] <- NA # Value 128 is NA
    pop <- raster(paste("code_output/Population/GPW4_", yi, ".tif", sep = ""))
  
  
    country_stats$ntl[ix] <- sum(ntl[cells_mask], na.rm = TRUE)
    country_stats$pop[ix] <- sum(pop[cells_mask], na.rm = TRUE)
    ntl_per_hab <- ntl[cells_mask]/pop[cells_mask]
    ntl_per_hab[!is.finite(ntl_per_hab)] <- NA
    country_stats$ntl_pkh[ix] <- 1000*mean(ntl_per_hab, na.rm = TRUE)
    country_stats$num_ntlpix[ix] <- sum(ntl[cells_mask]>0)
    
    print(c(yi, iso3j)) 
  }
}
    

save(country_stats, file = "code_output/country_stats.RData")
