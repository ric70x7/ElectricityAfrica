library(raster)

afri_main <- shapefile("data/Africa_main_country/Africa_main_country.shp")
plot(afri_main)

country_stats <- data.frame(year = sort(rep(2000:2015, length(afri_main$ISO3))),
                            iso3 = rep(afri_main$ISO3, 16),
                            sum_lights = NA, sum_pop = NA)

for(i in 1:16){
  yi <- 1999 + i
  ntl <- raster(paste("data/ntl/ts", yi, "T-1.tif", sep = ""))
  pop <- raster(paste("code_output/Population/GPW4_", yi, ".tif", sep = ""))
  for(iso3j in afri_main$ISO3){
    poly <- afri_main[afri_main$ISO3 == iso3j,]
    ix <- country_stats$year == yi & country_stats$iso3 == iso3j
    poly_ntl <- mask(ntl, poly)
    poly_pop <- mask(pop, poly)
    country_stats$sum_lights[ix] <- sum(poly_ntl[], na.rm = TRUE)
    country_stats$sum_lights[ix] <- sum(poly_pop[], na.rm = TRUE)
  }
}


country_stats$ws_ntl <- country_stats$sum_lights/country_stats$sum_pop

save(country_stats, file = "code_output/code_output/country_stats.RData")
