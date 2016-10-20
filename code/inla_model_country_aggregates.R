# Aggreagate raster data per country
# ----------------------------------
#
# Edited: October 17, 2016


library(INLA)
library(ggplot2)
library(raster)
library(maptools)

load("code_output/country_annual_estimates.RData")
afri_main <- readShapePoly("data/Africa_main_country/Africa_main_country.shp")

iso3list <- sort(unique(annual_data$iso3))

#df.aggregates <- data.frame(year = rep(2000:2015, length(iso3list)), iso3 = sort(rep(iso3list, 16)))
annual_data$agg_raster <- NA
for(iso3i in iso3list){
  isoshape <- afri_main[afri_main$ISO3 == iso3i, ]
  print(iso3i)
  for(i in 1:16){
    yi <- 1999 + i
    households <- raster(paste("code_output/Households/HHW4_", yi, ".tif", sep = ""))
    access <- raster(paste("code_output/Electricity/access_", yi, ".tif", sep = ""))
    isohouse <- mask(households, isoshape)
    isoaccess <- mask(access, isoshape)
    ix <- annual_data$iso3 == iso3i & annual_data$year == yi
    annual_data$agg_raster[ix] <- sum(isoaccess[!is.na(isoaccess)] * isohouse[!is.na(isohouse)])/sum(isohouse[!is.na(isohouse)])
    print(yi)
  }
}

num <- 12
ggplot(subset(annual_data, iso3 == iso3list[num]), aes(x = year)) + ylim(0,1) +
 geom_line(aes(y = r_mean), color = "blue") +
 geom_line(aes(y = agg_raster), color = "red") + xlab(iso3list[num])


BEN
annual_data[annual_data$iso3 == "BEN",]

