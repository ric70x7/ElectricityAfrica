# Add covariates to other.data
# ----------------------------
#
# Edited: September 30, 2016
# Covariates are also standardized/transformend


rm(list = ls())
library(raster)
library(maptools)


afri_main <- readShapePoly("data/Africa_main_country/Africa_main_country.shp")
load("code_output/country_annual_estimates.RData")
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
  filename <- paste("code_output/Population/GPW4_", yi, ".tif", sep = "") #5Km resolution
  afr <- raster(filename)
  pixels <- cellFromXY(afr, df2[mask, c("lon", "lat")])
  df2$pop[mask] <- afr[pixels]
}


# Add households data to the dataframe
df2$house <- NA
for(yi in years){
  mask <- df2$year == yi
  filename <- paste("code_output/Households/HHW4_", yi, ".tif", sep = "")
  afr <- raster(filename)
  pixels <- cellFromXY(afr, df2[mask, c("lon", "lat")])
  df2$house[mask] <- afr[pixels]
}


# Identify country
afri_main <- afri_main[afri_main$ISO3 %in% c("MWI", "ZMB"), ]
xy <- df2[, c("lon", "lat")]
df2$iso3 <- paste(over(SpatialPoints(xy), afri_main)$ISO3)
df2$country[df2$iso3 == "MWI"] <- "Malawi"
df2$country[df2$iso3 == "ZMB"] <- "Zambia"

# Add country stats
df2$country_r_mean <- NA
df2$country_r_low <- NA
df2$country_r_upp <- NA
df2$country_f_mean <- NA
df2$country_f_sd <- NA
for(iso3j in unique(df2$iso3)){
  years_j <- unique(subset(df2, iso3 == iso3j)$year)
  for(yj in years_j){
    csix <- country_stats$year == yj & country_stats$iso3 == iso3j
    dfix <- df2$year == yj & df2$iso3 == iso3j
    df2$country_r_mean[dfix] <- country_stats$r_mean[csix]
    df2$country_r_low[dfix] <- country_stats$r_low[csix]
    df2$country_r_upp[dfix] <- country_stats$r_upp[csix]
    df2$country_f_mean[dfix] <- country_stats$f_mean[csix]
    df2$country_f_sd[dfix] <- country_stats$f_sd[csix]
  }
}

## Standardize/transform values
#load("code_output/z_params.RData")
#df2$z.year <- scale(df2$year, center = center.year, scale = scale.year)
#df2$z.pop <- log(1+df2$pop)
#df2$z.ntl <- log(1+df2$ntl)
#df2$z.house <- log(1+df2$house)


save(df2, file = "code_output/other_data_w_covariates.RData")
