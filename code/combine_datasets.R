# Combine sets of surveys
# -----------------------
#
# The files combiend are:
# 1) electricity_dhs_w_covariates.RData
# 2) other_data_w_covariates.RData
# 3) df3_w_covariates.RData
#
# Edited: January 23, 2016

rm(list = ls())
library(raster)
library(maptools)


# Dataset 1 -> df1
load("code_output/electricity_dhs.RData")

# Aggregate survey data by pixel
filename <- paste("data/ntl/GP2_Africa_5k/GP2_Africa_2010.tif") #5Km resolution
afr <- raster(filename)
survey.data.agg$pixel <- cellFromXY(afr, survey.data.agg[, c("lon", "lat")])

df1 <- aggregate.data.frame(survey.data.agg$total,
                           by = survey.data.agg[, c("country", "iso3", "year", "pixel")],
                           FUN = sum)
colnames(df1)[5] <- "total"

df1$has_electricity <- aggregate.data.frame(survey.data.agg$has_electricity,
                                           by = survey.data.agg[, c("country", "iso3", "year", "pixel")],
                                           FUN = sum)[,5]
  
df1$r <- df1$has_electricity/df1$total
coords <- xyFromCell(afr, df1$pixel)
df1$lon <- coords[,1]
df1$lat <- coords[,2]
df1$obfuscated <- TRUE


# Dataset 3 -> df3
load("code_output/electricity_dhs_third.RData")
df3 <- set3[, c("country", "iso3", "lon", "lat", "year", "has_electricity", "total", "r")]
df3$obfuscated <- TRUE


# Dataset 2 -> df2
load("code_output/other_data_preprocess.RData")
afri_main <- readShapePoly("data/Africa_main_country/Africa_main_country.shp")
df2 <- other.data[, c("lon", "lat", "year", "electricity")]
colnames(df2)[4] <- "has_electricity"
df2$total <- 1
df2$r <- df2$has_electricity


# Identify country
afri_main <- afri_main[afri_main$ISO3 %in% c("MWI", "ZMB"), ]
xy <- df2[, c("lon", "lat")]
df2$iso3 <- paste(over(SpatialPoints(xy), afri_main)$ISO3)
df2$country[df2$iso3 == "MWI"] <- "Malawi"
df2$country[df2$iso3 == "ZMB"] <- "Zambia"
df2$obfuscated <- FALSE


# Merge df1, df2 and df3
df <- rbind(df1[, c("country", "iso3", "year", "lon", "lat",
                    "has_electricity", "total", "r", "obfuscated")],
            df3[, c("country", "iso3", "year", "lon", "lat",
                    "has_electricity", "total", "r", "obfuscated")],
            df2[, c("country", "iso3", "year", "lon", "lat",
                    "has_electricity", "total", "r", "obfuscated")])
 


# Add covariates
load("code_output/country_annual_estimates.RData")
years <- sort(unique(df$year))

# Add NTL data to the dataframe
df$ntl <- NA
for(i in seq(years)){
  yi <- 1999 + i
  mask <- df$year == yi
  filename <- paste("data/ntl/GP2_Africa_5k/GP2_Africa_", min(yi, 2013), ".tif", sep = "") #5Km resolution
  afr <- raster(filename)
  pixels <- cellFromXY(afr, df[mask, c("lon", "lat")])
  df$ntl[mask] <- afr[pixels]
}

# Add population data to the dataframe
df$pop <- NA
for(i in seq(years)){
  yi <- 1999 + i
  mask <- df$year == yi
  filename <- paste("code_output/Population/GPW4_", yi, ".tif", sep = "") #5Km resolution
  afr <- raster(filename)
  pixels <- cellFromXY(afr, df[mask, c("lon", "lat")])
  df$pop[mask] <- afr[pixels]
}

# Add std covariates to the dataframe
#df$zpositive.ntl <- NA
for(i in seq(years)){
  yi <- 1999 + i
  mask <- df$year == yi
  lit_filename <- paste("code_output/z_covariates/lit_", yi, ".tif", sep = "") #5Km resolution
  zpntl_filename <- paste("code_output/z_covariates/z_ntl_", yi, ".tif", sep = "") #5Km resolution
  zppop_filename <- paste("code_output/z_covariates/z_pop_", yi, ".tif", sep = "") #5Km resolution
  #zzpop_filename <- paste("code_output/z_covariates/zzero_pop_", yi, ".tif", sep = "") #5Km resolution
  
  lit <- raster(lit_filename)
  zpntl <- raster(zpntl_filename)
  zppop <- raster(zppop_filename)
  #zzpop <- raster(zzpop_filename)
  
  pixels <- cellFromXY(zpntl, df[mask, c("lon", "lat")])
  df$lit[mask] <- lit[pixels]
  df$z.ntl[mask] <- zpntl[pixels]
  df$z.pop[mask] <- zppop[pixels]
  #df$zzero.pop[mask] <- zzpop[pixels]
}


## Add households data to the dataframe
#df$house <- NA
#for(i in seq(years)){
#  yi <- 1999 + i
#  mask <- df$year == yi
#  filename <- paste("code_output/Households/HHW4_", yi, ".tif", sep = "")
#  afr <- raster(filename)
#  pixels <- cellFromXY(afr, df[mask, c("lon", "lat")])
#  df$house[mask] <- afr[pixels]
#}

# Add country stats
df$country_r_mean <- NA
df$country_r_lbou <- NA
df$country_r_ubou <- NA
df <- subset(df, iso3 != "COM") # This island is not part of our output
for(iso3j in unique(df$iso3)){
  years_j <- unique(subset(df, iso3 == iso3j)$year)
  for(yj in years_j){
    csix <- annual_data$year == yj & annual_data$iso3 == iso3j
    dfix <- df$year == yj & df$iso3 == iso3j
    df$country_r_mean[dfix] <- annual_data$r_mean[csix]
    df$country_r_lbou[dfix] <- annual_data$r_lbou[csix]
    df$country_r_ubou[dfix] <- annual_data$r_ubou[csix]
    #df$country_f_mean[dfix] <- annual_data$f_mean[csix]
  }
}
df$country_f_mean <- df$country_r_mean
df$country_f_mean[df$ntl == 0] <- 1e-4
df$country_f_mean <- log(df$country_f_mean/(1-df$country_f_mean))
     
# Remove observations ntl > 64, those are water bodies
df <- subset(df, df$ntl <= 64)
#df <- subset(df, df$pop > 0)


# Standardize covariates
df$z.year <- df$year - 1999
#df$z.ntl <-  log(1+ df$ntl)
#df$z.pop <-  log(1+ df$pop)
#df$lit <- 0
#df$lit[df$ntl>0] <- 1
#
#df$annual_r_min <- NA
#for(i in seq(years)){
#  yi <- 1999 + i
#  min_pi <- min(annual_data$r_mean[annual_data$year == yi])
#  df$annual_r_min[df$year == yi] <- log(min_pi/(1-min_pi))
#}
#
#zero_pi <- mean(df$r[df$obfuscated & df$ntl == 0], na.rm = TRUE)
#df$zero_r_mean <- log(zero_pi/(1-zero_pi))


# Add offset data to df
#df$pop <- NA
#for(i in seq(years)){
#  yi <- 1999 + i
#  mask <- df$year == yi
#  filename <- paste("code_output/Population/GPW4_", yi, ".tif", sep = "") #5Km resolution
#  afr <- raster(filename)
#  pixels <- cellFromXY(afr, df[mask, c("lon", "lat")])
#  df$pop[mask] <- afr[pixels]
#}

#df$logit_min_offset <- df$country_logit_r
#df$logit_min_offset[df$ntl == 0] <- df$annual_r_min[df$ntl == 0]
#
#df$logit_zero_offset <- df$country_logit_r
#df$logit_zero_offset[df$ntl == 0] <- df$zero_r_mean[df$ntl == 0]

save(df, file = "code_output/merged_data.RData")
