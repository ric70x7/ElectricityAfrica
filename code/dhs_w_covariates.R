# Add covariates to surveys dataframe
# -----------------------------------
#
# Edited: July 26, 2016
# Covariates are also standardized/transformend

rm(list = ls())
library(raster)


load("code_output/electricity_dhs.RData")


# Aggregate survey data by pixel
filename <- paste("data/ntl/Inland_water_masked_5k/ts2010W_template.tif") #5Km resolution
afr <- raster(filename)
survey.data.agg$pixel <- cellFromXY(afr, survey.data.agg[, c("lon", "lat")])

df <- aggregate.data.frame(survey.data.agg$total,
                           by = survey.data.agg[, c("country", "iso3", "year", "pixel")],
                           FUN = sum)
colnames(df)[5] <- "total"

df$has_electricity <- aggregate.data.frame(survey.data.agg$has_electricity,
                                           by = survey.data.agg[, c("country", "iso3", "year", "pixel")],
                                           FUN = sum)[,5]
  
df$r <- df$has_electricity/df$total
coords <- xyFromCell(afr, df$pixel)
df$lon <- coords[,1]
df$lat <- coords[,2]


# Time points in data
years <- sort(unique(df$year))

# Add NTL data to the dataframe
df$ntl <- NA
for(yi in years[1:13]){
  mask <- df$year == yi
  filename <- paste("data/ntl/Inland_water_masked_5k/ts", yi, "W_template.tif", sep = "") #5Km resolution
  afr <- raster(filename)
  pixels <- cellFromXY(afr, df[mask, c("lon", "lat")])
  df$ntl[mask] <- afr[pixels]
}
# Repeat values of 2013 in 2014
mask <- df$year == 2014
filename <- paste("data/ntl/Inland_water_masked_5k/ts", yi, "W_template.tif", sep = "") #5Km resolution
afr <- raster(filename)
pixels <- cellFromXY(afr, df[mask, c("lon", "lat")])
df$ntl[mask] <- getValues(afr)[pixels]


# Add population data to the dataframe
df$pop <- NA
for(yi in years){
  mask <- df$year == yi
  filename <- paste("code_output/Population/GPW3_", yi, ".tif", sep = "") #5Km resolution
  afr <- raster(filename)
  pixels <- cellFromXY(afr, df[mask, c("lon", "lat")])
  df$pop[mask] <- afr[pixels]
}


# Remove NA values
df <- subset(df, !is.na(df$ntl) & df$ntl <= 120 & !is.na(df$pop) & !is.na(r))


## Here come a bunch of heroic assumptions
# No people => no households => no electricity
mask <- df$r > 0 & df$ntl == 0 & df$pop == 0
df$r[mask] <- 0
df$pop[is.na(df$pop)] <- 0


# Standardize data
center.year <- mean(df$year)
scale.year <- sd(df$year)
df$z.year <- scale(df$year, center = center.year, scale = scale.year)

df$z.pop <- log(1+df$pop)
df$z.ntl <- log(1+df$ntl)


save(df, file = "code_output/electricity_dhs_w_covariates.RData")
save(center.year, scale.year, file = "code_output/z_params.RData")
