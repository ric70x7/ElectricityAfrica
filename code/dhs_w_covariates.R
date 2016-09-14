# Add covariates to surveys dataframe
# -----------------------------------
#
# Edited: August 26, 2016
# Covariates are also standardized/transformend

rm(list = ls())
library(raster)


load("code_output/electricity_dhs.RData")


# Aggregate survey data by pixel
filename <- paste("data/ntl/Inland_water_masked_5k/ts2010W_template.tif") #5Km resolution
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


# Time points in data
years <- sort(unique(df1$year))

# Add NTL data to the dataframe
df1$ntl <- NA
for(yi in years[1:13]){
  mask <- df1$year == yi
  filename <- paste("data/ntl/Inland_water_masked_5k/ts", yi, "W_template.tif", sep = "") #5Km resolution
  afr <- raster(filename)
  pixels <- cellFromXY(afr, df1[mask, c("lon", "lat")])
  df1$ntl[mask] <- afr[pixels]
}
# Repeat values of 2013 in 2014
mask <- df1$year == 2014
filename <- paste("data/ntl/Inland_water_masked_5k/ts", yi, "W_template.tif", sep = "") #5Km resolution
afr <- raster(filename)
pixels <- cellFromXY(afr, df1[mask, c("lon", "lat")])
df1$ntl[mask] <- getValues(afr)[pixels]


# Add population data to the dataframe
df1$pop <- NA
for(yi in years){
  mask <- df1$year == yi
  filename <- paste("code_output/Population/GPW3_", yi, ".tif", sep = "") #5Km resolution
  afr <- raster(filename)
  pixels <- cellFromXY(afr, df1[mask, c("lon", "lat")])
  df1$pop[mask] <- afr[pixels]
}


# Remove NA values
df1 <- subset(df1, !is.na(df1$ntl) & df1$ntl <= 120 & !is.na(df1$pop) & !is.na(r))


## Here come a bunch of heroic assumptions
# No people => no households => no electricity
mask <- df1$r > 0 & df1$ntl == 0 & df1$pop == 0
df1$r[mask] <- 0
df1$pop[is.na(df1$pop)] <- 0


# Standardize data
center.year <- mean(df1$year)
scale.year <- sd(df1$year)
df1$z.year <- scale(df1$year, center = center.year, scale = scale.year)

df1$z.pop <- log(1+df1$pop)
df1$z.ntl <- log(1+df1$ntl)


save(df1, file = "code_output/electricity_dhs_w_covariates.RData")
save(center.year, scale.year, file = "code_output/z_params.RData")
