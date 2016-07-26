# Add covariates to surveys dataframe
# -----------------------------------
#
# Edited: July 25, 2016
# Covariates are also standardized/transformend

rm(list = ls())
library(raster)


load("code_output/electricity_dhs.RData")


# Aggregate survey data by pixel
#filename <- paste("data/ntl/TSwater/ts", 2010, "W.tif", sep = "") #1Km resolution
filename <- paste("data/ntl/Inland_water_masked_5k/ts2010W_template.tif") #5Km resolution
afr <- raster(filename)
survey.data.agg$pixel <- cellFromXY(afr, survey.data.agg[, c("lon", "lat")])

df <- aggregate.data.frame(survey.data.agg$total,
                           by = survey.data.agg[, c("country", "iso3", "year", "pixel")],
                           #by = survey.data.agg[, c("year", "pixel")],
                           FUN = sum)
colnames(df)[5] <- "total"

df$has_electricity <- aggregate.data.frame(survey.data.agg$has_electricity,
                                           by = survey.data.agg[, c("country", "iso3", "year", "pixel")],
                                           #by = survey.data.agg[, c("year", "pixel")],
                                           FUN = sum)[,5]
  
df$r <- df$has_electricity/df$total
coords <- xyFromCell(afr, df$pixel)
df$lon <- coords[,1]
df$lat <- coords[,2]


# Add NTL data to the dataframe
df$ntl <- NA
years <- sort(unique(df$year))
for(yi in years[1:13]){
  mask <- df$year == yi
  #filename <- paste("data/ntl/TSwater/ts", yi, "W.tif", sep = "") #1km resolution
  filename <- paste("data/ntl/Inland_water_masked_5k/ts", yi, "W_template.tif", sep = "") #5Km resolution
  afr <- raster(filename)
  pixels <- cellFromXY(afr, df[mask, c("lon", "lat")])
  df$ntl[mask] <- afr[pixels]
}
# Repeat values of 2013 in 2014
mask <- df$year == 2014
#filename <- paste("data/ntl/TSwater/ts", yi, "W.tif", sep = "") #1km resolution
filename <- paste("data/ntl/Inland_water_masked_5k/ts", yi, "W_template.tif", sep = "") #5Km resolution
afr <- raster(filename)
pixels <- cellFromXY(afr, df[mask, c("lon", "lat")])
df$ntl[mask] <- getValues(afr)[pixels]


# Add population data to the dataframe
pop.list <- list(raster("data/Population/GPW3_2000.tif"),
                 raster("data/Population/GPW3_2005.tif"),
                 raster("data/Population/GPW3_2010.tif"),
                 raster("data/Population/GPW3_2015.tif"))
years <- sort(unique(df$year))
pop.years <- c(2000, 2005, 2010, 2015)
for(yi in years){
  mask <- df$year == yi
  pixels <- cellFromXY(pop.list[[1]], df[mask, c("lon", "lat")]) # Pixels are the same across pop files
  if(yi < 2005){
    fa <- 1
    fb <- 2
  }else{
    if(yi < 2010){
      fa <- 2
      fb <- 3
    }else{
      if(yi < 2015){
        fa <- 3
        fb <- 4
      }
    }
  }
  a <- (pop.years[fb] - yi) * .2
  b <- 1 - a
  df$pop[mask] <- a * getValues(pop.list[[fa]])[pixels] + b * getValues(pop.list[[fb]])[pixels]
  
}
#pop2010.raw <- raster("data/Africa-POP-2010_africa2010ppp/africa2010ppp.tif")
#pop2010 <- resample(pop2010.raw, afr)
#pixels <- cellFromXY(pop2010, df[, c("lon", "lat")])
#df$pop2010 <- getValues(pop2010)[pixels]


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

#df$z.lon <- df$lon + 1+abs(min(df$lon))
#df$z.lat <- df$lat + 1+abs(min(df$lat))

save(df, file = "code_output/electricity_dhs_w_covariates.RData")
save(center.year, scale.year, file = "code_output/z_params.RData")
