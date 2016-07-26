# Add covariates to other.data
# ----------------------------
#
# Edited: July 25, 2016
# Covariates are also standardized/transformend


rm(list = ls())
library(raster)


load("code_output/other_data_preprocess.RData")
df.test <- other.data[, c("lon", "lat", "year", "electricity")]


# Add pixel locations
filename <- paste("data/ntl/Inland_water_masked_5k/ts2010W_template.tif") #5Km resolution
afr <- raster(filename)
df.test$pixel <- cellFromXY(afr, other.data[, c("lon", "lat")])


# Add NTL
df.test$ntl <- NA
years <- sort(unique(df.test$year))
for(yi in years){ #NOTE years < 2013
  mask <- df.test$year == yi
  #filename <- paste("data/ntl/TSwater/ts", yi, "W.tif", sep = "") #1km resolution
  filename <- paste("data/ntl/Inland_water_masked_5k/ts", yi, "W_template.tif", sep = "") #5Km resolution
  afr <- raster(filename)
  pixels <- cellFromXY(afr, df.test[mask, c("lon", "lat")])
  df.test$ntl[mask] <- afr[pixels]
}


# Add population data to the dataframe
pop.list <- list(raster("data/Population/GPW3_2000.tif"),
                 raster("data/Population/GPW3_2005.tif"),
                 raster("data/Population/GPW3_2010.tif"),
                 raster("data/Population/GPW3_2015.tif"))
years <- sort(unique(df.test$year))
pop.years <- c(2000, 2005, 2010, 2015)
for(yi in years){
  mask <- df.test$year == yi
  pixels <- cellFromXY(pop.list[[1]], df.test[mask, c("lon", "lat")]) # Pixels are the same across pop files
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
  df.test$pop[mask] <- a * getValues(pop.list[[fa]])[pixels] + b * getValues(pop.list[[fb]])[pixels]
}
#pop2010.raw <- raster("data/Africa-POP-2010_africa2010ppp/africa2010ppp.tif")
#pop2010 <- resample(pop2010.raw, afr)
#pixels <- cellFromXY(pop2010, df.test[, c("lon", "lat")])
#df.test$pop2010 <- getValues(pop2010)[pixels]

# Standardize/transform values
load("code_output/z_params.RData")
df.test$z.year <- scale(df.test$year, center = center.year, scale = scale.year)
df.test$z.pop <- log(1+df.test$pop)
df.test$z.ntl <- log(1+df.test$ntl)


save(df.test, file = "code_output/other_data_w_covariates.RData")


# Remove NA values
#df.test <- subset(df.test, !is.na(df.test$ntl) & df.test$ntl <= 120 & !is.na(df.test$pop2010))


#load("code_output/electricity_dhs_w_covariates.RData")
#head(df)
#plot(df$lon, df$lat, pch = 16, col = "gray")
#points(df.test$lon, df.test$lat, pch = 16, col = "red")
#points(df.test$lon[mask][is.na(pixels)], df.test$lat[mask][is.na(pixels)], pch = 16, col = "blue")

