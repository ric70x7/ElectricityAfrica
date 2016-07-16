# Add covariates to surveys dataframe
# -----------------------------------
#
# Edited: July 14, 2016

rm(list = ls())
library(raster)


load("code_output/electricity_dhs.RData")


# Aggregate survey data by pixel
filename <- paste("data/ntl/TSwater/ts", 2010, "W.tif", sep = "")
afr <- raster(filename)
survey.data.agg$pixel <- cellFromXY(afr, survey.data.agg[, c("lon", "lat")])

df <- aggregate.data.frame(survey.data.agg$total,
                           by = survey.data.agg[, c("country", "iso3", "year", "pixel")],
                           FUN = sum)
colnames(df)[5] <- "total"

df$has_electricity <- aggregate.data.frame(survey.data.agg$has_electricity,
                                                   by = survey.data.agg[, c("year", "pixel")],
                                                   FUN = sum)[,3]
  
df$r <- df$has_electricity/df$total
coords <- xyFromCell(afr, df$pixel)
df$lon <- coords[,1]
df$lat <- coords[,2]

# Add NTL data to the dataframe
df$ntl <- NA
years <- sort(unique(df$year))
for(yi in years[1:13]){
  mask <- df$year == yi
  filename <- paste("data/ntl/TSwater/ts", yi, "W.tif", sep = "")
  afr <- raster(filename)
  pixels <- cellFromXY(afr, df[mask, c("lon", "lat")])
  rowcol <- rowColFromCell(afr, pixels) 
  df$ntl[mask] <- afr[rowcol]
  #df$ntl[mask] <- getValues(afr)[pixels]
}
# Repeat values of 2013 in 2014
mask <- df$year == 2014
filename <- paste("data/ntl/TSwater/ts", yi, "W.tif", sep = "")
afr <- raster(filename)
pixels <- cellFromXY(afr, df[mask, c("lon", "lat")])
df$ntl[mask] <- getValues(afr)[pixels]


# Add population data to the dataframe
pop2010.raw <- raster("data/Africa-POP-2010_africa2010ppp/africa2010ppp.tif")
pop2010 <- resample(pop2010.raw, afr)
pixels <- cellFromXY(pop2010, df[, c("lon", "lat")])
df$pop2010 <- getValues(pop2010)[pixels]


save(df, file = "code_output/electricity_dhs_w_covariates.RData")
