# Add covariates to surveys dataframe
# -----------------------------------
#
# Edited: July 5, 2016


library(raster)


load("code_output/electricity_dhs.RData")


years <- sort(unique(survey.data.agg$year))
survey.data.agg$ntl <- NA


# Add NTL data to the dataframe
for(yi in years[1:13]){
  mask <- survey.data.agg$year == yi
  filename <- paste("data/ntl/ts", yi, "T-1.tif", sep = "")
  afr <- raster(filename)
  pixels <- cellFromXY(afr, survey.data.agg[mask, c("lon", "lat")])
  survey.data.agg$ntl[mask] <- getValues(afr)[pixels]
}


# Add population data to the dataframe
pop2010 <- raster("data/Africa-POP-2010_africa2010ppp/africa2010ppp.tif")
pixels <- cellFromXY(pop2010, survey.data.agg[, c("lon", "lat")])
survey.data.agg$pop2010 <- getValues(pop2010)[pixels]


save(survey.data.agg, file = "code_output/electricity_dhs_w_covariates.RData")
