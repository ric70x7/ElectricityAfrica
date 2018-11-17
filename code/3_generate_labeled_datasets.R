# Generate labeled datasets: for training and to measure offset sensibility
# -------------------------------------------------------------------------
# Last edited: November 13, 2018

source("code/2_add_covariates.R")
load("code_output/merged_data2.RData")
df0 <- df

# Find locations outside africa Mainland and Madagascar
afri_main <- rgdal::readOGR("data/Africa_main_country/Africa_main_country.shp")
afri_border <- unionSpatialPolygons(afri_main, rep(1, nrow(afri_main)))

keep <- c()
for (i in 1:nrow(df0)) {
  xx <- sp::SpatialPoints(df0[i, c("lon", "lat")], proj4string=afri_border@proj4string)
  if (rgeos::gWithin(xx, afri_border)) {
    keep <- c(keep, i)
  }
}
save(keep, file="code_output/points_in_mainland.RData")
#430 locations removed (approx 0.8%)
df0[keep, ]


## Add covariates to the dataset
df_model <- add_covariates(df=df0[keep,])
save(df_model, file = "code_output/df_model.RData")



## Obfuscate dataset and add covariates
dhs_offset <- function(pts) {
  "Function to obfuscate points"
  
  afri_main <- rgdal::readOGR("data/Africa_main_country/Africa_main_country.shp")
  afri_border <- unionSpatialPolygons(afri_main, rep(1, nrow(afri_main)))

  # Function to offset coordinates  
  n <- nrow(pts)
  offset.dist <- rep(5/111, n) # offset everything up to 5 Km #ifelse(pts$URBAN_RURA == "U", 2000, 5000)
  rur.n <- floor(0.1*n)
  offset.dist[sample(1:n, rur.n, replace = FALSE)] <- 10/111 # 1% of points is offset up to 10 km
  r.pts0 <- c()
  
  #for(i in 1:nrow(pts)){
  for(i in 18586:nrow(pts)){
    print(i)
    pdsc <- spatstat::disc(radius = offset.dist[i], centre = as.numeric(pts[i,]))
    pdsc <- as(pdsc, "SpatialPolygons")
    pdsc@proj4string <- afri_border@proj4string
    offset.area <- rgeos::gIntersection(pdsc, afri_border)
    r.pts0 <- rbind(r.pts0, coordinates(spsample(offset.area, n=1, type="random")))
  }
  return(r.pts0)
  #save(r.pts0, file = "code_output/lon_lat_offset.RData")
}

# Obfuscate locations
#new_locations <- dhs_offset(df0[keep, c("lon", "lat")])
colnames(new_locations) <- c("lon", "lat")
new_locations <- as.data.frame(new_locations)
save(new_locations, file = "code_output/new_locations_offset.RData")

df_obfsc <- df0[keep, ]
df_obfsc$lon <- new_locations$lon
df_obfsc$lat <- new_locations$lat

# Add covariates to obfuscated data
df_obfsc <- add_covariates(df=df_obfsc)
save(df_obfsc, file = "code_output/df_obfsc.RData")

#df_model <- add_covariates(df=df0[keep,])
#save(df_model, file = "code_output/df_model.RData")
