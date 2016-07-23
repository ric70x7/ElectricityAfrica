# Predction points
#-----------------
#
# Edited July 22, 2016
# Points for generating maps on the latent field

graphics.off()
rm(list = ls())
library(raster)


# NTL
yi <- 2010
filename <- paste("data/ntl/Inland_water_masked_5k/ts", yi, "W_template.tif", sep = "") #5Km resolution
ntl2010 <- raster(filename)
ntl2010.x <- getValues(ntl2010)


# Population 2010
pop2010.raw <- raster("data/Africa-POP-2010_africa2010ppp/africa2010ppp.tif")
pop2010 <- resample(pop2010.raw, ntl2010)
pop2010.x <- getValues(pop2010)


# Whole grid
afr.locs <- xyFromCell(ntl2010, seq(ntl2010))


# Mask of valid values
ntl.mask <- !is.na(ntl2010.x) & ntl2010.x < 100
pop.mask <- !is.na(pop2010.x)
afr.mask <- ntl.mask & pop.mask


# Data frame of predictions
df.template <- data.frame(pixel = seq(ntl2010)[afr.mask],
                          lon = afr.locs[afr.mask, 1],
                          lat = afr.locs[afr.mask, 2],
                          year = NA,
                          ntl = NA,
                          pop2010 = NA,
                          z.year = NA,
                          z.ntl = NA,
                          z.pop2010 = NA, 
                          r = NA)

df.template$pop2010 <- pop2010[afr.mask]
df.template$z.pop2010 <- log(1+df.template$pop2010)


# Fill in data for all the years
df.predictions <- c()

for(pred.year in 2000:2015){
  if(pred.year <= 2013){
    yi <- pred.year
  }else{
    yi <- 2013
  }

  dfi <- df.template
  dfi$year <- pred.year
  
  filename <- paste("data/ntl/Inland_water_masked_5k/ts", yi, "W_template.tif", sep = "") #5Km resolution
  ntlraster <- raster(filename)
  ntli <- getValues(ntlraster)
  dfi$ntl <- ntli[afr.mask] 
  
  df.predictions <- rbind(df.predictions, dfi)
  
}


# Standardize/sacale data
load("code_output/z_params.RData")
df.predictions$z.year <- scale(df.predictions$year, center = center.year, scale = scale.year)
df.predictions$z.ntl <- log(1+df.predictions$ntl)


save(afr.locs, afr.mask, ntl.mask, pop.mask, df.predictions, file = "code_output/prediction_locations.RData")
  
  
#library(ggplot2)
#library(ggthemes)
#library(viridis)
#
#plots <- list()
#for(i in 1:14){
#  pltyear <- 1999 + i
#  plots[[i]] <- ggplot(subset(df.predictions, year == pltyear), aes(lon, lat)) + 
#    geom_raster(aes(fill = ntl))  +
#    coord_equal() +
#    theme_map() +
#    theme(legend.position = "bottom", legend.key.width = unit(2, "cm")) +
#    scale_fill_viridis(limits = c(0, 65), guide = guide_colorbar(title = paste("Covariate", pltyear, sep = " "))) #+
#}
#