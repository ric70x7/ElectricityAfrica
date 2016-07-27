# Interpolation of population raster files
# ----------------------------------------
#
# Edited July 26, 2016
# Assume geometric increments in the population

rm(list = ls())
graphics.off()
library(raster)

# Raster files
pop.years <- seq(2000, 2015, 5)
pop.list <- list(raster("data/Population/GPW3_2000.tif"),
                 raster("data/Population/GPW3_2005.tif"),
                 raster("data/Population/GPW3_2010.tif"),
                 raster("data/Population/GPW3_2015.tif"))


# Geometric interpolationper year
for(i in c(1:4, 6:9, 11:14)){
#for(i in c(1:2)){
  yi <- 2000 + i
  a <- sum(yi > pop.years)
  b <- a + 1
  pop.a <- getValues(pop.list[[a]])
  pop.b <- getValues(pop.list[[b]])
  pop.i <- pop.a
  mask <- !is.na(pop.a) & !is.na(pop.b) & (pop.a != pop.b)
  pop.i[pop.a == 0 & pop.b > 0] <- 1
  increment <- (pop.b[mask]/pop.a[mask])^(1/5)
  pop.i[mask] <-  pop.i[mask] * (increment)^((i%%5))
  
  pop.raster <- matrix(pop.i,
                       ncol = pop.list[[1]]@ncols,
                       nrow = pop.list[[1]]@nrows,
                       byrow = TRUE)
  
  pop.raster <- raster(pop.raster,
                       xmn = pop.list[[1]]@extent@xmin,
                       xmx = pop.list[[1]]@extent@xmax,
                       ymn = pop.list[[1]]@extent@ymin,
                       ymx = pop.list[[1]]@extent@ymax,
                       crs = pop.list[[1]]@crs)
  
  filename <- paste("code_output/Population/GPW3_", yi, sep = "")
  writeRaster(pop.raster, filename, format = "GTiff", overwrite = TRUE)
}

#plot(log(raster("code_output/Population/GPW3_2000.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#plot(log(raster("code_output/Population/GPW3_2001.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#plot(log(raster("code_output/Population/GPW3_2002.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#plot(log(raster("code_output/Population/GPW3_2003.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#plot(log(raster("code_output/Population/GPW3_2004.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#plot(log(raster("code_output/Population/GPW3_2005.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#
#plot(log(raster("code_output/Population/GPW3_2006.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#plot(log(raster("code_output/Population/GPW3_2007.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#plot(log(raster("code_output/Population/GPW3_2008.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#plot(log(raster("code_output/Population/GPW3_2009.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#plot(log(raster("code_output/Population/GPW3_2010.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#
#plot(log(raster("code_output/Population/GPW3_2011.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#plot(log(raster("code_output/Population/GPW3_2012.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#plot(log(raster("code_output/Population/GPW3_2013.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#plot(log(raster("code_output/Population/GPW3_2014.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#plot(log(raster("code_output/Population/GPW3_2015.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))


##

#plot((raster("code_output/Population/GPW3_2000.tif")), col = terrain.colors(16))
#plot((raster("code_output/Population/GPW3_2001.tif")), col = terrain.colors(16))
#plot((raster("code_output/Population/GPW3_2002.tif")), col = terrain.colors(16))
#plot((raster("code_output/Population/GPW3_2003.tif")), col = terrain.colors(16))
#plot((raster("code_output/Population/GPW3_2004.tif")), col = terrain.colors(16))
#plot((raster("code_output/Population/GPW3_2005.tif")), col = terrain.colors(16))
#
#plot((raster("code_output/Population/GPW3_2006.tif")), col = terrain.colors(16))
#plot((raster("code_output/Population/GPW3_2007.tif")), col = terrain.colors(16))
#plot((raster("code_output/Population/GPW3_2008.tif")), col = terrain.colors(16))
#plot((raster("code_output/Population/GPW3_2009.tif")), col = terrain.colors(16))
#plot((raster("code_output/Population/GPW3_2010.tif")), col = terrain.colors(16))
#
#plot((raster("code_output/Population/GPW3_2011.tif")), col = terrain.colors(16))
#plot((raster("code_output/Population/GPW3_2012.tif")), col = terrain.colors(16))
#plot((raster("code_output/Population/GPW3_2013.tif")), col = terrain.colors(16))
#plot((raster("code_output/Population/GPW3_2014.tif")), col = terrain.colors(16))
#plot((raster("code_output/Population/GPW3_2015.tif")), col = terrain.colors(16))
