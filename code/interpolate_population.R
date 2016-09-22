# Interpolation of population raster files
# ----------------------------------------
#
# Edited September 21, 2016
# Assume geometric increments in the population

rm(list = ls())
graphics.off()
library(raster)

# Raster files
pop.years <- seq(2000, 2015, 5)

#template <- raster("data/Population/density/GPW3_2000.tif")
pop.files <- c("data/Population/GPW4_2000.tif",
               "data/Population/GPW4_2005.tif",
               "data/Population/GPW4_2010.tif",
               "data/Population/GPW4_2015.tif")
               
pop.list <- list()
for(i in seq(pop.years)){
  yi <- pop.years[i]
  template <- raster(paste("data/ntl/Inland_water_masked_5k/ts", min(yi, 2013) , "W_template.tif", sep = ""))
  pop <- raster(pop.files[i])
  pop2 <- aggregate(pop, fac = 5, fun = sum)
  pop3 <- resample(pop2, template, method = "ngb")
  pop3[template[] == 128] <- NA # values of 128 in template are water masked areas
  pop3[is.na(pop3[]) & template[] != 128] <- 0
  pop.list <- c(pop.list, pop3)
  
  filename <- paste("code_output/Population/GPW4_", pop.years[i], sep = "")
  writeRaster(pop3, filename, format = "GTiff", overwrite = TRUE)
  
}


# Geometric interpolationper year
for(i in c(1:4, 6:9, 11:14)){
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
  
  filename <- paste("code_output/Population/GPW4_", yi, sep = "")
  writeRaster(pop.raster, filename, format = "GTiff", overwrite = TRUE)
}

#plot(log(1+raster("code_output/Population/GPW4_2000.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#plot(log(1+raster("code_output/Population/GPW4_2001.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#plot(log(1+raster("code_output/Population/GPW4_2002.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#plot(log(1+raster("code_output/Population/GPW4_2003.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#plot(log(1+raster("code_output/Population/GPW4_2004.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#plot(log(1+raster("code_output/Population/GPW4_2005.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))

#plot(log(1+raster("code_output/Population/GPW4_2006.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#plot(log(1+raster("code_output/Population/GPW4_2007.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#plot(log(1+raster("code_output/Population/GPW4_2008.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#plot(log(1+raster("code_output/Population/GPW4_2009.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#plot(log(1+raster("code_output/Population/GPW4_2010.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))

#plot(log(1+raster("code_output/Population/GPW4_2011.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#plot(log(1+raster("code_output/Population/GPW4_2012.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#plot(log(1+raster("code_output/Population/GPW4_2013.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#plot(log(1+raster("code_output/Population/GPW4_2014.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
#plot(log(1+raster("code_output/Population/GPW4_2015.tif")), col = terrain.colors(16), breaks = c(seq(-10,20,2)))
