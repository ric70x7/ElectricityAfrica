library(ggplot2)
library(raster)
library(maptools)

graphics.off()
rm(list = ls())

afri_main <- rgdal::readOGR("data/Africa_main_country/Africa_main_country.shp")
template <- raster::raster("code_output/Electricity/electricity_2000.tif")


for (yi in c(2000, 2005, 2010, 2015)) {
  filename <- paste0("data/gpw-v4-population-count/gpw_v4_population_count_rev10_", yi, "_2pt5_min.tif")
  popl <- raster::raster(filename)
  popl <- raster::crop(popl, afri_main)
  popl <- raster::projectRaster(popl, template, method = "ngb")

  new_filename <- paste0("code_output/GPW4_reprojected/pop_", yi)
  writeRaster(popl, new_filename, format = "GTiff", overwrite = TRUE)
}

# Geometric interpolation per year
popl_years <- seq(2000, 2015, 5)
popl_list <- list()
popl_template <- raster::raster("code_output/GPW4_reprojected/pop_2015.tif")
popl_mask <- !is.na(popl_template[])
for (i in 1:3) {
  a <- popl_years[i]
  b <- popl_years[i+1]
  file_a <- paste0("code_output/GPW4_reprojected/pop_", a, ".tif")
  file_b <- paste0("code_output/GPW4_reprojected/pop_", b, ".tif")
  raster_a <- raster::raster(file_a)
  raster_b <- raster::raster(file_b)
  popl_a <- raster_a[popl_mask]
  popl_b <- raster_b[popl_mask]
  increment <- (popl_b/popl_a)^(1/5)
  for (j in 1:4) {
    popl_j <- popl_template
    names(popl_j) <- paste0("pop_", a + j)
    popl_j[popl_mask] <- popl_a * increment^j
    
    new_filename <- paste0("code_output/GPW4_reprojected/pop_", a + j, ".tif")
    writeRaster(popl_j, new_filename, format = "GTiff", overwrite = TRUE)
  }
}
