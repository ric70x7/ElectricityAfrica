library(raster)
library(maptools)

graphics.off()
rm(list = ls())

afri_main <- rgdal::readOGR("data/Africa_main_country/Africa_main_country.shp")
afri_main$ISO3
num_countries <- length(afri_main$ISO3)
trends <- data.frame(iso3 = rep(afri_main$ISO3, 14 * 3),
                            year = rep(sort(rep(2000:2013,  num_countries)), 3),
                            type = sort(rep(c("urban", "rural", "total"), 14 * num_countries)),
                            pix_total = NA, pix_electricity = NA,
                            pop_total = NA, pop_electricity = NA)

for (i in 1:14) {
  
  file_fimp <- paste0("data/lulc_resampled/7class", i + 1999, ".tif")
  file_elec <- paste0("code_output/Electricity/electricity_", i + 1999, ".tif")
  file_popl <- paste0("code_output/GPW4_reprojected/pop_", i + 1999, ".tif")
  fimp <- raster::raster(file_fimp)
  elec <- raster::raster(file_elec)
  popl <- raster::raster(file_popl)

  for(iso3j in afri_main$ISO3){  #iso3j <- "MAR"
    
    shp_boundary <- afri_main[afri_main$ISO3 == iso3j,]
    iso3j_fimp <- mask(fimp, shp_boundary)
    iso3j_elec <- mask(elec, shp_boundary)
    iso3j_popl <- mask(popl, shp_boundary)

    mask_urban <- iso3j_fimp[] == 1
    mask_rural <- iso3j_fimp[] > 1
    mask_electricity <- iso3j_elec[] > .5

    row_mask <- trends$iso3 == iso3j & trends$year == i + 1999
    
    trends$pix_total[row_mask & trends$type == "urban"] <- sum(mask_urban, na.rm = TRUE)
    trends$pix_total[row_mask & trends$type == "rural"] <- sum(mask_rural, na.rm = TRUE)
    trends$pix_total[row_mask & trends$type == "total"] <- trends$pix_total[row_mask & trends$type == "urban"] +
                                                           trends$pix_total[row_mask & trends$type == "rural"]

    trends$pix_electricity[row_mask & trends$type == "urban"] <- sum(iso3j_elec[mask_urban & mask_electricity], na.rm =TRUE)
    trends$pix_electricity[row_mask & trends$type == "rural"] <- sum(iso3j_elec[mask_rural & mask_electricity], na.rm =TRUE)
    trends$pix_electricity[row_mask & trends$type == "total"] <- trends$pix_electricity[row_mask & trends$type == "urban"] +
                                                                 trends$pix_electricity[row_mask & trends$type == "rural"] 

    trends$pop_total[row_mask & trends$type == "urban"] <- sum(iso3j_popl[mask_urban], na.rm =TRUE)
    trends$pop_total[row_mask & trends$type == "rural"] <- sum(iso3j_popl[mask_rural], na.rm =TRUE)
    trends$pop_total[row_mask & trends$type == "total"] <- sum(iso3j_popl[mask_urban], na.rm =TRUE) +
                                                            sum(iso3j_popl[mask_rural], na.rm =TRUE)
    
    trends$pop_electricity[row_mask & trends$type == "urban"] <- sum(iso3j_popl[mask_urban & mask_electricity], na.rm =TRUE)
    trends$pop_electricity[row_mask & trends$type == "rural"] <- sum(iso3j_popl[mask_rural & mask_electricity], na.rm =TRUE)
    trends$pop_electricity[row_mask & trends$type == "total"] <- trends$pop_electricity[row_mask & trends$type == "urban"] +
                                                                 trends$pop_electricity[row_mask & trends$type == "rural"]

    print(subset(trends, year==1999+i & iso3 == iso3j))
  }
}

#save(trends, file = "code_output/e_trends.RData")
