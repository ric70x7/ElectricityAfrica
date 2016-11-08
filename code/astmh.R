# ASTMH poster figures
# ---------------------------
#
# Edited November 4 2016

graphics.off()
rm(list = ls())
library(raster)
library(ggthemes)
library(cowplot)
library(viridis)
library(maptools)

pltyear <- 2010

ntl <- raster(paste("data/ntl/Inland_water_masked_5k/ts", pltyear, "W_template.tif", sep = ""))
pea <- raster(paste("code_output/Electricity/access_", pltyear, ".tif",  sep = ""))
cea <- raster(paste("code_output/z_covariates/logit_r_", pltyear, ".tif", sep = ""))
pop <- raster(paste("code_output/Population/GPW4_", pltyear, ".tif", sep = ""))
est <- raster(paste("code_output/Electricity/access_", pltyear, ".tif", sep = ""))

afr <- shapefile("data/Africa_main_country/Africa_main_country.shp")


cbit <- afr[afr$ISO3 %in% c("LBY", "TCD", "SDN", "EGY", "CAF"),]
pbit <- afr[afr$ISO3 %in% c("ERI", "SOM", "ETH", "KEN", "UGA", "TZA", "RWA", "BDI", "DJI"),]
nbit <- afr[afr$ISO3 %in% c("MOZ", "MWI", "ZMB", "BWA", "ZAF", "LSO", "NAM", "ZWE", "SWZ", "MDG"),]
sbit <- afr[afr$ISO3 %in% c("AGO", "COD", "COG", "GAB", "GNQ", "CMR"),]
ebit <- afr[afr$ISO3 %in% afr$ISO3[!(afr$ISO3 %in% c("LBY", "TCD", "SDN", "EGY", "CAF",
                                                     "ERI", "SOM", "ETH", "KEN", "UGA",
                                                     "TZA", "RWA", "BDI", "DJI", "MOZ",
                                                     "MWI", "ZMB", "BWA", "ZAF", "LSO",
                                                     "NAM", "ZWE", "SWZ", "MDG", "AGO",
                                                     "COD", "COG", "GAB", "GNQ", "CMR"))],]


plot(afr)
plot(cbit, col = "green", add = TRUE)
plot(pbit, col = "blue", add = TRUE)
plot(nbit, col = "gray", add = TRUE)
plot(sbit, col = "red", add = TRUE)
plot(ebit, col = "black", add = TRUE)


cmask <- mask(cea, cbit)
cmask[!is.na(cmask)] <- 1/(1+exp(-cmask[!is.na(cmask)]))

pmask <- mask(pea, pbit)
pmask[!is.na(pmask)] <- log(1+pmask[!is.na(pmask)])/max(log(1+pmask[!is.na(pmask)]))

nmask <- mask(ntl, nbit)
nmask[!is.na(nmask)] <- log(1+nmask[!is.na(nmask)])/max(log(1+nmask[!is.na(nmask)]))

smask <- mask(ntl, sbit)
smask[!is.na(smask)] <- 0

emask <- mask(est, ebit)


xy <- xyFromCell(ntl, 1:length(ntl)) 
canvas <- data.frame(lon = xy[,1], lat = xy[,2], layer = NA, z = NA)

ix <- !is.na(cmask[])
canvas$z[ix] <- cmask[ix]
canvas$layer[ix] <- "aggregate"

ix <- !is.na(pmask[])
canvas$z[ix] <- pmask[ix]
canvas$layer[ix] <- "population"

ix <- !is.na(nmask[])
canvas$z[ix] <- nmask[ix]
canvas$layer[ix] <- "ntl"

ix <- !is.na(smask[])
canvas$z[ix] <- smask[ix]
canvas$layer[ix] <- "survey"

ix <- !is.na(emask[])
canvas$z[ix] <- emask[ix]
canvas$layer[ix] <- "electricity"


load("code_output/merged_data.RData")
df <- subset(df, iso3 %in% c("AGO", "COD", "COG", "GAB", "GNQ", "CMR"))
plot(df$lon, df$lat)

canvas <- subset(canvas, !is.na(z))



plt_1 <- ggplot(canvas, aes(lon, lat)) + xlim(-20,60) + ylim(-40,40) + 
         geom_raster(data = subset(canvas, layer == "aggregate"), aes(fill = z), show.legend = FALSE) +
         scale_fill_gradient(low = "#380474", high = "gold", guide = FALSE) +
         coord_equal() +
         theme_map()+
         theme(plot.background = element_rect(fill = "transparent",colour = NA)) 
ggsave("figs/astmh_m1.png" , plt_1, bg = "transparent", width = 30, height = 30, units = "in")

plt_2 <- ggplot(canvas, aes(lon, lat)) + xlim(-20,60) + ylim(-40,40) + 
         geom_raster(data = subset(canvas, layer == "population"), aes(fill = sapply(z, function(x) min(.4,x))), show.legend = FALSE) +
         scale_fill_gradient(limits = c(0,.4), low = "#380474", high = "magenta", guide = FALSE) +
         coord_equal() +
         theme_map() 
ggsave("figs/astmh_m2.png" , plt_2, bg = "transparent", width = 30, height = 30, units = "in")

plt_3 <- ggplot(canvas, aes(lon, lat)) + xlim(-20,60) + ylim(-40,40) + 
         geom_raster(data = subset(canvas, layer == "ntl"), aes(fill = z), show.legend = FALSE) +
         scale_fill_gradient(low = "#380474", high = "ivory", guide = FALSE) +
         coord_equal() +
         theme_map() 
ggsave("figs/astmh_m3.png" , plt_3, bg = "transparent", width = 30, height = 30, units = "in")

plt_4 <- ggplot(canvas, aes(lon, lat)) + xlim(-20,60) + ylim(-40,40) + 
         geom_raster(data = subset(canvas, layer == "survey"), fill = "#380474", show.legend = FALSE) +
         geom_point(data = df, color = "#E6E6FA") +#9FB6CD
         coord_equal() +
         theme_map() 
ggsave("figs/astmh_m4.png" , plt_4, bg = "transparent", width = 30, height = 30, units = "in")

plt_5 <- ggplot(canvas, aes(lon, lat)) + xlim(-20,60) + ylim(-40,40) + 
         geom_raster(data = subset(canvas, layer == "electricity"), aes(fill = z), show.legend = FALSE) +
         scale_fill_gradient(low = "#380474", high = "gold", guide = FALSE) +
         coord_equal() +
         theme_map() 
ggsave("figs/astmh_m5.png" , plt_5, bg = "transparent", width = 30, height = 30, units = "in")



pltntl <- ggplot(canvas, aes(lon, lat)) + xlim(-20,60) + ylim(-40,40) + 
          geom_raster(data = subset(canvas, layer == "aggregate"), aes(lon, lat, fill = z), show.legend = FALSE, inherit.aes = FALSE) +
          scale_fill_gradient(low = "white", high = "green") +
          geom_raster(data = subset(canvas, layer == "population"), aes(lon, lat, fill = z), show.legend = FALSE, inherit.aes = FALSE) +
          geom_raster(data = subset(canvas, layer == "ntl"), aes(lon, lat, fill = z), show.legend = FALSE, inherit.aes = FALSE) +
          #geom_raster(data = subset(canvas, layer == "survey"), aes(lon, lat, fill = z), show.legend = FALSE, inherit.aes = FALSE) +
          geom_raster(data = subset(canvas, layer == "electricity"), aes(lon, lat, fill = z), show.legend = FALSE, inherit.aes = FALSE) +
          scale_fill_viridis(limits = c(0, 1)) +
          coord_equal() +
          theme_map() 
          #theme(legend.position = c(.05,.05), legend.key.width = unit(.8, "cm")) +

  
  
       geom_raster(data = subset(canvas, layer == "population"), aes(fill = z)) +
       geom_raster(data = subset(canvas, layer == "ntl"), aes(fill = z)) +
       geom_raster(data = subset(canvas, layer == "survey"), aes(fill = z)) +
       geom_raster(data = subset(canvas, layer == "electricity"), aes(fill = z)) +
                     
                   
       coord_equal() +
       theme_map() +
       theme(legend.position = c(.05,.05), legend.key.width = unit(.8, "cm")) +
       scale_fill_viridis(limits = c(0, log(1+64)), guide = guide_colorbar(title = paste("NTL\n(log scale)\n", pltyear, sep = " ")))

      ggsave("figs/astmh_ntl.png", pltntl, width = 30, height = 30, units = "in")
