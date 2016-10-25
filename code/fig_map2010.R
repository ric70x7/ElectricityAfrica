# Figures: ggplot raster maps
# ---------------------------
#
# Edited October 24 2016


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


ntl[ntl[]==128] <- NA
ntl_xy <- xyFromCell(ntl, seq(ntl[])[!is.na(ntl[])])

pea_xy <- xyFromCell(ntl, seq(pea[])[!is.na(pea[])])
cea_xy <- xyFromCell(ntl, seq(pea[])[!is.na(cea[])])
pop_xy <- xyFromCell(ntl, seq(pea[])[!is.na(pop[])])

ntl_df <- data.frame(z = ntl[!is.na(ntl[])], lon = ntl_xy[,1], lat = ntl_xy[,2])
pea_df <- data.frame(z = pea[!is.na(pea[])], lon = pea_xy[,1], lat = pea_xy[,2])
cea_df <- data.frame(z = 1/(1+exp(-cea[!is.na(pea[])])), lon = cea_xy[,1], lat = cea_xy[,2])
pop_df <- data.frame(z = pop[!is.na(pop[])], lon = pop_xy[,1], lat = pop_xy[,2])


pltntl <- ggplot(ntl_df, aes(lon, lat)) +
       geom_raster(aes(fill = log(1+z))) +
       coord_equal() +
       theme_map() +
       theme(legend.position = c(.05,.05), legend.key.width = unit(.8, "cm")) +
       scale_fill_viridis(limits = c(0, log(1+64)), guide = guide_colorbar(title = paste("NTL\n(log scale)\n", pltyear, sep = " ")))

pltpea <- ggplot(pea_df, aes(lon, lat)) +
       geom_raster(aes(fill = z)) +
       coord_equal() +
       theme_map() +
       theme(legend.position = c(.05,.05), legend.key.width = unit(.8, "cm")) +
       scale_fill_viridis(limits = c(0, 1), guide = guide_colorbar(title = paste("Geostatistic\nmodel\n", pltyear, sep = " ")))

pltcea <- ggplot(cea_df, aes(lon, lat)) +
       geom_raster(aes(fill = z)) +
       coord_equal() +
       theme_map() +
       theme(legend.position = c(.05,.05), legend.key.width = unit(.8, "cm")) +
       scale_fill_viridis(limits = c(0, 1), guide = guide_colorbar(title = paste("Country average\nelectricity access\n", pltyear, sep = " ")))

pltpop <- ggplot(pop_df, aes(lon, lat)) +
       geom_raster(aes(fill = log(1+z))) +
       coord_equal() +
       theme_map() +
       theme(legend.position = c(.05,.05), legend.key.width = unit(.8, "cm")) +
       scale_fill_viridis(limits = c(0, 15), guide = guide_colorbar(title = paste("Population\n(log scale)\n", pltyear, sep = " ")))


fig_map <- ggdraw(xlim = c(0,8), ylim = c(0,8)) +
            draw_plot(pltntl, x = 0, y = 4, width = 4, height = 4) +
            draw_plot(pltpop, x = 4, y = 4, width = 4, height = 4) +
            draw_plot(pltcea, x = 0, y = 0, width = 4, height = 4) +
            draw_plot(pltpea, x = 4, y = 0, width = 4, height = 4) +
            draw_plot_label(c("A", "B", "C", "D"), c(0, 4, 0, 4), c(8, 8, 4, 4), size = 18, color = "grey")
            save_plot("figs/fig_map.pdf", fig_map, base_width = 8, base_height = 8)
            
            
            
plots_list <- list() 
for(i in 1:16){
  pltyear <- 1999 + i
  pea <- raster(paste("code_output/Electricity/access_", pltyear, ".tif",  sep = ""))
  pea_xy <- xyFromCell(ntl, seq(pea[])[!is.na(pea[])])
  pea_df <- data.frame(z = pea[!is.na(pea[])], lon = pea_xy[,1], lat = pea_xy[,2])
  plots_list[[i]] <- ggplot(pea_df, aes(lon, lat)) +
       geom_raster(aes(fill = z)) +
       coord_equal() +
       theme_map() +
       theme(legend.position = c(.05,.05), legend.key.width = unit(.8, "cm")) +
       scale_fill_viridis(limits = c(0, 1), guide = guide_colorbar(title = paste("Geostatistic\nmodel\n", pltyear, sep = " ")))
  save_plot(paste("figs/ggmaps/pea_", pltyear, ".png", sep = ""), plots_list[[i]], base_width = 8, base_height = 8)
}
