rm(list = ls())
library(raster)
library(maptools)

load("code_output/country_annual_estimates.RData")
years <- 2000:2015

annual_data$pop <- NA

# Create layer of offsets
ntl_x <- raster(paste("data/ntl/GP2_Africa_5k/GP2_Africa_2010.tif", sep = ""))
ntl_x[ntl_x[]==128] <- NA # Value 128 is NA

# Function to define new raster object
make_raster <- function(x, ref_raster = ntl_x){
  new_obj <- matrix(x, ncol = ref_raster@ncols, nrow = ref_raster@nrows, byrow = TRUE)
  new_obj <- raster(new_obj,
                    xmn = ref_raster@extent@xmin,
                    xmx = ref_raster@extent@xmax,
                    ymn = ref_raster@extent@ymin,
                    ymx = ref_raster@extent@ymax,
                    crs = ref_raster@crs)
  return(new_obj)
}

afri_main <- readShapePoly("data/Africa_main_country/Africa_main_country.shp")
afri_countries <- unique(annual_data$iso3)
i <- 1
for(i in seq(years)){
  yi <- 1999 + i
  print(yi)
  country_mean_layer <- rep(NA, length(ntl_x[]))
  country_f_layer <- rep(NA, length(ntl_x[]))
  lit_layer <- rep(NA, length(ntl_x[]))
  pop_i <- raster(paste("code_output/Population/GPW4_" , yi, ".tif", sep = ""))
  for(iso3j in afri_countries){ 
    shp_boundary <- afri_main[afri_main$ISO3 == iso3j, ]
    raster_mask <- mask(pop_i, shp_boundary)
    #plot(raster_mask)
    cells_mask <- !is.na(raster_mask[])
    annual_data$pop[annual_data$iso3 == iso3j & annual_data$year == yi] <- sum(pop_i[cells_mask])
  }  
} 

save(annual_data, file = "code_output/df_afritrend.RData")


load("code_output/df_afritrend.RData")
annual_data <- subset(annual_data, iso3 != "ESH")

for (i in 2000:2015) {
  annual_data$w_pop[annual_data$year == i] <- annual_data$pop[annual_data$year == i]/sum(annual_data$pop[annual_data$year == i])
  annual_data$ppl[annual_data$year == i] <- annual_data$pop[annual_data$year == i] * annual_data$r_mean[annual_data$year == i] 
}
annual_data$w_mean <- annual_data$r_mean * annual_data$w_pop

aggmean1 <- aggregate(annual_data$r_mean ~ annual_data$year, FUN = "mean")
aggmean2 <- aggregate(annual_data$w_mean ~ annual_data$year, FUN = "sum")
aggmean3 <- aggregate(annual_data$ppl ~ annual_data$year, FUN = "sum")

colnames(aggmean1) <- c("year", "r_mean")
aggmean1$w_mean <- aggmean2[,2]
aggmean1$ppl <- aggmean3[,2]

plot(aggmean1, type = "l", ylim = c(0, .5))
lines(aggmean2, col = "red")

plot(aggm)
plot(diff(aggmean1$ppl)/1000000)

colnames(aggmean) <- c("year", "r_mean")
head(aggmean)

fig_hbox1 <- ggplot(annual_data, aes(as.factor(year), r_mean)) +
            geom_boxplot(fill = "#0072B2") +
            geom_point(col = "#CC79A7", alpha = .5, size = 3) +
            geom_line(data = aggmean, aes(year-1999, r_mean), col = "#E69F00", size = 1) + 
            ylab("Electricity access (%)") +
            theme_bw() +
            scale_x_discrete(breaks = factor(seq(2000,2015, by = 3))) +
            theme(panel.border = element_blank(),
                  legend.position="none",
                  axis.title.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text = element_text(size = 12),
                  axis.title = element_text(size = 12))


diff_aggmean <- aggmean
diff_aggmean$r_mean[-c(1)] <- diff(aggmean$r_mean)
diff_aggmean$r_mean[1] <- 0
diff_aggmean$accr_mean <- cumsum(diff_aggmean$r_mean) 




fig_hbox2 <- ggplot(diff_aggmean, aes(as.factor(year), r_mean)) +
            geom_col(fill = "#E69F00", size = 1) + 
            ylim(c(0,1)) +
            ylab("Annual difference (percent points)") +
            theme_bw() +
            scale_x_discrete(breaks = factor(seq(2000,2015, by = 3))) +
            theme(panel.border = element_blank(),
                  legend.position="none",
                  axis.title.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text = element_text(size = 12),
                  axis.title = element_text(size = 12))


fig_hbox3 <- ggplot(diff_aggmean, aes(as.factor(year), accr_mean)) +
            geom_col(fill = "#E69F00", size = 1) + 
            ylab("Difference vs 2000 (percent points)") +
            theme_bw() +
            scale_x_discrete(breaks = factor(seq(2000,2015, by = 3))) +
            theme(panel.border = element_blank(),
                  legend.position="none",
                  axis.title.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text = element_text(size = 12),
                  axis.title = element_text(size = 12))


fig_hbox <- ggdraw(xlim = c(0,12), ylim = c(0,4)) +
            draw_plot(fig_hbox1, x = 0, y = 0, width = 4, height = 4) +
            draw_plot(fig_hbox2, x = 4, y = 0, width = 4, height = 4) +
            draw_plot(fig_hbox3, x = 8, y = 0, width = 4, height = 4) +
            draw_plot_label(c("A", "B", "C"), c(0, 4, 8), c(4, 4, 4), size = 18, color = "grey")


save_plot("figs/fig_hbox.pdf", fig_hbox, base_width = 12, base_height = 4)

