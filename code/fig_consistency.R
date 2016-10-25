# Comparison between country-level statistics and geostatistic average weighted by population
# -------------------------------------------------------------------------------------------
#
# Edited October 15 2016


library(raster)
library(maptools)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(latex2exp)


graphics.off()
rm(list = ls())

myblue <-"#6495ED"
mygreen <- "#86C67C"
myred <- "#EE6A50"
font_size <- 15

load("code_output/raster_aggregates.RData")

iso3list <- sort(unique(annual_data$iso3))


num_consistent <- c(30, 33, 41)
num_inconsistent <- c(25, 27, 40)
#for(num in seq(iso3list)){

annual_data$dummy1 <-"per household"
annual_data$dummy2 <-"per population"

i <- 0
plt_list <- list()
hlocation <- c(.85, .85, .85, .25, .25, .85)
for(num in c(num_consistent, num_inconsistent)){
  i <- i+1
  plt_list[[i]] <-  ggplot(subset(annual_data, iso3 == iso3list[num]), aes(x = year)) +
   geom_line(aes(y = r_mean, color = dummy1), size = 1.5) +
   geom_line(aes(y = agg_raster, color = dummy2), size = 1.5) +
   ylim(0,1) +
   ylab("Electricity access proportion") +
   scale_color_manual(values = c(myblue, mygreen), guide = guide_legend(title = iso3list[num])) +
      theme_hc(base_size = font_size) +
      theme(axis.title.y = element_text(),
            axis.title.x = element_blank(),
            legend.position = c(.5,hlocation[i])) 
}


fig_caver <-ggdraw(xlim = c(0,12), ylim = c(0,8)) +
            draw_plot(plt_list[[1]], x = 0, y = 4, width = 4, height = 3.8) +
            draw_plot(plt_list[[2]], x = 4, y = 4, width = 4, height = 3.8) +
            draw_plot(plt_list[[3]], x = 8, y = 4, width = 4, height = 3.8) +
            draw_plot(plt_list[[5]], x = 0, y = 0, width = 4, height = 3.8) +
            draw_plot(plt_list[[4]], x = 4, y = 0, width = 4, height = 3.8) +
            draw_plot(plt_list[[6]], x = 8, y = 0, width = 4, height = 3.8) +
            draw_plot_label(c("A", "B", "C", "D", "E", "F"),
                            c(0, 4, 8, 0, 4, 8), c(4, 4, 4, 8, 8, 8), size = 18, color = "grey")
save_plot("figs/fig_consistency.pdf", fig_caver, base_width = 12, base_height = 8)

