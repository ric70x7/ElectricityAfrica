library(ggplot2)

graphics.off()
rm(list = ls())

plt_list <- list()
for (i in 1:14) {
  file <- paste0("code_output/Electricity/electricity_", i + 1999, ".tif")
  r <- raster::raster(file)
  new_r <- raster::aggregate(r, fact=10, fun=mean)
  mask <- !is.na(new_r[])
  xy <- as.data.frame(raster::xyFromCell(new_r, seq(new_r[])[mask]))
  xy$p <- new_r[mask]
  
  plt_list[[i]] <- ggplot(xy, aes(x,y)) + geom_raster(aes(fill=p)) +
      viridis::scale_fill_viridis(option = "E", discrete = FALSE, limits=c(0,1), guide=FALSE) +
      coord_fixed() +
      theme_void()
  
  #ggsave(filename = paste0("figs/e_", i + 1999, ".pdf"), plot = plt_list[[i]])
  
}

psc=4
fig_data <- cowplot::ggdraw(xlim = c(0, 16), ylim = c(0, 16)) +
            cowplot::draw_plot(plt_list[[1]], x = 0, y = 12, width = 4, height = 4) +
            cowplot::draw_plot(plt_list[[2]], x = 4, y = 12, width = 4, height = 4) +
            cowplot::draw_plot(plt_list[[3]], x = 8, y = 12, width = 4, height = 4) +
            cowplot::draw_plot(plt_list[[4]], x = 12, y = 12, width = 4, height = 4) +

            cowplot::draw_plot(plt_list[[5]], x = 0, y = 8, width = 4, height = 4) +
            cowplot::draw_plot(plt_list[[6]], x = 4, y = 8, width = 4, height = 4) +
            cowplot::draw_plot(plt_list[[7]], x = 8, y = 8, width = 4, height = 4) +
            cowplot::draw_plot(plt_list[[8]], x = 12, y = 8, width = 4, height = 4) +

            cowplot::draw_plot(plt_list[[9]], x = 0, y = 4, width = 4, height = 4) +
            cowplot::draw_plot(plt_list[[10]], x = 4, y = 4, width = 4, height = 4) +
            cowplot::draw_plot(plt_list[[11]], x = 8, y = 4, width = 4, height = 4) +
            cowplot::draw_plot(plt_list[[12]], x = 12, y = 4, width = 4, height = 4) +

            cowplot::draw_plot(plt_list[[13]], x = 0, y = 0, width = 4, height = 4) +
            cowplot::draw_plot(plt_list[[14]], x = 4, y = 0, width = 4, height = 4) +

            cowplot::draw_plot_label(c(paste0(0, 0:9), 10:13),
                                     c(rep(0:3, 3), 0:1)*psc,
                                     c(sort(rep(4:2, 4), decreasing = TRUE), 1, 1) * psc,
                                 size=24, colour = "grey")

cowplot::save_plot("figs/11_fig_africa.pdf", fig_data, base_width = 16, base_height = 16)


