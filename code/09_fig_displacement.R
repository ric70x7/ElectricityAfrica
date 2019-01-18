
require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
library("ggplot2")
library("cowplot")
library("viridis")
require("plyr")
library(ggthemes)
library(viridis)
library(cowplot)
rm(list = ls())


make_circle <- function(r, x0, y0) {
  step <- .05
  length1 <- length(seq(-r, r, step))
  x <- c(seq(-r, r, step), seq(r - step, -r, -step))
  y <- sqrt(r^2 - x^2) * c(rep(1, length1), rep(-1, length1 - 1))
  return(data.frame(lon=x + x0, lat=y + y0))
}

dspl1 <-  as(spatstat::disc(radius = 5, centre = c(-8, 0)), "SpatialPolygons")
dspl1 <- as.data.frame(coordinates(spsample(dspl1, n=400, type = "random")))
colnames(dspl1) <- c("lon", "lat")

dspl2 <-  as(spatstat::disc(radius = 5, centre = c(3.5, 0)), "SpatialPolygons")
dspl2 <- as.data.frame(coordinates(spsample(dspl2, n=400, type = "random")))
colnames(dspl2) <- c("lon", "lat")

abc <- data.frame(lon=c(0, -8, 3.5), lat=c(0, 0, 0), name=LETTERS[1:3])
buff1 <- make_circle(r=6, x0=0, y0=0)
buff2 <- make_circle(r=3, x0=0, y0=0)

plt <- ggplot(data=abc, aes(lon, lat)) +
  geom_point(data = dspl1, color="magenta", alpha=1) +
  geom_point(data = dspl2, color="magenta", alpha=1) +
  geom_polygon(data = buff1, fill=viridis::viridis_pal(option="E")(2)[1], alpha=.6) +
  geom_polygon(data = buff2, fill=viridis::viridis_pal(option="E")(2)[2], alpha=.6) +
  geom_point(data=abc, col="darkred")  +
  theme_void() +
  guides(fill=FALSE) +
  coord_fixed() +
  geom_text(data=abc, aes(label=name), col="darkred", nudge_y = -.8, size=10) 

graphics.off()

fig_data <- ggdraw(xlim = c(0,12), ylim = c(0,4)) +
            draw_plot(plt, x = 0, y = 0, width = 12, height = 4) 
save_plot("figs/09_fig_displacement.pdf", fig_data, base_width = 12, base_height = 4)
