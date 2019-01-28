# Fig: Decay function to define proximity
# ---------------------------------------

library(ggplot)
library(ggthemes)
library(viridis)
library(cowplot)

funk <- function(x) {
  return(exp(-x/ 0.2811558))
}

decaydf <- data.frame(distance=1:10 / 10, delta = funk(1:10/ 10))
missing <- data.frame(distance=10:14 /10, delta = rep(0, 5))

plt <- ggplot(decaydf, aes(distance, delta)) +
  #geom_area(fill = viridis::viridis_pal(option="E")(2)[2], alpha=.8) +
  geom_line(data=decaydf, color=viridis::viridis_pal(option="E")(2)[1], size=1.3) +
  geom_line(data=missing, color=viridis::viridis_pal(option="E")(2)[1], size=1.3) +
  ylab("PIA") +
  xlab("Distance to closest impervious pixel (coordinate degrees)")

fig_data <- ggdraw(xlim = c(0, 8), ylim = c(0, 4)) +
            draw_plot(plt, x = 0, y = 0, width = 8, height = 4)

save_plot("figs/02_fig_decay.pdf", fig_data, base_width = 8, base_height = 4)
