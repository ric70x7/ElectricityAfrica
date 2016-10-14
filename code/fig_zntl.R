# Figures: r - r_country mean vs standardized NTL
#
# Edited October 13 2016

library(raster)
library(maptools)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(latex2exp)


graphics.off()
rm(list = ls())
load("code_output/train_and_test_data.RData")

df <- rbind(df.train, df.test1)

df$zero <- "NTL > 0"
df$zero[df$ntl == 0] <- "NTL = 0"

df$r_bounded <- df$r
df$new_var <- log(df$r_bounded/(1-df$r_bounded)) - df$r_country_logit

font_size = 15
plot_density <- ggplot(df, aes(r- country_r_mean)) + geom_density(aes(fill = factor(zero)), alpha = .7)  +
  scale_fill_manual(values = c("#6495ED", "#EE6A50"), guide = guide_legend(title = "")) +
  xlab("Access to Electricity\n(difference vs country estimate)")   +
  theme_hc(base_size = font_size)
  
plot_scatter <- ggplot(subset(df, ntl > 0), aes(z.ntl,  r - country_r_mean)) + geom_point(color = "#6495ED"  ) +
  xlab("NTL standardized") +
  #ylab(TeX("$Variation\\; around\\; \\tilde{p}$"))   +
  ylab("Electricity access\n(difference vs country estimate)")   +
  theme_hc(base_size = font_size)


graphics.off()

fig_zntl <- ggdraw(xlim = c(0,12), ylim = c(0,4)) +
            draw_plot(plot_density, x = 0, y = 0, width = 8, height = 4) +
            draw_plot(plot_scatter, x = 8, y = 0, width = 4, height = 4) +
            draw_plot_label(c("A", "B"), c(0, 8), c(4, 4), size = 18, color = "grey")
save_plot("figs/fig_zntl.pdf", fig_zntl, base_width = 12, base_height = 4)
