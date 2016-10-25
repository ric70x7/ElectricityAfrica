# Figures: Electricity access and NTL data
# ----------------------------------------
#
# Edited October 24 2016

library(raster)
library(maptools)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(latex2exp)


graphics.off()
rm(list = ls())
#load("code_output/train_and_test_data.RData")
load("code_output/merged_data.RData")

myblue <-"#6495ED"
mygreen <- "#86C67C"
myred <- "#EE6A50"
font_size <- 15

df <- subset(df, obfuscated)

df$zero <- "NTL > 0"
df$zero[df$ntl == 0] <- "NTL = 0"

df$r_bounded <- df$r
df$new_var <- log(df$r_bounded/(1-df$r_bounded)) - df$country_logit_r

plot_density <- ggplot(df, aes(r- country_r_mean)) + geom_density(aes(fill = factor(zero)), alpha = .7)  +
  scale_fill_manual(values = c(myblue, myred), guide = guide_legend(title = "Density")) +
  xlab("Access to Electricity\n(difference vs country estimate)")   +
  theme_hc(base_size = font_size) +
        theme(axis.title.y = element_blank(),
              #axis.title.x = element_blank(),
              #axis.ticks.x = element_blank(),
              #axis.text.y = element_blank(),
              legend.position = c(.2,.85)) 


plot_ntl_hist <- ggplot(df, aes(ntl)) + geom_histogram(fill = mygreen) +
  xlab("NTL\n(luminosity level)") +
  ylab("") +
  #ylab("Frequency")   +
  theme_hc(base_size = font_size)



plot_ntl <- ggplot(subset(df, ntl > 0), aes(zpositive.ntl,  r - country_r_mean)) + geom_point(color = myred, alpha=1) +
#plot_ntl <- ggplot(subset(df, ntl > 0 & is.finite(new_var)), aes(zpositive.ntl,  new_var)) + geom_point(color = myred, alpha=.3) +
  xlab("NTL\n(standardized)") +
  ylab("Electricity access\n(difference vs country estimate)")   +
  theme_hc(base_size = font_size)


plot_pop <- ggplot(subset(df, ntl > 0), aes(zpositive.pop,  r - country_r_mean)) + geom_point(color = mygreen, alpha=.3) +
#plot_pop <- ggplot(subset(df, ntl > 0 & is.finite(new_var)), aes(zpositive.pop,  new_var)) + geom_point(color = mygreen, alpha=.3) +
  xlim(-3,7) +
  xlab("Population\n(standardized)") +
  ylab("Electricity access\n(difference vs country estimate)")   +
  theme_hc(base_size = font_size)



graphics.off()

fig_zntl <- ggdraw(xlim = c(0,12), ylim = c(0,4)) +
            draw_plot(plot_ntl_hist, x = 0, y = 0, width = 4, height = 3.8) +
            draw_plot(plot_density, x = 4, y = 0, width = 4, height = 3.8) +
            draw_plot(plot_ntl, x = 8, y = 0, width = 4, height = 3.8) +
            draw_plot_label(c("A", "B", "C"), c(0, 4, 8), c(4, 4, 4), size = 18, color = "grey")
save_plot("figs/fig_zntl.pdf", fig_zntl, base_width = 12, base_height = 4)
