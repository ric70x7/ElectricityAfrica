
library(rstan)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(latex2exp)

rm(list=ls())

#load("code_output/vgpm_samples.RData")
load("code_output/country_stats.RData")
load("code_output/country_annual_estimates.RData")

myblue <-"#6495ED"
mygreen <- "#86C67C"
myred <- "#EE6A50"
font_size <- 15

iso3list <- unique(annual_data$iso3)
#cnums<- c(40,42,44)
#cnums<- c(47,8,2)
#13, 24, 37, 46
cnums<- c(13, 24, 38)

# 1
num <- cnums[1]
df <- data.frame(year = 2000:2015,
                 mest = subset(annual_data, iso3 == iso3list[num])$r_mean, 
                 cilo = subset(annual_data, iso3 == iso3list[num])$r_lbou, 
                 ciup = subset(annual_data, iso3 == iso3list[num])$r_ubou, 
                 obsv = subset(raw_country_stats, iso3 == iso3list[num])$r,
                 dummy1 = "Mean estimates",
                 dummy2 = "Observed data",
                 #dummy3 = "LP",
                 dummy4 = "95% credible intervals")
plot1 <- ggplot(df) + 
         #geom_line(aes(year, mest, color = dummy3), size = 1.3, alpha = .7) +
         geom_errorbar(aes(year, ymin = cilo, ymax = ciup, color = dummy4), size = .7, alpha = .7) +
         geom_line(aes(year, mest, color = dummy1), size = 1.) +
         ylim(.9,1) +
         ylab("Electricity access") +
         geom_point(aes(year, obsv, col = dummy2), size = 1.5) +
               #theme_hc(base_size = font_size) +
               theme(#axis.title.y = element_blank(),
                     axis.title.x = element_blank(),
                     axis.ticks.x = element_blank(),
                     #axis.text.y = element_blank(),
                     legend.position = c(.4,.20)) +
               scale_color_manual(name=iso3list[num], values=c(myred, myblue, mygreen)) 
          
  

# 2
num <- cnums[2]
df <- data.frame(year = 2000:2015,
                 mest = subset(annual_data, iso3 == iso3list[num])$r_mean, 
                 cilo = subset(annual_data, iso3 == iso3list[num])$r_lbou, 
                 ciup = subset(annual_data, iso3 == iso3list[num])$r_ubou, 
                 obsv = subset(raw_country_stats, iso3 == iso3list[num])$r,
                 dummy1 = "Mean estimates",
                 dummy2 = "Observed data",
                 #dummy3 = "LP",
                 dummy4 = "95% credible intervals")
plot2 <- ggplot(df) + 
         #geom_line(aes(year, mest, color = dummy3), size = 1.3, alpha = .7) +
         geom_errorbar(aes(year, ymin = cilo, ymax = ciup, color = dummy4), size = .7, alpha = .7) +
         geom_line(aes(year, mest, color = dummy1), size = 1.) +
         ylim(0, .2) +
         ylab("Electricity access") +
         geom_point(aes(year, obsv, col = dummy2), size = 1.5) +
               #theme_hc(base_size = font_size) +
               theme(#axis.title.y = element_blank(),
                     axis.title.x = element_blank(),
                     axis.ticks.x = element_blank(),
                     #axis.text.y = element_blank(),
                     legend.position = c(.4,.80)) +
               scale_color_manual(name=iso3list[num], values=c(myred, myblue, mygreen)) 
          
  

# 3
num <- cnums[3]
df <- data.frame(year = 2000:2015,
                 mest = subset(annual_data, iso3 == iso3list[num])$r_mean, 
                 cilo = subset(annual_data, iso3 == iso3list[num])$r_lbou, 
                 ciup = subset(annual_data, iso3 == iso3list[num])$r_ubou, 
                 obsv = subset(raw_country_stats, iso3 == iso3list[num])$r,
                 dummy1 = "Mean estimates",
                 dummy2 = "Observed data",
                 #dummy3 = "LP",
                 dummy4 = "95% credible intervals")
plot3 <- ggplot(df) + 
         #geom_line(aes(year, mest, color = dummy3), size = 1.3, alpha = .7) +
         geom_errorbar(aes(year, ymin = cilo, ymax = ciup, color = dummy4), size = .7, alpha = .7) +
         geom_line(aes(year, mest, color = dummy1), size = 1.) +
         ylim(.3, .7) +
         ylab("Electricity access") +
         geom_point(aes(year, obsv, col = dummy2), size = 1.5) +
               #theme_hc(base_size = font_size) +
               theme(#axis.title.y = element_blank(),
                     axis.title.x = element_blank(),
                     axis.ticks.x = element_blank(),
                     #axis.text.y = element_blank(),
                     legend.position = c(.4,.80)) +
               scale_color_manual(name=iso3list[num], values=c(myred, myblue, mygreen)) 
          


graphics.off()

fig_gpfit <- ggdraw(xlim = c(0,12), ylim = c(0,4)) +
            draw_plot(plot1, x = 0, y = 0, width = 4, height = 3.8) +
            draw_plot(plot2, x = 4, y = 0, width = 4, height = 3.8) +
            draw_plot(plot3, x = 8, y = 0, width = 4, height = 3.8) +
            draw_plot_label(c("A", "B", "C"), c(0, 4, 8), c(4, 4, 4), size = 18, color = "grey")
fig_gpfit
save_plot("figs/fig_gpfit.pdf", fig_gpfit, base_width = 12, base_height = 4)



