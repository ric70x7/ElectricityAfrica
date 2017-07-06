
library(rstan)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(latex2exp)

rm(list=ls())

#load("code_output/vgpm_samples.RData")
load("code_output/country_stats.RData")
load("code_output/country_annual_estimates.RData")

annual_data$r_mean <- annual_data$r_mean * 100
annual_data$reported_r <- annual_data$reported_r * 100
annual_data$r_lbou <- annual_data$r_lbou * 100
annual_data$r_ubou <- annual_data$r_ubou * 100
annual_data$f_mean <- annual_data$f_mean * 100
raw_country_stats$r <- raw_country_stats$r * 100


myblue <- "#0072B2" #"#6495ED"
mygreen <- "#999999"#"#009E73" #"#86C67C"
myred <- "#CC79A7"#"#D55E00" #"#EE6A50"
font_size <- 15

iso3list <- unique(annual_data$iso3)
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
                 dummy4 = "95% credible intervals")
plot1 <- ggplot(df) + 
         geom_errorbar(aes(year, ymin = cilo, ymax = ciup, color = dummy4), size = .7, alpha = .7) +
         geom_line(aes(year, mest, color = dummy1), size = 1.) +
         ylim(90,100) +
         ylab("Electricity access (%)") +
         geom_point(aes(year, obsv, col = dummy2), size = 3.5, alpha = .7) +
         theme_bw() +
         theme(panel.border = element_blank(),
               legend.position = c(.4,.20),
               legend.background = element_rect(fill = "transparent"),
               legend.text=element_text(size = 12),
               axis.title.x = element_blank(),
               axis.ticks.x = element_blank(),
               axis.text = element_text(size = 12),
               axis.title = element_text(size = 15)) +
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
                 dummy4 = "95% credible intervals")
plot2 <- ggplot(df) + 
         geom_errorbar(aes(year, ymin = cilo, ymax = ciup, color = dummy4), size = .7, alpha = .7) +
         geom_line(aes(year, mest, color = dummy1), size = 1.) +
         ylim(0, 20) +
         ylab("Electricity access (%)") +
         geom_point(aes(year, obsv, col = dummy2), size = 3.5, alpha = .7) +
         theme_bw() +
         theme(panel.border = element_blank(),
               legend.position = c(.4,.80),
               legend.background = element_rect(fill = "transparent"),
               legend.text=element_text(size = 12),
               axis.title.x = element_blank(),
               axis.ticks.x = element_blank(),
               axis.text = element_text(size = 12),
               axis.title = element_text(size = 15)) +
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
                 dummy4 = "95% credible intervals")
plot3 <- ggplot(df) + 
         geom_errorbar(aes(year, ymin = cilo, ymax = ciup, color = dummy4), size = .7, alpha = .7) +
         geom_line(aes(year, mest, color = dummy1), size = 1.) +
         ylim(30, 70) +
         ylab("Electricity access (%)") +
         geom_point(aes(year, obsv, col = dummy2), size = 3.5, alpha = .7) +
         theme_bw() +
         theme(panel.border = element_blank(),
               legend.position = c(.4,.80),
               legend.background = element_rect(fill = "transparent"),
               legend.text=element_text(size = 12),
               axis.title.x = element_blank(),
               axis.ticks.x = element_blank(),
               axis.text = element_text(size = 12),
               axis.title = element_text(size = 15)) +
         scale_color_manual(name=iso3list[num], values=c(myred, myblue, mygreen)) 

graphics.off()

fig_gpfit <- ggdraw(xlim = c(0,12), ylim = c(0,4)) +
            draw_plot(plot1, x = 0, y = 0, width = 4, height = 3.8) +
            draw_plot(plot2, x = 4, y = 0, width = 4, height = 3.8) +
            draw_plot(plot3, x = 8, y = 0, width = 4, height = 3.8) +
            draw_plot_label(c("A", "B", "C"), c(0, 4, 8), c(4, 4, 4), size = 18, color = "grey")
fig_gpfit
save_plot("figs/fig_gpfit.pdf", fig_gpfit, base_width = 12, base_height = 4)



