
library(ggplot2)
library(ggthemes)
library(cowplot)
library(latex2exp)

rm(list=ls())
graphics.off()

load("code_output/country_annual_estimates.RData")
annual_data$r_mean <- annual_data$r_mean * 100

myblue <- "#56B4E9" #"#6495ED"
mygreen <- "#009E73"#"#86C67C"
myred <- "#D55E00"#"#EE6A50"
font_size <- 15

iso3list <- unique(annual_data$iso3)

annual_data <- subset(annual_data, iso3 != "ESH") # Remove Western Sahara
data15 <- subset(annual_data, year == 2015)
ix <- order(data15$r_mean, decreasing = TRUE)
data15 <- data15[ix,]

isocol <- data.frame(iso3 = data15$iso3,
                     col3 = rep(c("#000000", "#E69F00",  "#56B4E9", "#009E73",
                                  "#999999", "#0072B2", "#D55E00", "#CC79A7"), 6))
annual_data$col3 <-isocol$col3[match(annual_data$iso3, isocol$iso3)]

# Left panel
annual_data$left <- FALSE
mask00 <- annual_data$year == 2000
annual_data$left <- FALSE
annual_data$left_y <- NA
annual_data$left_x <- NA
annual_data$left[mask00] <- TRUE
annual_data$left_y[mask00] <- annual_data$r_mean[mask00]
annual_data$left_x[mask00] <- 2000 - 1 

# Right panel
annual_data$right <- FALSE
mask15 <- annual_data$year == 2015
annual_data$right <- TRUE
annual_data$right_y <- NA
annual_data$right_x <- NA
annual_data$right[mask15] <- TRUE
annual_data$right_y[mask15] <- annual_data$r_mean[mask15]
annual_data$right_x[mask15] <- 2015 + .1

source("code/fig_gbox_locations.R")

fig_hbox <- ggplot(annual_data, aes(as.factor(year), r_mean)) +
            geom_boxplot(fill = "#0072B2") +
            geom_point(col = "#CC79A7", alpha = .5, size = 3) +
            ylab("Electricity access (%)") +
            theme_bw() +
            scale_x_discrete(breaks = factor(seq(2000,2015, by = 3))) +
            theme(panel.border = element_blank(),
                  legend.position="none",
                  axis.title.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text = element_text(size = 12),
                  axis.title = element_text(size = 12))
fig_hbox
save_plot("figs/fig_hbox.pdf", fig_hbox, base_width = 12, base_height = 4)


fig_gbox <- ggplot(annual_data, aes(year, r_mean)) +
            geom_text(data = subset(annual_data, right), aes(x = right_x, y = right_y, label = iso3, col = as.factor(iso3)), family = "mono", hjust = 0, size = 5) +
            geom_line(aes(year, r_mean, group = iso3, col = iso3), alpha = .7) +
            geom_point(aes(year, r_mean, col = as.factor(iso3)), size = 2, alpha = .5) +
            geom_text(data = subset(annual_data, left), aes(x = left_x, y = left_y, label = iso3, col = as.factor(iso3)), family = "mono", hjust = 0, size = 5) +
            expand_limits(x = 1997:2018) +
            ylab("Electricity access (%)") +
            scale_color_manual(values = paste(isocol$col3)) + 
            theme_bw() +
            theme(panel.border = element_blank(),
                  legend.position="none",
                  axis.title.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text = element_text(size = 12),
                  axis.title = element_text(size = 15))
fig_gbox

save_plot("figs/fig_gbox.pdf", fig_gbox, base_width = 12, base_height = 16)

#fig_data <- ggdraw(xlim = c(0,12), ylim = c(0,20)) +
#            draw_plot(fig_gbox, x = 0, y = 8, width = 12, height = 12) +
#            draw_plot(fig_hbox, x = 0, y = 0, width = 12, height = 8) +
#            draw_plot_label(c("A", "B"), c(0, 0), c(20, 4), size = 18, color = "grey")
#fig_data

save_plot("figs/fig_boxntrends.pdf", fig_data, base_width = 12, base_height = 8)
graphics.off( )

mean(annual_data$r_mean[annual_data$year == 2000])
mean(annual_data$r_mean[annual_data$year == 2015])

sum(annual_data$year == 2000 & annual_data$r_mean > .5)
sum(annual_data$year == 2015 & annual_data$r_mean > .5)
