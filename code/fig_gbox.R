
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

######


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

aggmean1$diff_ppl <- NA
aggmean1$diff_ppl[2:16] <- diff(aggmean1$ppl)

fig_hbox1 <- ggplot(annual_data, aes(as.factor(year), 100 * r_mean)) +
             geom_boxplot(fill = "#0072B2") +
             geom_point(col = "#CC79A7", alpha = .5, size = 3) +
             #geom_line(data = aggmean1, aes(year-1999, r_mean), col = "#E69F00", size = 1) + 
             ylab("Electricity access (%)") +
             theme_bw() +
             scale_x_discrete(breaks = factor(seq(2000,2015, by = 5))) +
             theme(panel.border = element_blank(),
                   legend.position="none",
                   axis.title.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 12))

fig_hbox2 <- ggplot(aggmean1, aes(as.factor(year), 100 * w_mean)) +
             geom_line(data = aggmean1, aes(year, 100 * w_mean), col = mygreen, size = 1) + 
             geom_point(data = aggmean1, aes(year, 100 * w_mean), col = mygreen, size = 3) + 
             ylim(c(35,50)) +
             ylab("Electricity access (%)") +
             theme_bw() +
             theme(panel.border = element_blank(),
                   legend.position="none",
                   axis.title.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.text = element_text(size = 12),
                   axis.title = element_text(size = 12))


fig_hbox3 <- ggplot(aggmean1, aes(as.factor(year), diff_ppl/1000000)) +
             geom_col(fill = "#E69F00", size = 1) + 
             ylab("Annual increase in electricity access\n (millions of people)") +
             theme_bw() +
             scale_x_discrete(breaks = factor(seq(2000,2015, by = 5))) +
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

fig_hboxz <- ggdraw(xlim = c(0,12), ylim = c(0,4)) +
             draw_plot(fig_hbox1, x = 0, y = 0, width = 6, height = 4) +
             draw_plot(fig_hbox2, x = 6, y = 0, width = 6, height = 4) +
             #draw_plot(fig_hbox3, x = 8, y = 0, width = 4, height = 4) +
             draw_plot_label(c("A", "B"), c(0, 6), c(4, 4), size = 18, color = "grey")

save_plot("figs/fig_hboxz.pdf", fig_hboxz, base_width = 12, base_height = 4)



#fig_data <- ggdraw(xlim = c(0,12), ylim = c(0,20)) +
#            draw_plot(fig_gbox, x = 0, y = 8, width = 12, height = 12) +
#            draw_plot(fig_hbox, x = 0, y = 0, width = 12, height = 8) +
#            draw_plot_label(c("A", "B"), c(0, 0), c(20, 4), size = 18, color = "grey")
#fig_data

#save_plot("figs/fig_boxntrends.pdf", fig_data, base_width = 12, base_height = 8)
graphics.off( )

mean(annual_data$r_mean[annual_data$year == 2000])
mean(annual_data$r_mean[annual_data$year == 2015])

sum(annual_data$year == 2000 & annual_data$r_mean > .5)
sum(annual_data$year == 2015 & annual_data$r_mean > .5)
