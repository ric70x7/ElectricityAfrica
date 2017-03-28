
library(ggplot2)
library(ggthemes)
library(cowplot)
library(latex2exp)

rm(list=ls())

load("code_output/country_annual_estimates.RData")

myblue <-"#6495ED"
mygreen <- "#86C67C"
myred <- "#EE6A50"
font_size <- 15

iso3list <- unique(annual_data$iso3)


y0 <- subset(annual_data, year == 2000)
y0 <- y0[order(y0$r_mean, decreasing = TRUE), ]
y0$str <- paste(y0$iso3)
y0$str[2] <- "DZA, EGY"
y0$str[3] <- NA
y0$str[6] <- "ZAF, MAR"#, ESH"
y0$str[7:8] <- NA
y0$str[10] <- "CIV, GNB"
y0$str[11] <- NA
y0$str[12] <- "NGA, DJI, CMR"
y0$str[c(13, 14)] <- NA
y0$str[16] <- "BWA, SEN"
y0$str[17] <- NA
y0$str[20] <- NA
y0$str[21] <- "GMB, AGO, ERI, SWZ"
y0$str[22:23] <- NA
y0$str[24] <- "SOM, SDN"
y0$str[25] <- NA
y0$str[26] <- "BEN, COG"
y0$str[27] <- NA
y0$str[29] <- NA
y0$str[30] <- "GIN, ZMB, TGO, MDG"
y0$str[31:32] <- NA
y0$str[33] <- "MLI, KEN, ETH"
y0$str[34:35] <- NA
y0$str[36] <- "TZA, SLE, COD, BFA, UGA"
y0$str[37:40] <- NA
y0$str[41] <- "NER, MOZ, CAF, MWI, LSO"
y0$str[42:45] <- NA
y0$str[43:44] <- NA
y0$str[45] <- "CAF, MWI LSO BDI RWA"
y0$str[46:47] <- NA
y0 <- subset(y0, !is.na(str))


y1 <- subset(annual_data, year == 2015)
y1 <- y1[order(y1$r_mean, decreasing = TRUE), ]
y1$str <- paste(y1$iso3)
y1$str[1] <- "LBY, TUN, DZA"
y1$str[2:3] <- NA
y1$str[4] <- "EGY, MAR"#, ESH"
y1$r_mean[4] <- y1$r_mean[4] - .03
y1$str[5:6] <- NA
y1$str[7] <- "GAB, ZAF"
y1$str[8] <- NA
y1$str[12] <- "GNB, CIV"
y1$str[13] <- NA
y1$str[14] <- "NGA, CMR, DJI"
y1$str[15:16] <- NA
y1$str[18] <- "NAM, COG"
y1$str[19] <- NA
y1$str[20] <- "TGO, SWZ"
y1$str[21] <- NA
y1$str[22] <- "ZWE, BEN, GMB"
y1$str[23:24] <- NA
y1$str[26] <- "KEN, ERI"
y1$str[27] <- NA
y1$str[28] <- "SDN, SOM"
y1$str[29] <- NA
y1$str[30] <- "ETH, LSO"
y1$str[31] <- NA
y1$str[32] <- "GIN, MOZ, MLI"
y1$str[33:34] <- NA
y1$str[36] <- "MRT, RWA"
y1$str[37] <- NA
y1$str[38] <- "UGA, BFA, TZA, COD"
y1$str[39:41] <- NA
y1$str[42] <- "SLE, MDG, NER, LBR"
y1$str[43:45] <- NA
y1$str[48] <- "TCD, BDI"
y1$str[49] <- NA
y1 <- subset(y1, !is.na(str))


annual_data <- subset(annual_data, iso3 != "ESH") # Remove Western Sahara
fig_gbox <- ggplot(annual_data, aes(factor(year), r_mean)) + geom_boxplot(col = myblue, alpha = .7) +
            geom_line(aes(factor(year), r_mean, group = iso3), col = "grey") +
            geom_point(aes(factor(year), r_mean), size = 1.5, col = myred, alpha = .7) +
            geom_text(data = y0, aes(x = factor(-.1 + year), y = r_mean, label = str), hjust = 1) +
            geom_text(data = y1, aes(x = factor(+.1 + year), y = r_mean, label = str), hjust = 0) +
            expand_limits(x = factor(1995:2020)) +
            ylab("Electricity access") +
            scale_x_discrete(breaks = factor(seq(2000,2015, by = 3))) +
             theme(axis.title.x = element_blank(),
                   axis.ticks.x = element_blank())
fig_gbox
save_plot("figs/fig_gbox.pdf", fig_gbox, base_width = 12, base_height = 9)
graphics.off( )

mean(annual_data$r_mean[annual_data$year == 2000])
mean(annual_data$r_mean[annual_data$year == 2015])

sum(annual_data$year == 2000 & annual_data$r_mean > .5)
sum(annual_data$year == 2015 & annual_data$r_mean > .5)
#annual_data$year2 <- annual_data$year
#annual_data$year2[annual_data$year == 2000] <- annual_data$year[annual_data$year == 2000] - .1 #- floor(runif(49, 0, 4))/10
#annual_data$year2[annual_data$year == 2015] <- annual_data$year[annual_data$year == 2015] + .1 #+ floor(runif(49, 0, 4))/10

#off1 <- c("DZA", "MAR", "GNB", "DJI", "SEN", "AGO", "SDN", "COG", "KEN", "TCD", "BDI", "NER", "COD", "ZMB")
#off2 <- c("ESH", "CMR", "ERI", "RWA", "ETH", "MWI", "TZA", "TGO", "BFA")
#off3 <- c("TZA", "CAF", "UGA")
#annual_data$year2[annual_data$year == 2000 &
#                  annual_data$iso3 %in% off1] <- annual_data$year2[annual_data$year == 2000 &
#                                                                   annual_data$iso3 %in% off1] - .1
#annual_data$year2[annual_data$year == 2000 &
#                  annual_data$iso3 %in% off2] <- annual_data$year2[annual_data$year == 2000 &
#                                                                   annual_data$iso3 %in% off2] - .2
#annual_data$year2[annual_data$year == 2000 &
#                  annual_data$iso3 %in% off3] <- annual_data$year2[annual_data$year == 2000 &
#                                                                   annual_data$iso3 %in% off3] - .3

#off5 <- c("DZA", "ZAF", "GNB", "CMR", "SWZ", "SOM", "MOZ", "RWA", "BFA", "BDI", "KEN", "ETH", "NER", "GM")
#off6 <- c("EGY", "AGO", "LBR", "ZMB", "TZA")#,     "CMR", "ERI", "RWA", "ETH", "MDG", "MWI", "UGA")
#off7 <- c("TUN", "COD")
#annual_data$year2[annual_data$year == 2015 &
#                  annual_data$iso3 %in% off5] <- annual_data$year2[annual_data$year == 2015 &
#                                                                   annual_data$iso3 %in% off5] + .1
#annual_data$year2[annual_data$year == 2015 &
#                  annual_data$iso3 %in% off6] <- annual_data$year2[annual_data$year == 2015 &
#                                                                   annual_data$iso3 %in% off6] + .2
#annual_data$year2[annual_data$year == 2015 &
#                  annual_data$iso3 %in% off7] <- annual_data$year2[annual_data$year == 2015 &
#                                                                   annual_data$iso3 %in% off7] + .3


