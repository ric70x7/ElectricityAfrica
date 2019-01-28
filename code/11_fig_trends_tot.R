library(ggplot2)

load("code_output/e_trends.RData")

df13 <- subset(trends, year==2000 & type=="total")
df13$p <- df13$pop_electricity/df13$pop_total
isocol <- data.frame(iso3 = df13$iso3[order(df13$p)],
                     col3 = c("#CC79A7", rep(c("#000000", "#E69F00",  "#56B4E9", "#009E73",
                                  "#999999", "#0072B2", "#D55E00", "#CC79A7"), 6)))
#isocol <- sample(isocol$col3, size = 49)

# Left panel
mask00 <- trends$year == 2000
trends$left_y <- NA
trends$left_x <- NA
trends$left_y[mask00] <- (trends$pop_pix/trends$pop_total)[mask00]
trends$left_x[mask00] <- 2000 - .75 
#trends$left_x[mask00 & trends$iso3 == "COG" & trends$type == "total"] <- 2000 - .75 - .7
#trends$left_x[mask00 & trends$iso3 == "GHA" & trends$type == "total"] <- 2000 - .75 - .7
#trends$left_x[mask00 & trends$iso3 == "TGO" & trends$type == "total"] <- 2000 - .75 - .7
#trends$left_x[mask00 & trends$iso3 == "BEN" & trends$type == "total"] <- 2000 - .75 - .7
#trends$left_x[mask00 & trends$iso3 == "GIN" & trends$type == "total"] <- 2000 - .75 - .7
#trends$left_x[mask00 & trends$iso3 == "MOZ" & trends$type == "total"] <- 2000 - .75 - .7
#trends$left_x[mask00 & trends$iso3 == "UGA" & trends$type == "total"] <- 2000 - .75 - .7
#trends$left_x[mask00 & trends$iso3 == "DJI" & trends$type == "total"] <- 2000 - .75 - .7
#trends$left_x[mask00 & trends$iso3 == "SOM" & trends$type == "total"] <- 2000 - .75 - .7
#trends$left_x[mask00 & trends$iso3 == "ERI" & trends$type == "total"] <- 2000 - .75 - .7
#trends$left_x[mask00 & trends$iso3 == "BWA" & trends$type == "total"] <- 2000 - .75 - .7
#trends$left_x[mask00 & trends$iso3 == "MDG" & trends$type == "total"] <- 2000 - .75 - .7
#trends$left_x[mask00 & trends$iso3 == "AGO" & trends$type == "total"] <- 2000 - .75 - .7
#trends$left_x[mask00 & trends$iso3 == "LBR" & trends$type == "total"] <- 2000 - .75 - .7
#
#trends$left_x[mask00 & trends$iso3 == "BDI" & trends$type == "total"] <- 2000 - .75 - .7 - .7
#trends$left_x[mask00 & trends$iso3 == "GAB" & trends$type == "total"] <- 2000 - .75 - .7 - .7
#trends$left_x[mask00 & trends$iso3 == "ESH" & trends$type == "total"] <- 2000 - .75 - .7 - .7
#trends$left_x[mask00 & trends$iso3 == "ETH" & trends$type == "total"] <- 2000 - .75 - .7 - .7
#trends$left_x[mask00 & trends$iso3 == "CAF" & trends$type == "total"] <- 2000 - .75 - .7 - .7
#trends$left_x[mask00 & trends$iso3 == "NER" & trends$type == "total"] <- 2000 - .75 - .7 - .7
#trends$left_x[mask00 & trends$iso3 == "SLE" & trends$type == "total"] <- 2000 - .75 - .7 - .7
#
#
#trends$left_y[mask00 & trends$iso3 == "ETH" & trends$type == "total"] <-
#  trends$left_y[mask00 & trends$iso3 == "ETH" & trends$type == "total"] - .002
#trends$left_y[mask00 & trends$iso3 == "MDG" & trends$type == "total"] <-
#  trends$left_y[mask00 & trends$iso3 == "MDG" & trends$type == "total"] - .002
#trends$left_y[mask00 & trends$iso3 == "SWZ" & trends$type == "total"] <-
#  trends$left_y[mask00 & trends$iso3 == "SWZ" & trends$type == "total"] - .005
#trends$left_y[mask00 & trends$iso3 == "LSO" & trends$type == "total"] <-
#  trends$left_y[mask00 & trends$iso3 == "LSO" & trends$type == "total"] - .005
#trends$left_y[mask00 & trends$iso3 == "SLE" & trends$type == "total"] <-
#  trends$left_y[mask00 & trends$iso3 == "SLE" & trends$type == "total"] - .005
#trends$left_y[mask00 & trends$iso3 == "MOZ" & trends$type == "total"] <-
#  trends$left_y[mask00 & trends$iso3 == "LSO" & trends$type == "total"]
#trends$left_y[mask00 & trends$iso3 == "COG" & trends$type == "total"] <-
#  trends$left_y[mask00 & trends$iso3 == "NGA" & trends$type == "total"]
#trends$left_y[mask00 & trends$iso3 == "BEN" & trends$type == "total"] <-
#  trends$left_y[mask00 & trends$iso3 == "ZMB" & trends$type == "total"]
#trends$left_y[mask00 & trends$iso3 == "DJI" & trends$type == "total"] <-
#  trends$left_y[mask00 & trends$iso3 == "SEN" & trends$type == "total"]
#trends$left_y[mask00 & trends$iso3 == "MLI" & trends$type == "total"] <-
#  trends$left_y[mask00 & trends$iso3 == "MDG" & trends$type == "total"]
#trends$left_y[mask00 & trends$iso3 %in% c("GHA", "GAB") & trends$type == "total"] <-
#  trends$left_y[mask00 & trends$iso3 == "NAM" & trends$type == "total"]
#trends$left_y[mask00 & trends$iso3 %in% c("TGO", "ESH") & trends$type == "total"] <-
#  trends$left_y[mask00 & trends$iso3 == "MRT" & trends$type == "total"]
#trends$left_y[mask00 & trends$iso3 %in% c("GIN", "TZA") & trends$type == "total"] <-
#  trends$left_y[mask00 & trends$iso3 == "ETH" & trends$type == "total"]
#trends$left_y[mask00 & trends$iso3 %in% c("BWA", "COD") & trends$type == "total"] <-
#  trends$left_y[mask00 & trends$iso3 == "CAF" & trends$type == "total"]
#trends$left_y[mask00 & trends$iso3 %in% c("UGA", "NER") & trends$type == "total"] <-
#  trends$left_y[mask00 & trends$iso3 == "MWI" & trends$type == "total"]
#trends$left_y[mask00 & trends$iso3 %in% c("AGO", "BDI") & trends$type == "total"] <-
#  trends$left_y[mask00 & trends$iso3 == "BFA" & trends$type == "total"]
#trends$left_y[mask00 & trends$iso3 %in% c("LBR", "GNQ") & trends$type == "total"] <-
#  trends$left_y[mask00 & trends$iso3 == "SLE" & trends$type == "total"]

# Right panel
mask13 <- trends$year == 2013
trends$right_y <- NA
trends$right_x <- NA
trends$right_y[mask13] <- (trends$pop_pix/trends$pop_total)[mask13]
trends$right_x[mask13] <- 2013 + .1

#trends$right_x[mask13 & trends$iso3 == "DZA" & trends$type == "total"] <- 2013 + .8
#trends$right_x[mask13 & trends$iso3 == "DZA" & trends$type == "total"] <- 2013 + .8
#trends$right_x[mask13 & trends$iso3 == "ESH" & trends$type == "total"] <- 2013 + .8
#trends$right_x[mask13 & trends$iso3 == "LBY" & trends$type == "total"] <- 2013 + .8
#trends$right_x[mask13 & trends$iso3 == "SEN" & trends$type == "total"] <- 2013 + .8
#trends$right_x[mask13 & trends$iso3 == "MOZ" & trends$type == "total"] <- 2013 + .8
#trends$right_x[mask13 & trends$iso3 == "UGA" & trends$type == "total"] <- 2013 + .8
#trends$right_x[mask13 & trends$iso3 == "SDN" & trends$type == "total"] <- 2013 + .8
#trends$right_x[mask13 & trends$iso3 == "RWA" & trends$type == "total"] <- 2013 + .8
#trends$right_x[mask13 & trends$iso3 == "ERI" & trends$type == "total"] <- 2013 + .8
#trends$right_x[mask13 & trends$iso3 == "CMR" & trends$type == "total"] <- 2013 + .8
#trends$right_x[mask13 & trends$iso3 == "GMB" & trends$type == "total"] <- 2013 + .8
#trends$right_x[mask13 & trends$iso3 == "NGA" & trends$type == "total"] <- 2013 + .8
#trends$right_x[mask13 & trends$iso3 == "LSO" & trends$type == "total"] <- 2013 + .8
#trends$right_x[mask13 & trends$iso3 == "MWI" & trends$type == "total"] <- 2013 + .8 + .7
#trends$right_x[mask13 & trends$iso3 == "COD" & trends$type == "total"] <- 2013 + .8 + .7
#trends$right_x[mask13 & trends$iso3 == "TZA" & trends$type == "total"] <- 2013 + .8 + .7
#
#trends$right_y[mask13 & trends$iso3 == "SLE" & trends$type == "total"] <-
#  trends$right_y[mask13 & trends$iso3 == "SLE" & trends$type == "total"] - .005
#trends$right_y[mask13 & trends$iso3 == "SWZ" & trends$type == "total"] <-
#  trends$right_y[mask13 & trends$iso3 == "SWZ" & trends$type == "total"] + .002
#trends$right_y[mask13 & trends$iso3 == "ZMB" & trends$type == "total"] <-
#  trends$right_y[mask13 & trends$iso3 == "ZMB" & trends$type == "total"] + .003
#trends$right_y[mask13 & trends$iso3 == "ETH" & trends$type == "total"] <-
#  trends$right_y[mask13 & trends$iso3 == "ETH" & trends$type == "total"] - .001
#trends$right_y[mask13 & trends$iso3 == "GIN" & trends$type == "total"] <-
#  trends$right_y[mask13 & trends$iso3 == "GIN" & trends$type == "total"] - .002
#trends$right_y[mask13 & trends$iso3 == "BWA" & trends$type == "total"] <-
#  trends$right_y[mask13 & trends$iso3 == "BWA" & trends$type == "total"] + .005
#trends$right_y[mask13 & trends$iso3 == "CAF" & trends$type == "total"] <-
#  trends$right_y[mask13 & trends$iso3 == "CAF" & trends$type == "total"] + .005
#trends$right_y[mask13 & trends$iso3 == "LSO" & trends$type == "total"] <-
#  trends$right_y[mask13 & trends$iso3 == "LSO" & trends$type == "total"] - .004
#trends$right_y[mask13 & trends$iso3 == "UGA" & trends$type == "total"] <-
#  trends$right_y[mask13 & trends$iso3 == "UGA" & trends$type == "total"] - .004
#
#trends$right_y[mask13 & trends$iso3 == "RWA" & trends$type == "total"] <-
#  trends$right_y[mask13 & trends$iso3 == "BDI" & trends$type == "total"]
#trends$right_y[mask13 & trends$iso3 == "DZA" & trends$type == "total"] <-
#  trends$right_y[mask13 & trends$iso3 == "TUN" & trends$type == "total"]
#trends$right_y[mask13 & trends$iso3 == "NGA" & trends$type == "total"] <-
#  trends$right_y[mask13 & trends$iso3 == "GAB" & trends$type == "total"]
#trends$right_y[mask13 & trends$iso3 == "ESH" & trends$type == "total"] <-
#  trends$right_y[mask13 & trends$iso3 == "GNB" & trends$type == "total"]
#trends$right_y[mask13 & trends$iso3 == "LYB" & trends$type == "total"] <-
#  trends$right_y[mask13 & trends$iso3 == "NAM" & trends$type == "total"]
#trends$right_y[mask13 & trends$iso3 == "LBY" & trends$type == "total"] <-
#  trends$right_y[mask13 & trends$iso3 == "NAM" & trends$type == "total"]
#trends$right_y[mask13 & trends$iso3 == "CMR" & trends$type == "total"] <-
#  trends$right_y[mask13 & trends$iso3 == "SWZ" & trends$type == "total"]
#trends$right_y[mask13 & trends$iso3 == "GMB" & trends$type == "total"] <-
#  trends$right_y[mask13 & trends$iso3 == "ZWE" & trends$type == "total"]
#trends$right_y[mask13 & trends$iso3 == "ERI" & trends$type == "total"] <-
#  trends$right_y[mask13 & trends$iso3 == "TGO" & trends$type == "total"]
#trends$right_y[mask13 & trends$iso3 == "SEN" & trends$type == "total"] <-
#  trends$right_y[mask13 & trends$iso3 == "KEN" & trends$type == "total"]
#trends$right_y[mask13 & trends$iso3 == "TZA" & trends$type == "total"] <-
#  trends$right_y[mask13 & trends$iso3 == "MOZ" & trends$type == "total"]
#
#trends$right_y[mask13 & trends$iso3 %in% c("COD", "SDN") & trends$type == "total"] <-
#  trends$right_y[mask13 & trends$iso3 == "NER" & trends$type == "total"]
#trends$right_y[mask13 & trends$iso3 == "BFA" & trends$type == "total"] <-
#  trends$right_y[mask13 & trends$iso3 == "UGA" & trends$type == "total"]
#trends$right_y[mask13 & trends$iso3 %in% c("MWI", "LSO") & trends$type == "total"] <-
#  trends$right_y[mask13 & trends$iso3 == "SLE" & trends$type == "total"]



tt <- "total"
fig_gbox <- ggplot(subset(trends, type==tt), aes(year, 100 * pop_pix/pop_total)) +
            geom_line(aes(col = iso3), alpha = .7) +
            #geom_point(aes(year, 100 * pop_electricity/pop_total, col = as.factor(iso3)), size = 2, alpha = .5) +
            geom_text(data = subset(trends, type==tt & year==2013), aes(x = right_x, y = 100 * right_y, label = iso3, col = as.factor(iso3)), family = "mono", hjust = 0, size = 5) +
            geom_text(data = subset(trends, type==tt & year==2000), aes(x = left_x, y = 100 * left_y, label = iso3, col = as.factor(iso3)), family = "mono", hjust = 0, size = 5) +
            expand_limits(x = 1997:2016) +
            ylab("Electricity access (%)") +
            scale_color_manual(values = paste(isocol$col3)) + 
            theme_bw() +
            theme(panel.border = element_blank(),
                  legend.position="none",
                  axis.title.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text = element_text(size = 12),
                  axis.title = element_text(size = 15))

cowplot::save_plot("figs/fig_trends_tot.pdf", fig_gbox, base_width = 12, base_height = 16)



afr_trend <- subset(trends, type="total")
head(afr_trend)


ggplot(subset(trends, type=="total" & iso3=="GNQ"), aes(year, 100 * pop_pix)) +
            geom_line() +
geom_line(data=subset(trends, type=="rural" & iso3=="GNQ"), aes(year, 100 * pix_electricity), col="red") 


ggplot(subset(trends, type=="total" & iso3=="GNQ"), aes(year, 100 * pix_electricity/pix_total)) +
            geom_line() +
  



tt <- "total"
iso_names <- iso_names[!is.na(iso_names)]
fig_gbox <- ggplot(subset(trends, type==tt), aes(year, 100 * pix_electricity / pix_total)) +
            geom_line(aes(col = iso3), alpha = .7) +
            #geom_point(aes(year, 100 * pop_electricity/pop_total, col = as.factor(iso3)), size = 2, alpha = .5) +
            #geom_text(data = subset(trends, type==tt & year==2013), aes(x = right_x, y = 100 * right_y, label = iso3, col = as.factor(iso3)), family = "mono", hjust = 0, size = 5) +
            #geom_text(data = subset(trends, type==tt & year==2000), aes(x = left_x, y = 100 * left_y, label = iso3, col = as.factor(iso3)), family = "mono", hjust = 0, size = 5) +
            expand_limits(x = 1997:2016) +
            ylab("Electricity access (%)") +
            scale_color_manual(values = paste(isocol$col3)) + 
            theme_bw() +
            ylim(0, 10) +
            theme(panel.border = element_blank(),
                  legend.position="none",
                  axis.title.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text = element_text(size = 12),
                  axis.title = element_text(size = 15))
fig_gbox
