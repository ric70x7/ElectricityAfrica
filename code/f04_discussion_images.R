# Fig: Discussion images
# ----------------------

library(ggplot2)
library(ggthemes)

graphics.off()
rm(list = ls())

load("code_output/merged_data2.RData")
afri_main <- rgdal::readOGR("data/Africa_main_country/Africa_main_country.shp")
afri_main$ISO3
num_countries <- length(afri_main$ISO3)

# Define trends per country
if FALSE { # Run only if an update is needed
  for (i in 1:14) {

    file_fimp <- paste0("data/lulc_resampled/7class", i + 1999, ".tif")
    file_elec <- paste0("code_output/Electricity/electricity_", i + 1999, ".tif")
    file_popl <- paste0("code_output/GPW4_reprojected/pop_", i + 1999, ".tif")
    fimp <- raster::raster(file_fimp)
    elec <- raster::raster(file_elec)
    popl <- raster::raster(file_popl)

    for(iso3j in afri_main$ISO3){  #iso3j <- "MAR"

      shp_boundary <- afri_main[afri_main$ISO3 == iso3j,]
      iso3j_fimp <- mask(fimp, shp_boundary)
      iso3j_elec <- mask(elec, shp_boundary)
      iso3j_popl <- mask(popl, shp_boundary)

      mask_urban <- iso3j_fimp[] == 1
      mask_rural <- iso3j_fimp[] > 1
      mask_electricity <- iso3j_elec[] > .5

      row_mask <- trends$iso3 == iso3j & trends$year == i + 1999

      trends$pix_total[row_mask & trends$type == "urban"] <- sum(mask_urban, na.rm = TRUE)
      trends$pix_total[row_mask & trends$type == "rural"] <- sum(mask_rural, na.rm = TRUE)
      trends$pix_total[row_mask & trends$type == "total"] <- trends$pix_total[row_mask & trends$type == "urban"] +
                                                             trends$pix_total[row_mask & trends$type == "rural"]

      trends$pix_electricity[row_mask & trends$type == "urban"] <- sum(mask_urban & mask_electricity, na.rm = TRUE)
      trends$pix_electricity[row_mask & trends$type == "rural"] <- sum(mask_rural & mask_electricity, na.rm = TRUE)
      trends$pix_electricity[row_mask & trends$type == "total"] <- trends$pix_electricity[row_mask & trends$type == "urban"] +
                                                                   trends$pix_electricity[row_mask & trends$type == "rural"]

      trends$pop_total[row_mask & trends$type == "urban"] <- sum(iso3j_popl[mask_urban], na.rm =TRUE)
      trends$pop_total[row_mask & trends$type == "rural"] <- sum(iso3j_popl[mask_rural], na.rm =TRUE)
      trends$pop_total[row_mask & trends$type == "total"] <- sum(iso3j_popl[mask_urban], na.rm =TRUE) +
                                                              sum(iso3j_popl[mask_rural], na.rm =TRUE)

      trends$pop_electricity[row_mask & trends$type == "urban"] <- sum(iso3j_popl[mask_urban] * iso3j_elec[mask_urban], na.rm =TRUE)
      trends$pop_electricity[row_mask & trends$type == "rural"] <- sum(iso3j_popl[mask_rural] * iso3j_elec[mask_rural], na.rm =TRUE)
      trends$pop_electricity[row_mask & trends$type == "total"] <- trends$pop_electricity[row_mask & trends$type == "urban"] +
                                                                   trends$pop_electricity[row_mask & trends$type == "rural"]

      trends$pop_pix[row_mask & trends$type == "urban"] <- sum(iso3j_popl[mask_urban & mask_electricity], na.rm =TRUE)
      trends$pop_pix[row_mask & trends$type == "rural"] <- sum(iso3j_popl[mask_rural & mask_electricity], na.rm =TRUE)
      trends$pop_pix[row_mask & trends$type == "total"] <- trends$pop_pix[row_mask & trends$type == "urban"] +
                                                                   trends$pop_pix[row_mask & trends$type == "rural"]


      print(subset(trends, year==1999+i & iso3 == iso3j))
    }
  }
  #save(trends, file = "code_output/e_trends.RData")
}

# Process data per country: "GHA", "MAR", "LBR"
gg_mar <- data.frame()
gg_lbr <- data.frame()
gg_gha <- data.frame()
for (i in 1:14) {
  for (iso3j in c("GHA", "MAR", "LBR")) {
    file_elec <- paste0("code_output/Electricity/electricity_", i + 1999, ".tif")
    file_popl <- paste0("code_output/GPW4_reprojected/pop_", i + 1999, ".tif")
    file_ntlg <- paste0("data/ntl/GP2_Africa_5k/GP2_Africa_", i + 1999, ".tif")
    file_lulc <- paste0("data/lulc_resampled/impervious_dist_", i + 1999, ".tif")
    elec <- raster::raster(file_elec)
    popl <- raster::raster(file_popl)
    ntlg <- raster::raster(file_ntlg)
    lulc <- raster::raster(file_lulc)
    shp_boundary <- afri_main[afri_main$ISO3 == iso3j,]
    iso3j_elec <- raster::crop(elec, shp_boundary)
    iso3j_popl <- raster::crop(popl, shp_boundary)
    iso3j_ntlg <- raster::crop(ntlg, shp_boundary)
    iso3j_lulc <- raster::crop(lulc, shp_boundary)
    iso3j_elec <- raster::mask(iso3j_elec, shp_boundary)
    iso3j_popl <- raster::mask(iso3j_popl, shp_boundary)
    iso3j_ntlg <- raster::mask(iso3j_ntlg, shp_boundary)
    iso3j_lulc <- raster::mask(iso3j_lulc, shp_boundary)

    mask_electricity <- !is.na(iso3j_elec[])
    xy <- raster::xyFromCell(iso3j_elec, (1:length(mask_electricity))[mask_electricity])
    fimp <- iso3j_lulc[mask_electricity]
    fimp[fimp !=1] <- 0
    new_df <- data.frame(lon=xy[,1], lat=xy[,2],
                         elec=iso3j_elec[mask_electricity],
                         popl=iso3j_popl[mask_electricity],
                         ntlg=iso3j_ntlg[mask_electricity],
                         lulc=iso3j_lulc[mask_electricity],
                         fimp=fimp, year =1999+i)

    if (iso3j == "GHA") {
      gg_gha <- rbind(gg_gha, new_df)
    } else if (iso3j == "MAR") {
      gg_mar <- rbind(gg_mar, new_df)
    } else if (iso3j == "LBR") {
      gg_lbr <- rbind(gg_lbr, new_df)
    }
  }
}


# MAR
# ---
yi <- 2004
gg_df <- gg_mar
font_size <- 15

plt1 <- ggplot(data=subset(gg_df, year == yi), aes(lon, lat)) + geom_raster(fill="black") +
  geom_raster(data=subset(gg_df, year == yi & gg_df$ntlg > 0), aes(fill=log(ntlg)))  +
  viridis::scale_fill_viridis(option = "magma", discrete = FALSE, guide=FALSE,
                              breaks = c(0, 1, 2, 3, 4),
                              labels=c('0 - 2', '2 - 5', '5 - 12', '12 - 27', '27 - 63')) +
  guides(fill=guide_legend(title="Dynamic\nrange")) +
  theme_map(base_size = font_size) +
  coord_fixed() +
  theme_void()

plt2 <- ggplot(data=subset(gg_df, year == yi), aes(lon, lat)) +
  geom_raster(data=subset(gg_df, year == yi ), aes(fill=lulc))  +
  viridis::scale_fill_viridis(option = "viridis", discrete = FALSE, limits=c(0,1), guide=FALSE,
                              breaks = c(.2, .4, .6, .8, 1),
                              labels=c('0.0 - 0.2', '0.2 - 0.4', '0.4 - 0.6', '0.6 - 0.8', '0.8 - 1.0')) +
  guides(fill=guide_legend(title="Proximity")) +
  theme_map(base_size = font_size) +
  coord_fixed() +
  theme_void()

plt3 <- ggplot(data=subset(gg_df, year == yi), aes(lon, lat)) +
  geom_raster(data=subset(gg_df, year == yi ), aes(fill=elec))  +
  viridis::scale_fill_viridis(option = "E", discrete = FALSE, limits=c(0,1), guide=FALSE,
                              breaks = c(.2, .4, .6, .8, 1),
                              labels=c('0.0 - 0.2', '0.2 - 0.4', '0.4 - 0.6', '0.6 - 0.8', '0.8 - 1.0')) +
  guides(fill=guide_legend(title="Probability")) +
  theme_map(base_size = font_size) +
  coord_fixed() +
  theme_void()

plt4 <- ggplot(data=subset(gg_df, year == yi), aes(lon, lat)) + geom_raster(fill="brown", alpha=.2) +
  geom_point(data=subset(df, iso3=="MAR" & lat > 27.2), aes(color=has_electricity/total)) +
  viridis::scale_color_viridis(option = "E", discrete = FALSE, limits=c(0,1), guide=FALSE,
                              breaks = c(.2, .4, .6, .8, 1),
                              labels=c('0.0 - 0.2', '0.2 - 0.4', '0.4 - 0.6', '0.6 - 0.8', '0.8 - 1.0')) +
  guides(color=guide_legend(title="Fraction")) +
  theme_map(base_size = font_size) +
  coord_fixed() +
  theme_void()

#fig_data <- cowplot::ggdraw(xlim = c(0, 16), ylim = c(0, 4)) +
#            cowplot::draw_plot(plt4, x = 0, y = 0, width = 4, height = 4) +
#            cowplot::draw_plot(plt1, x = 4, y = 0, width = 4, height = 4) +
#            cowplot::draw_plot(plt2, x = 8, y = 0, width = 4, height = 4) +
#            cowplot::draw_plot(plt3, x = 12, y = 0, width = 4, height = 4) +
#
#            cowplot::draw_plot_label(LETTERS[1:4], c(0, 4, 8, 12), c(4, 4, 4, 4),
#                                     size=24, colour = "grey")

fig_data <- cowplot::ggdraw(xlim = c(0, 12), ylim = c(0, 8)) +
            cowplot::draw_plot(plt4, x = 0, y = 4, width = 6, height = 4) +
            cowplot::draw_plot(plt1, x = 6, y = 4, width = 6, height = 4) +
            cowplot::draw_plot(plt2, x = 0, y = 0, width = 6, height = 4) +
            cowplot::draw_plot(plt3, x = 6, y = 0, width = 6, height = 4) +

            cowplot::draw_plot_label(LETTERS[1:4], c(0, 6, 0, 6), c(8, 8, 4, 4),
                                     size=24, colour = "grey")


#cowplot::save_plot("figs/13_fig_mar.pdf", fig_data, base_width = 16, base_height = 4)
cowplot::save_plot("figs/13_fig_mar.eps", fig_data, base_width = 12, base_height = 8, device=cairo_ps)


# GHA
# ---
years <- c(2000, 2005, 2010, 2013)
elec_list <- list()
ntlg_list <- list()
lulc_list <- list()
for (i in 1:4) {
  yi <- years[i]
  gg_df <- gg_gha
  gg_df$elec[gg_df$elec < .25] <- 0
  gg_df$elec[gg_df$elec >= .75] <- 2
  gg_df$elec[gg_df$elec >= .25 & gg_df$elec < .75] <- 1
  gg_df$popl[gg_df$popl > 10000] <- 10000

  plt1 <- ggplot(data=subset(gg_df, year == yi), aes(lon, lat)) +
    geom_raster(data=subset(gg_df, year == yi ), aes(fill=popl))  +
    viridis::scale_fill_viridis(option = "magma", discrete = FALSE, limits=c(0,10000), guide=FALSE) +
    coord_fixed() +
    theme_void()

  plt2 <- ggplot(data=subset(gg_df, year == yi), aes(lon, lat)) +
    geom_raster(data=subset(gg_df, year == yi ), aes(fill=lulc))  +
    viridis::scale_fill_viridis(option = "viridis", discrete = FALSE, limits=c(0,1), guide=FALSE) +
    coord_fixed() +
    theme_void()

  plt3 <- ggplot(data=subset(gg_df, year == yi), aes(lon, lat)) +
    geom_raster(data=subset(gg_df, year == yi ), aes(fill=factor(elec)))  +
    viridis::scale_fill_viridis(option = "E", discrete = TRUE, guide=FALSE) +
    coord_fixed() +
    theme_void()

  ntlg_list[[i]] <- plt1
  lulc_list[[i]] <- plt2
  elec_list[[i]] <- plt3

}


font_size <- 9
plt_legend1 <- ggplot() + theme_void() + coord_equal() + xlim(0, 4) + ylim(0, 6.5) +
  geom_text() + annotate("text", label="Population\n(x 1000)", x=2, y=6, size=font_size+1)
intervals = c("0 - 2", "2 - 4", "4 - 6", "6 - 8", "> 8")
for (i in 1:5) {
  square <- data.frame(x=c(0, 1, 1, 0, 0), y=c(i-1, i-1, i, i, i-1))
  plt_legend1 <- plt_legend1 + geom_boxplot(data=square, aes(x=x, y=y),
                                          fill = viridis::viridis_pal(option="magma")(5)[i]) + 
    geom_text() + annotate("text", label=intervals[i], x = 2.5, y=i - .5, size = font_size)
}

plt_legend2 <- ggplot() + theme_void() + coord_equal() + xlim(0, 4) + ylim(0, 6.5) +
  geom_text() + annotate("text", label="Probability", x=2, y=6, size=font_size+1)
intervals = c("0.0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1.0")
for (i in 1:5) {
  square <- data.frame(x=c(0, 1, 1, 0, 0), y=c(i-1, i-1, i, i, i-1))
  plt_legend2 <- plt_legend2 + geom_boxplot(data=square, aes(x=x, y=y),
                                          fill = viridis::viridis_pal(option="E")(5)[i]) + 
    geom_text() + annotate("text", label=intervals[i], x = 2.8, y=i - .5, size = font_size)
}




psc=4
fig_data <- cowplot::ggdraw(xlim = c(0, 20), ylim = c(0, 8)) +
            cowplot::draw_plot(ntlg_list[[1]], x = 0, y = 4, width = 4, height = 4) +
            cowplot::draw_plot(ntlg_list[[2]], x = 4, y = 4, width = 4, height = 4) +
            cowplot::draw_plot(ntlg_list[[3]], x = 8, y = 4, width = 4, height = 4) +
            cowplot::draw_plot(ntlg_list[[4]], x = 12, y = 4, width = 4, height = 4) +

            cowplot::draw_plot(elec_list[[1]], x = 0, y = 0, width = 4, height = 4) +
            cowplot::draw_plot(elec_list[[2]], x = 4, y = 0, width = 4, height = 4) +
            cowplot::draw_plot(elec_list[[3]], x = 8, y = 0, width = 4, height = 4) +
            cowplot::draw_plot(elec_list[[4]], x = 12, y = 0, width = 4, height = 4) +
  
            cowplot::draw_plot(plt_legend1, x = 16, y = 4, width = 4, height = 4) +
            cowplot::draw_plot(plt_legend2, x = 16, y = 0, width = 4, height = 4) +

            cowplot::draw_plot_label(rep(c("00", "05", "10", "13"), 2),
                                     rep(0:3, 2)*psc,
                                     sort(rep(2:1, 4), decreasing = TRUE) * psc,
                                     size=24, colour = "grey")

#cowplot::save_plot("figs/13_fig_gha.pdf", fig_data, base_width = 16, base_height = 8)
cowplot::save_plot("figs/13_fig_gha.eps", fig_data, base_width = 20, base_height = 8, device=cairo_ps)



# LBR
# ---
years <- c(2000, 2013)
elec_list <- list()
ntlg_list <- list()
lulc_list <- list()
for (i in 1:2) {
  yi <- years[i]
  gg_df <- gg_lbr
  gg_df$elec[gg_df$elec > .5] <- .5
  gg_df$elec <- log(gg_df$elec)

  plt3 <- ggplot(data=subset(gg_df, year == yi), aes(lon, lat)) +
    geom_raster(data=subset(gg_df, year == yi ), aes(fill=elec))  +
    viridis::scale_fill_viridis(option = "E", discrete = FALSE, limits=c(-6, log(.5)), guide=FALSE) +
    coord_fixed() +
    theme_void()
  elec_list[[i]] <- plt3
}


# Electricity access trend in LBR
load("code_output/e_trends.RData")
trends_lbr <- subset(trends, type == "total" & iso3 == "LBR")

plt_trend <- ggplot(subset(trends, type == "total" & iso3 == "LBR"),
                    aes(year, 100 * pop_pix/pop_total)) +
  geom_line(col=viridis::viridis_pal(option="E")(2)[1], alpha=.7) +
  geom_point(col=viridis::viridis_pal(option="E")(2)[1]) +
  scale_x_continuous("Year") +
  scale_y_continuous("Population in areas with\nlikely access to electricity\n(%)") +
  theme_classic() +
  theme(panel.border = element_blank(),
                legend.title = element_text(size=16),
                legend.background = element_rect(fill = "transparent"),
                legend.text=element_text(size = 16),
                axis.text = element_text(size = 16),
                axis.title = element_text(size = 16))

font_size <- 6
plt_legend3 <- ggplot() + theme_void() + coord_equal() + xlim(0, 4) + ylim(0, 6.5) +
  geom_text() + annotate("text", label="Probability", x=2, y=6, size=font_size+2)
intervals = c("0.000 - 0.007", "0.007 - 0.020", "0.020 - 0.060", "0.060 - 0.173", "0.173 - 0.500")
for (i in 1:5) {
  square <- data.frame(x=c(0, 1, 1, 0, 0), y=c(i-1, i-1, i, i, i-1))
  plt_legend3 <- plt_legend3 + geom_boxplot(data=square, aes(x=x, y=y),
                                          fill = viridis::viridis_pal(option="E")(5)[i]) + 
    geom_text() + annotate("text", label=intervals[i], x = 3, y=i - .5, size = font_size)
}



psc=4
fig_data <- cowplot::ggdraw(xlim = c(0, 20), ylim = c(0, 4)) +
            cowplot::draw_plot(plt_legend3, x = 0, y = 0, width = 4, height = 4) +
            cowplot::draw_plot(elec_list[[1]], x = 4, y = 0, width = 4, height = 4) +
            cowplot::draw_plot(elec_list[[2]], x = 8, y = 0, width = 4, height = 4) +
            cowplot::draw_plot(plt_trend, x = 12, y = 0, width = 8, height = 4) +

            cowplot::draw_plot_label(LETTERS[1:3], c(4, 8, 12), rep(4, 3),
                                     size=24, colour = "grey")

#cowplot::save_plot("figs/13_fig_lbr.pdf", fig_data, base_width = 16, base_height = 4)
cowplot::save_plot("figs/13_fig_lbr.eps", fig_data, base_width = 20, base_height = 4, device=cairo_ps)


# Countries timeline
# ------------------
load("code_output/e_trends.RData")
afr_total <- subset(trends, type == "total")
afr_total <- aggregate(afr_total[, c("pop_pix", "pop_total")], by = list(afr_total$year), FUN=sum)
names(afr_total)[1] <- "year"
iso_sample <- c("EGY", "MAR", "GHA", "AGO", "TCD", "LBR", "MLI", "TUN", "CIV", "TGO")
afr_sample <- subset(trends, type=="total" & iso3 %in% iso_sample)
afr_sample$name <- NA
afr_sample$name[afr_sample$iso3 == "EGY"] <- "Egypt"
afr_sample$name[afr_sample$iso3 == "MAR"] <- "Morocco"
afr_sample$name[afr_sample$iso3 == "GHA"] <- "Ghana"
afr_sample$name[afr_sample$iso3 == "AGO"] <- "Angola"
afr_sample$name[afr_sample$iso3 == "TCD"] <- "Chad"
afr_sample$name[afr_sample$iso3 == "LBR"] <- "Liberia"
afr_sample$name[afr_sample$iso3 == "MLI"] <- "Mali"
afr_sample$name[afr_sample$iso3 == "TUN"] <- "Tunisia"
afr_sample$name[afr_sample$iso3 == "CIV"] <- "Cote d'Ivoire"
afr_sample$name[afr_sample$iso3 == "TGO"] <- "Togo"

plt_fig <- ggplot(afr_total, aes(year, 100*pop_pix/pop_total)) +
          geom_line(color="darkred", size=1.2, alpha=.3) +
          geom_point(color="darkred", size=2) +
          geom_line(data=afr_sample, aes(col=iso3), size=1, alpha=.3) +
          geom_point(data=afr_sample, aes(col=iso3), size=2, alpha=.7) +
          geom_text(data = subset(afr_sample, year==2013),
                    aes(x = 2013.2, y = 100*pop_pix/pop_total, label = name, col = as.factor(iso3)),
                    family = "mono", hjust = 0, size = 7) +
          geom_text(data = subset(afr_total, year==2013),
                    aes(x = 2013.2, y = 100*pop_pix/pop_total + 1, label = "Africa"),
                    family = "mono", hjust = 0, size = 7, color="darkred") +
            expand_limits(x = 2000:2016) +
            ylab("Population in areas with\nlikely access to electricity (%)") +
            theme_bw() +
            theme(panel.border = element_blank(),
                  legend.position="none",
                  axis.title.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text = element_text(size = 20),
                  axis.title = element_text(size = 18))

#cowplot::save_plot("figs/11_fig_trends_tot.pdf", plt_fig, base_width = 12, base_height = 16)
cowplot::save_plot("figs/11_fig_trends_tot.eps", plt_fig, base_width = 12, base_height = 16, device=cairo_ps)
