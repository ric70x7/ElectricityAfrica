# Fig: Data per country/year/survey
# -------------------------------------

require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("plyr")
library(ggthemes)
library(viridis)
library(cowplot)
rm(list = ls())

obfuscated_only <- TRUE

load("code_output/merged_data2.RData")
df <- subset(df, iso3 != "COM") #outiside mainland U Madagazcar

if (obfuscated_only) {
  df <- subset(df, obfuscated)
}

# Load and fortify shapefile
afri_main <- readOGR(dsn = "data/Africa_main_country", layer="Africa_main_country")
afri_main@data$id <- rownames(afri_main@data)
afri_main.points <- fortify(afri_main, region = "id", avoidGEOS = FALSE)
afri_main.df <- join(afri_main.points, afri_main@data, by = "id")

# Add survey data in shp dataframe
adf1 <- afri_main.df
adf1$years <- NA
sum1 <- 0
for(iso3i in unique(afri_main.df$ISO3)){
  ix <- adf1$ISO3 == iso3i
  s1 <- nrow(unique(subset(df, year <= 2013 & iso3 == iso3i)[, c("year", "iso3")]))
  adf1$years[ix] <- s1
  sum1 <- sum1 + s1
}

dfyears <- data.frame(year=2000:2013)
dfyears$num <- NA
sum1 <- 0
for (yi in 2000:2013) {
  ix <- dfyears$year == yi
  s1 <- length(unique(subset(df, year == yi)$iso3))
  if(length(s1) > 0) {
    dfyears$num[ix] <- s1
  }
  sum1 <- sum1 + s1
}

# DHS country-level data points
font_size = 15

if (obfuscated_only) {
  dhs_map <- ggplot(afri_main.df, aes(long, lat)) +
             geom_polygon(aes(group = group), colour = "grey", fill = "white") +
             geom_polygon(data = subset(adf1, years > 0), aes(long, lat, group = group,  fill = factor(years)),  colour = "grey") +
             #geom_polygon(data = subset(afri_main.df, ISO3 == "ZMB"), aes(long, lat), colour="darkred", fill="white", alpha=0) +
             #geom_polygon(data = subset(afri_main.df, ISO3 == "MWI"), aes(long, lat), colour="darkred", fill="white", alpha=0) +
             viridis::scale_fill_viridis(option = "E", discrete = TRUE) +
             guides(fill=guide_legend(title="Number\n of surveys")) +
             theme_map(base_size = font_size) +
             coord_equal()
  
} else {
  dhs_map <- ggplot(afri_main.df, aes(long, lat)) +
             geom_polygon(aes(group = group), colour = "grey", fill = "white") +
             geom_polygon(data = subset(adf1, years > 0), aes(long, lat, group = group,  fill = factor(years)),  colour = "grey") +
             geom_polygon(data = subset(afri_main.df, ISO3 == "ZMB"), aes(long, lat), colour="darkred", fill="white", alpha=0) +
             geom_polygon(data = subset(afri_main.df, ISO3 == "MWI"), aes(long, lat), colour="darkred", fill="white", alpha=0) +
             viridis::scale_fill_viridis(option = "E", discrete = TRUE) +
             guides(fill=guide_legend(title="Number\n of surveys")) +
             theme_map(base_size = font_size) +
             coord_equal()
}
  
# Annual country-level data points
timeline <- ggplot(dfyears, aes(factor(year), num)) +
            geom_bar(stat="identity", fill = viridis::viridis_pal(option="E")(2)[1], alpha=.8) +
            ylab("Number of surveys") +
            xlab("Year") +
            ylim(0,8) +
            theme_hc(base_size = font_size) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))

graphics.off()

fig_data <- ggdraw(xlim = c(0,12), ylim = c(0,4)) +
            draw_plot(dhs_map, x = 0, y = 0, width = 4, height = 4) +
            draw_plot(timeline,x = 4, y = 0, width = 8, height = 4) +
            draw_plot_label(c("A", "B"), c(0, 4), c(4, 4), size = 18, colour = "grey")
#save_plot("figs/01_fig_data.pdf", fig_data, base_width = 12, base_height = 4)
#save_plot("figs/01_fig_data_obfuscated_only.pdf", fig_data, base_width = 12, base_height = 4)
save_plot("figs/01_fig_data_obfuscated_only.eps", fig_data, base_width = 12, base_height = 4,
          device = cairo_ps)
