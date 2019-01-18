# Figures: Data per country/year/survey
#
# Edited March 8 2017


require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("plyr")
library(ggthemes)
library(viridis)
library(cowplot)
rm(list = ls())

# Load and fortify shapefile
afri_main <- readOGR(dsn = "data/Africa_main_country", layer="Africa_main_country")
afri_main@data$id <- rownames(afri_main@data)
afri_main.points <- fortify(afri_main, region = "id", avoidGEOS = FALSE)
afri_main.df <- join(afri_main.points, afri_main@data, by = "id")

# Identify survey in  country stats
electricity_access <- read.csv("data/DHS_electricity_access_by_country.csv")
electricity_access <- subset(electricity_access, year >= 2000)

# Islands "COM" and "STP" not in study
electricity_access <- subset(electricity_access, iso3 != "COM" & iso3 != "STP")

ixdhs <- grep("DHS", electricity_access$survey)
ixmis <- grep("MIS", electricity_access$survey)
ixais <- grep("AIS", electricity_access$survey)

electricity_access$sv <- NA
electricity_access$sv[ixdhs] <- "dhs"
electricity_access$sv[c(ixmis, ixais)] <- "mis_ais"


# WDI data
wdi <- read.csv("data/World_Development_Indicators/Data2.csv")


# New data frame
afri_list <- paste(unique(afri_main.df$ISO3))
all_surveys <- data.frame(iso3 = rep(afri_list, 16),
                          year = sort(rep(2000:2015, length(afri_list))))
all_surveys$survey3 <- NA
all_surveys$survey1 <- NA
all_surveys$survey <- NA
for(iso3i in afri_list){
  wdidf <- subset(wdi, Country.Code == iso3i)
  if(nrow(wdidf) > 0){
    all_surveys$survey3[all_surveys$iso3 == iso3i & all_surveys$year == 2000] <- "wdi"
    all_surveys$survey3[all_surveys$iso3 == iso3i & all_surveys$year == 2010] <- "wdi"
    all_surveys$survey3[all_surveys$iso3 == iso3i & all_surveys$year == 2012] <- "wdi"
  }
  
  svs <- subset(electricity_access, iso3 == iso3i)
  if(nrow(svs)){
    for(y in svs$year){
      if(length(svs$sv[svs$year == y])>1){
        print(c(iso3i,y))
      }
      all_surveys$survey1[all_surveys$iso3 == iso3i & all_surveys$year == y] <- svs$sv[svs$year == y]
    }
  }
  
}

for(i in 1:nrow(all_surveys)){
  if(!is.na(all_surveys$survey1[i])){
    all_surveys$survey[i] <- all_surveys$survey1[i]
  }else{
    if(!is.na(all_surveys$survey3[i])){
      all_surveys$survey[i] <- all_surveys$survey3[i]
    }
  }
}


# suverys per  year
year_surveys <- all_surveys[!is.na(all_surveys$survey), c("iso3", "year", "survey")]
year_surveys$survey[year_surveys$survey == "wdi"] <- "WDI"
year_surveys$survey[year_surveys$survey == "dhs"] <- "DHS"
year_surveys$survey[year_surveys$survey == "mis_ais"] <- "MIS/AIS"


# Sample points
load("code_output/train_and_test_data.RData")
df.train <- df.train[,c("lon", "lat")]
df.test1 <- df.test1[,c("lon", "lat")]
df.test2 <- df.test2[,c("lon", "lat")]
df.train$survey <- "DHS"
df.test1$survey <- "DHS"
df.test2$survey <- "MIS"
df.samples <- rbind(df.train, df.test1, df.test2)


# Add survey data in shp dataframe
adf1 <- afri_main.df
adf2 <- afri_main.df
adf3 <- afri_main.df
adf1$years <- NA
adf2$years <- NA
adf3$years <- NA
sum1 <- 0
sum2 <- 0
sum3 <- 0
for(iso3i in unique(afri_main.df$ISO3)){
  ix <- adf1$ISO3 == iso3i
  s1 <- nrow(subset(all_surveys, survey == "dhs" & iso3 == iso3i))
  s2 <- nrow(subset(all_surveys, survey == "mis_ais" & iso3 == iso3i))
  s3 <- nrow(subset(all_surveys, survey == "wdi" & iso3 == iso3i))
  adf1$years[ix] <- s1
  adf2$years[ix] <- s2
  adf3$years[ix] <- s3
  sum1 <- sum1 + s1
  sum2 <- sum2 + s2
  sum3 <- sum3 + s3
}


# DHS country-level data points
font_size = 15
dhs_map <- ggplot(afri_main.df, aes(long, lat)) +
           geom_polygon(aes(group = group), colour = "grey", fill = "white") +
           geom_polygon(data = subset(adf1, years > 0), aes(long, lat, group = group,  fill = factor(years)),  colour = "grey") +
           scale_fill_brewer(palette = "Blues", guide = guide_legend(title = "DHS\nNo. years")) +
           theme_map(base_size = font_size) +
           coord_equal()

# MIS/AIS country-level data points
mis_map <- ggplot(afri_main.df) + aes(long, lat, group = group) +
           geom_polygon(colour = "grey", fill = "white") +
           geom_polygon(data = subset(adf2, years > 0), aes(long, lat, group = group,  fill = factor(years)),  colour = "grey") +
           scale_fill_brewer(palette = "Reds", guide = guide_legend(title = "MIS/AIS\nNo. years")) +
           theme_map(base_size = font_size) +
           coord_equal()
          
# WDI country-level data points
wdi_map <- ggplot(afri_main.df) + aes(long, lat, group = group) +
           geom_polygon(colour = "grey", fill = "white") +
           geom_polygon(data = subset(adf3, years > 0), aes(long, lat, group = group,  fill = factor(years)),  colour = "grey") +
           scale_fill_brewer(palette = "Greens", guide = guide_legend(title = "WDI\nNo. years")) +
           theme_map(base_size = font_size) +
           coord_equal()

myblue <- #56B4E9" #"#6495ED"
mygreen <- "#009E73"#"#86C67C"
myred <- "#D55E00"#"#EE6A50"



# Sample points
afri_aux <- afri_main
afri_aux$survey[afri_aux$survey == "DHS"] <- "Obfuscated" 
afri_aux$survey[afri_aux$survey == "MIS"] <- "Not obfuscated" 
sample_vec <- runif(nrow(df.samples), 0, 1)  < .1
pts_map <- ggplot(afri_main.df, aes(long, lat)) +
           geom_polygon(aes(group = group), colour = "grey", fill = "white") +
           geom_point(data = subset(df.samples, sample_vec), aes(lon, lat, color = survey), alpha = .15, size = 1) +
           #scale_color_manual(values = c("#6495ED", "#EE6A50"), guide = guide_legend(title = "Survey\nlocations")) +
           scale_color_manual(values = c("#56B4E9", "#D55E00"), guide = guide_legend(title = "Survey\nlocations")) +
           theme_map(base_size = font_size) +
           coord_equal()

# Annual country-level data points
timeline <- ggplot(year_surveys, aes(factor(year))) + geom_bar(aes(fill = survey)) +
            #scale_fill_manual(values = c("#6495ED", "#EE6A50", "#86C67C"), guide = guide_legend(title = "Source")) +
            #scale_fill_manual(values = c("#6495ED", "#EE6A50", "#86C67C"), guide = guide_legend(title = "Source")) +
            scale_fill_manual(values = c("#56B4E9", "#D55E00", "#009E73"), guide = guide_legend(title = "Source")) +
            ylab("No. countries") +
            theme_hc(base_size = font_size) +
            theme(axis.title.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text.x = element_text(angle = 90, hjust = 1))

graphics.off()

fig_data <- ggdraw(xlim = c(0,12), ylim = c(0,8)) +
            draw_plot(dhs_map, x = 0, y = 4, width = 4, height = 4) +
            draw_plot(mis_map, x = 4, y = 4, width = 4, height = 4) +
            draw_plot(wdi_map, x = 8, y = 4, width = 4, height = 4) +
            draw_plot(timeline,x = 0, y = 0, width = 8, height = 4) +
            draw_plot(pts_map, x = 8, y = 0, width = 4, height = 4) +
            draw_plot_label(c("A", "B", "C", "D", "E"), c(0, 4, 8, 0, 8), c(8, 8, 8, 4, 4), size = 18, color = "grey")
save_plot("figs/fig_data.pdf", fig_data, base_width = 12, base_height = 8)
