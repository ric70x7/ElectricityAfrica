
library(rstan)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(latex2exp)

rm(list=ls())

load("code_output/vgpm_samples.RData")
load("code_output/country_stats.RData")
solkm <- read.csv("data/sol_km_w_iso3.csv")

myblue <-"#6495ED"
mygreen <- "#86C67C"
myred <- "#EE6A50"
font_size <- 15



## Drivers data
# Create data frame that will contain the results
annual_data <- merge(raw_country_stats[,c("iso3", "year", "r")],
                     country_stats[,c("iso3", "year", "num_households", "num_lithouseholds")],
                     by = c("year", "iso3"))
colnames(annual_data)[3] <- "reported_r"
# Add sum of lights per km^2
annual_data$sol_km2 <- NA
for(iso3i in unique(annual_data$iso3)){
  ixsl <- solkm$iso3 == iso3i
  ixad <- annual_data$iso3 == iso3i
  annual_data$sol_km2[ixad] <- as.numeric(solkm[ixsl,2:17])
}
# Remove NTL related data of 2014 and 2015
# NOTE: NTL values of these years is not available, 2013 was used
annual_data[annual_data$year %in% c(2014,2015), c("sol_km2","num_lithouseholds")] <- NA
# Define objects that will be passed to rstan
X <- 2000:2015
iso3list <- paste(unique(country_stats$iso3))
num_countries <- length(iso3list)
num_years <- length(X)
num_data <- matrix(rep(NA, num_countries), dimnames = list(iso3list))
ix_data <- matrix(-1, nrow = num_countries, ncol = num_years, dimnames = list(iso3list))
N <- matrix(NA, nrow = num_countries, ncol = num_years, dimnames = list(iso3list))
Y <- matrix(NA, nrow = num_countries, ncol = num_years, dimnames = list(iso3list))
Z1 <- matrix(NA, nrow = num_countries, ncol = num_years, dimnames = list(iso3list))
Z2 <- matrix(NA, nrow = num_countries, ncol = num_years, dimnames = list(iso3list))
MR_prior <- Y
MY_prior <- Y
MZ1_prior <- Z1
MZ2_prior <- Z2

for(iso3i in iso3list){
  ixad <- annual_data$iso3 == iso3i
  ixmt <- dimnames(Y)[[1]] == iso3i
  df <- annual_data[ixad,]
  # Use data of Morocco on Western Sahara Territory
  if(iso3i == "ESH"){
    df$reported_r <- annual_data$reported_r[annual_data$iso3 == "MAR"]
  }
  # Observed points mask
  mask_obsv <- !is.na(df$reported_r)
  ix_data[ixmt,seq(sum(mask_obsv))] <- seq(num_years)[mask_obsv]
  num_data[ixmt] <- sum(mask_obsv)
  #offset_param <- mean(logit(df$reported_r), na.rm = TRUE)
  offset_param <- mean((df$reported_r), na.rm = TRUE)
  if(sum(mask_obsv)==1){
    #scale_param <- sd(logit(annual_data$reported_r), na.rm = TRUE)
    scale_param <- sd((annual_data$reported_r), na.rm = TRUE)
  }else{
    #scale_param <- sd(logit(df$reported_r), na.rm = TRUE)
    scale_param <- sd((df$reported_r), na.rm = TRUE)
  }
  N[ixmt,] <- df$num_households
  Y[ixmt,] <- (df$reported_r)# - offset_param)/scale_param
  Z1[ixmt,] <- scale(df$sol_km2) * scale_param + offset_param
  Z2[ixmt,] <- scale(df$num_lithouseholds) * scale_param + offset_param
}



graphics.off()

cnums<- c(40,42,44)

# 1
num <- cnums[1]
df <- data.frame(year = 2000:2015,
                 mean = apply(1/(1+exp(-vgpm_samples$GPY[,num,])), 2, mean),
                 cilo = apply(1/(1+exp(-vgpm_samples$GPY[,num,])), 2, quantile, .025),
                 ciup = apply(1/(1+exp(-vgpm_samples$GPY[,num,])), 2, quantile, .975),
                 Y = Y[num,],
                 V = Z1[num,],
                 W = Z2[num,],
                 dummy1 = "Electricity access estimates",
                 dummy2 = "Observed data",
                 dummy3 = "NTL associated variables",
                 dummy4 = "95% credible intervals")
plot1 <- ggplot(df) + 
         geom_line(aes(year, V, color = dummy3), size = 1.3, alpha = .7) +
         geom_line(aes(year, W, color = dummy3), size = 1.3, alpha = .7) +
         geom_errorbar(aes(year, ymin = cilo, ymax = ciup, color = dummy4), size = 1, alpha = .7) +
         geom_line(aes(year, mean, color = dummy1), size = 1.3) +
         ylim(0,1) +
         geom_point(aes(year, Y, col = dummy2), size = 3) +
               theme_hc(base_size = font_size) +
               theme(axis.title.y = element_blank(),
                     axis.title.x = element_blank(),
                     axis.ticks.x = element_blank(),
                     #axis.text.y = element_blank(),
                     legend.position = c(.4,.20)) +
               scale_color_manual(name=iso3list[num], values=c(myred, myblue, "grey", mygreen)) 
          
  

# 2
num <- cnums[2]
df <- data.frame(year = 2000:2015,
                 mean = apply(1/(1+exp(-vgpm_samples$GPY[,num,])), 2, mean),
                 cilo = apply(1/(1+exp(-vgpm_samples$GPY[,num,])), 2, quantile, .025),
                 ciup = apply(1/(1+exp(-vgpm_samples$GPY[,num,])), 2, quantile, .975),
                 Y = Y[num,],
                 V = Z1[num,],
                 W = Z2[num,],
                 dummy1 = "Electricity access estimates",
                 dummy2 = "Observed data",
                 dummy3 = "NTL associated variables",
                 dummy4 = "95% credible intervals")
plot2 <- ggplot(df) + 
         geom_line(aes(year, V, color = dummy3), size = 1.3, alpha = .7) +
         geom_line(aes(year, W, color = dummy3), size = 1.3, alpha = .7) +
         geom_errorbar(aes(year, ymin = cilo, ymax = ciup, color = dummy4), size = 1, alpha = .7) +
         geom_line(aes(year, mean, color = dummy1), size = 1.3) +
         ylim(0,1) +
         geom_point(aes(year, Y, col = dummy2), size = 3) +
               theme_hc(base_size = font_size) +
               theme(axis.title.y = element_blank(),
                     axis.title.x = element_blank(),
                     axis.ticks.x = element_blank(),
                     #axis.text.y = element_blank(),
                     legend.position = c(.4,.85)) +
               scale_color_manual(name=iso3list[num], values=c(myred, myblue, "grey", mygreen)) 
  

# 3
num <- cnums[3]
df <- data.frame(year = 2000:2015,
                 mean = apply(1/(1+exp(-vgpm_samples$GPY[,num,])), 2, mean),
                 cilo = apply(1/(1+exp(-vgpm_samples$GPY[,num,])), 2, quantile, .025),
                 ciup = apply(1/(1+exp(-vgpm_samples$GPY[,num,])), 2, quantile, .975),
                 Y = Y[num,],
                 V = Z1[num,],
                 W = Z2[num,],
                 dummy1 = "Electricity access estimates",
                 dummy2 = "Observed data",
                 dummy3 = "NTL associated variables",
                 dummy4 = "95% credible intervals")
plot3 <- ggplot(df) + 
         geom_line(aes(year, V, color = dummy3), size = 1.3, alpha = .7) +
         geom_line(aes(year, W, color = dummy3), size = 1.3, alpha = .7) +
         geom_errorbar(aes(year, ymin = cilo, ymax = ciup, color = dummy4), size = 1, alpha = .7) +
         geom_line(aes(year, mean, color = dummy1), size = 1.3) +
         ylim(0,1) +
         geom_point(aes(year, Y, col = dummy2), size = 3) +
               theme_hc(base_size = font_size) +
               theme(axis.title.y = element_blank(),
                     axis.title.x = element_blank(),
                     axis.ticks.x = element_blank(),
                     #axis.text.y = element_blank(),
                     legend.position = c(.4,.85)) +
               scale_color_manual(name=iso3list[num], values=c(myred, myblue, "grey", mygreen)) 
  


graphics.off()

fig_gpfit <- ggdraw(xlim = c(0,12), ylim = c(0,4)) +
            draw_plot(plot2, x = 0, y = 0, width = 4, height = 3.8) +
            draw_plot(plot3, x = 4, y = 0, width = 4, height = 3.8) +
            draw_plot(plot1, x = 8, y = 0, width = 4, height = 3.8) +
            draw_plot_label(c("A", "B", "C"), c(0, 4, 8), c(4, 4, 4), size = 18, color = "grey")
save_plot("figs/fig_gpfit.pdf", fig_gpfit, base_width = 12, base_height = 4)



#hist(1/(1+exp(-vgpm_samples$GPY[,num,2])))
#mean(1/(1+exp(-vgpm_samples$GPY[,num,2])))
#quantile(1/(1+exp(-vgpm_samples$GPY[,num,2])), .05)
#quantile(1/(1+exp(-vgpm_samples$GPY[,num,2])), .95)



