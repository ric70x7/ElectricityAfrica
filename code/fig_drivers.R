# Figures: Trends comparison y, v, w
#
# Edited October 13 2016

library(ggplot2)
library(cowplot)
library(ggthemes)


load("code_output/country_stats.RData")
#solkm <- read.csv("data/sol_km_w_iso3.csv")


# Link functions function (transforms offests values 0 and 1)
logit <- function(x){
  y <- ifelse(x == 0 | x == 1, -1 + (.001)^(1-x) + (1-.001)^(x), x)
  return(log(y/(1-y)))
}
inv_logit <- function(x) 1/(1+exp(-x))


# Create data frame that will contain the results
annual_data <- merge(raw_country_stats[,c("iso3", "year", "r")],
                     country_stats[,c("iso3", "year", "num_litpix", "num_litpoppix")],
                     by = c("year", "iso3"))
colnames(annual_data)[3] <- "reported_r"
annual_data$pop_lit <- annual_data$num_litpoppix/annual_data$num_litpix

# Add sum of lights per km^2
#annual_data$sol_km2 <- NA
#for(iso3i in unique(annual_data$iso3)){
#  ixsl <- solkm$iso3 == iso3i
#  ixad <- annual_data$iso3 == iso3i
#  annual_data$sol_km2[ixad] <- as.numeric(solkm[ixsl,2:17])
#}

# Remove NTL related data of 2014 and 2015
# NOTE: NTL values of these years is not available, 2013 was used
annual_data[annual_data$year %in% c(2014,2015), c("num_litpix","pop_lit")] <- NA


# Define objects that will be passed to rstan
X <- 2000:2015
iso3list <- paste(unique(country_stats$iso3))
num_countries <- length(iso3list)
num_years <- length(X)
num_data <- matrix(rep(NA, num_countries), dimnames = list(iso3list))
ix_data <- matrix(-1, nrow = num_countries, ncol = num_years, dimnames = list(iso3list))
#N <- matrix(NA, nrow = num_countries, ncol = num_years, dimnames = list(iso3list))
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
  
  #N[ixmt,] <- df$num_households
  #Y[ixmt,] <- logit(df$reported_r)# - offset_param)/scale_param
  Y[ixmt,] <- (df$reported_r)# - offset_param)/scale_param
  Z1[ixmt,] <- scale(df$num_litpix) * scale_param + offset_param
  #Z2[ixmt,] <- scale(df$ntl_pmp) * scale_param + offset_param
  Z2[ixmt,] <- scale(df$pop_lit) * scale_param + offset_param
}


nans <- rep(0, nrow(Y))
for(i in 1:nrow(Y)){
  nans[i] <- sum(!is.na(Y[i,]))
}

ids <- (1:nrow(Y))[nans > 6]
years <- 2000:2015


font_size = 15
num <- 14
df1 <- data.frame(year = rep(2000:2015, 3), x = c(Y[num,], Z1[num,], Z2[num,]),
                  variable = c(rep("EA", 16), rep("LP", 16), rep("LPWP", 16)))
num <- 36
df2 <- data.frame(year = rep(2000:2015, 3), x = c(Y[num,], Z1[num,], Z2[num,]),
                  variable = c(rep("EA", 16), rep("LP", 16), rep("LPWP", 16)))

num <- 46#46
df3 <- data.frame(year = rep(2000:2015, 3), x = c(Y[num,], Z1[num,], Z2[num,]),
                  variable = c(rep("EA", 16), rep("LP", 16), rep("LPWP", 16)))

num <- 33
df4 <- data.frame(year = rep(2000:2015, 3), x = c(Y[num,], Z1[num,], Z2[num,]),
                  variable = c(rep("EA", 16), rep("LP", 16), rep("LPWP", 16)))


names_used <- iso3list[c(14,36,46, 33)]

plot1 <- ggplot(df1, aes(year, x)) +
         geom_point(aes(color = variable), size = 3) + 
         geom_line(data = subset(df1, variable == "EA" & !is.na(x)), aes(year,x), color = "#EE6A50", size = 1, linetype =2) +
         geom_line(data = subset(df1, variable == "LP" & !is.na(x)), aes(year,x), color = "#6495ED", size = 1) +
         geom_line(data = subset(df1, variable == "LPWP" & !is.na(x)), aes(year,x), color = "#86C67C", size = 1) +
         scale_color_manual(values = c("#EE6A50", "#6495ED", "#86C67C"), guide = guide_legend(title = names_used[1])) +
            theme_hc(base_size = font_size) +
            theme(axis.title.y = element_blank(),
                  axis.title.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.text.x = element_text(angle = 90, hjust = 1))


  
plot2 <- ggplot(df2, aes(year, x)) +
         geom_point(aes(color = variable), size = 3) + 
         geom_line(data = subset(df2, variable == "EA" & !is.na(x)), aes(year,x), color = "#EE6A50", size = 1, linetype =2) +
         geom_line(data = subset(df2, variable == "LP" & !is.na(x)), aes(year,x), color = "#6495ED", size = 1) +
         geom_line(data = subset(df2, variable == "LPWP" & !is.na(x)), aes(year,x), color = "#86C67C", size = 1) +
         scale_color_manual(values = c("#EE6A50", "#6495ED", "#86C67C"), guide = guide_legend(title = names_used[2])) +
            theme_hc(base_size = font_size) +
            theme(axis.title.y = element_blank(),
                  axis.title.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.text.x = element_text(angle = 90, hjust = 1))
  
plot3 <- ggplot(df3, aes(year, x)) +
         geom_point(aes(color = variable), size = 3) + 
         geom_line(data = subset(df3, variable == "EA" & !is.na(x)), aes(year,x), color = "#EE6A50", size = 1, linetype =2) +
         geom_line(data = subset(df3, variable == "LP" & !is.na(x)), aes(year,x), color = "#6495ED", size = 1) +
         geom_line(data = subset(df3, variable == "LPWP" & !is.na(x)), aes(year,x), color = "#86C67C", size = 1) +
         scale_color_manual(values = c("#EE6A50", "#6495ED", "#86C67C"), guide = guide_legend(title = names_used[3])) +
            theme_hc(base_size = font_size) +
            theme(axis.title.y = element_blank(),
                  axis.title.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.text.x = element_text(angle = 90, hjust = 1))
  
plot4 <- ggplot(df4, aes(year, x)) +
         geom_point(aes(color = variable), size = 3) + 
         geom_line(data = subset(df4, variable == "EA" & !is.na(x)), aes(year,x), color = "#EE6A50", size = 1, linetype =2) +
         geom_line(data = subset(df4, variable == "LP" & !is.na(x)), aes(year,x), color = "#6495ED", size = 1) +
         geom_line(data = subset(df4, variable == "LPWP" & !is.na(x)), aes(year,x), color = "#86C67C", size = 1) +
         scale_color_manual(values = c("#EE6A50", "#6495ED", "#86C67C"), guide = guide_legend(title = names_used[4])) +
            theme_hc(base_size = font_size) +
            theme(axis.title.y = element_blank(),
                  axis.title.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.text.x = element_text(angle = 90, hjust = 1))
  
  

graphics.off()

fig_drivers <- ggdraw(xlim = c(0,12), ylim = c(0,4)) +
            draw_plot(plot1, x = 0, y = 0, width = 4, height = 3.8) +
            draw_plot(plot4, x = 4, y = 0, width = 4, height = 3.8) +
            draw_plot(plot3, x = 8, y = 0, width = 4, height = 3.8) +
            draw_plot_label(c("A", "B", "C"), c(0, 4, 8), c(4,4,4), size = 18, color = "grey")
save_plot("figs/fig_drivers.pdf", fig_drivers, base_width = 12, base_height = 4)


plot(Z2[1,],Y[1,], pch = 16, xlim = c(-.1,1.1), ylim = c(-.1,1.1))
for(i in 1:nrow(Y)){
  points(Z2[i,], Y[i,], pch = 16, col = i)
}