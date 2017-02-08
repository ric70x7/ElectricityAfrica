# Country estimates table
# ------------------
#
# Edited: February 7, 2017

library(reshape)
library(Hmisc)

graphics.off()
rm(list = ls())
load("code_output/country_annual_estimates.RData")

head(annual_data)
aux <- cast(data = annual_data[,c("iso3", "year", "r_mean")], iso3 ~ year, value = "r_mean")
aux[2:ncol(aux)] <- 100 * round(aux[2:ncol(aux)], 2)

write.csv(aux, file = "code_output/tab_countries.csv", row.names = FALSE)
