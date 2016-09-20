# Beta regression models to fit WDI estimates of electricity access
# -----------------------------------------------------------------
#
# Edited: September 19, 2016
# This is the core file (train and test with non obfuscated data)

library(betareg)

rm(list=ls())

# WDI data
wdi <- read.csv("data/World_Development_Indicators/Data2.csv")
wdi <- wdi[complete.cases(wdi),]
wdi$Country.Name <- paste(wdi$Country.Name)
wdi$Country.Code <- paste(wdi$Country.Code)


# Sum of lights related fles
sol <- read.csv("data/SOLbyCountry.csv")
lpk <- read.csv("data/SOLperKm.csv")


# Replace names
sol$COUNTRY <- paste(sol$COUNTRY)
lpk$COUNTRY <- paste(lpk$COUNTRY)
current_names <- list("Congo DR", "Congo Rep", "Cote d Ivoire","Egypt", "Gambia", "Guinea Bissau")
replace_names <- list("Congo, Dem. Rep.", "Congo, Rep.", "Cote d'Ivoire", "Egypt, Arab Rep.", "Gambia, The", "Guinea-Bissau")
for(i in seq(current_names)){
  sol$COUNTRY[sol$COUNTRY == current_names[[i]]] <- replace_names[[i]]
  lpk$COUNTRY[lpk$COUNTRY == current_names[[i]]] <- replace_names[[i]]
}
# Add ISO3
sol$ISO3 <- NA
lpk$ISO3 <- NA
lpk <- lpk[,c(1, ncol(lpk), 2:(ncol(lpk)-1))]
for(country in sol$COUNTRY){
  if(country == "Western Sahara"){ #USE Moroco
    sol$ISO3[sol$COUNTRY == paste(country)] <- "ESH"
    lpk$ISO3[lpk$COUNTRY == paste(country)] <- "ESH"
  }else{
    sol$ISO3[sol$COUNTRY == paste(country)] <- paste(wdi$Country.Code[wdi$Country.Name == paste(country)])
    lpk$ISO3[lpk$COUNTRY == paste(country)] <- paste(wdi$Country.Code[wdi$Country.Name == paste(country)])
  }  
}
# Relative size
deltasol <- sol[,c("COUNTRY", "ISO3"),]
for(i in 1:16){
  base <- sol$YR2000
  k <- min(i, 14)
  deltasol[[paste("YR", 1999 + i, sep = "")]] <- sol[,3+k]/base
}

lpk$YR2014 <- lpk$YR2013
lpk$YR2015 <- lpk$YR2013

# Remove WDI not in SOL
ix <- match(deltasol$ISO3, wdi$Country.Code)
ix <- ix[!is.na(ix)]
wdi <- wdi[ix,]


# Beta regressions
wdi.estimates <- list()
for(i in seq(nrow(wdi))){
  code.i <- wdi$Country.Code[i]
  
  # Dataframe for each country
  wdi.data <- data.frame(year = 2000:2015, sol = NA, lpk = NA, r = NA)
  wdi.data$sol <- as.numeric(deltasol[deltasol$ISO3 == code.i, 3:18])
  wdi.data$lpk <- as.numeric(lpk[lpk$ISO3 == code.i, 3:18])
  wdi.data$r[c(1,11,13)] <- as.numeric(wdi[wdi$Country.Code == code.i, c("YR2000", "YR2010", "YR2012")])/100
  
  # proportion must be between 0 and 1
  wdi.data$r[wdi.data$r == 1] <- .999
  wdi.data$r[wdi.data$r == 0] <- .001
  
  predictor <- r ~ 1 + lpk
  beta_model <- betareg(predictor, data = subset(wdi.data, !is.na(r)))#,  control = betareg.control(phi=FALSE, hessian = TRUE, start = c(mean(wdi.data$r, na.rm = TRUE),1, 100)))
  
  beta_pred <- predict(beta_model, newdata = wdi.data)
  wdi.estimates[[code.i]] <- data.frame(year = wdi.data$year, sol = wdi.data$sol, r = beta_pred)
  
  ##plot(wdi.data$sol, wdi.data$r, pch = 16, col = "blue")
  ##lines(wdi.data$sol, beta_pred, col = "red")
  #plot(wdi.data$lpk, wdi.data$r, pch = 16, col = "red", ylim = c(0,2))
  #points(wdi.data$lpk, beta_pred, col = "gray")
  #plot(wdi.data$year, beta_pred)
  #points(wdi.data$year, wdi.data$r, pch = 16, col = "red")
  
}
graphics.off()

save(wdi.estimates, file = "code_output/wdi_factors.RData")
