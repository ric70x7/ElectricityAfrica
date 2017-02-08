# Third set of surveys data pre-process
# ------------------------------
#
# Edited: September 27, 2016
# 16 surveys


rm(list = ls())
library(foreign)


# Function to define a data frame with longitude and latitude form shapefile data
createdf <- function(shpfile){
  return(data.frame(ID = paste(shpfile$DHSCC),
                    year = shpfile$DHSYEAR,
                    lon = shpfile$X,
                    lat = shpfile$Y,
                    cluster = shpfile$DHSCLUST ))
}


# Function to define a data frame with electricity access data
accessdf <- function(rawfile){
  newdf <- data.frame(cluster = rawfile$hv001, electricity = rawfile$hv206)
  binary <- rep(0, nrow(newdf))
  binary[newdf$electricity == "yes"] <- 1
  newdf$electricity <- binary
  aggdf <- aggregate(electricity ~ cluster, data = newdf, FUN = sum)
  aggdf$total <- aggregate(electricity ~ cluster, data = newdf, FUN = length)$electricity
  aggdf$r <- aggdf$electricity/aggdf$total
  return(aggdf)
}


# List of new data files
rawdatalist <- list("data/Burkina Faso 2014 MIS/BFHR70FL.DTA",
                    "data/Ghana 2014 MIS/GHHR70FL.DTA",
                    "data/Egypt 2014 DHS/EGHR61FL.DTA",
                    "data/Kenya 2014 DHS/KEHR70FL.DTA",
                    "data/Kenya 2015 MIS/KEHR7HFL.DTA",
                    "data/Malawi 2014 MIS/MWHR71FL.DTA",
                    "data/Rwanda 2014 DHS/RWHR70FL.DTA",
                    "data/Uganda 2014 MIS/UGHR72FL.DTA")

shpdatalist <- list("data/Burkina Faso 2014 MIS/burkina_faso_2014.csv",
                    "data/Ghana 2014 MIS/ghana_2014.csv",
                    "data/Egypt 2014 DHS/egypt_2014.csv",
                    "data/Kenya 2014 DHS/kenya_2014.csv",
                    "data/Kenya 2015 MIS/kenya_2015.csv",
                    "data/Malawi 2014 MIS/malawi_2014.csv",
                    "data/Rwanda 2014 DHS/rwanda_2014.csv",
                    "data/Uganda 2014 MIS/uganda_2014.csv")

iso3list <- list("BFA", "GHA", "EGY", "KEN", "KEN", "MWI", "RWA", "UGA")
nameslist <- list("Burkina Faso", "Ghana", "Egypt", "Kenya", "Kenya", "Malawi", "Rwanda", "Uganda")
                  

# Combine data from all files
set3 <- data.frame()
for(i in seq(rawdatalist)){
  shpdata <- read.csv(shpdatalist[[i]])
  rawdata <- read.dta(rawdatalist[[i]])
  df <- createdf(shpdata)
  df2 <- accessdf(rawdata)
  ix <- match(df$cluster, df2$cluster)
  df$iso3 <- rep(iso3list[[i]], nrow(df))
  df$country <- rep(nameslist[[i]], nrow(df))
  df$has_electricity <- df2$electricity[ix]
  df$total <- df2$total[ix]
  df$r <- df2$r[ix]
  set3 <- rbind(set3, df)
}

save(set3, file = "code_output/electricity_dhs_third.RData")
