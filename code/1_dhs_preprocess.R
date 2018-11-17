# Load DHS data and preprocess it
# -------------------------------
# Last edited: November 5, 2018

library(foreign)
library(raster)
library(maptools)


dhs_preprocess_set1 <- function() {
  # Function for the first batch of data.
  
  # Load data
  # NOTE:
  # HV206_HasElectricity_n: Number of houses in the survey.
  # HV206_HasElectricity.nYES: Number of houses in the survey that have access to electricity.
  # Spatial_LONGNUM, Spatial_LATNUM: Longitude and latitude associated to the survey observation.
  # Year: Year(s) in which the survey was carried on
  
  # Longitude and latitude do not represent the center of the village in the survey.
  # To preserve the confidentiality of the informants, the GPS location has been randomly offset a few kilometers.
  # Not all the surveys have associated a pair of coordiantes.
  # Some surveys span over two years.
  # Information available goes from 1990 to 2014, but it is not regularly gathered at the same locations.
  survey.data <- read.table("data/ElectricityEtcByCluster_All.csv",
                            header = TRUE, sep = ",", quote = "")
  
  # Remove NA values in Latitude and Longitude
  survey.data <- survey.data[!is.na(survey.data$Spatial_LONGNUM),]
  survey.data <- survey.data[!is.na(survey.data$Spatial_LATNUM),]
  
  # Remove values where latitude and longitude values are the same
  # NOTE: This values have been checked and are wrong
  survey.data <- survey.data[!(survey.data$Spatial_LATNUM == survey.data$Spatial_LONGNUM),]
  
  # Desaggregate years
  # NOTE: Surveys that span over one year will be associated to the year
  # in which they were finalized.
  Year2 <- c()
  for(i in 1:nrow(survey.data)){
    aux <- as.vector.factor(survey.data$Year[i])
    if(nchar(aux) == 4){
      Year2[i] <- aux
    }else{
      if(substring(aux,6,7)<16) {
        Year2[i] <- paste("20", substring(aux,6,7), sep = "")
      }else{
        Year2[i] <- paste("19", substring(aux,6,7), sep = "")
      }
    } 
  }
  survey.data$Year <- as.numeric(Year2)
  rm(Year2) #Not needed any more
  
  # NOTE: No interest in data before 2000
  survey.data <- survey.data[survey.data$Year >= 2000, ]
  
  # Aggregate data according to location and time
  # NOTE: The database contains different entries regarding a same survey.
  # These are not repeated values, but different observations that are part of a
  # grand total. Such entries were aggregated, after checking they are associated
  # to the same year, longitude and latitude values.
  
  # Define key values of unique combinations (year - longitude - latitude)
  survey.keys <- data.frame(key = paste(survey.data$Year,
                                        survey.data$Spatial_LONGNUM,
                                        survey.data$Spatial_LATNUM, sep = ""))
  
  # Data frame of Unique key values
  survey.groups <- data.frame(key = unique(survey.keys))
  survey.groups$group <- 1:nrow(survey.groups)
  
  # Identify observations with the same key values
  survey.keys$group <- apply(survey.keys, 1,
                             function(x) survey.groups[survey.groups$key == x,"group"])
  
  # Aggregate data with the same key values
  survey.data.agg <- survey.data[!duplicated(survey.keys),c(2,3,4,6,7)]
  survey.data.agg$HV206_HasElectricity.nYES <-
    rowsum(survey.data$HV206_HasElectricity.nYES, survey.keys$group)
  survey.data.agg$HV206_HasElectricity_n <-
    rowsum(survey.data$HV206_HasElectricity_n, survey.keys$group)
  
  # Just data from Africa
  survey.data.agg <- survey.data.agg[survey.data.agg$Spatial_LONGNUM > -26, ]
  survey.data.agg <- survey.data.agg[survey.data.agg$Spatial_LONGNUM < 61, ]
  survey.data.agg <- survey.data.agg[survey.data.agg$Spatial_LATNUM < 39, ]
  c1 <- survey.data.agg$Spatial_LONGNUM > 40
  c2 <- survey.data.agg$Spatial_LATNUM > 30
  survey.data.agg <- survey.data.agg[!(c1 & c2), ]
  survey.data.agg <- survey.data.agg[survey.data.agg$ISO3 != "JOR",]
  
  # Renaming and new column
  colnames(survey.data.agg) <- c("country", "iso3", "year", "lat", "lon",
                                 "has_electricity", "total")
  survey.data.agg$r <- survey.data.agg$has_electricity/survey.data.agg$total
  #save(survey.data.agg, file = "code_output/electricity_dhs.RData")
  #
  ## List of surveys used
  #all.countries <- unique(survey.data$ISO3)
  #afr.countries <- unique(survey.data.agg$iso3)
  #surveys <- survey.data[survey.data$ISO3 %in% afr.countries, c("SurveyID", "Country", "ISO3", "Year")]
  #surveys <- unique(surveys)
  #write.csv(surveys, "code_output/surveys_in_dhs_preporcess.csv")
  #
  ## 73 surveys:  dim(unique(survey.data.agg[,1:3]))
  return(survey.data.agg)
}

dhs_preprocess_set2 <- function() {
  # Function for the second batch of data.
  
  # The locations in these surveys were not obfuscated
  # These data points will be used for testing
  # 8 surveys, Zambia: 2006, 2008, 2010, 2012; Malawi: 2006, 2008, 2010, 2012

  other.data <- read.table("data/Zam_ma_electricity.csv", header = TRUE, sep = ",", quote = "")
  colnames(other.data)[1:2] <- c("lat", "lon")
  
  # Remove NA
  other.data <- other.data[!is.na(other.data$lat) & !is.na(other.data$lon) & !is.na(other.data$electricity),]
  other.data <- other.data[!(other.data$lon == 0 & other.data$lat == 0), ]
  other.data <- other.data[other.data$lon < 100 & other.data$lat < 0, ]
  #save(other.data, file = "code_output/other_data_preprocess.RData")
  return(other.data)
} 

createdf <- function(shpfile){
  # Define a data frame with longitude and latitude form shapefile data
  return(data.frame(ID = paste(shpfile$DHSCC),
                    year = shpfile$DHSYEAR,
                    lon = shpfile$X,
                    lat = shpfile$Y,
                    cluster = shpfile$DHSCLUST ))
}

accessdf <- function(rawfile){
  # Define a data frame with electricity access data
  newdf <- data.frame(cluster = rawfile$hv001, electricity = rawfile$hv206)
  binary <- rep(0, nrow(newdf))
  binary[newdf$electricity == "yes"] <- 1
  newdf$electricity <- binary
  aggdf <- aggregate(electricity ~ cluster, data = newdf, FUN = sum)
  aggdf$total <- aggregate(electricity ~ cluster, data = newdf, FUN = length)$electricity
  aggdf$r <- aggdf$electricity/aggdf$total
  return(aggdf)
}

dhs_preprocess_set3 <- function() {
  # Function for the third batch of data.
  
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
    rawdata <- read.dta(rawdatalist[[i]], convert.factors = FALSE)
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
  return(set3)
  #save(set3, file = "code_output/electricity_dhs_third.RData")
}

# Merge three data batches
# Dataset 1 -> df1
#load("code_output/electricity_dhs.RData")
df1 <- dhs_preprocess_set1()
df1$obfuscated <- TRUE

# Dataset 3 -> df3
#load("code_output/electricity_dhs_third.RData")
df3 <- dhs_preprocess_set3()
df3 <- df3[, c("country", "iso3", "lon", "lat", "year", "has_electricity", "total", "r")]
df3$obfuscated <- TRUE

# Dataset 2 -> df2
#load("code_output/other_data_preprocess.RData")
df2 <- dhs_preprocess_set2()
df2 <- df2[, c("lon", "lat", "year", "electricity")]
colnames(df2)[4] <- "has_electricity"
df2$total <- 1
df2$r <- df2$has_electricity

## Identify country
afri_main <- readShapePoly("data/Africa_main_country/Africa_main_country.shp")
afri_main <- afri_main[afri_main$ISO3 %in% c("MWI", "ZMB"), ]
xy <- df2[, c("lon", "lat")]
df2$iso3 <- paste(over(SpatialPoints(xy), afri_main)$ISO3)
df2$country[df2$iso3 == "MWI"] <- "Malawi"
df2$country[df2$iso3 == "ZMB"] <- "Zambia"
df2$obfuscated <- FALSE

# Merge df1, df2 and df3
cnames <- c("country", "iso3", "year", "lon", "lat", "has_electricity", "total", "r", "obfuscated")
df <- rbind(rbind(df3[, cnames], df2[, cnames]), df1[, cnames])

save(df, file = "code_output/merged_data2.RData")
