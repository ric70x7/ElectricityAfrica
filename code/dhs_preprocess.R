# DHS data pre-process
# --------------------
#
# Edited: July 21, 2016


# Load data
# NOTE:
# HV206_HasElectricity_n: Number of houses in the survey.
# HV206_HasElectricity.nYES: Number of houses in the survey that have access to electricity.
# Spatial_LONGNUM, Spatial_LATNUM: Longitude and latitude associated to the survey observation.
# Year: Year(s) in which the survey was carried on

# Longitude and latitude are do not represent the center of the village in the survey. To preserve the confidentiality of the informants, the GPS location has been randomly offset a few kilometers.
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
# NOTE: Surveys that span over one year, will be associated to the year
# in with they were finalized.
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


save(survey.data.agg, file = "code_output/electricity_dhs.RData")

# List of surveys used
all.countries <- unique(survey.data$ISO3)
afr.countries <- unique(survey.data.agg$iso3)
surveys <- survey.data[survey.data$ISO3 %in% afr.countries, c("SurveyID", "Country", "ISO3", "Year")]
surveys <- unique(surveys)
write.csv(surveys, "code_output/surveys_in_dhs_preporcess.csv")

# 73 surveys:  dim(unique(survey.data.agg[,1:3]))