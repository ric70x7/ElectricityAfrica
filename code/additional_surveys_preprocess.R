# Other surveys data pre-process
# ------------------------------
#
# Edited: July 21, 2016
# The locations in these surveys were not obfuscated
# These data points will be used for testing
# 8 surveys, Zambia: 2006, 2008, 2010, 2012; Malawi: 2006, 2008, 2010, 2012


other.data <- read.table("data/Zam_ma_electricity.csv", header = TRUE, sep = ",", quote = "")
colnames(other.data)[1:2] <- c("lat", "lon")

# Remove NA
other.data <- other.data[!is.na(other.data$lat) & !is.na(other.data$lon) & !is.na(other.data$electricity),]
other.data <- other.data[!(other.data$lon == 0 & other.data$lat == 0), ]
other.data <- other.data[other.data$lon < 100 & other.data$lat < 0, ]

save(other.data, file = "code_output/other_data_preprocess.RData")
