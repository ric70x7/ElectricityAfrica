# Combine sets of surveys
# ------------------
#
# The files combiend are:
# 1) electricity_dhs_w_covariates.RData
# 2) other_data_w_covariates.RData
# 3) df3_w_covariates.RData
#
# Edited: August 25, 2016

rm(list = ls())

load("code_output/electricity_dhs_w_covariates.RData")
load("code_output/other_data_w_covariates.RData")
load("code_output/set3_w_covariates.RData")


# Add country name and iso3 naming to df3
df3$country <- NA
df3$iso3 <- NA

df3$country[df3$ID == "BF"] <- "Burkina Faso"
df3$iso3[df3$ID == "BF"] <- "BFA"

df3$country[df3$ID == "GH"] <- "Ghana"
df3$iso3[df3$ID == "GH"] <- "GHA"

df3$country[df3$ID == "EG"] <- "Egypt"
df3$iso3[df3$ID == "EG"] <- "EGY"

df3$country[df3$ID == "KE"] <- "Kenya"
df3$iso3[df3$ID == "KE"] <- "KEN"

df3$country[df3$ID == "MW"] <- "Malawi"
df3$iso3[df3$ID == "MW"] <- "MWI"

df3$country[df3$ID == "RW"] <- "Rwanda"
df3$iso3[df3$ID == "RW"] <- "RWA"

df3$country[df3$ID == "RW"] <- "Rwanda"
df3$iso3[df3$ID == "RW"] <- "RWA"

df3$country[df3$ID == "UG"] <- "Uganda"
df3$iso3[df3$ID == "UG"] <- "UGA"


# Merge df1 and df2
df <- rbind(df1[, c("country", "iso3", "year", "lon", "lat", "pixel",
                    "has_electricity", "total", "r", "ntl", "pop",
                    "z.year", "z.ntl", "z.pop")],
            df3[, c("country", "iso3", "year", "lon", "lat", "pixel",
                    "has_electricity", "total", "r", "ntl", "pop",
                    "z.year", "z.ntl", "z.pop")])
      

# Split df into df.train and df.test1
ix <- sample(1:nrow(df))
df.train <- df[ix[1:15000],]
df.test1 <- df[ix[15001:nrow(df)],]


# Set df2 as df.test2 
#NOTE: df2 is not DHS data,
#it has not been obfuscated and
#it is not measured as percentage of people with electricity access
df.test2 <- df2[,c("year", "lon", "lat", "pixel", "electricity", "ntl",
                   "pop", "z.year", "z.ntl", "z.pop")]


save(df.train, df.test1, df.test2, file = "code_output/train_and_test_data.RData")
