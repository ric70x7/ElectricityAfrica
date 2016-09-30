# Combine sets of surveys
# -----------------------
#
# The files combiend are:
# 1) electricity_dhs_w_covariates.RData
# 2) other_data_w_covariates.RData
# 3) df3_w_covariates.RData
#
# Edited: September 30, 2016

rm(list = ls())

load("code_output/electricity_dhs_w_covariates.RData")
load("code_output/other_data_w_covariates.RData")
load("code_output/set3_w_covariates.RData")
load("code_output/z_params.RData")


# Merge df1 and df2
df <- rbind(df1[, c("country", "iso3", "year", "lon", "lat", "pixel",
                    "has_electricity", "total", "r", "ntl", "pop", "house",
                    "country_r_mean", "country_f_mean", "country_f_sd")],
            df3[, c("country", "iso3", "year", "lon", "lat", "pixel",
                    "has_electricity", "total", "r", "ntl", "pop", "house",
                    "country_r_mean", "country_f_mean", "country_f_sd")])
      

df$z.year <- (df$year - z_year_mean)/z_year_sd
df$lit <- sapply(df$ntl, FUN = function(x) min(x, 1))
df$r_country_logit <- log(df$country_r_mean/(1-df$country_r_mean))
head(df.z_factors)
# Standardize values
for(iso3i in unique(df$iso3)){
  dfix <- df$iso3 == iso3i
  zix <- df.z_factors$iso3 == iso3i
  
  #ntl 
  df$z.ntl[dfix] <- (df$ntl[dfix] - df.z_factors$ntl_mean[zix])/df.z_factors$ntl_sd[zix]
  
  #pop 
  df$z.pop[dfix] <- (df$pop[dfix] - df.z_factors$pop_mean[zix])/df.z_factors$pop_sd[zix]
  
  #house 
  df$z.house[dfix] <- (df$house[dfix] - df.z_factors$house_mean[zix])/df.z_factors$house_sd[zix]
  
}



# Split df into df.train and df.test1
ix <- sample(1:nrow(df))
df.train <- df[ix[1:15000],]
df.test1 <- df[ix[15001:nrow(df)],]


# Set df2 as df.test2 
#NOTE: df2 is not DHS data,
#it has not been obfuscated and
#it is not measured as percentage of people with electricity access
df.test2 <- df2[, c("country", "iso3", "year", "lon", "lat", "pixel",
                    "electricity", "ntl", "pop", "house",
                    "country_r_mean", "country_f_mean", "country_f_sd")]
                

# Add households data to the dataframe
df.test2$z.year <- (df.test2$year - z_year_mean)/z_year_sd
df.test2$lit <- sapply(df.test2$ntl, FUN = function(x) min(x, 1))
df.test2$r_country_logit <- log(df.test2$country_r_mean/(1-df.test2$country_r_mean))
head(df.z_factors)
# Standardize values
for(iso3i in unique(df$iso3)){
  dfix <- df.test2$iso3 == iso3i
  zix <- df.z_factors$iso3 == iso3i
  
  #ntl 
  df.test2$z.ntl[dfix] <- (df.test2$ntl[dfix] - df.z_factors$ntl_mean[zix])/df.z_factors$ntl_sd[zix]
  
  #pop 
  df.test2$z.pop[dfix] <- (df.test2$pop[dfix] - df.z_factors$pop_mean[zix])/df.z_factors$pop_sd[zix]
  
  #house 
  df.test2$z.house[dfix] <- (df.test2$house[dfix] - df.z_factors$house_mean[zix])/df.z_factors$house_sd[zix]
  
}


save(df.train, df.test1, df.test2, file = "code_output/train_and_test_data.RData")
