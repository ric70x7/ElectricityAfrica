# Compute electricity access and households size per country
# ----------------------------------------------------------
#
# Edited: October 14, 2016


library(raster)
rm(list = ls())


# DHS (and other surveys) country data
household_members <- read.csv("data/DHS_household_members_by_country.csv")
electricity_access <- read.csv("data/DHS_electricity_access_by_country.csv")
household_members$iso3 <- paste(household_members$iso3)
electricity_access$iso3 <- paste(electricity_access$iso3)
household_members <- subset(household_members, year >= 2000)
electricity_access <- subset(electricity_access, year >= 2000)


# World Development Index data
wdi <- read.csv("data/World_Development_Indicators/Data2.csv")
wdi$Country.Code <- paste(wdi$Country.Code)


# Shape file of Africa
afri_main <- shapefile("data/Africa_main_country/Africa_main_country.shp")


# Data frame with country stats
raw_country_stats <- data.frame(year = sort(rep(2000:2015, length(afri_main$ISO3))),
                            iso3 = rep(afri_main$ISO3, 16),
                            r = NA,
                            household_members = NA)


# Fill-in data on household members
for(i in 1:nrow(household_members)){
  yi <- household_members$year[i]
  iso3i <- household_members$iso3[i]
  ix <- raw_country_stats$year == yi & raw_country_stats$iso3 == iso3i
  raw_country_stats$household_members[ix] <- household_members$members[i]
}


# Fill-in data on electricity access
# First WDI values
for(iso3i in wdi$Country.Code){
  ix00 <- raw_country_stats$year == 2000 & raw_country_stats$iso3 == iso3i
  ix10 <- raw_country_stats$year == 2010 & raw_country_stats$iso3 == iso3i
  ix12 <- raw_country_stats$year == 2012 & raw_country_stats$iso3 == iso3i
  
  raw_country_stats$r[ix00] <-  wdi$YR2000[wdi$Country.Code == iso3i]/100
  raw_country_stats$r[ix10] <-  wdi$YR2010[wdi$Country.Code == iso3i]/100
  raw_country_stats$r[ix12] <-  wdi$YR2012[wdi$Country.Code == iso3i]/100
}


# Then DHS/MIS/AIS values (overwrite WDI if both are available)
for(i in 1:nrow(electricity_access)){
  yi <- electricity_access$year[i]
  iso3i <- electricity_access$iso3[i]
  ix <- raw_country_stats$year == yi & raw_country_stats$iso3 == iso3i
  raw_country_stats$r[ix] <- electricity_access$r[i]/100
}


# Data frame of filled-in data
country_stats <- data.frame(year = sort(rep(2000:2015, length(afri_main$ISO3))),
                            iso3 = rep(afri_main$ISO3, 16),
                            household_members = NA,
                            num_households = NA,
                            pop = NA,
                            ntl = NA,
                            num_lithouseholds = NA,
                            num_poppix = NA,
                            num_litpix = NA,
                            num_litpoppix = NA)


# Fill-in gaps in household size 
for(iso3i in unique(country_stats$iso3)){
  df <- subset(raw_country_stats, iso3 == iso3i & !is.na(household_members))[,c("year", "household_members")]
  
  if(nrow(df) == 1){
    # Replicate value
    ix <- country_stats$iso3 == iso3i
    country_stats$household_members[ix] <- df$household_members
  }else{
    # Fill-in years in between
    for(i in 1:(nrow(df)-1)){
      ix <- country_stats$iso3 == iso3i & country_stats$year >= df$year[i] & country_stats$year < df$year[i+1]
      country_stats$household_members[ix] <- df$household_members[i]
    }
    # Fill-in up to 2000
    ix <- country_stats$iso3 == iso3i & country_stats$year >= 2000 & country_stats$year < df$year[1]
    country_stats$household_members[ix] <- df$household_members[1]
    # Fill-in up to 2015
    ix <- country_stats$iso3 == iso3i & country_stats$year >= df$year[nrow(df)] & country_stats$year <= 2015
    country_stats$household_members[ix] <- df$household_members[nrow(df)]
  }
}
# For countries with no data use the annual average of the continent
for(yi in unique(country_stats$year)){
  ix <- country_stats$year == yi
  country_stats$household_members[ix & is.na(country_stats$household_members)] <-
    mean(country_stats$household_members[ix], na.rm = TRUE)
  
}


# Aggreagate data from raster files
for(iso3j in afri_main$ISO3){
  # Base layers
  ntl_x <- raster(paste("data/ntl/Inland_water_masked_5k/ts2010W_template.tif", sep = ""))
  ntl_x[ntl_x[]==128] <- NA # Value 128 is NA
  # Masks
  shp_boundary <- afri_main[afri_main$ISO3 == iso3j,]
  raster_mask <- mask(ntl_x, shp_boundary)
  cells_mask <- !is.na(raster_mask[])
  for(i in 1:16){
  
    yi <- 1999 + i
    ix <- country_stats$year == yi & country_stats$iso3 == iso3j
  
    ntl <- raster(paste("data/ntl/Inland_water_masked_5k/ts", min(yi, 2013) , "W_template.tif", sep = ""))
    ntl[ntl[]==128] <- NA # Value 128 is NA
    pop <- raster(paste("code_output/Population/GPW4_", yi, ".tif", sep = ""))
  
  
    country_stats$ntl[ix] <- sum(ntl[cells_mask], na.rm = TRUE)
    country_stats$pop[ix] <- sum(pop[cells_mask], na.rm = TRUE)
    
    hfactor <- country_stats$household_members[country_stats$year == yi & country_stats$iso3 == iso3j]
    country_stats$num_households[ix] <-  sum(sapply(pop[cells_mask],
                                                FUN = function(x) ifelse(x == 0, 0, max(1, floor(x/hfactor)))), na.rm = TRUE)
    
    country_stats$num_lithouseholds[ix] <-  sum(sapply((pop[cells_mask])[ntl[cells_mask]>0],
                                                FUN = function(x) ifelse(x == 0, 0, max(1, floor(x/hfactor)))), na.rmna.rm = TRUE)
    
    country_stats$num_poppix[ix] <- sum(pop[cells_mask]>0, na.rm = TRUE)
    country_stats$num_litpix[ix] <- sum(ntl[cells_mask]>0, na.na.rm = TRUE)
    country_stats$num_litpoppix[ix] <- sum(ntl[cells_mask]>0 & ntl[cells_mask]>0, na.rm = TRUE)
    
    print(c(yi, iso3j)) 
  }
}


country_stats$ntl_pkh <- 1000 * country_stats$ntl/country_stats$num_households


save(raw_country_stats, country_stats, file = "code_output/country_stats.RData")
