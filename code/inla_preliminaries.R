# INLA preliminaries
# ------------------
#
# Edited: September 27, 2016


library(INLA)
library(raster)
library(maptools)


graphics.off()
rm(list = ls())
#load("code_output/train_and_test_data.RData")
load("code_output/merged_data.RData")
afri_main <- readShapePoly("data/Africa_main_country/Africa_main_country.shp")


# Split data
ix <- sample(1:sum(df$obfuscated))
num.train <- round(sum(df$obfuscated)*.8)
df.train <- subset(df, obfuscated)[ix[1:num.train],]
df.test1 <- subset(df, obfuscated)[ix[(1 + num.train):sum(df$obfuscated)],]
df.test2 <- subset(df, !obfuscated)


#DELETE these lines
#df.train <- df.train[sample(1:nrow(df.train), 5000), ]
#df.test1 <- df.test1[sample(1:nrow(df.test1), 1000), ]
#df.test2 <- df.test1[sample(1:nrow(df.test2), 1000), ]
#isosample <- c("KEN")
#afri_main <- afri_main[afri_main$ISO3 %in% isosample, ]
#df.train <- subset(df.train, iso3 %in% isosample)
#df.test1 <- subset(df.test1, iso3 %in% isosample)
#df.train <- df.train[1:1000,]
#df.test1 <- df.test1[1:100,]
#df.test2 <- df.test2[1:100,]



# Define a list to store metadata
meta <- list()
meta$points <- list()
meta$num <- list()
meta$ix <- list()


# Temporal mesh
mesh.t <- inla.mesh.1d(loc = df.train$year, interval = c(min(df.train$year), max(df.train$year)))


# Spatial mesh
afri_border <- unionSpatialPolygons(afri_main, rep(1, nrow(afri_main)))
afri_segment <- inla.sp2segment(afri_border)
mesh.s <- inla.mesh.2d(boundary = afri_segment, max.edge = 1.8, cutoff = .9)
#  mesh.s <- inla.mesh.2d(boundary = afri_segment, max.edge = .3, cutoff = .2)
#xy <- df[, c("lon", "lat")]
#plot(mesh.s)
#points(df.train$lon, df.train$lat, pch = 16, col = "blue", cex = .2)
#points(df.test1$lon, df.test1$lat, pch = 16, col = "red", cex = .2)
afr.spde <- inla.spde2.matern(mesh = mesh.s, alpha = 2)


# Indices associated to the observations
meta$num$data <- nrow(df.train)
meta$points$time <- df.train$year
meta$points$span.period <- range(mesh.t$loc)
meta$ix$time.order <- meta$points$time - min(meta$points$span.period) + 1
meta$num$time.knots <- max(meta$points$span.period) - min(df.train$year) + 1
meta$points$spatial <- df.train[, c("lon", "lat")]


# Index of observations
meta$ix$stack <- list()
