# INLA preliminaries
# ------------------
#
# Edited: September 27, 2016


library(INLA)
library(raster)
library(maptools)


graphics.off()
rm(list = ls())
set.seed(100)
load("code_output/df_model.RData")
afri_main <- rgdal::readOGR("data/Africa_main_country/Africa_main_country.shp")

# Split data
df_model <- subset(df_model, year <= 2013)
num.train <- round(sum(df_model$obfuscated)*.7)
ix <- sample(1:sum(df_model$obfuscated))
df.train <- subset(df_model, obfuscated)[ix[1:num.train],]
df.test1 <- subset(df_model, obfuscated)[ix[(1 + num.train):sum(df_model$obfuscated)],]
df.test2 <- subset(df_model, !obfuscated)

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
mesh.s <- inla.mesh.2d(boundary = afri_segment, max.edge = 1.5, cutoff = 1.4)
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