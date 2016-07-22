# INLA preliminaries
# ------------------
#
# Edited: July 20, 2016


library(INLA)
library(raster)

graphics.off()
rm(list = ls())
load("code_output/electricity_dhs_w_covariates.RData")
survey.iso3 <- unique(df$iso3)

xy <- df[, c("lon", "lat")]

#countries <- c("MWI", "ZMB")
#df <- subset(df, iso3 %in% countries)
  ix <- sample(seq(nrow(df)), nrow(df))
#df.test <- df[ix[1101:nrow(df)],]
  df <- df[ix[1:1100],]
#df.test <- df[ix[601:1000],]
#df <- df[ix[1:250],]

# Define a list to store metadata
meta <- list()
meta$points <- list()
meta$num <- list()
meta$ix <- list()


# Temporal mesh
mesh.t <- inla.mesh.1d(loc = df$year,
                       interval = c(min(df$year), max(df$year)))


# Spatial mesh
bound <- inla.nonconvex.hull(as.matrix(xy), convex = -.08, concave = -.28)
mesh.s <- inla.mesh.2d(boundary = bound, max.edge = 2.5, cutoff = 2.0)
#plot(mesh.s)
#points(xy[,1],xy[,2], col = "red", pch = 16)
afr.spde <- inla.spde2.matern(mesh = mesh.s, alpha = 2)


# Indices associated to the observations
meta$num$data <- nrow(df)
meta$points$time <- df$year
meta$points$span.period <- range(mesh.t$loc)
meta$ix$time.order <- meta$points$time - min(meta$points$span.period) + 1
meta$num$time.knots <- max(meta$points$span.period) - min(df$year) + 1
meta$points$spatial <- df[, c("lon", "lat")]


# Index of observations
meta$ix$stack <- list()