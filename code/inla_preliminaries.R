# INLA preliminaries
# ------------------
#
# Edited: September 14, 2016


library(INLA)
library(raster)

#graphics.off()
#rm(list = ls())

load("code_output/train_and_test_data.RData")

#DELETE these lines
df.train <- df.train[1:1000,]
df.test1 <- df.test1[1:100,]
#df.test2 <- df.test2[1:100,]

xy <- df.train[, c("lon", "lat")]


# Define a list to store metadata
meta <- list()
meta$points <- list()
meta$num <- list()
meta$ix <- list()


# Temporal mesh
mesh.t <- inla.mesh.1d(loc = df.train$year, interval = c(min(df.train$year), max(df.train$year)))


# Spatial mesh
bound <- inla.nonconvex.hull(as.matrix(xy), convex = -.15, concave = -.5)
mesh.s <- inla.mesh.2d(boundary = bound, max.edge = 2, cutoff = 1.5)
#plot(mesh.s)
#points(xy[,1],xy[,2], col = "red", pch = 16)
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