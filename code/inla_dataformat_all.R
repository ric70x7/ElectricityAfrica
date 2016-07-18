# Format all observed data
# ------------------------
#
# Edited: July 15, 2016

library(INLA)
library(raster)


rm(list = ls())
load("code_output/electricity_dhs_w_covariates.RData")
survey.iso3 <- unique(df$iso3)


# Remove NA values
df <- subset(df, !is.na(df$ntl) & !is.na(df$pop2010) & !is.na(r))


## Here come a bunch of heroic assumptions
# No people => no households => no electricity
mask <- df$r > 0 & df$ntl == 0 & df$pop2010 == 0
df$r[mask] <- 0
df$pop2010[is.na(df$pop2010)] <- 0


# Standardize data
center.year <- mean(df$year)
scale.year <- sd(df$year)
df$z.year <- scale(df$year, center = center.year, scale = scale.year)

df$z.pop2010 <- log(1+df$pop2010)
df$z.ntl <- log(1+df$ntl)


# Define a list to store metadata
meta <- list()
meta$points <- list()
meta$num <- list()
meta$ix <- list()


# Temporal mesh
mesh.t <- inla.mesh.1d(loc = df$year,
                       interval = c(min(df$year), max(df$year)))


# Bounds of the convex hull
#afr.xy <- as.matrix(rbind(df[,c("lon", "lat")],test.survey[,c("lon", "lat")]))
template <- raster("data/ntl/TSwater/ts2013W.tif")
afr.xy <- xyFromCell(template, 1:length(template))
afr.z <- getValues(template)
afr.xy <- afr.xy[!is.na(afr.z), ]
afr.xy <- afr.xy[sample(seq(nrow(afr.xy)), 3000), ]
bound <- inla.nonconvex.hull(afr.xy, convex = 2.5, concave = 2.5)
mesh.s <- inla.mesh.2d(boundary = bound, max.edge = 1.5, cutoff = 1)
#mesh.s <- inla.mesh.2d(boundary = bound, max.edge = 3.5, cutoff = 3)
afr.spde <- inla.spde2.matern(mesh = mesh.s, alpha = 2)
#plot(mesh.s)
#points(afr.xy[,1],afr.xy[,2], col = "red", pch = 16)
#points(df$lon, df$lat, pch = 16, col = "blue")

# Indices associated to the observations
meta$num$data <- nrow(df)
meta$points$time <- df$year
meta$points$span.period <- range(mesh.t$loc)
meta$ix$time.order <- meta$points$time - min(meta$points$span.period) + 1
meta$num$time.knots <- max(meta$points$span.period) - min(df$year) + 1
meta$points$spatial <- df[, c("lon", "lat")]

