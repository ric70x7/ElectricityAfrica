# 
# -----------------
#
# Edited: July 5, 2016

library(INLA)
library(raster)


load("code_output/electricity_dhs_w_covariates.RData")


# Standardize data
survey.data.agg$z.year <- scale(survey.data.agg$year,
                                center = mean(survey.data.agg$year),
                                scale = sd(survey.data.agg$year))

survey.data.agg$z.pop2010 <- scale(survey.data.agg$pop2010,
                                center = mean(survey.data.agg$pop2010, na.rm = TRUE),
                                scale = sd(survey.data.agg$pop2010, na.rm = TRUE))


# Define a list to store metadata
meta <- list()
meta$points <- list()
meta$num <- list()
meta$ix <- list()


# Temporal mesh
mesh.t <- inla.mesh.1d(loc = survey.data.agg$year,
                       interval = c(min(survey.data.agg$year), max(survey.data.agg$year)))


# Bounds of the convex hull
template <- raster("data/ntl/ts2013W_template.tif")
afr.xy <- xyFromCell(template, seq(template)[getValues(template)>0])
bound <- inla.nonconvex.hull(afr.xy, convex = 2.5, concave = 2., resolution = 60)
mesh.s <- inla.mesh.2d(boundary = bound, max.edge = .9)
afr.spde <- inla.spde2.matern(mesh = mesh.s, alpha = 2)


# Indices associated to the observations
meta$num$data <- nrow(survey.data.agg)
meta$points$time <- survey.data.agg$year
meta$points$span.period <- range(mesh.t$loc)
meta$ix$time.order <- meta$points$time - min(meta$points$span.period) + 1
meta$num$time.knots <- max(meta$points$span.period) - min(survey.data.agg$year) + 1
meta$points$spatial <- survey.data.agg[, c("lon", "lat")]
