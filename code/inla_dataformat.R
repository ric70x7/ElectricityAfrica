# 
# -----------------
#
# Edited: July 5, 2016

rm(list = ls())
library(INLA)
library(raster)


load("code_output/electricity_dhs_w_covariates.RData")


# Standardize train data
center.year <- mean(survey.data.agg$year)
scale.year <- sd(survey.data.agg$year)
survey.data.agg$z.year <- scale(survey.data.agg$year,
                                center = center.year,
                                scale = scale.year)

survey.data.agg$pop2010[is.na(survey.data.agg$pop2010)] <- 0
center.pop = mean(survey.data.agg$pop2010, na.rm = TRUE)
scale.pop = sd(survey.data.agg$pop2010, na.rm = TRUE)
survey.data.agg$z.pop2010 <- scale(survey.data.agg$pop2010,
                                center = center.pop,
                                scale = scale.pop)


#FIXME: these lines should be deleted
survey.data.agg <- survey.data.agg[survey.data.agg$year > 2008,]
ix <- sample(seq(nrow(survey.data.agg)), 6000)
test.survey <- survey.data.agg[ix[3001:6000],]
survey.data.agg <- survey.data.agg[ix[1:3000],]

# Standardize test data
test.survey$z.year <- scale(test.survey$year,
                                center = center.year,
                                scale = scale.year)

test.survey$pop2010[is.na(test.survey$pop2010)] <- 0
test.survey$z.pop2010 <- scale(test.survey$pop2010,
                                center = center.pop,
                                scale = scale.pop)



# Define a list to store metadata
meta <- list()
meta$points <- list()
meta$num <- list()
meta$ix <- list()


# Temporal mesh
mesh.t <- inla.mesh.1d(loc = survey.data.agg$year,
                       interval = c(min(survey.data.agg$year), max(survey.data.agg$year)))

test.mesh.t <- inla.mesh.1d(loc = test.survey$year,
                            interval = c(min(test.survey$year), max(test.survey$year)))

# Bounds of the convex hull
template <- raster("data/ntl/ts2013W_template.tif")
afr.xy <- xyFromCell(template, 1:length(template))
afr.z <- getValues(template)
afr.xy <- afr.xy[afr.z < 100, ]
afr.xy <- afr.xy[sample(seq(nrow(afr.xy)), 3000), ]
bound <- inla.nonconvex.hull(afr.xy, convex = 2.5, concave = 2.5)
mesh.s <- inla.mesh.2d(boundary = bound, max.edge = 1.5, cutoff = 1)
#mesh.s <- inla.mesh.2d(boundary = bound, max.edge = 3.5, cutoff = 3)
afr.spde <- inla.spde2.matern(mesh = mesh.s, alpha = 2)
plot(mesh.s)
#points(afr.xy[,1],afr.xy[,2], col = "red", pch = 16)
#points(afr.xy[,1],afr.xy[,2], col = "blue", pch = 16)
#graphics.off()
#points(survey.data.agg$lon, survey.data.agg$lat, pch = 16, col = "red")

# Indices associated to the observations
meta$num$data <- nrow(survey.data.agg)
meta$points$time <- survey.data.agg$year
meta$points$span.period <- range(mesh.t$loc)
meta$ix$time.order <- meta$points$time - min(meta$points$span.period) + 1
meta$num$time.knots <- max(meta$points$span.period) - min(survey.data.agg$year) + 1
meta$points$spatial <- survey.data.agg[, c("lon", "lat")]


library(ggplot2)
ggplot(survey.data.agg, aes(lon, lat)) + geom_point(aes(col = r))
graphics.off()


