# Core model: predictions and interpolations
# ------------------------------------------
#
# Edited: July 22, 2016
# This is the core file (train and test with non obfuscated data)

rm(list = ls())
library(INLA)
load("code_output/prediction_locations.RData")
load("code_output/geos1/core_latent_interpolation.RData")


# Projector matrix
A.latn <- inla.spde.make.A(mesh = mesh.s,
                           loc = as.matrix(afr.locs))


# Posterior samples
num.samples <- 10000
post.samples <- inla.posterior.sample(n = num.samples, result = m_core)


# Organize samples
obj.names <- rownames(post.samples[[1]]$latent)
u.ix <- grepl("u.field", obj.names)
intercept.ix <- grepl("intercept", obj.names)
year.ix <- grepl("year", obj.names)
population.ix <- grepl("population", obj.names)
ntl.ix <- grepl("ntl", obj.names)
  
u.samples <- matrix(NA, nrow = num.samples, ncol = length(meta_core$ix$stack$latn))
intercept.samples <- rep(NA, num.samples)
year.samples <- rep(NA, num.samples)
population.samples <- rep(NA, num.samples)
ntl.samples <- rep(NA, num.samples)
for(i in 1:num.samples){
  u.samples[i,] <- post.samples[[i]]$latent[u.ix]
  intercept.samples[i] <- post.samples[[i]]$latent[intercept.ix]
  year.samples[i] <- post.samples[[i]]$latent[year.ix]
  population.samples[i] <- post.samples[[i]]$latent[population.ix]
  ntl.samples[i] <- post.samples[[i]]$latent[ntl.ix]
}


#  