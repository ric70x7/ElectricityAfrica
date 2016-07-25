# Core model: predictions and interpolations
# ------------------------------------------
#
# Edited: July 22, 2016
# This is the core file (train and test with non obfuscated data)
#
# Predictor
# eta = beta_0 + beta_1 * year + beta_2 * population + beta_3 * ntl + u.field
#     = mu + u.field, where mu = beta_0 + beta_1 * year + beta_2 * population + beta_3 * ntl
# y = exp(eta)/(1 + exp(eta)) 


rm(list = ls())
library(raster)
library(INLA)
library(parallel)
library(doParallel)
load("code_output/z_params.RData")
#load("code_output/prediction_locations.RData")
load("code_output/geos1/core_latent_interpolation.RData")


# Prediction grid and covarirates values
num.layers <- 16

# NTL
yi <- 2010
filename <- paste("data/ntl/Inland_water_masked_5k/ts", yi, "W_template.tif", sep = "") #5Km resolution
ntl2010 <- raster(filename)
ntl2010.x <- getValues(ntl2010)


# Population 2010
pop2010.raw <- raster("data/Africa-POP-2010_africa2010ppp/africa2010ppp.tif")
pop2010 <- resample(pop2010.raw, ntl2010)
pop2010.x <- getValues(pop2010)


# Whole grid
afr.locs <- xyFromCell(ntl2010, seq(ntl2010))


# Mask of valid values
ntl.mask <- !is.na(ntl2010.x) & ntl2010.x < 100
pop.mask <- !is.na(pop2010.x)
afr.mask <- ntl.mask & pop.mask


# Covariates
z.year <- as.list((2000:2015 - center.year)/scale.year)
z.pop2010 <- log(1+pop2010.x[afr.mask])
z.ntl <- list()
for(i in seq(z.year)){
  pred.year <- 1999 + i
  if(pred.year <= 2013){
    yi <- pred.year
  }else{
    yi <- 2013
  }
  filename <- paste("data/ntl/Inland_water_masked_5k/ts", yi, "W_template.tif", sep = "") #5Km resolution
  ntlraster <- raster(filename)
  z.ntl[[i]] <- log(1+getValues(ntlraster)[afr.mask])
}


# Posterior samples
num.samples <- 300# FIXME: change to 10000
post.samples <- inla.posterior.sample(n = num.samples, result = m_core)


# Organize samples
obj.names <- rownames(post.samples[[1]]$latent)
rand.u.ix <- grepl("u.field", obj.names)
beta.i.ix <- grepl("intercept", obj.names)
beta.y.ix <- grepl("year", obj.names)
beta.p.ix <- grepl("population", obj.names)
beta.n.ix <- grepl("ntl", obj.names)
  
rand.u.samples <- matrix(NA, nrow = num.samples, ncol = length(meta_core$ix$stack$latn))
beta.i.samples <- rep(NA, num.samples)
beta.y.samples <- rep(NA, num.samples)
beta.p.samples <- rep(NA, num.samples)
beta.n.samples <- rep(NA, num.samples)
for(i in 1:num.samples){
  rand.u.samples[i,] <- post.samples[[i]]$latent[rand.u.ix]
  beta.i.samples[i] <- post.samples[[i]]$latent[beta.i.ix]
  beta.y.samples[i] <- post.samples[[i]]$latent[beta.y.ix]
  beta.p.samples[i] <- post.samples[[i]]$latent[beta.p.ix]
  beta.n.samples[i] <- post.samples[[i]]$latent[beta.n.ix]
}


# Projector matrix to interpolate nodes in mesh.s
A.latn <- inla.spde.make.A(mesh = mesh.s,
                           loc = as.matrix(afr.locs[afr.mask,]))


# Function to recover a sample of inv.link.logit(eta)
boot.predictor <- function(slices, time.point){
  eta <- as.matrix(A.latn %*% t(rand.u.samples[slices,]))
  eta <- t(t(eta) +
             beta.i.samples[slices] +
             beta.y.samples[slices] * z.year[[time.point]])
  eta <- eta + t(matrix(rep(z.pop2010, ncol(eta)),
                        nrow = ncol(eta),
                        byrow = TRUE) * beta.p.samples[slices])
  eta <- eta + t(matrix(rep(z.ntl[[time.point]], ncol(eta)),
                        nrow = ncol(eta),
                        byrow = TRUE) * beta.n.samples[slices])
  return(rowMeans(inla.link.invlogit(eta)))
}

#aux <- boot.predictor(slices = slices.list[[1]], time.point = 3)
#length(aux)

# Definition of settings to parallelize
num.cores <- 3 # FIXME: change to 10
batch.size <- 100 # FIXME: change to 1000
num.slices <- num.samples/batch.size

slices.list <- c()
for(i in 1:num.slices){
  slices.list[[i]] <- (i-1) * batch.size + 1:batch.size
}


# Parallel processing
map.values <- list()
#i <- 1 #FIXME: delete this line
for(i in 1:16){
  cl <- makeCluster(num.cores)
  registerDoParallel(cl)
  expected <- foreach(from = slices.list, 
                      .combine = cbind,
                      .packages = c("INLA")) %dopar%{
                        boot.predictor(from, time.point = i)
                      }
  stopCluster(cl)
  map.values[[i]] <- rowMeans(expected)
}


df.template <- data.frame(lon = afr.locs[,1],
                          lat = afr.locs[,2],
                          pixel = seq(afr.locs[,1]),
                          year = NA,
                          r = NA)

df.predicted <- data.frame()
for(i in 1:16){
  df.i <- df.template
  df.i$year <- 1999 + i
  df.i$r[afr.mask] <- map.values[[i]]
  
  df.predicted <- rbind(df.predicted, df.i)
}



#####################################

library(ggplot2)
library(ggthemes)
library(viridis)

pltyear <- 1999 + 16
ggplot(df.predicted[!is.na(df.predicted$r) & df.predicted$year == pltyear,], aes(lon, lat)) +
    #geom_raster(aes(fill = log(r))) +
    geom_raster(aes(fill = r)) +
    coord_equal() +
    theme_map() +
    theme(legend.position = "bottom", legend.key.width = unit(2, "cm")) +
    scale_fill_viridis(limits = c(0, 1), guide = guide_colorbar(title = paste("access", pltyear, sep = " ")))
    #scale_fill_viridis(guide = guide_colorbar(title = paste("access", pltyear, sep = " ")))
graphics.off()



##### Sanity check

load("code_output/other_data_w_covariates.RData")

predicted.test.mean <- c()
for(i in seq(nrow(df.test))){
  predicted.test.mean[i] <- inla.emarginal(inla.link.invlogit,
                                            m_core$marginals.linear.predictor[meta_core$ix$stack$test][[i]] )
}
df.test$r <- df.test$electricity
plot(df.test$r, predicted.test.mean)


head(df.test)

y.validation <- 2010
A.validation <- inla.spde.make.A(mesh = mesh.s,
                                 loc = as.matrix(df.test[df.test$year==y.validation,c("lon","lat")]))
dim(A.validation)
zz.y <- df.test$z.year[df.test$year==y.validation][1]

# Function to recover a sample of inv.link.logit(eta)
eta <- as.matrix(A.validation %*% t(rand.u.samples))
eta <- t(t(eta) +
           beta.i.samples +
           beta.y.samples * zz.y)
eta <- eta + t(matrix(rep(df.test$z.pop2010[df.test$year==y.validation], ncol(eta)),
                      nrow = ncol(eta),
                      byrow = TRUE) * beta.p.samples)
eta <- eta + t(matrix(rep(df.test$z.ntl[df.test$year==y.validation], ncol(eta)),
                      nrow = ncol(eta),
                      byrow = TRUE) * beta.n.samples)
eta.mean <- rowMeans(eta)
output.mean <- rowMeans(inla.link.invlogit(eta))

plot(m_core$summary.fitted.values$mean[meta_core$ix$stack$test][df.test$year==y.validation],
     eta.mean)

plot(predicted.test.mean[df.test$year==y.validation], inla.link.invlogit(eta.mean), col = "blue", pch = 16, cex=.1) 
points(predicted.test.mean[df.test$year==y.validation], output.mean, col = "red", pch = 16, cex = .1)

plot(inla.link.invlogit(eta.mean), output.mean)
      
points(.5,.5,pch=16, col="red")
lines(c(0,1), c(0,1), col= "red")

length(predicted.test.mean)
graphics.off()
