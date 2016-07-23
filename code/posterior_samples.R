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
num.samples <- 1000#0
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
  return(rowSums(inla.link.invlogit(
           beta.i.samples[slices] +
           beta.y.samples[slices] * z.year[[time.point]] +
           beta.p.samples[slices] * z.pop2010 +
           beta.n.samples[slices] * z.ntl[[time.point]] +
           as.matrix(A.latn %*% t(rand.u.samples[slices,]))))/length(slices))
}


# List of slices to parallelize
slices.list <- list()
for(i in 1:1000){
  slices.list[[i]] <- (i-1)*100 + 1:100
}
slices <- slices.list[[2]]

boot.predictor(slices.list[[23]], time.point = 15)


# Parallel processing
expected <- rep(0, sum(afr.mask))
num.cores <- 3#FIXME
for(tp in 1:16){
  cl <- makeCluster(num.cores)
  registerDoParallel(cl)
  expected <- 
  for
}




expected <- rep(0, sum(afr.mask))
#for(i in seq(1, num.samples, 100)){
batch.size <- 100
for(i in seq(1, 10000, batch.size)){
  partial <- rep(0, sum(afr.mask))
  #for(j in i:(i+batch.size-1)){
  for(j in 1:100){
  partial <- partial + boot.eta(j,
                                z.year = z.year[1],
                                z.pop = z.pop,
                                z.ntl = z.ntl)
  }
  print(i)
  expected <- expected + partial/batch.size
}
expected <- expected/length(seq(1, 10000, batch.size))



#####################################33



# Functions to extract samples
boot.i <- function(nrows = sum(afr.mask), nsamples = num.samples){
  return(matrix(rep(beta.i.samples[1:nsamples], nrows),
                ncol = nsamples,
                byrow = TRUE))
}

boot.y <- function(z.year, nrows = sum(afr.mask), nsamples = num.samples){
  # NOTE z.year must be a scalar
  return(z.year * matrix(rep(beta.y.samples[1:nsamples], nrows),
                       ncol = nsamples,
                       byrow = TRUE))
}

boot.p <- function(z.pop, nrows = sum(afr.mask), nsamples = num.samples){
  # NOTE z.pop must be a vector of lenght nrows
  return(z.pop * matrix(rep(beta.p.samples[1:nsamples], nrows),
                       ncol = nsamples,
                       byrow = TRUE))
}

boot.n <- function(z.ntl, nrows = sum(afr.mask), nsamples = num.samples){
  # NOTE z.ntl must be a vector of lenght nrows
  return(z.ntl * matrix(rep(beta.n.samples[1:nsamples], nrows),
                       ncol = nsamples,
                       byrow = TRUE))
}
  
boot.u <- function(u, nrows = sum(afr.mask), nsamples = num.samples){
  # NOTE z.ntl must be a vector of lenght nrows
  return(z.ntl * matrix(rep(beta.n.samples[1:nsamples], nrows),
                       ncol = nsamples,
                       byrow = TRUE))
}
  

# Projector matrix to interpolate the latent variable
A.latn <- inla.spde.make.A(mesh = mesh.s,
                           loc = as.matrix(afr.locs[afr.mask,]))


# Project field in the exponentiated space
rand.g.samples <- exp(rand.u.samples) # g = exp(u)
g.nodes <- apply(rand.g.samples, 2, mean)
g.field <- as.vector(A.latn %*% g.nodes)




# Expected exp(beta_1 * year)
gamma.yx <- c()
for(i in 1:num.layers){
  z.year <- (1999 + i - center.year)/scale.year
  gamma.yx[i] <- mean(exp(beta.y.samples * z.year))
}

# Expected exp(beta_2 * population)
z.pop2010 <- log(1 + pop2010.x[afr.mask])
expand.z.pop <- matrix(rep(z.pop2010, num.layers), ncol = num.samples, byrow = FALSE)
length(beta.p.samples)
length(z.pop2010)

# Expected exp(beta_3 * ntl)
for ....

aux <- runif(100,1,10)
maux <- mean(aux)
ea1 <- mean(aux/(1+aux))
ea2 <- maux/(1+maux)


#}  

library(ggplot2)

plot(afr.locs[afr.mask,1][pick], afr.locs[afr.mask,2][pick], pch = 16, col = "gray")

ggplot(dfr, aes(lon, lat)) + geom_raster(aes(fill = u))


dfr <- data.frame(lon = mesh.s$loc[,1],
                  lat = mesh.s$loc[,2],
                  u = rand.u.samples[50,])
ggplot(dfr, aes(lon, lat)) + geom_point(aes(col = u), size = 5)


pick <- 100000:300000
g.field <-  as.vector(A.latn %*% rand.u.samples[50,])
dfg <- data.frame(lon = afr.locs[afr.mask,1][pick],
                  lat = afr.locs[afr.mask,2][pick],
                  g = g.field[pick])


ggplot(dfr, aes(lon, lat)) + geom_point(aes(col = u), size = 5) +
geom_point(data = dfg, aes(lon, lat, col = g))


graphics.off()
