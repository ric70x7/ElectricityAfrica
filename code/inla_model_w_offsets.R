# Geostatistical model
# --------------------
#
# Edited: September 30, 2016
# This is the core file (train and test with non obfuscated data)


graphics.off()
rm(list = ls())

source("code/inla_preliminaries.R")
load("code_output/z_params.RData")
load("code_output/country_annual_estimates.RData")


# Predictor
predictor <- y ~ -1 + 
                  offset(r_country_logit) + 
                  lit +
                  year +
                  ntl +
                  households +
                  f(u.field, model = afr.spde) + 
                  f(epsilon, model = "iid", hyper = list(theta = list(prior = "loggamma", param = c(1, .01))))
        

# Index of spatial points in the training set
u.f <- inla.spde.make.index(name = "u.field", n.spde = afr.spde$n.spde)


# Matrix A & stack for training sample
A.train <- inla.spde.make.A(mesh = mesh.s,
                           loc = as.matrix(meta$points$spatial))

stack.train <- inla.stack(data = list(y = df.train$has_electricity),
                         A = list(A.train, 1, 1, 1, 1, 1, 1),
                         effects = list(u.f,
                                        list(r_country_logit = df.train$r_country_logit),
                                        list(lit = df.train$lit),
                                        list(year = df.train$z.year),
                                        list(ntl = df.train$z.ntl),
                                        list(households = df.train$z.house),
                                        list(epsilon = 1:meta$num$data)),
                         tag = "train")


# matrix A & stack for test1 sample
#load("code_output/other_data_w_covariates.RData")
A.test1 <- inla.spde.make.A(mesh = mesh.s,
                           loc = as.matrix(df.test1[, c("lon", "lat")]))

stack.test1 <- inla.stack(data = list(y = NA),
                         A = list(A.test1, 1, 1, 1, 1, 1, 1),
                         effects = list(u.f,
                                        list(r_country_logit = df.test1$r_country_logit),
                                        list(lit = df.test1$lit),
                                        list(year = df.test1$z.year),
                                        list(ntl = df.test1$z.ntl),
                                        list(households = df.test1$z.house),
                                        list(epsilon = meta$num$data + 1:nrow(df.test1))),
                         tag = "test1")
num.test1 <- nrow(df.test1)


## matrix A & stack for test2 sample
#A.test2 <- inla.spde.make.A(mesh = mesh.s,
#                           loc = as.matrix(df.test2[, c("lon", "lat")]))
#
#stack.test2 <- inla.stack(data = list(y = NA),
#                         A = list(A.test2, 1, 1, 1, 1, 1),
#                         effects = list(c(u.f, list(intercept = 1)),
#                                        list(ntl = df.test2$z.ntl),
#                                        list(households = df.test2$z.house),
#                                        list(year = df.test2$z.year),
#                                        list(epsilon = meta$num$data + 1:nrow(df.test2)),
#                                        list(r_country_logit = ...) ),
#                         tag = "test2")
#num.test2 <- nrow(df.test2)


# stack for computing the latent variable at the mesh nodes
stack.latn <- inla.stack(data = list(y = NA),
                         A = list(1),
                         effects = list(u.f),
                         tag = "latn")
num.latn <- nrow(stack.latn$A)


# Join stacks
#stack.all <- do.call(inla.stack, list(stack.train, stack.test1, stack.test2, stack.latn))
stack.all <- do.call(inla.stack, list(stack.train, stack.test1, stack.latn))


# Indices to recover data
meta$ix$stack$train <- inla.stack.index(stack.all, tag = "train")$data
meta$ix$stack$test1 <- inla.stack.index(stack.all, tag = "test1")$data
#meta$ix$stack$test2 <- inla.stack.index(stack.all, tag = "test2")$data
meta$ix$stack$latn <- inla.stack.index(stack.all, tag = "latn")$data


# Fit model
m <- inla(predictor, 
          data = inla.stack.data(stack.all),
          family = "binomial",
          #Ntrials = c(df.train$total, rep(1, num.test1), rep(1, num.test2), rep(1, num.latn)),
          Ntrials = c(df.train$total, rep(1, num.test1), rep(1, num.latn)),
          control.predictor = list(A = inla.stack.A(stack.all),
                                   compute = TRUE),
          control.compute = list(dic = TRUE, config = TRUE))



# Save model
m_core <- m
meta_core <- meta
#save(mesh.s, m_core, meta_core, file = "code_output/geos1/new_core_latent_interpolation_prelim.RData")
#save(df.train, df.test1, df.test2, file = "code_output/geos1/new_core_datasets_prelim.RData")




predicted.train.mean <- c()
for(i in seq(nrow(df.train))){
  predicted.train.mean[i] <- inla.emarginal(inla.link.invlogit,
                                           m_core$marginals.linear.predictor[meta_core$ix$stack$train][[i]] )
}
plot(df.train$r, predicted.train.mean)

predicted.test.mean <- c()
for(i in seq(nrow(df.test1))){
  predicted.test.mean[i] <- inla.emarginal(inla.link.invlogit,
                                           m_core$marginals.linear.predictor[meta_core$ix$stack$test1][[i]] )
}
plot(df.test1$r, predicted.test.mean)


num.samples <- 1000
post.samples <- inla.posterior.sample(n = num.samples, result = m_core)
# Projector matrix to interpolate nodes in mesh.s
A.zero <- inla.spde.make.A(mesh = mesh.s, loc = as.matrix(df.test1[,c("lon", "lat")]))
# Organize samples
obj.names <- rownames(post.samples[[1]]$latent)
beta.l.ix <- grepl("lit", obj.names)
beta.y.ix <- grepl("year", obj.names)
rand.u.ix <- grepl("u.field", obj.names)
beta.n.ix <- grepl("ntl", obj.names)
beta.h.ix <- grepl("households", obj.names)
beta.l.samples <- rep(NA, num.samples)
beta.y.samples <- rep(NA, num.samples)
beta.n.samples <- rep(NA, num.samples)
beta.h.samples <- rep(NA, num.samples)
rand.u.samples <- matrix(NA, nrow = num.samples, ncol = length(meta_core$ix$stack$latn))
for(i in 1:num.samples){
  beta.l.samples[i] <- post.samples[[i]]$latent[beta.l.ix]
  beta.y.samples[i] <- post.samples[[i]]$latent[beta.y.ix]
  beta.n.samples[i] <- post.samples[[i]]$latent[beta.n.ix]
  beta.h.samples[i] <- post.samples[[i]]$latent[beta.h.ix]
  rand.u.samples[i,] <- post.samples[[i]]$latent[rand.u.ix]
}


# Function to recover a sample of inv.link.logit(eta)
#boot.predictor <- function(slices, time.point){
# latent variable
eta <- as.matrix(A.zero %*% t(rand.u.samples))
# offset
eta <- eta + t(matrix(rep(df.test1$r_country_logit, ncol(eta)), nrow = ncol(eta), byrow = TRUE))
# lit
eta <- eta + t(matrix(rep(df.test1$lit, ncol(eta)), nrow = ncol(eta), byrow = TRUE) * beta.l.samples)
# year 
eta <- eta + t(matrix(rep(df.test1$z.year, ncol(eta)), nrow = ncol(eta), byrow = TRUE) * beta.y.samples)
# ntl
eta <- eta + t(matrix(rep(df.test1$z.ntl, ncol(eta)), nrow = ncol(eta), byrow = TRUE) * beta.n.samples)
# households
eta <- eta + t(matrix(rep(df.test1$z.house, ncol(eta)), nrow = ncol(eta), byrow = TRUE) * beta.h.samples)

plot(m_core$summary.fitted.values$mean[meta_core$ix$stack$test1], rowMeans(eta))
lines(c(-6,4), c(-6,4), col = "red")

inv_logit_eta <- rowMeans(inla.link.invlogit(eta))
plot(predicted.test.mean, inv_logit_eta)
lines(c(0,1), c(0,1), col = "red")
lines(c(0,1), c(0.5,.5), col = "gray")
lines(c(0.5,.5), c(0,1), col = "gray")
#points(predicted.test.mean, inla.link.invlogit(rowMeans(eta)), col = "blue")
     







# Prediction grid and covarirates values
library(parallel)
library(doParallel)
load("code_output/z_params.RData")
num.layers <- 16

# Whole grid
ntl2010 <- raster(paste("data/ntl/Inland_water_masked_5k/ts2010W_template.tif", sep = ""))
  ntl2010 <- mask(ntl2010, afri_main)
afr.locs <- xyFromCell(ntl2010, seq(ntl2010))
  
# Covariates
z.year <- as.list((2000:2015 - z_year_mean)/z_year_sd)
z.ntl <- list()
lit <- list()
z.house <- list()
r_country_logit <- list()
zero_mask <- list()
for(i in 1:num.layers){
  
  yi <- 1999 + i
  
  # Files
  offsetfilename <- paste("code_output/z_covariates/logitr_", yi, ".tif", sep = "")
  litfilename <- paste("code_output/z_covariates/lit_", yi, ".tif", sep = "")
  ntlfilename <- paste("code_output/z_covariates/zntl_", yi, ".tif", sep = "")
  housefilename <- paste("code_output/z_covariates/zhouse_", yi, ".tif", sep = "")
  
  # Data layers
  offraster <- raster(offsetfilename)
  litraster <- raster(litfilename)
  ntlraster <- raster(ntlfilename)
  houseraster <- raster(housefilename)
    offsetraster <- mask(offraster, afri_main)
    litraster <- mask(litraster, afri_main)
    ntlraster <- mask(ntlraster, afri_main)
    houseraster <- mask(houseraster, afri_main)
    
  ntl_mask <- !is.na(ntlraster[])
  #zero_mask[[i]] <- ntl_mask & (houseraster[] == 0 | ntlraster[] > 100)
  r_country_logit[[i]] <- offsetraster[ntl_mask]
  lit[[i]] <- litraster[ntl_mask]
  z.ntl[[i]] <- ntlraster[ntl_mask]
  z.house[[i]] <- houseraster[ntl_mask]
}


# Posterior samples
num.samples <- 500# FIXME: change to 10000
post.samples <- inla.posterior.sample(n = num.samples, result = m_core)


# Organize samples
obj.names <- rownames(post.samples[[1]]$latent)
beta.l.ix <- grepl("lit", obj.names)
beta.y.ix <- grepl("year", obj.names)
rand.u.ix <- grepl("u.field", obj.names)
beta.n.ix <- grepl("ntl", obj.names)
beta.h.ix <- grepl("households", obj.names)
beta.l.samples <- rep(NA, num.samples)
beta.y.samples <- rep(NA, num.samples)
beta.n.samples <- rep(NA, num.samples)
beta.h.samples <- rep(NA, num.samples)
rand.u.samples <- matrix(NA, nrow = num.samples, ncol = length(meta_core$ix$stack$latn))
for(i in 1:num.samples){
  beta.l.samples[i] <- post.samples[[i]]$latent[beta.l.ix]
  beta.y.samples[i] <- post.samples[[i]]$latent[beta.y.ix]
  beta.n.samples[i] <- post.samples[[i]]$latent[beta.n.ix]
  beta.h.samples[i] <- post.samples[[i]]$latent[beta.h.ix]
  rand.u.samples[i,] <- post.samples[[i]]$latent[rand.u.ix]
}


# Projector matrix to interpolate nodes in mesh.s
A.latn <- inla.spde.make.A(mesh = mesh.s, loc = as.matrix(afr.locs[ntl_mask,]))


# Function to recover a sample of inv.link.logit(eta)
boot.predictor <- function(slices, time.point){
  # latent variable
  eta <- as.matrix(A.latn %*% t(rand.u.samples[slices,]))
  # offset
  eta <- eta + t(matrix(rep(r_country_logit[[time.point]], ncol(eta)), nrow = ncol(eta), byrow = TRUE))
  # lit
  eta <- eta + t(matrix(rep(lit[[time.point]], ncol(eta)), nrow = ncol(eta), byrow = TRUE) * beta.l.samples[slices])
  # year 
  eta <- eta + matrix(rep(z.year[[time.point]], nrow(eta) * ncol(eta)), ncol = ncol(eta)) * beta.y.samples[slices]
  # ntl
  eta <- eta + t(matrix(rep(z.ntl[[time.point]], ncol(eta)), nrow = ncol(eta), byrow = TRUE) * beta.n.samples[slices])
  # households
  eta <- eta + t(matrix(rep(z.house[[time.point]], ncol(eta)), nrow = ncol(eta), byrow = TRUE) * beta.h.samples[slices])
  
  return(rowMeans(inla.link.invlogit(eta)))
}


# Definition of settings to parallelize
num.cores <- 5 # FIXME: change to 10
batch.size <- 100 # FIXME: change to 1000
num.slices <- num.samples/batch.size

slices.list <- c()
for(i in 1:num.slices){
  slices.list[[i]] <- (i-1) * batch.size + 1:batch.size
}


# Parallel processing
map.values <- list()
for(i in 1:16){
  cl <- makeCluster(num.cores)
  registerDoParallel(cl)
  expected <- foreach(from = slices.list, 
                      .combine = cbind,
                      .packages = c("INLA")) %dopar%{
                        boot.predictor(from, time.point = i)
                      }
  stopCluster(cl)
  
  # Zero probability in places with no population 
  houseraster <- raster(paste("code_output/Population/GPW4_", yi, ".tif", sep = ""))
  houseraster <- mask(houseraster, afri_main)
  housebin <- houseraster[!is.na(houseraster)] 
  housebin <- sapply(housebin, function(x) ifelse(x > 0, 1, 0))
  map.values[[i]] <- rowMeans(expected) * housebin
}


#save(map.values, file = "code_output/geos1/map_values.Rdata")



# Store predictions in data frame format
df.template <- data.frame(lon = afr.locs[,1],
                          lat = afr.locs[,2],
                          pixel = seq(afr.locs[,1]),
                          year = NA,
                          r = NA)
df.predicted <- data.frame()
df.annual_estimates <- c()
for(i in 1:16){
  
  housefilename <- paste("code_output/Households/HHW4_", yi, ".tif", sep = "")
  houseraster <- raster(housefilename)
    houseraster <- mask(houseraster, afri_main)
  
  #ntlfilename <- paste("code_output/z_covariates/zntl_", yi, ".tif", sep = "")
  #ntlraster <- raster(ntlfilename)
  #  ntlraster <- mask(ntlraster, afri_main)
  #ntl_mask <- !is.na(ntlraster[])
  
  df.i <- df.template
  df.i$year <- 1999 + i
  df.i$r[ntl_mask] <- map.values[[i]]
  df.i$house <- houseraster[]
    #df.i$house[ntl_mask] <- houseraster[ntl_mask]
  #df.i$r[df.i$house == 0] <- 0
  
  df.predicted <- rbind(df.predicted, df.i)
  
  df.annual_estimates[i] <- sum(df.i$r * df.i$house, na.rm = TRUE)/sum(df.i$house, na.rm = TRUE)
  
}
#save(df.predicted, file = "code_output/geos1/predicted_data.RData")

plot(subset(country_stats, iso3 %in% isosample)$r_mean, df.annual_estimates, pch = 16, col = "blue", ylim = c(0,1)) 
lines(0:1, 0:1, col = "red")

plot(2000:2015, subset(country_stats, iso3 %in% isosample)$r_mean, type = "l", col = "blue", ylim = c(0,1))
lines(2000:2015, df.annual_estimates, pch = 16, col = "red") 


plot(log(subset(country_stats, iso3 %in% isosample)$r_mean/(1-subset(country_stats, iso3 %in% isosample)$r_mean)), log(df.annual_estimates/(1-df.annual_estimates)), pch = 16, col = "blue") 
lines(-2:2, -2:2, col = "red")

#####################################
# ggplots
library(ggplot2)
library(ggthemes)
library(viridis)
library(animation)

for(i in 1:16){
#ggdraw <- function(i){
  pltyear <- 1999 + i
  newdf <- subset(df.predicted, !is.na(r) & year == pltyear)
  plt <- ggplot(df.predicted[!is.na(df.predicted$r) & df.predicted$year == pltyear,], aes(lon, lat)) +
      geom_raster(aes(fill = r)) +
      coord_equal() +
      theme_map() +
      theme(legend.position = "bottom", legend.key.width = unit(2, "cm")) +
      scale_fill_viridis(limits = c(0, 1), guide = guide_colorbar(title = paste("electricity access", pltyear, sep = " ")))
  #ggsave(filename = paste("code_output/geos1/access_", pltyear, ".png", sep = ""), plt)
  print(plt)
}


graphics.off()
# Raster files
template <- rep(NA, ntlraster@nrows * ntlraster@ncols)
for(i in 1:16){
  eaccess <- template
  eaccess[ntl_mask] <- map.values[[i]]
  #eaccess[zero_mask[[i]]] <- 0 # Zero probability to areas with no households
  eaccess <- matrix(eaccess,
                    ncol = ntlraster@ncols,
                    nrow = ntlraster@nrows,
                    byrow = TRUE)
  eaccess <- raster(eaccess,
                    xmn = ntlraster@extent@xmin,
                    xmx = ntlraster@extent@xmax,
                    ymn = ntlraster@extent@ymin,
                    ymx = ntlraster@extent@ymax,
                    crs = ntlraster@crs)
  pltyear <- 1999 + i
  #filename <- paste("code_output/geos1/electricity_access_", pltyear, sep = "")
  #writeRaster(eaccess, filename, format = "GTiff", overwrite = TRUE)
}

