# Maps form core_model
# ------------------------
#
# Edited: October 19, 2016


library(INLA)
library(raster)
library(parallel)
library(doParallel)
rm(list = ls())

load("code_output/core_model_fit.RData")
load("code_output/core_model_data.RData")
load("code_output/country_annual_estimates.RData")


# Prediction grid and covarirates values
num.layers <- 16

# Whole grid
#ntl2010 <- raster(paste("data/ntl/Inland_water_masked_5k/ts2010W_template.tif", sep = ""))
ntl2010 <- raster(paste("data/ntl/GP2_Africa_5k/GP2_Africa_2010.tif", sep = ""))
afr.locs <- xyFromCell(ntl2010, seq(ntl2010))
  
# Covariates
z.year <- 1:16
r_country_logit <- list()
dark_offset <- list()
z.ntl <- list()
z.pop <- list()
zero_mask <- list()
for(i in 1:num.layers){
  
  yi <- 1999 + i
  
  # Files
  #offsetfilename <- paste("code_output/z_covariates/logit_zero_offset", yi, ".tif", sep = "")
  #offsetfilename <- paste("code_output/z_covariates/logit_min_offset", yi, ".tif", sep = "")
  offsetfilename <- paste("code_output/z_covariates/logit_r_", yi, ".tif", sep = "")
  litfilename <- paste("code_output/z_covariates/lit_", yi, ".tif", sep = "")
  
  #ntlfilename <- paste("data/ntl/Inland_water_masked_5k/ts", min(2013,yi), "w_template.tif", sep = "")
  #popfilename <- paste("code_output/Population/GPW4_", yi, ".tif", sep = "")
  ntlfilename <- paste("code_output/z_covariates/zpositive_ntl_", yi, ".tif", sep = "")
  popfilename <- paste("code_output/z_covariates/zpositive_pop_", yi, ".tif", sep = "")
  
  # Data layers
  offraster <- raster(offsetfilename)
  litraster <- raster(litfilename)
  ntlraster <- raster(ntlfilename)
  popraster <- raster(popfilename)
    
  ntl_mask <- !is.na(ntlraster[])
  r_country_logit[[i]] <- offraster[ntl_mask] * litraster[ntl_mask]
  dark_offset[[i]] <- 1 - litraster[ntl_mask]  
  z.ntl[[i]] <- ntlraster[ntl_mask]
  z.pop[[i]] <- popraster[ntl_mask]
}


# Posterior samples
num.samples <- 1000#500# FIXME: change to 10000
post.samples <- inla.posterior.sample(n = num.samples, result = m_core)
hype.samples <- inla.hyperpar.sample(n = num.samples, result = m_core)

# Organize samples
obj.names <- rownames(post.samples[[1]]$latent)

beta.y.ix <- grepl("year", obj.names)
#beta.l.ix <- grepl("predictor_offset_lit", obj.names)
beta.d.ix <- grepl("predictor_offset_dark", obj.names)
beta.n.ix <- grepl("ntl", obj.names)
#beta.h.ix <- grepl("pop", obj.names)
rand.u.ix <- grepl("u.field", obj.names)

beta.y.samples <- rep(NA, num.samples)
#beta.l.samples <- rep(NA, num.samples)
beta.d.samples <- rep(NA, num.samples)
beta.n.samples <- rep(NA, num.samples)
beta.h.samples <- hype.samples[,1]#rep(NA, num.samples)
rand.u.samples <- matrix(NA, nrow = num.samples, ncol = length(meta_core$ix$stack$latn))
for(i in 1:num.samples){
  beta.y.samples[i] <- post.samples[[i]]$latent[beta.y.ix]
#  beta.l.samples[i] <- post.samples[[i]]$latent[beta.l.ix]
  beta.d.samples[i] <- post.samples[[i]]$latent[beta.d.ix]
  beta.n.samples[i] <- post.samples[[i]]$latent[beta.n.ix]
#  beta.h.samples[i] <- post.samples[[i]]$latent[beta.h.ix]
  rand.u.samples[i,] <- post.samples[[i]]$latent[rand.u.ix]
}


# Projector matrix to interpolate nodes in mesh.s
A.latn <- inla.spde.make.A(mesh = mesh.s, loc = as.matrix(afr.locs[ntl_mask,]))


# Function to recover a sample of inv.link.logit(eta)
boot.predictor <- function(slices, time.point){
  # latent variable
  eta <- as.matrix(A.latn %*% t(rand.u.samples[slices,]))
  # year 
  eta <- eta + matrix(rep(z.year[[time.point]], nrow(eta) * ncol(eta)), ncol = ncol(eta)) * beta.y.samples[slices]
  # offsets
  eta <- eta + t(matrix(rep(r_country_logit[[time.point]], ncol(eta)), nrow = ncol(eta), byrow = TRUE)) # * beta.l.samples[slices]
  eta <- eta + t(matrix(rep(dark_offset[[time.point]], ncol(eta)), nrow = ncol(eta), byrow = TRUE) * beta.d.samples[slices])
  # ntl
  eta <- eta + t(matrix(rep(z.ntl[[time.point]], ncol(eta)), nrow = ncol(eta), byrow = TRUE) * beta.n.samples[slices])
  # pop
  eta <- eta + t(matrix(rep(z.pop[[time.point]], ncol(eta)), nrow = ncol(eta), byrow = TRUE) * beta.h.samples[slices])
  
  return(rowMeans(inla.link.invlogit(eta)))
}


# Definition of settings to parallelize
num.cores <- 10#5 # FIXME: change to 10
batch.size <- 100#100 # FIXME: change to 1000
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
  
  yi <- 1999 + i
  map.values[[i]] <- rowMeans(expected)
}


# Raster files
template <- rep(NA, ntlraster@nrows * ntlraster@ncols)
for(i in 1:16){
  eaccess <- template
  eaccess[ntl_mask] <- map.values[[i]]
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
  filename <- paste("code_output/Electricity/access_", pltyear, sep = "")
  writeRaster(eaccess, filename, format = "GTiff", overwrite = TRUE)
}

