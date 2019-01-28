# Geostatistic model predictions
# ------------------------------

graphics.off()
rm(list = ls())
####
#library(raster)
#library(maptools)
#load("code_output/merged_data2.RData")
#load("code_output/points_in_mainland.RData")
#
## Find locations outside africa Mainland and Madagascar
#afri_main <- rgdal::readOGR("data/Africa_main_country/Africa_main_country.shp")
#afri_border <- unionSpatialPolygons(afri_main, rep(1, nrow(afri_main)))
#df <- df[keep,]
#
#years <- 2000:2015
#for(i in seq(years)){
#  yi <- years[i]
#  mask <- df$year == yi
#  if (sum(mask) > 0) {
#    filename <- paste("data/lulc_resampled/impervious_dist_", yi, ".tif", sep = "") #5Km resolution
#    afr <- raster(filename)
#    df$dist2impervious[mask] <- extract(afr, df[mask, c("lon", "lat")], buffer=5000, fun = function(x) {mean(x, na.rm=TRUE)})
#  }
#}
#
## Transformed distance
#dist2scale <- sd(df$dist2impervious, na.rm = TRUE) #0.2811558
#df$kz <- exp(- df$dist2impervious / sd(df$dist2impervious, na.rm = TRUE))
#df$kz[is.na(df$kz)] <- 0
#source("code_output/df_model.RData")

source("code/5_inla_mesh.R")
load("code_output/validation_data.RData")
beta_fixed <- m_6$summary.fixed$mean
beta_names <- m_6$names.fixed

# rename
lulc <- rep(NA, nrow(df.train))

for (i in 1:7) {
  lulc[df.train$ctyp == i] <- beta_fixed[i]
}

df.train$Z <- df.train$udis * beta_fixed[8] +
              df.train$ntlg * beta_fixed[9] +
              df.train$fimp * beta_fixed[10] +
              df.train$year * beta_fixed[11] +
              lulc

df.train <- df.train[1:2000,]

predictor_z <- y ~ -1 + offset(Z) +
  f(u.field, model=afr.spde)#, hyper = list(theta=list(param=m_6$summary.hyperpar$mean, fixed=TRUE)))

u.f <- inla.spde.make.index(name = "u.field", n.spde = afr.spde$n.spde) # random field model 5 and 6
A_z <- inla.spde.make.A(mesh = mesh.s, loc = as.matrix(df.train[, c("lon", "lat")])) # A model 6

stack_z <- inla.stack(data = list(y = df.train$y),
                      A = list(A_z, 1),
                      effects = list(u.f, list(Z = df.train$Z)),
                      tag = "train")

stack_latn <- inla.stack(data = list(y = NA),
                         A = list(1),
                         effects = list(u.f),
                         tag = "latn")

# Join stacks
stack_model <- do.call(inla.stack, list(stack_z, stack_latn))
ix_train <- inla.stack.index(stack_model, tag="train")
ix_latn <- inla.stack.index(stack_model, tag="latn")

# Fit model
model <- inla(predictor_z,  data = inla.stack.data(stack_model), family = "binomial",
              Ntrials = c(df.train$total, rep(1, nrow(stack_latn$A))),
              control.predictor = list(A=inla.stack.A(stack_model) , compute=TRUE),
              control.inla = list(h=.03),
              control.compute = list(config = TRUE))

#save(model, file="code_output/model_predictions.RData")
load("code_output/model_predictions.RData")
#source("code/5_inla_mesh.R")
sigma_u <- 0.2811558

cnst <- list()
for (i in 1:14) {
  yi <- i + 1999
  ntlg <- raster(paste0("data/ntl/GP2_Africa_5k/GP2_Africa_", yi, ".tif"))
  ntlg[ntlg[] > 100] <- NA
  ctyp <- raster(paste0("data/lulc_resampled/7class", yi, ".tif"))
  for (j in 1:7) {
    ctyp[ctyp[] == j] <- beta_fixed[j]
  }
  fimp <- raster(paste0("data/lulc_resampled/prob", yi, ".tif"))
  mask <- !is.na(ntlg[]) & !is.na(ctyp[]) & !is.na(fimp[])
  udis <- raster(paste0("data/lulc_resampled/impervious_dist_", yi, ".tif"))
  udis[mask] <- exp(-udis[mask]/sigma_u)
  udis[is.na(udis)] <- 0
  udis[!mask] <- NA

  new_cnst <- ctyp[mask] + beta_fixed[8] * udis[mask] +
    beta_fixed[9] * ntlg[mask] + beta_fixed[10] * fimp[mask] + beta_fixed[11] * i

  cnst[[i]] <- new_cnst
}

# Projector matrix to interpolate nodes in mesh.s
afr_mask <- xyFromCell(ntlg, (1:length(mask))[mask])
A_mask <- inla.spde.make.A(mesh = mesh.s, loc = as.matrix(afr_mask))

num_samples <- 200
post_samples <- inla.posterior.sample(n = num_samples, result = model)
obj_names <- rownames(post_samples[[1]]$latent)
rand_u_ix <- grepl("u.field", obj_names)
save(cnst, post_samples, obj_names, rand_u_ix, file = "code_output/eta_simulations.RData")

empty_layer <- ntlg
empty_layer[] <- NA
eta_samples <- matrix(NA, nrow = nrow(A_mask) , ncol = num_samples)

for (i in 1:14) {
  aux <- rep(0, nrow(A_mask))
  for (j in 1:num_samples) {
    aux <- aux + 1 / (1 + exp(-as.numeric(A_mask %*% post_samples[[j]]$latent[rand_u_ix] + cnst[[i]])))
  }
  eta_samples[,i] <- aux/num_samples
  empty_layer[mask] <- eta_samples[, i]
  names(empty_layer) <- paste0("Electricity_", i + 1999)
  filename <- paste0("code_output/Electricity/electricity_", i + 1999, sep = "")
  writeRaster(empty_layer, filename, format = "GTiff", overwrite = TRUE)
}

raster::plot(empty_layer)
