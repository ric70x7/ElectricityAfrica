# GLM
# ---
#
# Edited: July 20, 2016


graphics.off()
rm(list = ls())
source("code/inla_preliminaries.R")


# Predictor
predictor <- y ~ -1 + intercept +
                  year +
                  population +
                  ntl
        

# Stack for training sample
stack.obsv <- inla.stack(data = list(y = df$has_electricity),
                         A = list(1, 1, 1, 1),
                         effects = list(list(intercept = rep(1, nrow(df))),
                                        list(ntl = df$z.ntl),
                                        list(population = df$z.pop2010),
                                        list(year = df$z.year)),
                         tag = "obsv")
meta$ix$stack$obsv <- inla.stack.index(stack.all, tag = "obsv")$data

  
# Layer for predictions
pred.year <- 2014
if(pred.year <= 2013){
  yi <- pred.year
}else{
  yi <- 2013
}

# NTL
filename <- paste("data/ntl/Inland_water_masked_5k/ts", yi, "W_template.tif", sep = "") #5Km resolution
afr <- raster(filename)
afr.x <- getValues(afr)
afr.mask <- !is.na(afr.x) & afr.x < 100
afr.locs <- xyFromCell(afr, seq(afr))

# Population data
pop2010.raw <- raster("data/Africa-POP-2010_africa2010ppp/africa2010ppp.tif")
pop2010 <- resample(pop2010.raw, afr)


# Data frame of predictions
df.test.all <- data.frame(pixel = seq(afr)[afr.mask], lon = afr.locs[afr.mask, 1], lat = afr.locs[afr.mask, 2],
                      year = NA, ntl = NA, pop2010 = NA, r = NA) 
df.test.all$year <- pred.year # Do not put yi
df.test.all$ntl <- getValues(afr)[afr.mask]
df.test.all$pop2010 <- pop2010[afr.mask]
wtr.pixels <- df.test.all$pixel[is.na(df.test.all$pop2010)] 
df.test.all <- df.test.all[!is.na(df.test.all$pop2010),]

# Standardize/sacale data
load("code_output/z_params.RData")
df.test.all$z.year <- scale(df.test.all$year, center = center.year, scale = scale.year)
df.test.all$z.pop2010 <- log(1+df.test.all$pop2010)
df.test.all$z.ntl <- log(1+df.test.all$ntl)

# Batches
batches <- list()
bstart <- 1
bstep <- 50000
for(i in 1:29){
  batches <- c(batches, list(bstep * (i-1) + (bstart:bstep)))
}
batches <- c(batches, list( ((bstep * i) + bstart):nrow(df.test.all)))

# List to store predictions
predictions <- list()

for(i in 1:2){
  
  df.test <- df.test.all[batches[[i]],]
  
  stack.test <- inla.stack(data = list(y = NA),
                           A = list(1, 1, 1, 1),
                           effects = list(list(intercept = rep(1, nrow(df.test))),
                                          list(ntl = df.test$z.ntl),
                                          list(population = df.test$z.pop2010),
                                          list(year = df.test$z.year)),
                           tag = as.character(pred.year))
  
  stack.all <- do.call(inla.stack, list(stack.obsv, stack.test))
  meta$ix$stack$test <- inla.stack.index(stack.all, tag = as.character(pred.year))$data
  num.test <- nrow(df.test)
  
  
  # Train model
  m <- inla(predictor, 
            data = inla.stack.data(stack.all),
            family = "binomial",
            Ntrials = c(df$total, rep(1, num.test)),
            control.predictor = list(A = inla.stack.A(stack.all),
                                     compute = TRUE))#, control.inla = list(strategy = "laplace", npoints = 21),
            #verbose = TRUE)
  
  
  # Marginal predictions
  predicted.test.mean <- c()
  for(j in seq(nrow(df.test))){
    predicted.test.mean[j] <- inla.emarginal(inla.link.invlogit,
                                              m$marginals.linear.predictor[meta$ix$stack$test][[j]] )
  }
  predictions[[i]] <- data.frame(pixel = df.test$pixel, lon = df.test$lon, lat = df.test$lat, r = predicted.test.mean)
  
}
