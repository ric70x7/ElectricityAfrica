# Geostatistical model
# --------------------
#
# Edited: July 22, 2016
# This is the core file (train and test with non obfuscated data)


graphics.off()
rm(list = ls())
source("code/inla_preliminaries.R")
load("code_output/prediction_locations.RData")


# Predictor
predictor <- y ~ -1 + intercept +
                  year +
                  population +
                  ntl +
                  f(u.field, model = afr.spde) + 
                  f(epsilon, model = "iid", hyper = list(theta = list(prior = "loggamma", param = c(1, .01))))
        

# Index of spatial points in the training set
u.f <- inla.spde.make.index(name = "u.field", n.spde = afr.spde$n.spde)


# Matrix A & stack for training sample
A.obsv <- inla.spde.make.A(mesh = mesh.s,
                           loc = as.matrix(meta$points$spatial))

stack.obsv <- inla.stack(data = list(y = df$has_electricity),
                         A = list(A.obsv, 1, 1, 1, 1),
                         effects = list(c(u.f, list(intercept = 1)),
                                        list(ntl = df$z.ntl),
                                        list(population = df$z.pop2010),
                                        list(year = df$z.year),
                                        list(epsilon = 1:meta$num$data)),
                         tag = "obsv")


# matrix A & stack for test sample
load("code_output/other_data_w_covariates.RData")
A.test <- inla.spde.make.A(mesh = mesh.s,
                           loc = as.matrix(df.test[, c("lon", "lat")]))

stack.test <- inla.stack(data = list(y = NA),
                         A = list(A.test, 1, 1, 1, 1),
                         effects = list(c(u.f, list(intercept = 1)),
                                        list(ntl = df.test$z.ntl),
                                        list(population = df.test$z.pop2010),
                                        list(year = df.test$z.year),
                                        list(epsilon = meta$num$data + 1:nrow(df.test))),
                         tag = "test")
num.test <- nrow(df.test)


# stack for computing the latent variable at the mesh nodes
stack.latn <- inla.stack(data = list(y = NA),
                         A = list(1),
                         effects = list(c(u.f, list(intercept = 1))),
                         tag = "latn")
num.latn <- nrow(stack.latn$A)


# Join stacks
stack.all <- do.call(inla.stack, list(stack.obsv, stack.test, stack.latn))


# Indices to recover data
meta$ix$stack$obsv <- inla.stack.index(stack.all, tag = "obsv")$data
meta$ix$stack$test <- inla.stack.index(stack.all, tag = "test")$data
meta$ix$stack$latn <- inla.stack.index(stack.all, tag = "latn")$data


# Fit model
m <- inla(predictor, 
          data = inla.stack.data(stack.all),
          family = "binomial",
          Ntrials = c(df$total, rep(1, num.test), rep(1, num.latn)),
          control.predictor = list(A = inla.stack.A(stack.all),
                                   compute = TRUE),
          control.compute = list(dic = TRUE, config = TRUE))



# Save model
m_core <- m
meta_core <- meta
save(mesh.s, m_core, meta_core, file = "code_output/geos1/core_latent_interpolation.RData")





## Marginal predictions
#predicted.train.mean <- c()
#for(i in seq(nrow(df))){
#  predicted.train.mean[i] <- inla.emarginal(inla.link.invlogit,
#                                            m$marginals.linear.predictor[meta$ix$stack$obsv][[i]] )
#}
##plot(df$r, predicted.train.mean)
#
#
#predicted.test.mean <- c()
#for(i in seq(nrow(df.test))){
#  predicted.test.mean[i] <- inla.emarginal(inla.link.invlogit,
#                                            m$marginals.linear.predictor[meta$ix$stack$test][[i]] )
#}
#df.test$r <- df.test$electricity
##plot(df.test$r, predicted.test.mean)
#
#
#
#
#
#A.latn <- inla.spde.make.A(mesh = mesh.s,
#                           loc = as.matrix(df.latn[, c("lon", "lat")]))
#
#num.samples <- 1000
#post.samples <- inla.posterior.sample(n = num.samps, result = m)
#
#obj.names <- rownames(post.samples[[1]]$latent)
#u.batch <- grepl("u.field", obj.names)
#
#
#u.samples <- matrix(NA, nrow = num.samps, ncol = num.latn)
#for(i in 1:num.samps){
#  u.samples[i,] <- post.samps[[i]]$latent[u.batch]
#}
#
#u.mean <- apply(u.samples, 2, mean)
#new.field <-  A.latn %*% u.mean
#
#df.latn$gp <- as.vector(new.field) + m$summary.fixed["intercept", "mean"]
#
#
#
##df.latn$gp <- m$summary.fitted.values$mean[meta$ix$stack$latn]
#library(ggplot2)
#ggplot(df.latn, aes(lon, lat)) + geom_raster(aes(fill = gp)) +
#  geom_point(data = df.test, aes(lon, lat, col = r)) +
#  geom_point(data = df, aes(lon, lat, col = r))
#



#m11 <- sum(df.test$r <= .5 & predicted.test.mean <= .5)
#m22 <- sum(df.test$r > .5 & predicted.test.mean > .5)
#m21 <- sum(df.test$r <= .5 & predicted.test.mean > .5)
#m12 <- sum(df.test$r > .5 & predicted.test.mean <= .5)
#(m11 + m22)/(m11 + m22 + m21 + m12)
#plot(m)