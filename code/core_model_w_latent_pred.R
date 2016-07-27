# Geostatistical model
# --------------------
#
# Edited: July 26, 2016
# This is the core file (train and test with non obfuscated data)


graphics.off()
rm(list = ls())
source("code/inla_preliminaries.R")
#load("code_output/prediction_locations.RData")


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
                                        list(population = df$z.pop),
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
                                        list(population = df.test$z.pop),
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
save(df, df.test, file = "code_output/geos1/core_datasets.RData")
