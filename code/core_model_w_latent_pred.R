# Geostatistical model
# --------------------
#
# Edited: September 14, 2016
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
A.train <- inla.spde.make.A(mesh = mesh.s,
                           loc = as.matrix(meta$points$spatial))

stack.train <- inla.stack(data = list(y = df.train$has_electricity),
                         A = list(A.train, 1, 1, 1, 1),
                         effects = list(c(u.f, list(intercept = 1)),
                                        list(ntl = df.train$z.ntl),
                                        list(population = df.train$z.pop),
                                        list(year = df.train$z.year),
                                        list(epsilon = 1:meta$num$data)),
                         tag = "train")


# matrix A & stack for test1 sample
#load("code_output/other_data_w_covariates.RData")
A.test1 <- inla.spde.make.A(mesh = mesh.s,
                           loc = as.matrix(df.test1[, c("lon", "lat")]))

stack.test1 <- inla.stack(data = list(y = NA),
                         A = list(A.test1, 1, 1, 1, 1),
                         effects = list(c(u.f, list(intercept = 1)),
                                        list(ntl = df.test1$z.ntl),
                                        list(population = df.test1$z.pop),
                                        list(year = df.test1$z.year),
                                        list(epsilon = meta$num$data + 1:nrow(df.test1))),
                         tag = "test1")
num.test1 <- nrow(df.test1)


# matrix A & stack for test2 sample
A.test2 <- inla.spde.make.A(mesh = mesh.s,
                           loc = as.matrix(df.test2[, c("lon", "lat")]))

stack.test2 <- inla.stack(data = list(y = NA),
                         A = list(A.test2, 1, 1, 1, 1),
                         effects = list(c(u.f, list(intercept = 1)),
                                        list(ntl = df.test2$z.ntl),
                                        list(population = df.test2$z.pop),
                                        list(year = df.test2$z.year),
                                        list(epsilon = meta$num$data + 1:nrow(df.test2))),
                         tag = "test2")
num.test2 <- nrow(df.test2)


# stack for computing the latent variable at the mesh nodes
stack.latn <- inla.stack(data = list(y = NA),
                         A = list(1),
                         effects = list(c(u.f, list(intercept = 1))),
                         tag = "latn")
num.latn <- nrow(stack.latn$A)


# Join stacks
stack.all <- do.call(inla.stack, list(stack.train, stack.test1, stack.test2, stack.latn))


# Indices to recover data
meta$ix$stack$train <- inla.stack.index(stack.all, tag = "train")$data
meta$ix$stack$test1 <- inla.stack.index(stack.all, tag = "test1")$data
meta$ix$stack$test2 <- inla.stack.index(stack.all, tag = "test2")$data
meta$ix$stack$latn <- inla.stack.index(stack.all, tag = "latn")$data


# Fit model
m <- inla(predictor, 
          data = inla.stack.data(stack.all),
          family = "binomial",
          Ntrials = c(df.train$total, rep(1, num.test1), rep(1, num.test2), rep(1, num.latn)),
          control.predictor = list(A = inla.stack.A(stack.all),
                                   compute = TRUE),
          control.compute = list(dic = TRUE, config = TRUE))



# Save model
m_core <- m
meta_core <- meta
save(mesh.s, m_core, meta_core, file = "code_output/geos1/new_core_latent_interpolation_prelim.RData")
save(df, df.test, file = "code_output/geos1/new_core_datasets_prelim.RData")
