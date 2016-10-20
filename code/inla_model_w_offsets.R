# Geostatistical model
# --------------------
#
# Edited: October 19, 2016
# This is the core file (train and test with non obfuscated data)


graphics.off()
rm(list = ls())

source("code/inla_preliminaries.R")
load("code_output/z_params.RData")
load("code_output/country_annual_estimates.RData")


# Predictor
predictor <- y ~ -1 + 
                  predictor_offset +
                  year +
                  ntl +
                  pop +
                  f(u.field, model = afr.spde) + 
                  f(epsilon, model = "iid", hyper = list(theta = list(prior = "loggamma", param = c(1, .01))))
        

# Index of spatial points in the training set
u.f <- inla.spde.make.index(name = "u.field", n.spde = afr.spde$n.spde)


# Matrix A & stack for training sample
A.train <- inla.spde.make.A(mesh = mesh.s,
                           loc = as.matrix(meta$points$spatial))

stack.train <- inla.stack(data = list(y = df.train$has_electricity),
                         A = list(A.train, 1, 1, 1, 1, 1),
                         effects = list(u.f,
                                        list(predictor_offset = df.train$logit_zero_offset),
                                        #list(predictor_offset = df.train$logit_min_offset),
                                        list(year = df.train$z.year),
                                        list(ntl = df.train$ntl),
                                        list(pop = df.train$pop),
                                        list(epsilon = 1:meta$num$data)),
                         tag = "train")


# matrix A & stack for test1 sample
A.test1 <- inla.spde.make.A(mesh = mesh.s,
                           loc = as.matrix(df.test1[, c("lon", "lat")]))

stack.test1 <- inla.stack(data = list(y = NA),
                         A = list(A.test1, 1, 1, 1, 1, 1),
                         effects = list(u.f,
                                        list(predictor_offset = df.test1$logit_zero_offset),
                                        #list(predictor_offset = df.test1$logit_min_offset),
                                        list(year = df.test1$z.year),
                                        list(ntl = df.test1$ntl),
                                        list(pop = df.test1$pop),
                                        list(epsilon = meta$num$data + 1:nrow(df.test1))),
                         tag = "test1")
num.test1 <- nrow(df.test1)


## matrix A & stack for test2 sample
A.test2 <- inla.spde.make.A(mesh = mesh.s,
                           loc = as.matrix(df.test2[, c("lon", "lat")]))

stack.test2 <- inla.stack(data = list(y = NA),
                         A = list(A.test2, 1, 1, 1, 1, 1),
                         effects = list(u.f,
                                        list(predictor_offset = df.test2$logit_zero_offset),
                                        #list(predictor_offset = df.test2$logit_min_offset),
                                        list(year = df.test2$z.year),
                                        list(ntl = df.test2$ntl),
                                        list(pop = df.test2$pop),
                                        list(epsilon = meta$num$data + num.test1 + 1:nrow(df.test2))),
                         tag = "test2")
num.test2 <- nrow(df.test2)


# stack for computing the latent variable at the mesh nodes
stack.latn <- inla.stack(data = list(y = NA), A = list(1), effects = list(u.f), tag = "latn")
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
save(mesh.s, m_core, meta_core, file = "code_output/core_model_fit.RData")
save(df.train, df.test1, df.test2, file = "code_output/core_model_data.RData")
