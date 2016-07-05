

source("code/inla_dataformat.R")


# Predictor
predictor <- y ~ -1 + intercept + ntl + population + year +
                  f(u.field, model = afr.spde) +
                  f(epsilon, model = "iid")
        

# Index of spatial points in the training set
u.f <- inla.spde.make.index(name = "u.field", n.spde = afr.spde$n.spde)

# A matrix & stack for training sample
A.obsv <- inla.spde.make.A(mesh = mesh.s,
                           loc = as.matrix(meta$points$spatial))

stack.obsv <- inla.stack(data = list(y = survey.data.agg$has_electricity),
                         A = list(A.obsv, 1, 1, 1, 1),
                         effects = list(c(u.f, list(intercept = 1)),
                                        list(ntl = survey.data.agg$ntl),
                                        list(population = survey.data.agg$pop2010),
                                        list(year = survey.data.agg$z.year),
                                        list(epsilon = 1:meta$num$data)),
                         tag = "obsv")


stack.all <- do.call(inla.stack, list(stack.obsv))

meta$ix$stack <- list()
meta$ix$stack$obsv <- inla.stack.index(stack.all, tag = "obsv")$data

# Train model
m <- inla(predictor, 
          data = inla.stack.data(stack.all),
          family = "binomial",
          Ntrials = survey.data.agg$total,
          control.predictor = list(A = inla.stack.A(stack.all),
                                   compute = TRUE),
          control.compute = list(cpo = TRUE, dic = TRUE, config = TRUE))

summary(m)