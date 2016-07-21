# Geostatistical model
# --------------------
#
# Edited: July 21, 2016


graphics.off()
rm(list = ls())
source("code/inla_preliminaries.R")

# Predictor
predictor <- y ~ -1 + intercept +
                  year +
                  population +
                  ntl +
                  f(u.field, model = afr.spde) + 
                  f(epsilon, model = "iid", hyper = list(theta = list(prior = "loggamma", param = c(1, .01))))
        

# Index of spatial points in the training set
u.f <- inla.spde.make.index(name = "u.field", n.spde = afr.spde$n.spde)


# A matrix & stack for training sample
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



if(TRUE){
  load("code_output/other_data_w_covariates.RData")
  # A matrix & stack for test sample
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
  
  stack.all <- do.call(inla.stack, list(stack.obsv, stack.test))
  meta$ix$stack$test <- inla.stack.index(stack.all, tag = "test")$data
  num.test <- nrow(df.test)
  
}else{
  
  stack.all <- do.call(inla.stack, list(stack.obsv))
  num.test <- 0
  
}

meta$ix$stack$obsv <- inla.stack.index(stack.all, tag = "obsv")$data



# Train model
m <- inla(predictor, 
          data = inla.stack.data(stack.all),
          family = "binomial",
          Ntrials = c(df$total, rep(1, num.test)),
          control.predictor = list(A = inla.stack.A(stack.all),
                                   compute = TRUE),
          control.compute = list(dic = TRUE))
summary(m)


# Marginal predictions
predicted.train.mean <- c()
for(i in seq(nrow(df))){
  predicted.train.mean[i] <- inla.emarginal(inla.link.invlogit,
                                            m$marginals.linear.predictor[meta$ix$stack$obsv][[i]] )
}
plot(df$r, predicted.train.mean )


predicted.test.mean <- c()
for(i in seq(nrow(df.test))){
  predicted.test.mean[i] <- inla.emarginal(inla.link.invlogit,
                                            m$marginals.linear.predictor[meta$ix$stack$test][[i]] )
}
df.test$r <- df.test$electricity
plot(df.test$r, predicted.test.mean )


#m11 <- sum(df.test$r <= .5 & predicted.test.mean <= .5)
#m22 <- sum(df.test$r > .5 & predicted.test.mean > .5)
#m21 <- sum(df.test$r <= .5 & predicted.test.mean > .5)
#m12 <- sum(df.test$r > .5 & predicted.test.mean <= .5)
(m11 + m22)/(m11 + m22 + m21 + m12)
plot(m)

m_core <- m
meta_core <- meta_core
  
save(m_core, meta_core, "code_output/geos1/core_train_test.RData")