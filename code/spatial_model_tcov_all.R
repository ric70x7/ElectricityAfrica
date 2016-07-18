graphics.off()
rm(list = ls())
source("code/inla_dataformat_all.R")


# Predictor
predictor <- y ~ -1 + intercept +
                  year +
                  f(population, model = "sigm") +#population + 
                  f(ntl, model = "sigm") +#ntl + 
                  f(u.field, model = afr.spde) + 
                  f(epsilon, model = "iid", hyper = list(theta = list(prior = "loggamma", param = c(1, .01))))
        

# Index of spatial points in the training set
u.f <- inla.spde.make.index(name = "u.field", n.spde = afr.spde$n.spde)#, n.group = mesh.t$n)


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


stack.all <- do.call(inla.stack, list(stack.obsv))

meta$ix$stack <- list()
meta$ix$stack$obsv <- inla.stack.index(stack.all, tag = "obsv")$data


# Train model
m <- inla(predictor, 
          data = inla.stack.data(stack.all),
          family = "binomial",
          Ntrials = df$total,
          control.predictor = list(A = inla.stack.A(stack.all),
                                   compute = TRUE),
          control.compute = list(cpo = TRUE, dic = TRUE, config = TRUE))

summary(m)



hist(m$cpo$pit)
pvals <- c()
for(i in seq(nrow(df))){
  pvals[i] <- inla.pmarginal(q = df$r[i], marginal = m$marginals.fitted.values[[i]])
}
hist(pvals)



# Marginal predictions
predicted.train.mean <- c()
for(i in seq(nrow(df))){
  predicted.train.mean[i] <- inla.emarginal(inla.link.invlogit,
                                            m$marginals.linear.predictor[meta$ix$stack$obsv][[i]] )
}
plot(df$r, predicted.train.mean )


save(m, df, predictor, meta, file = "code_output/firstresults.RData")