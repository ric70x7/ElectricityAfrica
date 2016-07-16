graphics.off()
rm(list = ls())
#source("code/inla_dataformat.R")
source("code/inla_dataformat_tzz.R")


# Predictor
predictor <- y ~ -1 + intercept +
                  year +
                  population + #f(population, model = "sigm") +
                  ntl + #f(ntl, model = "sigm") +
                  f(u.field, model = afr.spde) + 
                  #f(u.field, model = afr.spde,
                  # group = u.field.group,
                  # control.group = list(model = "rw1")) #+
                  f(epsilon, model = "iid", hyper = list(theta = list(prior = "loggamma", param = c(1, .01))))
        

# Index of spatial points in the training set
u.f <- inla.spde.make.index(name = "u.field", n.spde = afr.spde$n.spde)#, n.group = mesh.t$n)


# A matrix & stack for training sample
A.obsv <- inla.spde.make.A(mesh = mesh.s,
                           loc = as.matrix(meta$points$spatial))#,
                           #group = meta$ix$time.order,
                           #group.mesh = mesh.t)


stack.obsv <- inla.stack(data = list(y = df$has_electricity),
                         A = list(A.obsv, 1, 1, 1, 1),
                         effects = list(c(u.f, list(intercept = 1)),
                                        #list(ntl = df$ntl),
                                        list(ntl = df$z.ntl),
                                        list(population = df$z.pop2010),
                                        list(year = df$z.year),
                                        list(epsilon = 1:meta$num$data)),
                         tag = "obsv")


# A matrix & stack for test sample
A.test <- inla.spde.make.A(mesh = mesh.s,
                           loc = as.matrix(test.survey[, c("lon", "lat")]))#,
                           #group = test.survey$year - min(test.survey$year) + 1,
                           #group.mesh = test.mesh.t)

stack.test <- inla.stack(data = list(y = NA),
                         A = list(A.test, 1, 1, 1, 1),
                         effects = list(c(u.f, list(intercept = 1)),
                                        #list(ntl = test.survey$ntl),
                                        list(ntl = test.survey$z.ntl),
                                        list(population = test.survey$z.pop2010),
                                        list(year = test.survey$z.year),
                                        list(epsilon = 1:nrow(test.survey))),
                         tag = "test")


stack.all <- do.call(inla.stack, list(stack.obsv, stack.test))

meta$ix$stack <- list()
meta$ix$stack$obsv <- inla.stack.index(stack.all, tag = "obsv")$data
meta$ix$stack$test <- inla.stack.index(stack.all, tag = "test")$data

# Train model
m <- inla(predictor, 
          data = inla.stack.data(stack.all),
          family = "binomial",
          #family = "zeroinflatedbinomial0",
          Ntrials = c(df$total, rep(1, nrow(test.survey))),
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

predicted.test.mean <- c()
for(i in seq(nrow(test.survey))){
  predicted.test.mean[i] <- inla.emarginal(inla.link.invlogit,
                                            m$marginals.linear.predictor[meta$ix$stack$test][[i]] )
}
plot(test.survey$r, predicted.test.mean )

m11 <- sum(test.survey$r <= .5 & predicted.test.mean <= .9)
m22 <- sum(test.survey$r > .5 & predicted.test.mean > .9)

m21 <- sum(test.survey$r <= .5 & predicted.test.mean > .9)
m12 <- sum(test.survey$r > .5 & predicted.test.mean <= .9)

(m11 + m22)/(m11 + m22 + m21 + m12)

plot(df$z.ntl,inla.link.invlog(df$r))
#ggplot(subset(df, year == 2009), aes(lon, lat)) + geom_point(aes(col = r), pch = 16)
#ggplot(subset(df, year == 2010), aes(lon, lat)) + geom_point(aes(col = r), pch = 16)
#ggplot(subset(df, year == 2011), aes(lon, lat)) + geom_point(aes(col = r), pch = 16)
