# GLM
# ---
#
# Edited: July 20, 2016


graphics.off()
rm(list = ls())
source("code/inla_preliminaries.R")
plot(mesh.s)
points(df$lon, df$lat, pch = 16, col = "blue", cex = 1)

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


if(split.data){
  
  stack.test <- inla.stack(data = list(y = NA),
                           A = list(1, 1, 1, 1),
                           effects = list(list(intercept = rep(1, nrow(df.test))),
                                          list(ntl = df.test$z.ntl),
                                          list(population = df.test$z.pop2010),
                                          list(year = df.test$z.year)),
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
          control.compute = list(cpo = TRUE, dic = TRUE, config = TRUE))
#          control.inla = list(strategy = "laplace", npoints = 21))

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
plot(df.test$r, predicted.test.mean )

m11 <- sum(df.test$r <= .5 & predicted.test.mean <= .9)
m22 <- sum(df.test$r > .5 & predicted.test.mean > .9)
m21 <- sum(df.test$r <= .5 & predicted.test.mean > .9)
m12 <- sum(df.test$r > .5 & predicted.test.mean <= .9)

(m11 + m22)/(m11 + m22 + m21 + m12)

plot(m)