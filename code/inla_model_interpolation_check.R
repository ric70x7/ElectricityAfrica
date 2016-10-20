# Check INLA model posterior approximation based on latent field interpolation
# ----------------------------------------------------------------------------
#
# Edited: October 19, 2016

library(INLA)
library(ggplot2)
rm(list = ls())

load("code_output/core_model_fit.RData")
load("code_output/core_model_data.RData")
#myblue <-"#6495ED"
#mygreen <- "#86C67C"
#myred <- "#EE6A50"
#font_size <- 15


predicted.train.mean <- c()
for(i in seq(nrow(df.train))){
  predicted.train.mean[i] <- inla.emarginal(inla.link.invlogit,
                                           m_core$marginals.linear.predictor[meta_core$ix$stack$train][[i]] )
}

predicted.test.mean <- c()
for(i in seq(nrow(df.test1))){
  predicted.test.mean[i] <- inla.emarginal(inla.link.invlogit,
                                           m_core$marginals.linear.predictor[meta_core$ix$stack$test1][[i]] )
}


num.samples <- 3000
post.samples <- inla.posterior.sample(n = num.samples, result = m_core)

# Projector matrix to interpolate nodes in mesh.s
A.zero <- inla.spde.make.A(mesh = mesh.s, loc = as.matrix(df.test1[,c("lon", "lat")]))

# Organize samples
obj.names <- rownames(post.samples[[1]]$latent)
beta.y.ix <- grepl("year", obj.names)
rand.u.ix <- grepl("u.field", obj.names)
beta.n.ix <- grepl("ntl", obj.names)
beta.h.ix <- grepl("pop", obj.names)
beta.y.samples <- rep(NA, num.samples)
beta.n.samples <- rep(NA, num.samples)
beta.h.samples <- rep(NA, num.samples)
rand.u.samples <- matrix(NA, nrow = num.samples, ncol = length(meta_core$ix$stack$latn))
for(i in 1:num.samples){
  beta.y.samples[i] <- post.samples[[i]]$latent[beta.y.ix]
  beta.n.samples[i] <- post.samples[[i]]$latent[beta.n.ix]
  beta.h.samples[i] <- post.samples[[i]]$latent[beta.h.ix]
  rand.u.samples[i,] <- post.samples[[i]]$latent[rand.u.ix]
}


# linear predictor
# latent variable
eta <- as.matrix(A.zero %*% t(rand.u.samples))
# offset
eta <- eta + t(matrix(rep(df.test1$logit_zero_offset, ncol(eta)), nrow = ncol(eta), byrow = TRUE))
# year 
eta <- eta + t(matrix(rep(df.test1$z.year, ncol(eta)), nrow = ncol(eta), byrow = TRUE) * beta.y.samples)
# ntl
eta <- eta + t(matrix(rep(df.test1$z.ntl, ncol(eta)), nrow = ncol(eta), byrow = TRUE) * beta.n.samples)
# households
eta <- eta + t(matrix(rep(df.test1$z.pop, ncol(eta)), nrow = ncol(eta), byrow = TRUE) * beta.h.samples)
# Noise
#eta <- eta + matrix(rnorm(length(eta), mean = 0, 1/.1833), ncol = ncol(eta), nrow = nrow(eta))

plot(m_core$summary.fitted.values$mean[meta_core$ix$stack$test1], rowMeans(eta))
lines(c(-10,11), c(-10,11), col = "red", lw = 2)

xx <- (-100:100)/10
plot(m_core$summary.fitted.values$mean[meta_core$ix$stack$test1], predicted.test.mean)
lines(xx, inla.link.invlogit(xx), col = "blue")

inv_logit_eta <- rowMeans(inla.link.invlogit(eta))
plot(predicted.test.mean, inv_logit_eta)
lines(c(0,1), c(0,1), col = "red", lw = 2)
lines(c(0,1), c(0.5,.5), col = "gray")
lines(c(0.5,.5), c(0,1), col = "gray")
#points(predicted.test.mean, inla.link.invlogit(rowMeans(eta)), col = "blue")
     