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


num.samples <- 1000
post.samples <- inla.posterior.sample(n = num.samples, result = m_core)
hype.samples <- inla.hyperpar.sample(n = num.samples, result = m_core)

# Projector matrix to interpolate nodes in mesh.s
A.zero <- inla.spde.make.A(mesh = mesh.s, loc = as.matrix(df.test1[,c("lon", "lat")]))

obj.names <- rownames(post.samples[[1]]$latent)

beta.y.ix <- grepl("year", obj.names)
#beta.l.ix <- grepl("predictor_offset_lit", obj.names)
beta.d.ix <- grepl("predictor_offset_dark", obj.names)
beta.n.ix <- grepl("ntl", obj.names)
beta.h.ix <- grepl("pop", obj.names)
rand.u.ix <- grepl("u.field", obj.names)

beta.y.samples <- rep(NA, num.samples)
#beta.l.samples <- rep(NA, num.samples)
beta.d.samples <- rep(NA, num.samples)
beta.n.samples <- rep(NA, num.samples)
beta.h.samples <- hype.samples[,1]#rep(NA, num.samples)
rand.u.samples <- matrix(NA, nrow = num.samples, ncol = length(meta_core$ix$stack$latn))
  aux <- hype.samples[,4]#matrix(NA, nrow = num.samples, ncol = length(meta_core$ix$stack$latn))
for(i in 1:num.samples){
  beta.y.samples[i] <- post.samples[[i]]$latent[beta.y.ix]
#  beta.l.samples[i] <- post.samples[[i]]$latent[beta.l.ix]
  beta.d.samples[i] <- post.samples[[i]]$latent[beta.d.ix]
  beta.n.samples[i] <- post.samples[[i]]$latent[beta.n.ix]
  #beta.h.samples[i] <- post.samples[[i]]$latent[beta.h.ix]
  rand.u.samples[i,] <- post.samples[[i]]$latent[rand.u.ix]
}

# linear predictor
# latent variable
eta <- as.matrix(A.zero %*% t(rand.u.samples))
# year 
eta <- eta + t(matrix(rep(df.test1$z.year, ncol(eta)), nrow = ncol(eta), byrow = TRUE) * beta.y.samples)
# offsets
eta <- eta + t(matrix(rep(df.test1$country_logit_r * df.test1$lit, ncol(eta)), nrow = ncol(eta), byrow = TRUE) ) #* beta.l.samples
eta <- eta + t(matrix(rep(1 - df.test1$lit, ncol(eta)), nrow = ncol(eta), byrow = TRUE) * beta.d.samples)
# ntl
eta <- eta + t(matrix(rep(df.test1$zpositive.ntl, ncol(eta)), nrow = ncol(eta), byrow = TRUE) * beta.n.samples)
# pop
eta <- eta + t(matrix(rep(df.test1$zpositive.pop, ncol(eta)), nrow = ncol(eta), byrow = TRUE) * beta.h.samples)
# Noise
#eta <- matrix(rnorm(length(eta), mean = 0, 1/.1833), ncol = ncol(eta), nrow = nrow(eta))

#graphics.off()
plot(m_core$summary.fitted.values$mean[meta_core$ix$stack$test1], rowMeans(eta))
lines(c(-10,11), c(-10,11), col = "red", lw = 2)

#xx <- (-100:100)/10
#plot(m_core$summary.fitted.values$mean[meta_core$ix$stack$test1], predicted.test.mean)
#lines(xx, inla.link.invlogit(xx), col = "blue")

inv_logit_eta <- rowMeans(inla.link.invlogit(eta))
#inv_logit_eta <- rowMeans(inla.link.invlogit(eta.u + eta.y + eta.l + eta.d + eta.n))

plot(predicted.test.mean, inv_logit_eta)
lines(c(0,1), c(0,1), col = "red", lw = 2)
lines(c(0,1), c(0.5,.5), col = "gray")
lines(c(0.5,.5), c(0,1), col = "gray")
points(predicted.test.mean, inla.link.invlogit(rowMeans(eta)), col = "blue")
     