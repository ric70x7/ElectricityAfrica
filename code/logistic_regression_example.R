# GAMs vs GPs with mean prior fitted by a GAM

library(rstan)
library(ggplot2)

rstan_options(aut_write = TRUE)
options(mc.cores = parallel::detectCores())

graphics.off()
rm(list = ls())


# This is the lantent function
# Raw signals
f1 <- function(x){
  return(sin(x/9 * pi))
}

f2 <- function(x){
  return(3*log(x/7.5 * 1.4 + 2))
}

fx <- function(x){
  return(5 + f1(x) + f2(x))
}

# Generate data
x.obsv <- sort(runif(40, 0, 30))
x.pred <- 0:40
epsilon = rnorm(length(x.obsv), 0, .2)
obsv.df <- data.frame(x = x.obsv,
                 fx = fx(x.obsv),
                 epsilon = epsilon,
                 y = round(5 + f1(x.obsv) + f2(x.obsv) + epsilon))

pred.df <- data.frame(x = x.pred,
                      fx = fx(x.pred))

ggplot() +
  geom_point(data = obsv.df, aes(x, y), size = 2, color = "steelblue") + 
  geom_line(data = pred.df, aes(x, fx), size = .8, color = "black", linetype = 2) 


# Fit an predict GAM
mgcv_gam <- mgcv::gam(y ~ s(x, bs = 'cr', sp = .9), data = obsv.df, method="REML")
mgcv_pred <- predict(mgcv_gam, newdata = pred.df, se.fit = TRUE)
pred.df$gam.mean <- mgcv_pred$fit 
pred.df$gam.uci <- mgcv_pred$fit + 1.96 * mgcv_pred$se.fit
pred.df$gam.lci <- mgcv_pred$fit - 1.96 * mgcv_pred$se.fit



# STAN -> hyperparameters estimation
input_dim <- 1
gam_obsv_mean <- mgcv_gam$fitted.values

hyper.stan <- stan(file="gp_noiseless_hyper.stan",
                   data=list(X_data = matrix(obsv.df$x,ncol = input_dim),
                             Y_data = obsv.df$y,
                             input_dim = input_dim,
                             num_data = nrow(obsv.df),
                             M_prior_data = gam_obsv_mean),
                   warmup = 300, iter = 1000, chains = 1)


hyper.samples <- extract(hyper.stan, permuted = TRUE)
rbf_lengthscale_sq_mean <- mean(hyper.samples$rbf_lengthscale_sq)
rbf_var_mean <- mean(hyper.samples$rbf_var)
offset_mean <- mean(hyper.samples$rbf_var)


#gp_var
plot(density(hyper.samples$rbf_var), xlab=expression(rbf_var), main="rbf_var distribution", col = "red")
curve(dgamma(x, shape = 8, rate = 1),  add=TRUE, col="blue", lty = 1)# Analytical prior parameter
legend(x="topright", col=c("blue", "red"), lty=c(1,1), bty="n", legend=c("Prior", "Sampling posterior"))

# Priors and posteriors
op <- par(mfrow=c(3,1))

#lengthscale_sq
plot(density(hyper.samples$rbf_lengthscale_sq), xlab=expression(rbf_lengthscale_sq), main="rbf_lengthscale_sq distribution", col = "red")
curve(dgamma(x, shape = 3, rate = 1),  add=TRUE, col="blue", lty = 1)# Analytical prior parameter
legend(x="topright", col=c("blue", "red"), lty=c(1,1), bty="n", legend=c("Prior", "Sampling posterior"))

#gp_var
plot(density(hyper.samples$rbf_var), xlab=expression(rbf_var), main="rbf_var distribution", col = "red")
curve(dgamma(x, shape = 8, rate = 1),  add=TRUE, col="blue", lty = 1)# Analytical prior parameter
legend(x="topright", col=c("blue", "red"), lty=c(1,1), bty="n", legend=c("Prior", "Sampling posterior"))

#offset
plot(density(hyper.samples$offset), xlab=expression(offset), main="offset distribution", col = "red")
curve(dgamma(x, shape = 8, rate = 1),  add=TRUE, col="blue", lty = 1)# Analytical prior parameter
legend(x="topright", col=c("blue", "red"), lty=c(1,1), bty="n", legend=c("Prior", "Sampling posterior"))


# STAN -> Y prediction
gam_pred_mean <- pred.df$gam.mean
gam_pred_mean[pred.df$x >= max(obsv.df$x)] <- gam_obsv_mean[nrow(obsv.df)] # Use estimate of last observation as prior
gp.stan <- stan(file="lgcp_posterior.stan",
                   data=list(X_data = matrix(obsv.df$x, ncol = input_dim),
                             X_pred = matrix(pred.df$x, ncol = input_dim),
                             Y_data = obsv.df$y,
                             input_dim = input_dim,
                             num_data = nrow(obsv.df),
                             num_pred = nrow(pred.df),
                             M_prior_data = gam_obsv_mean,
                             M_prior_pred = gam_pred_mean,
                             rbf_lengthscale_sq = rbf_lengthscale_sq_mean,
                             rbf_var = rbf_var_mean,
                             offset = offset_mean),
                   warmup = 300, iter = 1000, chains = 1)


gp_samples <- extract(gp.stan, permuted = TRUE)

pred.df$latent.mean <- apply(gp_samples$GP_pred, 2, FUN = mean)
pred.df$latent.lci <- apply(gp_samples$GP_pred, 2, FUN = quantile, probs = .025)
pred.df$latent.uci <- apply(gp_samples$GP_pred, 2, FUN = quantile, probs = .975)
pred.df$latent.median <- apply(gp_samples$GP_pred, 2, FUN = quantile, probs = .5)

pred.df$y.mean <- apply(log(1+exp(gp_samples$GP_pred)), 2, FUN = mean)
pred.df$y.lci <- apply(log(1+exp(gp_samples$GP_pred)), 2, FUN = quantile, probs = .025)
pred.df$y.uci <- apply(log(1+exp(gp_samples$GP_pred)), 2, FUN = quantile, probs = .975)


# Plot results
# GAM
ggplot() +
  geom_ribbon(data = pred.df, aes(x = x, y = gam.mean, ymin = gam.lci, ymax = gam.uci), size = 1.3, fill = "darkblue", alpha = .2) +
  geom_line(data = pred.df, aes(x = x, y = gam.mean), size = 1.2, color = "darkblue", alpha = .8) +
  geom_point(data = obsv.df, aes(x, y), size = 2, color = "black") + 
  geom_line(data = pred.df, aes(x, fx), size = .8, color = "black", linetype = 2) 

# latent
ggplot() + 
  geom_ribbon(data = pred.df, aes(x = x, y = latent.mean, ymin = latent.lci, ymax = latent.uci), size = 1.3, fill = "firebrick", alpha = .2) +
  geom_line(data = pred.df, aes(x = x, y = latent.mean), size = 1.2, color = "firebrick", alpha = .8) +
  geom_point(data = obsv.df, aes(x, y), size = 2, color = "steelblue") + 
  geom_line(data = pred.df, aes(x, fx), size = .8, color = "black", linetype = 2) 


# Both
ggplot() + #ylim(c(6,13)) +
  geom_ribbon(data = pred.df, aes(x = x, y = gam.mean, ymin = gam.lci, ymax = gam.uci), size = 1.3, fill = "darkblue", alpha = .2) +
  geom_line(data = pred.df, aes(x = x, y = gam.mean), size = 1.2, color = "darkblue", alpha = .8) +

  geom_ribbon(data = pred.df, aes(x = x, y = y.mean, ymin = y.lci, ymax = y.uci), size = 1.3, fill = "firebrick", alpha = .2) +
  geom_line(data = pred.df, aes(x = x, y = y.mean), size = 1.2, color = "firebrick", alpha = .8) +
  
  geom_point(data = obsv.df, aes(x, y), size = 2, color = "steelblue") + 
  geom_line(data = pred.df, aes(x, fx), size = .8, color = "black", linetype = 2) 

