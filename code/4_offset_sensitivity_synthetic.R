# INLA offset_sensitivity (synthetic data)
# ------------------

library(INLA)
library(raster)
library(maptools)

graphics.off()
rm(list = ls())
set.seed(100)
load("code_output/df_model.RData")
load("code_output/df_obfsc.RData")
afri_main <- rgdal::readOGR("data/Africa_main_country/Africa_main_country.shp")
afri_border <- unionSpatialPolygons(afri_main, rep(1, nrow(afri_main)))
afri_segment <- inla.sp2segment(afri_border)

# Just use data between 2008 and 2012
dftruth <- subset(df_model, year>=2008 & year<=2012)[, c("has_electricity", "freq_impervious", "ntl", "kz", "lctype", "total", "lon", "lat")]
dfobfsc <- subset(df_obfsc, year>=2008 & year<=2012)[, c("has_electricity", "freq_impervious", "ntl", "kz", "lctype", "total", "lon", "lat")]
colnames(dftruth) <- c("y", "fimp", "ntlg", "udis", "ctyp", "total", "lon", "lat")
colnames(dfobfsc) <- c("y", "fimp", "ntlg", "udis", "ctyp", "total", "lon", "lat")

# Remove NAs
ix <- (1:nrow(dftruth))[!is.na(dftruth$fimp) & !is.na(dftruth$ntlg) & !is.na(dfobfsc$fimp) & !is.na(dfobfsc$ntlg)
                        & !is.na(dftruth$ctyp)]
dftruth <- dftruth[ix, ]
dfobfsc <- dfobfsc[ix, ]
off_len <- sqrt(rowSums((dftruth[, c("lon", "lat")] -  dfobfsc[, c("lon", "lat")])^2)) * 111
ix_005 <- (1:nrow(dfobfsc))[off_len > 5] # Points displaced between 5 to 10 Km
ix_995 <- (1:nrow(dfobfsc))[off_len <= 5] # Points displaced up to 5 Km
ix <- c(sample(ix_005, 50), sample(ix_995, 9950))
dftruth <- dftruth[ix,]
dfobfsc <- dfobfsc[ix,]

# Linear predictor without random field
predictor_0 <- y ~ -1 + factor(ctyp) + fimp + ntlg + udis

# Compute the fixed effects parameters
base_model <- inla(predictor_0,  data = dftruth, family = "binomial",
                   Ntrials = dftruth$total, control.predictor = list(compute=TRUE),
                   control.compute = list(dic=TRUE, cpo=TRUE, config=TRUE))

summary(base_model)
beta_params <- round(base_model$summary.fixed$mean, 2)
epsilon <- dftruth$y/dftruth$total - base_model$summary.fitted.values$mean
hist(epsilon)
save(beta_params, epsilon, file = "code_output/synthetic_params.RData")

# Replace y with synthetic data
f <- beta_params[dftruth$ctyp] +
     beta_params[7+1] * dftruth$fimp +
     beta_params[7+2] * dftruth$ntlg +
     beta_params[7+3] * dftruth$udis + epsilon
g <-  1/(1+exp(-f))
for (i in 1:nrow(dftruth)) {
  dftruth$y[i] <- rbinom(1, size=dftruth$total[i], p=g[i])
  dfobfsc$y[i] <- dftruth$y[i]
}

# Linear predictor without random field
predictor_0 <- y ~ -1 + factor(ctyp) + fimp + ntlg + udis

# Linear predictor with random field
predictor_1 <- y ~ -1 + factor(ctyp) + fimp + ntlg + udis + f(u.field, model = afri_spde)

# A matrix for truth and obfuscated
max_ <- 1.5
cut_ <- 1.4
afri_mesh <- inla.mesh.2d(boundary = afri_segment, max.edge = max_, offset=c(1, 2), cutoff = cut_)
afri_spde <- inla.spde2.matern(mesh = afri_mesh, alpha = 2)
u.f <- inla.spde.make.index(name = "u.field", n.spde = afri_spde$n.spde)
afri_A_tr <- inla.spde.make.A(mesh = afri_mesh, loc = as.matrix(dftruth[, c("lon", "lat")]))
afri_A_ob <- inla.spde.make.A(mesh = afri_mesh, loc = as.matrix(dfobfsc[, c("lon", "lat")]))

# Stacks
afri_stack_tr <- inla.stack(data = list(y = dftruth$y),
                            A = list(afri_A_tr, 1, 1, 1, 1),
                            effects = list(u.f,
                                        list(ctyp = dftruth$ctyp),
                                        list(ntlg = dftruth$ntlg),
                                        list(fimp = dftruth$fimp),
                                        list(udis = dftruth$udis)),
                            tag = "afri_data")

afri_stack_ob <- inla.stack(data = list(y = dfobfsc$y),
                            A = list(afri_A_ob, 1, 1, 1, 1),
                            effects = list(u.f,
                                        list(ctyp = dfobfsc$ctyp),
                                        list(ntlg = dfobfsc$ntlg),
                                        list(fimp = dfobfsc$fimp),
                                        list(udis = dfobfsc$udis)),
                            tag = "afri_data")

stack_ix_tr <- inla.stack.index(afri_stack_tr, tag = "afri_data")
stack_ix_ob <- inla.stack.index(afri_stack_ob, tag = "afri_data")

# Fit predictor 0 using grounth truth
model_p0_tr <- inla(predictor_0,  data = dftruth, family = "binomial",
                        Ntrials = dftruth$total,
                        control.predictor = list(compute=TRUE),
                        control.compute = list(dic=TRUE, cpo=TRUE, config=TRUE))

# Fit predictor 0 using obfuscated data
model_p0_ob <- inla(predictor_0,  data = dfobfsc, family = "binomial",
                        Ntrials = dfobfsc$total,
                        control.predictor = list(compute=TRUE),
                        control.compute = list(dic=TRUE, cpo=TRUE, config=TRUE))

# Fit predictor 1 using true data
model_p1_tr <- inla(predictor_1,  data = inla.stack.data(afri_stack_tr), family = "binomial",
                        Ntrials = dftruth$total,
                        control.predictor = list(A=inla.stack.A(afri_stack_tr), compute=TRUE),
                        control.compute = list(dic=TRUE, cpo=TRUE, config=TRUE))

# Fit predictor 1 using obfuscated data
model_p1_ob <- inla(predictor_1,  data = inla.stack.data(afri_stack_ob), family = "binomial",
                        Ntrials = dfobfsc$total,
                        control.predictor = list(A=inla.stack.A(afri_stack_ob), compute=TRUE),
                        control.compute = list(dic=TRUE, cpo=TRUE, config=TRUE))


save(model_p0_tr, model_p0_ob, model_p1_tr, model_p1_ob, dftruth, dfobfsc, beta_params, file = "code_output/offset_sensitivity.RData")


# Fixed effects bias
dfmainb <- data.frame(true_param = exp(beta_params),
                      obsf_param = exp(model_p1_ob$summary.fixed$mean))
dfmainb$diff_absolute <- abs(dfmainb$true_param - dfmainb$obsf_param)
round(dfmainb, 2)

dfbias <- data.frame(true_param = exp(model_p1_tr$summary.fixed$mean),
                     obsf_param = exp(model_p1_ob$summary.fixed$mean))
dfbias$diff_absolute <- abs(dfbias$true_param - dfbias$obsf_param)
round(dfbias, 2)

# Random effects log mean
logparams_tr <- rbind(out_p1_tr$summary.log.kappa,
                      out_p1_tr$summary.log.variance.nominal,
                      out_p1_tr$summary.log.range.nominal)
logparams_ob <- rbind(out_p1_ob$summary.log.kappa,
                      out_p1_ob$summary.log.variance.nominal,
                      out_p1_ob$summary.log.range.nominal)

dflogbias <- data.frame(true_param = logparams_tr$mean, obsf_param = logparams_ob$mean)
dflogbias$diff_absolute <- abs(dflogbias$true_param - dflogbias$obsf_param)
dflogbias$diff_percent <- 100 * dflogbias$diff_absolute / abs(dflogbias$true_param)
round(dflogbias, 2)

# Errors and CPO
sum(log(model_p0_tr$cpo$cpo))
sum(log(model_p0_ob$cpo$cpo))
sum(log(model_p1_tr$cpo$cpo))
sum(log(model_p1_ob$cpo$cpo))

sse_0_tr <- sum((dftruth$y / dftruth$total - model_p0_tr$summary.fitted.values$mean)^2)
sse_0_ob <- sum((dftruth$y / dftruth$total - model_p0_ob$summary.fitted.values$mean)^2)
sse_1_tr <- sum((dftruth$y / dftruth$total - model_p1_tr$summary.fitted.values$mean[stack_ix_tr$data])^2)
sse_1_ob <- sum((dftruth$y / dftruth$total - model_p1_ob$summary.fitted.values$mean[stack_ix_ob$data])^2)

sse_0_ob / sse_0_tr
100 *(sse_1_ob / sse_1_tr - 1)

save(df_fixed, df_marginals, dfmainb, dfbias, dfsd, dflogbias,
     dflogsd, dfvs, sse_0_tr, sse_0_ob, sse_1_tr, sse_1_ob,
     file = "code_output/dataframes_sensitivity.RData")
#load("code_output/dataframes_sensitivity.RData")
#beta_params <- dfmainb$true_param
