# INLA offset_sensitivity (synthetic data)
# ------------------
#
# Edited: December 3, 2018

library(ggplot2)
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

# Just use data from 2010
dftruth <- subset(df_model, year>=2008 & year<=2012)[, c("has_electricity", "freq_impervious", "ntl", "kz", "lctype", "total", "lon", "lat")]
dfobfsc <- subset(df_obfsc, year>=2008 & year<=2012)[, c("has_electricity", "freq_impervious", "ntl", "kz", "lctype", "total", "lon", "lat")]
colnames(dftruth) <- c("y", "fimp", "ntlg", "udis", "ctyp", "total", "lon", "lat")
colnames(dfobfsc) <- c("y", "fimp", "ntlg", "udis", "ctyp", "total", "lon", "lat")

# Remove NAs
ix <- (1:nrow(dftruth))[!is.na(dftruth$fimp) & !is.na(dftruth$ntlg) & !is.na(dfobfsc$fimp) & !is.na(dfobfsc$ntlg)
                        & !is.na(dftruth$ctyp)]
ix <- sample(ix, 10000)
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

lcov_factor <- function(x) {
    
}
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


save(model_p0_tr, model_p0_ob, model_p1_tr, model_p1_ob, dftruth, dfobfsc, file = "code_output/offset_sensitivity.RData")




# Fixed effects parameters distributions 
df_fixed <- list()
for(i in 1:10) {
  mat_ <- rbind(model_p0_tr$marginals.fixed[[i]], 
                model_p0_ob$marginals.fixed[[i]],
                model_p1_tr$marginals.fixed[[i]],
                model_p1_ob$marginals.fixed[[i]]
                )
  df_ <- as.data.frame(mat_)
  len_ <- nrow(df_)/4
  df_$model <- c(rep("true_linear", len_),
                 rep("obfuscted_linear", len_),
                 rep("true_geostatistic", len_),
                 rep("obfuscated_geostatistic", len_)
                 )
  df_$data_type <- c(rep("Exact", len_), rep("Obfuscated", len_), rep("Exact", len_), rep("Obfuscated", len_))
  df_$model_type <- c(rep("linear", len_*2), rep("geostatistic", len_*2))
  df_fixed[[i]] <- df_
}
  
# Fixed effects parameter names
beta_names <- c("Land cover type 1",
                "Land cover type 2",
                "Land cover type 3",
                "Land cover type 4",
                "Land cover type 5",
                "Land cover type 6",
                "Land cover type 7",
                "Percentage of impervious surface",
                "Nighttime lights",
                "Distance to impervious cluster")

# X limits for plots
xlims_p0 <- list(c(-3.05, -2.45),
                 c(-2.58, -2.43),
                 c(-2.15, -1.82),
                 c(-2.35, -1.98),
                 c(-2.51, -2.22),
                 c(-2.83, -2.51),
                 c(-2.95, -2.35),
                 c(-.8, .9),
                 c(.061, .075),
                 c(1.9, 2.4))

xlims_p1 <- list(c(-3.05, -2.4),
                 c(-2.58, -2.38),
                 c(-2.28, -1.8),
                 c(-2.4, -1.95),
                 c(-2.7, -2.2),
                 c(-2.85, -2.42),
                 c(-2.92, -2.25),
                 c(-.8, .9),
                 c(.055, .075),
                 c(1.85, 2.5))

# Location of text
xytext_p0 <- list(c(.08, 7),
                  c(.02, 25),
                  c(.045, 19),
                  c(.05, 11),
                  c(.04, 11.5),
                  c(.045, 11),
                  c(.09, 7),
                  c(.25, 2.4),
                  c(.002, 300),
                  c(.075, 14))

xytext_p1 <- list(c(.09, 6.2),
                  c(.03, 14.5),
                  c(.07, 12),
                  c(.07, 9),
                  c(.07, 8),
                  c(.06, 8.5),
                  c(.09, 6.2),
                  c(.25, 2.),
                  c(.003, 230),
                  c(.095, 11))

# Generic theme
gtheme <- theme(panel.border = element_blank(),
                legend.title = element_text(size=12),
                legend.background = element_rect(fill = "transparent"),
                legend.text=element_text(size = 12),
                axis.text = element_text(size = 12),
                axis.title = element_text(size = 15))

# Distributions comparison for fixed effects
fixed_plot <- function(num_param, xytext, xlims_p=NULL, mtype="linear") {
  if(!is.null(xlims_p)) {
    xsc <- scale_x_continuous(beta_names[num_param], limits = xlims_p[[num_param]])
  } else {
    xsc <- scale_x_continuous(beta_names[num_param])
  }
  
  plt <- ggplot(subset(df_fixed[[num_param]], model_type==mtype), aes(x, y)) +
    geom_area(aes(fill=data_type), alpha=.7) +
    geom_vline(xintercept = beta_params[num_param], col="darkred") +
    geom_text(data=data.frame(x=beta_params[num_param] + xytext[[num_param]][1],
                              y = xytext[[num_param]][2], m='True value'),
              aes(x,y, label=m), col="darkred", size=5) +
    theme_classic() +
    xsc +
    scale_y_continuous("marginal distribution") +
    guides(fill=guide_legend(title="Data type")) +
    viridis::scale_fill_viridis(option = "E", discrete = TRUE) +
    gtheme
  return(plt)
}

# Linear
fixed_plot(num_param = 1, xytext = xytext_p0, xlims_p = xlims_p0, mtype = "linear")
fixed_plot(num_param = 2, xytext = xytext_p0, xlims_p = xlims_p0, mtype = "linear")
fixed_plot(num_param = 3, xytext = xytext_p0, xlims_p = xlims_p0, mtype = "linear")
fixed_plot(num_param = 4, xytext = xytext_p0, xlims_p = xlims_p0, mtype = "linear")
fixed_plot(num_param = 5, xytext = xytext_p0, xlims_p = xlims_p0, mtype = "linear")
fixed_plot(num_param = 6, xytext = xytext_p0, xlims_p = xlims_p0, mtype = "linear")
fixed_plot(num_param = 7, xytext = xytext_p0, xlims_p = xlims_p0, mtype = "linear")
fixed_plot(num_param = 8, xytext = xytext_p0, xlims_p = xlims_p0, mtype = "linear")
fixed_plot(num_param = 9, xytext = xytext_p0, xlims_p = xlims_p0, mtype = "linear")
fixed_plot(num_param = 10, xytext = xytext_p0, xlims_p = xlims_p0, mtype = "linear")

# Geostat
fixed_plot(num_param = 1, xytext = xytext_p1, xlims_p = xlims_p1, mtype = "geostatistic")
fixed_plot(num_param = 2, xytext = xytext_p1, xlims_p = xlims_p1, mtype = "geostatistic")
fixed_plot(num_param = 3, xytext = xytext_p1, xlims_p = xlims_p1, mtype = "geostatistic")
fixed_plot(num_param = 4, xytext = xytext_p1, xlims_p = xlims_p1, mtype = "geostatistic")
fixed_plot(num_param = 5, xytext = xytext_p1, xlims_p = xlims_p1, mtype = "geostatistic")
fixed_plot(num_param = 6, xytext = xytext_p1, xlims_p = xlims_p1, mtype = "geostatistic")
fixed_plot(num_param = 7, xytext = xytext_p1, xlims_p = xlims_p1, mtype = "geostatistic")
fixed_plot(num_param = 8, xytext = xytext_p1, xlims_p = xlims_p1, mtype = "geostatistic")
fixed_plot(num_param = 9, xytext = xytext_p1, xlims_p = xlims_p1, mtype = "geostatistic")
fixed_plot(num_param = 10, xytext = xytext_p1, xlims_p = xlims_p1, mtype = "geostatistic")

# Random effects parameters distributions
out_p1_tr <- inla.spde2.result(inla=model_p1_tr, name="u.field", spde=afri_spde, do.transform = TRUE)
out_p1_ob <- inla.spde2.result(inla=model_p1_ob, name="u.field", spde=afri_spde, do.transform = TRUE)

df_marginals <- rbind(out_p1_tr$marginals.kappa[[1]],
                      out_p1_tr$marginals.variance.nominal[[1]],
                      out_p1_tr$marginals.range.nominal[[1]],
                      out_p1_ob$marginals.kappa[[1]],
                      out_p1_ob$marginals.variance.nominal[[1]],
                      out_p1_ob$marginals.range.nominal[[1]])
df_marginals <- as.data.frame(df_marginals)
nrow_ <- nrow(df_marginals)/6
df_marginals$variable <- rep(c(rep("kappa", nrow_), rep("variance nominal", nrow_), rep("range nominal", nrow_)), 2)
df_marginals$data_type <- c(rep("Exact", nrow_*3), rep("Obfuscated", nrow_*3))

# Distributions comparison for random effects
random_plot <- function(param, xlims_p=NULL) {
  if(!is.null(xlims_p)) {
    xsc <- scale_x_continuous(param, limits = xlims_p)
  } else {
    xsc <- scale_x_continuous(param)
  }
  
  plt <- ggplot(subset(df_marginals, variable==param), aes(x, y)) +
    geom_area(aes(fill=data_type), alpha=.7) +
    theme_classic() +
    xsc +
    scale_y_continuous("marginal distribution") +
    viridis::scale_fill_viridis(option = "E", discrete = TRUE) +
    guides(fill=guide_legend(title="Data type")) +
    gtheme
  return(plt)
}

random_plot(param = "kappa", xlims_p = NULL)
random_plot(param = "variance nominal", xlims_p = NULL)
random_plot(param = "range nominal", xlims_p = NULL)


# Obfuscated vs Non-obfuscated
ix <- dftruth$y > 0 & dftruth$y < dftruth$total #sample(1:nrow(dftruth), 5000)
logitf <- function(x) {return(log(x/(1-x)))}
dfvs <- data.frame(raw = logitf(model_p1_tr$summary.fitted.values$mean[stack_ix_tr$data])[ix],
                   obfuscated = logitf(model_p1_ob$summary.fitted.values$mean[stack_ix_ob$data])[ix],
                   true = logitf((dftruth$y/dftruth$total)[ix]))

graphics.off()

dots_col = "#0072B2"
ggplot(dfvs, aes(true, raw)) +
  geom_point(col=dots_col, alpha=.2) +
  geom_abline(intercept = 0, slope = 1, col="darkred") +
  theme_classic() +
  scale_x_continuous("True target values", limits = c(-4.2, 4.2)) +
  scale_y_continuous("Predictions with exact data", limits = c(-4.2, 4.2)) +
  gtheme

ggplot(dfvs, aes(true, obfuscated)) +
  geom_point(col=dots_col, alpha=.2) +
  geom_abline(intercept = 0, slope = 1, col="darkred") +
  theme_classic() +
  scale_x_continuous("True target values", limits=c(-4.2, 4.2)) +
  scale_y_continuous("Predictions with obfuscated data", limits = c(-4.2, 4.2)) +
  gtheme
  
ggplot(dfvs, aes(obfuscated, raw)) +
  geom_point(col=dots_col, alpha=.2) +
  geom_abline(intercept = 0, slope = 1, col="darkred") +
  theme_classic() +
  scale_x_continuous("Predictions with raw data", limits = c(-4.2, 4.2)) +
  scale_y_continuous("Predictions with obfuscated data", limits = c(-4.2, 4.2)) +
  gtheme


# Fixed effects bias
dfmainb <- data.frame(true_param = beta_params,
                      obsf_param = model_p1_ob$summary.fixed$mean)
dfmainb$diff_absolute <- abs(dfmainb$true_param - dfmainb$obsf_param)
dfmainb$diff_percent <- 100 * dfmainb$diff_absolute / abs(dfmainb$true_param)
round(dfmainb, 2)

dfbias <- data.frame(true_param = model_p1_tr$summary.fixed$mean,
                     obsf_param = model_p1_ob$summary.fixed$mean)
dfbias$diff_absolute <- abs(dfbias$true_param - dfbias$obsf_param)
dfbias$diff_percent <- 100 * dfbias$diff_absolute / abs(dfbias$true_param)
round(dfbias, 2)

# Fixed effects standard deviation
dfsd <- data.frame(true_param = model_p1_tr$summary.fixed$sd,
                     obsf_param = model_p1_ob$summary.fixed$sd)
dfsd$diff_absolute <- abs(dfsd$true_param - dfsd$obsf_param)
dfsd$diff_percent <- 100 * dfsd$diff_absolute / abs(dfsd$true_param)
round(dfsd, 2)


# Fixed effects log mean
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

dflogsd <- data.frame(true_param = logparams_tr$sd, obsf_param = logparams_ob$sd)
dflogsd$diff_absolute <- abs(dflogsd$true_param - dflogsd$obsf_param)
dflogsd$diff_percent <- 100 * dflogsd$diff_absolute / abs(dflogsd$true_param)
round(dflogsd, 2)


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
sse_1_ob / sse_1_tr

save(df_fixed, df_marginals, dfmainb, dfbias, dfsd, dflogbias,
     dflogsd, dfvs, sse_0_tr, sse_0_ob, sse_1_tr, sse_1_ob,
     file = "code_output/dataframes_sensitivity.RData")
