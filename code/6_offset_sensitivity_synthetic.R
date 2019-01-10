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


save(model_p0_tr, model_p0_ob, model_p1_tr, model_p1_ob, dftruth, dfobfsc, beta_params, file = "code_output/offset_sensitivity.RData")
#load("code_output/offset_sensitivity.RData")

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
beta_names <- c("Impervious surface",
                "Low biomass",
                "High biomass",
                "Bare soil",
                "Sand",
                "Rock",
                "Water",
                "Impervious area proportion",
                "Nighttime lights",
                "Distance to impervious pixel")

# X limits for plots
xlims_p0 <- list(c(-3.05, -2.45),
                 c(-2.58, -2.43),
                 c(-2.15, -1.82),
                 c(-2.35, -1.98),
                 c(-2.51, -2.22),
                 c(-2.83, -2.51),
                 c(-2.95, -2.35),
                 c(-.8, .9),
                 c(.061, .077),
                 c(1.9, 2.4))

xlims_p1 <- list(c(-3.05, -2.4),
                 c(-2.58, -2.38),
                 c(-2.28, -1.8),
                 c(-2.4, -1.95),
                 c(-2.7, -2.2),
                 c(-2.85, -2.44),
                 c(-2.92, -2.25),
                 c(-.8, .9),
                 c(.055, .077),
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


# Distributions comparison for fixed effects
fixed_plot <- function(num_param, xytext, xlims_p=NULL, mtype="linear") {
  if(!is.null(xlims_p)) {
    #xsc <- scale_x_continuous(beta_names[num_param], limits = xlims_p[[num_param]])
    xsc <- scale_x_continuous("", limits = xlims_p)
  } else {
    #xsc <- scale_x_continuous(beta_names[num_param])
    xsc <- scale_x_continuous("")
  }
  
  plt <- ggplot(subset(df_fixed[[num_param]], model_type==mtype), aes(x, y)) +
    geom_area(aes(fill=data_type), alpha=.7) +
    geom_vline(xintercept = beta_params[num_param], col="darkred") +
    #geom_text(data=data.frame(x=beta_params[num_param] + xytext[[num_param]][1],
    #                          y = xytext[[num_param]][2], m='True value'),
    #          aes(x,y, label=m), col="darkred", size=5) +
    theme_classic() +
    xsc +
    scale_y_continuous("") +
    guides(fill=FALSE) +
    #guides(fill=guide_legend(title="Data type")) +
    viridis::scale_fill_viridis(option = "E", discrete = TRUE) +
    gtheme
  return(plt)
}


# Distributions comparison for random effects
random_plot <- function(param, xlims_p=NULL) {
  if(!is.null(xlims_p)) {
    xsc <- scale_x_continuous("", limits = xlims_p)
  } else {
    xsc <- scale_x_continuous("")
  }
  
  plt <- ggplot(subset(df_marginals, variable==param), aes(log(x), y)) +
    geom_area(aes(fill=data_type), alpha=.7) +
    theme_classic() +
    xsc +
    scale_y_continuous("") +
    viridis::scale_fill_viridis(option = "E", discrete = TRUE) +
    guides(fill=FALSE) +
    gtheme
  return(plt)
}


# Generic theme
gtheme <- theme(panel.border = element_blank(),
                legend.title = element_text(size=16),
                legend.background = element_rect(fill = "transparent"),
                legend.text=element_text(size = 16),
                axis.text = element_text(size = 16),
                axis.title = element_text(size = 16))



# Linear
#fixed_plot(num_param = 1, xytext = xytext_p0, xlims_p = xlims_p0, mtype = "linear")
#fixed_plot(num_param = 2, xytext = xytext_p0, xlims_p = xlims_p0, mtype = "linear")
#fixed_plot(num_param = 3, xytext = xytext_p0, xlims_p = xlims_p0, mtype = "linear")
#fixed_plot(num_param = 4, xytext = xytext_p0, xlims_p = xlims_p0, mtype = "linear")
#fixed_plot(num_param = 5, xytext = xytext_p0, xlims_p = xlims_p0, mtype = "linear")
#fixed_plot(num_param = 6, xytext = xytext_p0, xlims_p = xlims_p0, mtype = "linear")
#fixed_plot(num_param = 7, xytext = xytext_p0, xlims_p = xlims_p0, mtype = "linear")
#fixed_plot(num_param = 8, xytext = xytext_p0, xlims_p = xlims_p0, mtype = "linear")
#fixed_plot(num_param = 9, xytext = xytext_p0, xlims_p = xlims_p0, mtype = "linear")
#fixed_plot(num_param = 10, xytext = xytext_p0, xlims_p = xlims_p0, mtype = "linear")

# Geostat
plt1 <- fixed_plot(num_param = 1, xytext = xytext_p1, xlims_p = c(-2.8, -2), mtype = "geostatistic")
plt2 <- fixed_plot(num_param = 2, xytext = xytext_p1, xlims_p = c(-2.6, -2.2), mtype = "geostatistic")
plt3 <- fixed_plot(num_param = 3, xytext = xytext_p1, xlims_p = c(-2.4, -1.8), mtype = "geostatistic")
plt4 <- fixed_plot(num_param = 4, xytext = xytext_p1, xlims_p = c(-2.3, -1.7), mtype = "geostatistic")
plt5 <- fixed_plot(num_param = 5, xytext = xytext_p1, xlims_p = c(-2.7, -2.1), mtype = "geostatistic")
plt6 <- fixed_plot(num_param = 6, xytext = xytext_p1, xlims_p = c(-3.3, -2.5), mtype = "geostatistic")
plt7 <- fixed_plot(num_param = 7, xytext = xytext_p1, xlims_p = c(-2.6, -2), mtype = "geostatistic")
plt8 <- fixed_plot(num_param = 8, xytext = xytext_p1, xlims_p = c(-2.2, 0), mtype = "geostatistic")
plt9 <- fixed_plot(num_param = 9, xytext = xytext_p1, xlims_p = c(.065, .095), mtype = "geostatistic")
plt10 <- fixed_plot(num_param = 10, xytext = xytext_p1, xlims_p = c(1.8, 2.4), mtype = "geostatistic")

plt11 <- random_plot(param = "kappa", xlims_p = NULL)
plt12 <- random_plot(param = "variance nominal", xlims_p = NULL)
plt13 <- random_plot(param = "range nominal", xlims_p = NULL)

# Panel plot fixed effects
psc <- 4
plt_fixed <- cowplot::ggdraw(xlim=c(0,3*psc), ylim = c(0,4*psc)) +
        cowplot::draw_plot(plt1, x=0*psc, y=3*psc, width = 1*psc, height = 1*psc) + 
        cowplot::draw_plot(plt2, x=1*psc, y=3*psc, width = 1*psc, height = 1*psc) + 
        cowplot::draw_plot(plt3, x=2*psc, y=3*psc, width = 1*psc, height = 1*psc) + 

        cowplot::draw_plot(plt4, x=0*psc, y=2*psc, width = 1*psc, height = 1*psc) + 
        cowplot::draw_plot(plt5, x=1*psc, y=2*psc, width = 1*psc, height = 1*psc) + 
        cowplot::draw_plot(plt6, x=2*psc, y=2*psc, width = 1*psc, height = 1*psc) + 

        cowplot::draw_plot(plt7, x=0*psc, y=1*psc, width = 1*psc, height = 1*psc) + 
        cowplot::draw_plot(plt8, x=1*psc, y=1*psc, width = 1*psc, height = 1*psc) + 
        cowplot::draw_plot(plt9, x=2*psc, y=1*psc, width = 1*psc, height = 1*psc) + 

        cowplot::draw_plot(plt10, x=1*psc, y=0*psc, width = 1*psc, height = 1*psc) + 

        cowplot::draw_plot_label(LETTERS[1:10], c(rep(0:2, 3), 1)*psc,
                                 c(sort(rep(2:4, 3)*psc, decreasing = TRUE), 1*psc),
                                 size=18, colour = "grey")
  
cowplot::ggsave('figs/comparison_fixed.pdf', plt_fixed, width=12, height=16)

# Panel plot random effects
psc <- 4
plt_random <- cowplot::ggdraw(xlim=c(0,3*psc), ylim = c(0,1*psc)) +
        cowplot::draw_plot(plt11, x=0*psc, y=0*psc, width = 1*psc, height = 1*psc) + 
        cowplot::draw_plot(plt12, x=1*psc, y=0*psc, width = 1*psc, height = 1*psc) + 
        cowplot::draw_plot(plt13, x=2*psc, y=0*psc, width = 1*psc, height = 1*psc) + 
  
        cowplot::draw_plot_label(LETTERS[1:3], (0:2)*psc, rep(1, 3)*psc,
                                 size=18, colour = "grey")

cowplot::ggsave('figs/comparison_random.pdf', plt_random, width=12, height=4)


# Legend squares
square <- data.frame(x=c(0, 1, 1, 0, 0), y=c(0, 0, 1, 1, 0))
plt <- ggplot(square, aes(x=x, y=y)) +
  geom_boxplot(fill = viridis::viridis_pal(option="E")(2)[1], alpha=.7) +
  theme_void()
ggsave('figs/binary_label_1.pdf', plot = plt)

plt <- ggplot(square, aes(x=x, y=y)) +
  geom_boxplot(fill = viridis::viridis_pal(option="E")(2)[2], alpha=.7 ) +
  theme_void()
ggsave('figs/binary_label_2.pdf', plot = plt)


# Obfuscated vs Non-obfuscated estimates
ix <- dftruth$y > 0 & dftruth$y < dftruth$total #sample(1:nrow(dftruth), 5000)
logitf <- function(x) {return(log(x/(1-x)))}
dfvs <- data.frame(raw = logitf(model_p1_tr$summary.fitted.values$mean[stack_ix_tr$data])[ix],
                   obfuscated = logitf(model_p1_ob$summary.fitted.values$mean[stack_ix_ob$data])[ix],
                   true = logitf((dftruth$y/dftruth$total)[ix]))

graphics.off()

dots_col = viridis::viridis_pal(option="E")(2)[1]

pltvs1 <- ggplot(dfvs, aes(true, raw)) +
  geom_point(col=dots_col, alpha=.2) +
  geom_abline(intercept = 0, slope = 1, col="darkred") +
  theme_classic() +
  scale_x_continuous("Target", limits = c(-4.2, 4.2)) +
  scale_y_continuous("Estimate (exact)", limits = c(-4.2, 4.2)) +
  gtheme

pltvs2 <- ggplot(dfvs, aes(true, obfuscated)) +
  geom_point(col=dots_col, alpha=.2) +
  geom_abline(intercept = 0, slope = 1, col="darkred") +
  theme_classic() +
  scale_x_continuous("Target", limits=c(-4.2, 4.2)) +
  scale_y_continuous("Estimate (obfuscated)", limits = c(-4.2, 4.2)) +
  gtheme
  
pltvs3 <- ggplot(dfvs, aes(obfuscated, raw)) +
  geom_point(col=dots_col, alpha=.2) +
  geom_abline(intercept = 0, slope = 1, col="darkred") +
  theme_classic() +
  scale_x_continuous("Estimate (exact)", limits = c(-4.2, 4.2)) +
  scale_y_continuous("Estimate (obfuscated)", limits = c(-4.2, 4.2)) +
  gtheme

# Panel plot
psc <- 4
plt_vs <- cowplot::ggdraw(xlim=c(0,3*psc), ylim = c(0,1*psc)) +
        cowplot::draw_plot(pltvs1, x=0*psc, y=0*psc, width = 1*psc, height = 1*psc) + 
        cowplot::draw_plot(pltvs2, x=1*psc, y=0*psc, width = 1*psc, height = 1*psc) + 
        cowplot::draw_plot(pltvs3, x=2*psc, y=0*psc, width = 1*psc, height = 1*psc) + 

        cowplot::draw_plot_label(LETTERS[1:3], (0:2)*psc, rep(1, 3)*psc,
                                 size=18, colour = "grey")

cowplot::ggsave('figs/comparison_estimates.pdf', plt_vs, width=12, height=4)


# Exact data vs obfuscated
diff_locs <- data.frame(lon_tr = dftruth$lon, lon_ob = dfobfsc$lon,
                        lat_tr = dftruth$lat, lat_ob = dfobfsc$lat)
ixlocs <- sample(1:nrow(diff_locs), 4000)
diff_locs <- diff_locs[ixlocs, ]

distob <- data.frame(distance = sqrt(rowSums((dftruth[, c("lon", "lat")] - dfobfsc[, c("lon", "lat")])^2)) * 111)

pltlon <- ggplot(diff_locs, aes(lon_tr, lon_ob)) +
  geom_point(col=viridis::viridis_pal(option="E")(2)[1]) +
  scale_y_continuous("Longitude (obfuscated)", limits = c(-2, 0)) +
  scale_x_continuous("Longitude (exact)", limits = c(-2, 0)) +
  theme_classic() +
  gtheme

pltlat <- ggplot(diff_locs, aes(lat_tr, lat_ob)) +
  geom_point(col=viridis::viridis_pal(option="E")(2)[1]) +
  scale_y_continuous("Latitude (obfuscated)", limits = c(-2, 0)) +
  scale_x_continuous("Latitude (exact)", limits = c(-2, 0)) +
  theme_classic() +
  gtheme

plthist <- ggplot(distob, aes(distance)) +
  geom_histogram(breaks=c(seq(0,10, by=1)), fill=viridis::viridis_pal(option="E")(2)[2],
                 col=viridis::viridis_pal(option="E")(2)[1]) +
  scale_y_continuous("Count") +
  scale_x_continuous("Offset (Km)") +
  theme_classic() +
  gtheme

# Panel plot
psc <- 4
plt_off <- cowplot::ggdraw(xlim=c(0,3*psc), ylim = c(0,1*psc)) +
        cowplot::draw_plot(pltlon, x=0*psc, y=0*psc, width = 1*psc, height = 1*psc) + 
        cowplot::draw_plot(pltlat, x=1*psc, y=0*psc, width = 1*psc, height = 1*psc) + 
        cowplot::draw_plot(plthist, x=2*psc, y=0*psc, width = 1*psc, height = 1*psc) + 

        cowplot::draw_plot_label(LETTERS[1:3], (0:2)*psc, rep(1, 3)*psc,
                                 size=18, colour = "grey")

cowplot::ggsave('figs/offset_distance.pdf', plt_off, width=12, height=4)


# Fixed effects bias
dfmainb <- data.frame(true_param = exp(beta_params),
                      obsf_param = exp(model_p1_ob$summary.fixed$mean))
dfmainb$diff_absolute <- abs(dfmainb$true_param - dfmainb$obsf_param)
#dfmainb$diff_percent <- 100 * dfmainb$diff_absolute / abs(dfmainb$true_param)
round(dfmainb, 2)

dfbias <- data.frame(true_param = exp(model_p1_tr$summary.fixed$mean),
                     obsf_param = exp(model_p1_ob$summary.fixed$mean))
dfbias$diff_absolute <- abs(dfbias$true_param - dfbias$obsf_param)
#dfbias$diff_percent <- 100 * dfbias$diff_absolute / abs(dfbias$true_param)
round(dfbias, 2)


# Fixed effects standard deviation
#dfsd <- data.frame(true_param = model_p1_tr$summary.fixed$sd,
#                     obsf_param = model_p1_ob$summary.fixed$sd)
#dfsd$diff_absolute <- abs(dfsd$true_param - dfsd$obsf_param)
#dfsd$diff_percent <- 100 * dfsd$diff_absolute / abs(dfsd$true_param)
#round(dfsd, 2)


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

#dflogsd <- data.frame(true_param = logparams_tr$sd, obsf_param = logparams_ob$sd)
#dflogsd$diff_absolute <- abs(dflogsd$true_param - dflogsd$obsf_param)
#dflogsd$diff_percent <- 100 * dflogsd$diff_absolute / abs(dflogsd$true_param)
#round(dflogsd, 2)


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
#load("code_output/dataframes_sensitivity.RData")
#beta_params <- dfmainb$true_param
