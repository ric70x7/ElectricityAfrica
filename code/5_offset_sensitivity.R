# INLA offset_sensitivity
# ------------------
#
# Edited: December 3, 2018


library(INLA)
library(raster)
library(maptools)

graphics.off()
rm(list = ls())
set.seed(10)
load("code_output/df_model.RData")
load("code_output/df_obfsc.RData")
afri_main <- rgdal::readOGR("data/Africa_main_country/Africa_main_country.shp")
afri_border <- unionSpatialPolygons(afri_main, rep(1, nrow(afri_main)))
afri_segment <- inla.sp2segment(afri_border)

# Just use data from 2010
dftruth <- subset(df_model, year==2010)[, c("has_electricity", "freq_impervious", "ntl", "kz", "total", "lon", "lat")]
dfobfsc <- subset(df_obfsc, year==2010)[, c("has_electricity", "freq_impervious", "ntl", "kz", "total", "lon", "lat")]

colnames(dftruth) <- c("y", "fimp", "ntlg", "udis", "total", "lon", "lat")
colnames(dfobfsc) <- c("y", "fimp", "ntlg", "udis", "total", "lon", "lat")

max_ <- 1.5 
cut_ <- 1.4
afri_mesh <- inla.mesh.2d(boundary = afri_segment, max.edge = max_, offset=c(1, 2), cutoff = cut_)
afri_spde <- inla.spde2.matern(mesh = afri_mesh, alpha = 2)
u.f <- inla.spde.make.index(name = "u.field", n.spde = afri_spde$n.spde)

# Linear predictor without random field
predictor_0 <- y ~ fimp + ntlg + udis

# Linear predictor with random field
predictor_1 <- y ~-1 + intercept + fimp + ntlg + udis + f(u.field, model = afri_spde) 

# A matrix for truth and obfuscated
afri_A_tr <- inla.spde.make.A(mesh = afri_mesh, loc = as.matrix(dftruth[, c("lon", "lat")]))
afri_A_ob <- inla.spde.make.A(mesh = afri_mesh, loc = as.matrix(dfobfsc[, c("lon", "lat")]))

# Stacks for truth and obfuscated
afri_stack_tr <- inla.stack(data = list(y = dftruth$y),
                            A = list(afri_A_tr, 1, 1, 1 ),
                            effects = list(c(u.f, list(intercept=1)),
                                        list(ntlg = dftruth$ntlg),
                                        list(fimp = dftruth$fimp),
                                        list(udis = dftruth$udis)),
                         tag = "afri_data")

afri_stack_ob <- inla.stack(data = list(y = dfobfsc$y),
                            A = list(afri_A_ob, 1, 1, 1),
                            effects = list(c(u.f, list(intercept=1)),
                                        list(ntlg = dfobfsc$ntlg),
                                        list(fimp = dfobfsc$fimp),
                                        list(udis = dfobfsc$udis)),
                         tag = "afri_data")

stack_ix_tr <- inla.stack.index(afri_stack_tr, tag = "afri_data")
stack_ix_ob <- inla.stack.index(afri_stack_ob, tag = "afri_data")

# Fit predictor 0 using grounth truth
afri_model_p0_tr <- inla(predictor_0,  data = dftruth, family = "binomial",
                        Ntrials = dftruth$total,
                        control.predictor = list(compute=TRUE),
                        control.compute = list(dic=TRUE, cpo=TRUE, config=TRUE))

# Fit predictor 0 using obfuscated data
afri_model_p0_ob <- inla(predictor_0,  data = dfobfsc, family = "binomial",
                        Ntrials = dfobfsc$total,
                        control.predictor = list(compute=TRUE),
                        control.compute = list(dic=TRUE, cpo=TRUE, config=TRUE))


# Fit predictor 1 using grounth truth
afri_model_p1_tr <- inla(predictor_1,  data = inla.stack.data(afri_stack_tr), family = "binomial",
                        Ntrials = dftruth$total,
                        control.predictor = list(A=inla.stack.A(afri_stack_tr), compute=TRUE),
                        control.compute = list(dic=TRUE, cpo=TRUE, config=TRUE))

# Fit predictor 1 using obfuscated data
afri_model_p1_ob <- inla(predictor_1,  data = inla.stack.data(afri_stack_ob), family = "binomial",
                        Ntrials = dfobfsc$total,
                        control.predictor = list(A=inla.stack.A(afri_stack_ob), compute=TRUE),
                        control.compute = list(dic=TRUE, cpo=TRUE, config=TRUE))



summary(afri_model_p0_tr)
summary(afri_model_p0_ob)
summary(afri_model_p1_tr)
summary(afri_model_p1_ob)
sum(afri_model_p0_tr$cpo$cpo)
sum(afri_model_p0_ob$cpo$cpo)

ix <- sample(1:nrow(dftruth), 1000)

logitf <- function(x) {return(log(x/(1-x)))}
plot(logitf(afri_model_p0_tr$summary.fitted.values$mean[ix]), logitf(afri_model_p1_tr$summary.fitted.values$mean[ix]), col="blue")
points(logitf(afri_model_p0_tr$summary.fitted.values$mean[ix]), logitf(afri_model_p1_ob$summary.fitted.values$mean[ix]), col="red")
lines(c(-5, 5), c(-5,5), col="blue")

plot(logitf(afri_model_p0_tr$summary.fitted.values$mean[ix]), logitf(afri_model_p0_ob$summary.fitted.values$mean[ix]))
lines(c(-5, 5), c(-5,5), col="blue")

points(logitf(afri_model_p1_tr$summary.fitted.values$mean[ix]), logitf(afri_model_p1_ob$summary.fitted.values$mean[ix]), col="red")

sum((logitf(afri_model_p0_tr$summary.fitted.values$mean[ix]) - logitf(afri_model_p1_tr$summary.fitted.values$mean[ix]))^2)
sum((logitf(afri_model_p0_tr$summary.fitted.values$mean[ix]) - logitf(afri_model_p1_tr$summary.fitted.values$mean[ix]))^2)

sse_0_tr <- sum((dftruth$y / dftruth$total - afri_model_p0_tr$summary.fitted.values$mean)^2)
sse_1_tr <- sum((dftruth$y / dftruth$total - afri_model_p1_tr$summary.fitted.values$mean[stack_ix_tr$data])^2)
sse_0_ob <- sum((dftruth$y / dftruth$total - afri_model_p0_ob$summary.fitted.values$mean)^2)
sse_1_ob <- sum((dftruth$y / dftruth$total - afri_model_p1_ob$summary.fitted.values$mean[stack_ix_ob$data])^2)

sse_0_ob / sse_0_tr
sse_1_tr / sse_0_tr
sse_1_ob / sse_0_tr
sse_1_ob / sse_1_tr

# Make predictions
afri_pred <- c()
for(i in seq(nrow(df_10))){
  afri_pred[i] <- inla.emarginal(inla.link.invlogit,
                                 afri_model$marginals.linear.predictor[stack_ix$data][[i]] )
}




# Fixed effects parameters distributions + non-geostat model
i <- 1
plot(afri_model_p0_tr$marginals.fixed[[i]][,1], afri_model_p0_tr$marginals.fixed[[i]][,2], type="l", c(-6,-3), main="intercept")
lines(afri_model_p0_ob$marginals.fixed[[i]][,1], afri_model_p0_ob$marginals.fixed[[i]][,2], col="blue")
lines(afri_model_p1_ob$marginals.fixed[[i]][,1], afri_model_p1_ob$marginals.fixed[[i]][,2], col="red")

i <- 2
plot(afri_model_p0_tr$marginals.fixed[[i]][,1], afri_model_p0_tr$marginals.fixed[[i]][,2], type="l", c(0, 6), main="freq of impervious")
lines(afri_model_p0_ob$marginals.fixed[[i]][,1], afri_model_p0_ob$marginals.fixed[[i]][,2], col="blue")
lines(afri_model_p1_ob$marginals.fixed[[i]][,1], afri_model_p1_ob$marginals.fixed[[i]][,2], col="red")

i <- 3
plot(afri_model_p0_tr$marginals.fixed[[i]][,1], afri_model_p0_tr$marginals.fixed[[i]][,2], type="l", c(0, .05), main="ntl")
lines(afri_model_p0_ob$marginals.fixed[[i]][,1], afri_model_p0_ob$marginals.fixed[[i]][,2], col="blue")
lines(afri_model_p1_ob$marginals.fixed[[i]][,1], afri_model_p1_ob$marginals.fixed[[i]][,2], col="red")

i <- 4
plot(afri_model_p0_tr$marginals.fixed[[i]][,1], afri_model_p0_tr$marginals.fixed[[i]][,2], type="l", c(2, 2.8), main="ntl")
lines(afri_model_p0_ob$marginals.fixed[[i]][,1], afri_model_p0_ob$marginals.fixed[[i]][,2], col="blue")
lines(afri_model_p1_ob$marginals.fixed[[i]][,1], afri_model_p1_ob$marginals.fixed[[i]][,2], col="red")





# Fixed effects parameters distributions + geostat model
names_fxef <- names(afri_model_p1_tr$marginals.fixed)

i <- 1
plot(afri_model_p1_tr$marginals.fixed[[names_fxef[[i]]]][,1], afri_model_p1_tr$marginals.fixed[[names_fxef[[i]]]][,2], type="l", c(-20, 0))
plot(afri_model_p1_ob$marginals.fixed[[names_fxef[[i]]]][,1], afri_model_p1_ob$marginals.fixed[[names_fxef[[i]]]][,2], col="red")
i <- 8
plot(afri_model_p1_tr$marginals.fixed[[names_fxef[[i]]]][,1], afri_model_p1_tr$marginals.fixed[[names_fxef[[i]]]][,2], type="l", c(0.04, .1))
lines(afri_model_p1_ob$marginals.fixed[[names_fxef[[i]]]][,1], afri_model_p1_ob$marginals.fixed[[names_fxef[[i]]]][,2], col="red")
i <- 9
plot(afri_model_p1_tr$marginals.fixed[[names_fxef[[i]]]][,1], afri_model_p1_tr$marginals.fixed[[names_fxef[[i]]]][,2], type="l", c(1.5, 3))
lines(afri_model_p1_ob$marginals.fixed[[names_fxef[[i]]]][,1], afri_model_p1_ob$marginals.fixed[[names_fxef[[i]]]][,2], col="red")


# Random effects parameters distributions
plot(afri_model_p1_tr$marginals.hyperpar[[1]][,1], afri_model_p1_tr$marginals.hyperpar[[1]][,2], type="l", c(-6,-2))
lines(afri_model_p1_ob$marginals.hyperpar[[1]][,1], afri_model_p1_ob$marginals.hyperpar[[1]][,2], col="red")

plot(afri_model_p1_tr$marginals.hyperpar[[2]][,1], afri_model_p1_tr$marginals.hyperpar[[2]][,2], type="l", c(-2,2))
lines(afri_model_p1_ob$marginals.hyperpar[[2]][,1], afri_model_p1_ob$marginals.hyperpar[[2]][,2], col="red")

## Transformed
out_tr <- inla.spde2.result(inla=afri_model_p1_tr, name="u.field", spde=afri_spde, do.transform = TRUE)
out_ob <- inla.spde2.result(inla=afri_model_p1_ob, name="u.field", spde=afri_spde, do.transform = TRUE)

plot(out_tr$marginals.kappa[[1]][,1], out_tr$marginals.kappa[[1]][,2], type="l", xlim=c(0,7))
lines(out_ob$marginals.kappa[[1]][,1], out_ob$marginals.kappa[[1]][,2], col="red")

plot(out_tr$marginals.variance.nominal[[1]][,1], out_tr$marginals.variance.nominal[[1]][,2], type="l")
plot(out_ob$marginals.variance.nominal[[1]][,1], out_ob$marginals.variance.nominal[[1]][,2], col="red")


plot(out_tr$marginals.range.nominal[[1]][,1], out_tr$marginals.range.nominal[[1]][,2], type="l")
plot(out_ob$marginals.range.nominal[[1]][,1], out_ob$marginals.range.nominal[[1]][,2], col="red")

out_tr$marginals.