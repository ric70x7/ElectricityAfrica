# Geostatistic model covariates selection
# ---------------------------------------

graphics.off()
rm(list = ls())
source("code/5_inla_mesh.R")

# rename
df.train <- df.train[, c("has_electricity", "freq_impervious", "ntl", "kz", "lctype", "total", "lon", "lat", "z.year")]
df.test1 <- df.test1[, c("has_electricity", "freq_impervious", "ntl", "kz", "lctype", "total", "lon", "lat", "z.year")]
df.test2 <- df.test2[, c("has_electricity", "freq_impervious", "ntl", "kz", "lctype", "total", "lon", "lat", "z.year")]
colnames(df.train) <- c("y", "fimp", "ntlg", "udis", "ctyp", "total", "lon", "lat", "year")
colnames(df.test1) <- c("y", "fimp", "ntlg", "udis", "ctyp", "total", "lon", "lat", "year")
colnames(df.test2) <- c("y", "fimp", "ntlg", "udis", "ctyp", "total", "lon", "lat", "year")

#predictor_1 <- y ~ -1 + factor(ctyp)
#predictor_2 <- y ~ -1 + factor(ctyp) + udis
#predictor_3 <- y ~ -1 + factor(ctyp) + udis + ntlg
predictor_4 <- y ~ -1 + factor(ctyp) + udis + ntlg + fimp
predictor_5 <- y ~ -1 + factor(ctyp) + udis + ntlg + fimp + f(u.field, model=afr.spde)
predictor_6 <- y ~ -1 + factor(ctyp) + udis + ntlg + fimp + f(u.field, model=afr.spde) + year
predictor_7 <- y ~ -1 + factor(ctyp) + udis + ntlg + fimp + f(u_ar.field, model=afr.spde, group=u_ar.field.group, control.group=list(model="ar1"))

u.f <- inla.spde.make.index(name = "u.field", n.spde = afr.spde$n.spde) # random field model 5 and 6
u_ar.f <- inla.spde.make.index(name = "u_ar.field", n.spde = afr.spde$n.spde, n.group = 14) # random field model 7

A_5 <- inla.spde.make.A(mesh = mesh.s, loc = as.matrix(meta$points$spatial)) # A model 5
A_6 <- inla.spde.make.A(mesh = mesh.s, loc = as.matrix(meta$points$spatial)) # A model 6
A_7 <- inla.spde.make.A(mesh = mesh.s, loc = as.matrix(meta$points$spatial),
                        group = meta$ix$time.order, n.group = 14) # A model 7

stack_5 <- inla.stack(data = list(y = df.train$y),
                      A = list(A_5, 1, 1, 1, 1),
                      effects = list(u.f,
                                     list(ctyp = df.train$ctyp),
                                     list(ntlg = df.train$ntlg),
                                     list(fimp = df.train$fimp),
                                     list(udis = df.train$udis)),
                      tag = "train")

stack_6 <- inla.stack(data = list(y = df.train$y),
                      A = list(A_6, 1, 1, 1, 1, 1),
                      effects = list(u.f,
                                 list(ctyp = df.train$ctyp),
                                 list(ntlg = df.train$ntlg),
                                 list(fimp = df.train$fimp),
                                 list(udis = df.train$udis),
                                 list(year = df.train$year)),
                      tag = "train")

stack_7 <- inla.stack(data = list(y = df.train$y),
                      A = list(A_7, 1, 1, 1, 1, 1),
                      effects = list(u_ar.f,
                                     list(ctyp = df.train$ctyp),
                                     list(ntlg = df.train$ntlg),
                                     list(fimp = df.train$fimp),
                                     list(udis = df.train$udis),
                                     list(year = df.train$year)),
                      tag = "train")


# Fit model
#m_1 <- inla(predictor_1,  data = df.train, family = "binomial", Ntrials = df.train$total,
#            control.predictor = list(compute=TRUE),
#            control.compute = list(dic=TRUE, cpo=TRUE, config=TRUE))
#
#m_2 <- inla(predictor_2,  data = df.train, family = "binomial", Ntrials = df.train$total,
#            control.predictor = list(compute=TRUE),
#            control.compute = list(dic=TRUE, cpo=TRUE, config=TRUE))
#
#m_3 <- inla(predictor_3,  data = df.train, family = "binomial", Ntrials = df.train$total,
#            control.predictor = list(compute=TRUE),
#            control.compute = list(dic=TRUE, cpo=TRUE, config=TRUE))

m_4 <- inla(predictor_4,  data = df.train, family = "binomial", Ntrials = df.train$total,
            control.predictor = list(compute=TRUE),
            control.compute = list(dic=TRUE, cpo=TRUE, config=TRUE))

m_5 <- inla(predictor_5,  data = inla.stack.data(stack_5), family = "binomial", Ntrials = df.train$total,
          control.predictor = list(A=inla.stack.A(stack_5) , compute=TRUE),
          control.compute = list(dic=TRUE, cpo=TRUE, config=TRUE),
          control.inla = list(h=.03))
          #control.inla = list(tolerance=1e-6))

m_6 <- inla(predictor_6,  data = inla.stack.data(stack_6), family = "binomial", Ntrials = df.train$total,
          control.predictor = list(A=inla.stack.A(stack_6) , compute=TRUE),
          control.compute = list(dic=TRUE, cpo=TRUE, config=TRUE),
          control.inla = list(h=.03))

m_7 <- inla(predictor_7,  data = inla.stack.data(stack_7), family = "binomial", Ntrials = df.train$total,
          control.predictor = list(A=inla.stack.A(stack_7) , compute=TRUE),
          control.compute = list(dic=TRUE, cpo=TRUE, config=TRUE),
          control.inla = list(h=.03))

#load("code_output/m_57.RData")

#sum(log(m_1$cpo$cpo))
#sum(log(m_2$cpo$cpo))
#sum(log(m_3$cpo$cpo))
sum(log(m_4$cpo$cpo))
sum(log(m_5$cpo$cpo))
sum(log(m_6$cpo$cpo))
sum(log(m_7$cpo$cpo))
