# Geostatistic model covariates selection
# --------------------
#
# Edited: January 10, 2019


graphics.off()
rm(list = ls())
source("code/7_inla_mesh.R")
#source("7_inla_mesh.R")

# rename
df.train <- df.train[, c("has_electricity", "freq_impervious", "ntl", "kz", "lctype", "total", "lon", "lat", "z.year")]
df.test1 <- df.test1[, c("has_electricity", "freq_impervious", "ntl", "kz", "lctype", "total", "lon", "lat", "z.year")]
df.test2 <- df.test2[, c("has_electricity", "freq_impervious", "ntl", "kz", "lctype", "total", "lon", "lat", "z.year")]
colnames(df.train) <- c("y", "fimp", "ntlg", "udis", "ctyp", "total", "lon", "lat", "year")
colnames(df.test1) <- c("y", "fimp", "ntlg", "udis", "ctyp", "total", "lon", "lat", "year")
colnames(df.test2) <- c("y", "fimp", "ntlg", "udis", "ctyp", "total", "lon", "lat", "year")

predictor_6 <- y ~ -1 + factor(ctyp) + udis + ntlg + fimp + f(u.field, model=afr.spde) + year

u.f <- inla.spde.make.index(name = "u.field", n.spde = afr.spde$n.spde)
A_6_1 <- inla.spde.make.A(mesh = mesh.s, loc = as.matrix(df.train[, c("lon", "lat")]))
A_6_2 <- inla.spde.make.A(mesh = mesh.s, loc = as.matrix(df.test1[, c("lon", "lat")]))
A_6_3 <- inla.spde.make.A(mesh = mesh.s, loc = as.matrix(df.test2[, c("lon", "lat")]))

stack_6_1 <- inla.stack(data = list(y = df.train$y),
                        A = list(A_6_1, 1, 1, 1, 1, 1),
                        effects = list(u.f,
                                   list(ctyp = df.train$ctyp),
                                   list(ntlg = df.train$ntlg),
                                   list(fimp = df.train$fimp),
                                   list(udis = df.train$udis),
                                   list(year = df.train$year)),
                        tag = "train")

stack_6_2 <- inla.stack(data = list(y = NA),
                        A = list(A_6_2, 1, 1, 1, 1, 1),
                        effects = list(u.f,
                                   list(ctyp = df.test1$ctyp),
                                   list(ntlg = df.test1$ntlg),
                                   list(fimp = df.test1$fimp),
                                   list(udis = df.test1$udis),
                                   list(year = df.test1$year)),
                        tag = "test1")

stack_6_3 <- inla.stack(data = list(y = NA),
                        A = list(A_6_3, 1, 1, 1, 1, 1),
                        effects = list(u.f,
                                   list(ctyp = df.test2$ctyp),
                                   list(ntlg = df.test2$ntlg),
                                   list(fimp = df.test2$fimp),
                                   list(udis = df.test2$udis),
                                   list(year = df.test2$year)),
                        tag = "test2")


# Join stacks
stack_6 <- do.call(inla.stack, list(stack_6_1, stack_6_2, stack_6_3))
ix_train <- inla.stack.index(stack_6, tag="train")
ix_test1 <- inla.stack.index(stack_6, tag="test1")
ix_test2 <- inla.stack.index(stack_6, tag="test2")

# Fit model
m_6 <- inla(predictor_6,  data = inla.stack.data(stack_6), family = "binomial",
            Ntrials = c(df.train$total, df.test1$total, df.test2$total),
            control.predictor = list(A=inla.stack.A(stack_6) , compute=TRUE),
            control.compute = list(dic=TRUE, cpo=TRUE, config=TRUE),
            control.inla = list(h=.03))

#save(df.train, df.test1, df.test2, ix_train, ix_test1, ix_test2, m_6,
#     file="code_output/validation_data.RData")

library(INLA)
library(raster)
library(maptools)
load("code_output/validation_data.RData")

# Make predictions
pred_train <- c()
pred_test1 <- c()
pred_test2 <- c()

for(i in seq(nrow(df.train))){
  pred_train[i] <- inla.emarginal(inla.link.invlogit,
                                  m_6$marginals.linear.predictor[ix_train$data][[i]])
}

for(i in seq(nrow(df.test1))){
  pred_test1[i] <- inla.emarginal(inla.link.invlogit,
                                  m_6$marginals.linear.predictor[ix_test1$data][[i]])
}

for(i in seq(nrow(df.test2))){
  pred_test2[i] <- inla.emarginal(inla.link.invlogit,
                                  m_6$marginals.linear.predictor[ix_test2$data][[i]])
}

# Simulations Train
sim0 <- c()
ksim <- 100
for(j in 1:ksim){
  sim_sub <- rep(NA, nrow(df.train))
  for(i in 1:nrow(df.train)){
    sim_sub[i] <- rbinom(n = 1, size = 1, prob = (df.train$y/df.train$total)[i])
  }
  sim0 <- c(sim0, sim_sub)
}
trn0 <- rep(pred_train, ksim)
nsim0 <- ksim * nrow(df.train)

# ROC
false_pos <- c()
true_pos <- c()
for(threshold in 0:10/10){
  false_pos <- c(false_pos, sum(sim0 == 0 & trn0 > threshold)/ sum(sim0 == 0) )
  true_pos <- c(true_pos, sum(sim0 == 1 & trn0 > threshold) / sum(sim0 == 1) )
}
plot(false_pos, true_pos, type = "bo")



# Simulations
sim1 <- c()
ksim <- 100
for(j in 1:ksim){
  sim_sub <- rep(NA, nrow(df.test1))
  for(i in 1:nrow(df.test1)){
    sim_sub[i] <- rbinom(n = 1, size = 1, prob = (df.test1$y/df.test1$total)[i])
  }
  sim1 <- c(sim1, sim_sub)
}
tst1 <- rep(pred_test1, ksim)
nsim1 <- ksim * nrow(df.test1)
threshold = .5


# Confusion matrix V1
round(matrix(100 * c(sum(sim1 == 0 & tst1 <= threshold)/nsim1,
                     sum(sim1 == 0 & tst1 > threshold)/nsim1,
                     sum(sim1 == 1 & tst1 <= threshold)/nsim1,
                     sum(sim1 == 1 & tst1 > threshold)/nsim1),
             nrow = 2, byrow = TRUE), 2)

tp1 <- sum(sim1 == 1 & tst1 > threshold)
fp1 <- sum(sim1 == 1 & tst1 <= threshold)
tn1 <- sum(sim1 == 0 & tst1 <= threshold)
fn1 <- sum(sim1 == 0 & tst1 > threshold)

# Precision
tp1/(tp1 + fp1)
# Recall
tp1/(tp1 + fn1)



# Confusion matrix V2
threshold = .5
round(matrix(100 * c(sum(df.test2$y == 0 & pred_test2 <= threshold)/nrow(df.test2),
                     sum(df.test2$y == 0 & pred_test2 > threshold)/nrow(df.test2),
                     sum(df.test2$y == 1 & pred_test2 <= threshold)/nrow(df.test2),
                     sum(df.test2$y == 1 & pred_test2 > threshold)/nrow(df.test2)),
             nrow = 2, byrow = TRUE), 2)

tp2 <- sum(df.test2$y == 1 & pred_test2 > threshold)
fp2 <- sum(df.test2$y == 1 & pred_test2 <= threshold)
tn2 <- sum(df.test2$y == 0 & pred_test2 <= threshold)
fn2 <- sum(df.test2$y == 0 & pred_test2 > threshold)

# Precision
tp2/(tp2 + fp2)
# Recall
tp2/(tp2 + fn2)


