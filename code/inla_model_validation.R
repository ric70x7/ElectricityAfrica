# Validation of core_model
# ------------------------
#
# Edited: October 17, 2016


library(INLA)
library(ggplot2)
rm(list = ls())

load("code_output/core_model_fit.RData")
load("code_output/core_model_data.RData")
myblue <-"#6495ED"
mygreen <- "#86C67C"
myred <- "#EE6A50"
font_size <- 15


# Train data
predicted.train.mean <- c()
for(i in seq(nrow(df.train))){
  predicted.train.mean[i] <- inla.emarginal(inla.link.invlogit,
                                           m_core$marginals.linear.predictor[meta_core$ix$stack$train][[i]] )
}


# Test1 data
predicted.test1.mean <- c()
for(i in seq(nrow(df.test1))){
  predicted.test1.mean[i] <- inla.emarginal(inla.link.invlogit,
                                            m_core$marginals.linear.predictor[meta_core$ix$stack$test1][[i]] )
}


# Test2 data
predicted.test2.mean <- c()
for(i in seq(nrow(df.test2))){
  predicted.test2.mean[i] <- inla.emarginal(inla.link.invlogit,
                                            m_core$marginals.linear.predictor[meta_core$ix$stack$test2][[i]] )
}


dftrain <- data.frame(observed = df.train$r, predicted = predicted.train.mean)
dftrain$dummy <- "no"
dftrain$dummy[dftrain$observed>.5] <- "yes"
ggplot(dftrain, aes(observed, predicted)) + geom_point(color = mygreen, alpha = .5)
ggplot(dftrain, aes(predicted)) + geom_density(aes(fill = factor(dummy)), alpha = .5) + xlim(0,1)


dftest1 <- data.frame(observed = df.test1$r, predicted = predicted.test1.mean)
dftest1$dummy <- "no"
dftest1$dummy[dftest1$observed>.5] <- "yes"
ggplot(dftest1, aes(observed, predicted)) + geom_point(color = myblue, alpha = .5)
ggplot(dftest1, aes(predicted)) + geom_density(aes(fill = factor(dummy)), alpha = .5) + xlim(0,1)


dftest2 <- data.frame(observed = df.test2$r, predicted = predicted.test2.mean)
ggplot(dftest2, aes(observed, predicted)) + geom_point(color = myred, alpha = .5)
ggplot(dftest2, aes(predicted)) + geom_density(aes(fill = factor(observed)), alpha = .5) + xlim(0,1)


false_pos <- c()
true_pos <- c()
for(threshold in 1:1000/1000){
  false_pos <- c(false_pos, sum(df.test2$r == 0 & predicted.test2.mean > threshold)/sum(df.test2$r==0))
  true_pos <- c(true_pos, sum(df.test2$r == 1 & predicted.test2.mean > threshold)/sum(df.test2$r==1))
}
plot(false_pos, true_pos, type = "l")


threshold = .5
matrix(100 * c(sum(df.test1$r <= .5 & predicted.test1.mean <= threshold)/nrow(df.test1),
               sum(df.test1$r <= .5 & predicted.test1.mean > threshold)/nrow(df.test1),
               sum(df.test1$r > .5 & predicted.test1.mean <= threshold)/nrow(df.test1),
               sum(df.test1$r > .5 & predicted.test1.mean > threshold)/nrow(df.test1)),
       nrow = 2, byrow = TRUE)


threshold = .5
matrix(100 * c(sum(df.test2$r == 0 & predicted.test2.mean <= threshold)/nrow(df.test2),
               sum(df.test2$r == 0 & predicted.test2.mean > threshold)/nrow(df.test2),
               sum(df.test2$r == 1 & predicted.test2.mean <= threshold)/nrow(df.test2),
               sum(df.test2$r == 1 & predicted.test2.mean > threshold)/nrow(df.test2)),
       nrow = 2, byrow = TRUE)


