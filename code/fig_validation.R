# Validation figures
# ------------------------
#
# Edited: October 22, 2016


library(INLA)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(xtable)
rm(list = ls())
graphics.off()

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
dftrain$dummy <- "no electricity"
dftrain$dummy[dftrain$observed>.5] <- "electricity"
#ggplot(dftrain, aes(observed, predicted)) + geom_point(color = mygreen, alpha = .5)
train_dens <- ggplot(dftrain, aes(predicted)) + geom_density(aes(fill = factor(dummy)), alpha = .5) + xlim(0,1)


dftest1 <- data.frame(observed = df.test1$r, predicted = predicted.test1.mean)
dftest1$dummy <- "no electricity"
dftest1$dummy[dftest1$observed>.5] <- "electricity"

test1_dens <-
  ggplot(dftest1, aes(predicted)) +
  geom_density(aes(fill = factor(dummy)), alpha = .70) +
  xlim(0,1) +
  xlab("probability estimates") +
  ylab("density") +
  theme_hc(base_size = font_size) +
  theme(legend.position = c(.75,.85)) +
  scale_fill_manual(values = c(myblue, myred), guide = guide_legend(title = "Actual category\n(V1)"))


dftest2 <- data.frame(observed = df.test2$r, predicted = predicted.test2.mean)
dftest2$dummy <- "no electricity"
dftest2$dummy[dftest2$observed>.5] <- "electricity"

test2_dens <-
  ggplot(dftest2, aes(predicted)) +
  geom_density(aes(fill = factor(dummy)), alpha = .70) +
  xlim(0,1) +
  xlab("probability estimates") +
  ylab("density") +
  theme_hc(base_size = font_size) +
  theme(legend.position = c(.75,.85)) +
  scale_fill_manual(values = c(myblue, myred), guide = guide_legend(title = "Actual category\n(V2)"))


false_pos1 <- c()
true_pos1 <- c()
for(threshold in 0:1000/1000){
  false_pos1 <- c(false_pos1, sum(df.test1$r <= .5 & predicted.test1.mean > threshold)/sum(df.test1$r<=.5))
  true_pos1 <- c(true_pos1, sum(df.test1$r > .5 & predicted.test1.mean > threshold)/sum(df.test1$r>.5))
}

false_pos2 <- c()
true_pos2 <- c()
for(threshold in 0:1000/1000){
  false_pos2 <- c(false_pos2, sum(df.test2$r == 0 & predicted.test2.mean > threshold)/sum(df.test2$r==0))
  true_pos2 <- c(true_pos2, sum(df.test2$r == 1 & predicted.test2.mean > threshold)/sum(df.test2$r==1))
}
roc_df <- data.frame(tp1 = true_pos1, fp1 = false_pos1, tp2 = true_pos2, fp2 = false_pos2, dummy1 = "DHS hold out", dummy2 = "MIS/AIS")


plt_roc <- 
  ggplot(roc_df)  +
  geom_path(aes(fp1, tp1, color = dummy1), size = 1.5) +
  geom_path(aes(fp2, tp2, color = dummy2), size = 1.5, alpha = .7) +
  xlab("false positives") +
  ylab("true  positives") +
  theme_hc(base_size = font_size) +
  theme(legend.position = c(.75,.3)) +
  scale_color_manual(name=c("ROC curve"), values = c(myred, mygreen)) 
  

fig_validation <-
  ggdraw(xlim = c(0,12), ylim = c(0,4)) +
  draw_plot(test1_dens, x = 0, y = 0, width = 4, height = 3.7) +
  draw_plot(test2_dens, x = 4, y = 0, width = 4, height = 3.7) +
  draw_plot(plt_roc, x = 8, y = 0, width = 4, height = 3.7) +
  draw_plot_label(c("A", "B", "C"), c(0, 4, 8), c(4, 4, 4), size = 18, color = "grey")
  save_plot("figs/fig_validation.pdf", fig_validation, base_width = 12, base_height = 4)


  

threshold = .5
conf_matrix1 <- matrix(100 * c(sum(df.test1$r <= .5 & predicted.test1.mean <= threshold)/nrow(df.test1),
                sum(df.test1$r <= .5 & predicted.test1.mean > threshold)/nrow(df.test1),
                sum(df.test1$r > .5 & predicted.test1.mean <= threshold)/nrow(df.test1),
                sum(df.test1$r > .5 & predicted.test1.mean > threshold)/nrow(df.test1)),
                nrow = 2, byrow = TRUE)
colnames(conf_matrix1) <- c("No electricity", "Electricity")
rownames(conf_matrix1) <- c("No electricity", "Electricity")
conf_matrix1


threshold = .5
conf_matrix2 <- matrix(100 * c(sum(df.test2$r == 0 & predicted.test2.mean <= threshold)/nrow(df.test2),
                sum(df.test2$r == 0 & predicted.test2.mean > threshold)/nrow(df.test2),
                sum(df.test2$r == 1 & predicted.test2.mean <= threshold)/nrow(df.test2),
                sum(df.test2$r == 1 & predicted.test2.mean > threshold)/nrow(df.test2)),
                nrow = 2, byrow = TRUE)
colnames(conf_matrix2) <- c("No electricity", "Electricity")
rownames(conf_matrix2) <- c("No electricity", "Electricity")
conf_matrix2

xtable(conf_matrix1, align = c(rep("|l",3),"|"))
xtable(conf_matrix2, align = c(rep("|l",3),"|"))
