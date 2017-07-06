# Validation of core_model
# ------------------------
#
# Edited: October 17, 2016


library(INLA)
library(ggplot2)
library(ggthemes)
library(cowplot)
rm(list = ls())

load("code_output/core_model_fit.RData")
load("code_output/core_model_data.RData")
myblue <- "#E69F00"# "#56B4E9"#"#6495ED"
mygreen <- "#86C67C"
myred <- "#0072B2"#"#009E73" #"#CC79A7"#"#EE6A50"
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



false_pos <- c()
true_pos <- c()
for(threshold in 1:1000/1000){
  false_pos <- c(false_pos, sum(df.test2$r == 0 & predicted.test2.mean > threshold)/sum(df.test2$r==0))
  true_pos <- c(true_pos, sum(df.test2$r == 1 & predicted.test2.mean > threshold)/sum(df.test2$r==1))
}
plot(false_pos, true_pos, type = "l")


sim1 <- c()
for(j in 1:1000){
  for(i in 1:nrow(df.test1)){
    sim1[i] <- rbinom(n = 1, size = 1, prob = df.test1$r[i])
  }
}
tst1 <- rep(predicted.test1.mean, 1000)
nsim1 <- 1000 * nrow(df.test1)
threshold = .25

round(matrix(100 * c(sum(sim1 == 0 & tst1 <= threshold)/nsim1,
               sum(sim1 == 0 & tst1 > threshold)/nsim1,
               sum(sim1 == 1 & tst1 <= threshold)/nsim1,
               sum(sim1 == 1 & tst1 > threshold)/nsim1),
       nrow = 2, byrow = TRUE), 2)


threshold = .25
round(matrix(100 * c(sum(df.test2$r == 0 & predicted.test2.mean <= threshold)/nrow(df.test2),
               sum(df.test2$r == 0 & predicted.test2.mean > threshold)/nrow(df.test2),
               sum(df.test2$r == 1 & predicted.test2.mean <= threshold)/nrow(df.test2),
               sum(df.test2$r == 1 & predicted.test2.mean > threshold)/nrow(df.test2)),
       nrow = 2, byrow = TRUE), 2)



dftest1 <- data.frame(observed = sim1, predicted = predicted.test1.mean)
dftest1$dummy <- "No electricity"
dftest1$dummy[dftest1$observed>.5] <- "Electricity"

test1_dens <-
  ggplot(dftest1, aes(predicted)) +
  geom_density(aes(fill = factor(dummy)), alpha = .70, adjust = 1.5) +
  xlim(0,1) +
  xlab("Probability estimates") +
  ylab("Density") +
  scale_fill_manual(values = c(myblue, myred), guide = guide_legend(title = "Actual category\n(V1)")) +
  theme_bw() +
  theme(panel.border = element_blank(),
        legend.position = c(.75,.85),
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size = 12),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15)) 


dftest2 <- data.frame(observed = df.test2$r, predicted = predicted.test2.mean)
dftest2$dummy <- "No electricity"
dftest2$dummy[dftest2$observed>.5] <- "Electricity"
test2_dens <-
  ggplot(dftest2, aes(predicted)) +
  geom_density(aes(fill = factor(dummy)), alpha = .70, adjust = 3) +
  xlim(0,1) +
  xlab("Probability estimates") +
  ylab("Density") +
  theme(legend.position = c(.75,.85)) +
  scale_fill_manual(values = c(myblue, myred), guide = guide_legend(title = "Actual category\n(V2)")) +
  theme_bw() +
  theme(panel.border = element_blank(),
        legend.position = c(.75,.85),
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size = 12),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15)) 


false_pos1 <- c()
true_pos1 <- c()
for(threshold in 0:1000/1000){
  false_pos1 <- c(false_pos1, sum(sim1 == 0 & predicted.test1.mean > threshold)/sum(sim1 == 0))
  true_pos1 <- c(true_pos1, sum(sim1 == 1 & predicted.test1.mean > threshold)/sum(sim1 == 1))
}

false_pos2 <- c()
true_pos2 <- c()
for(threshold in 0:1000/1000){
  false_pos2 <- c(false_pos2, sum(df.test2$r == 0 & predicted.test2.mean > threshold)/sum(df.test2$r==0))
  true_pos2 <- c(true_pos2, sum(df.test2$r == 1 & predicted.test2.mean > threshold)/sum(df.test2$r==1))
}
roc_df <- data.frame(tp1 = true_pos1, fp1 = false_pos1, tp2 = true_pos2, fp2 = false_pos2,
                     dummy1 = "V1", dummy2 = "V2")

plt_roc <- 
  ggplot(roc_df)  +
  geom_path(aes(fp1*100, tp1*100, color = dummy1), size = 1.5) +
  geom_path(aes(fp2*100, tp2*100, color = dummy2), size = 1.5, alpha = .7) +
  xlab("False positives (%)") +
  ylab("True  positives (%)") +
  scale_color_manual(name=c("ROC curve"), values = c("#D55E00", "#0072B2")) +
  theme_bw() +
  theme(panel.border = element_blank(),
        legend.position = c(.75,.3),
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15)) 


fig_validation <-
  ggdraw(xlim = c(0,12), ylim = c(0,4)) +
  draw_plot(test1_dens, x = 0, y = 0, width = 4, height = 3.7) +
  draw_plot(test2_dens, x = 4, y = 0, width = 4, height = 3.7) +
  draw_plot(plt_roc, x = 8, y = 0, width = 4, height = 3.7) +
  draw_plot_label(c("A", "B", "C"), c(0, 4, 8), c(4, 4, 4), size = 18, color = "grey")
save_plot("figs/fig_validation.pdf", fig_validation, base_width = 12, base_height = 4)

