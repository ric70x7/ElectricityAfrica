
library(rstan)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(latex2exp)

rm(list=ls())

load("code_output/vgpm_samples.RData")

myblue <-"#6495ED"
mygreen <- "#86C67C"
myred <- "#EE6A50"
font_size <- 15


rbf_var= density(vgpm_samples$rbf_var)
rbf_lengthscale = density(vgpm_samples$rbf_lengthscale)
noise_var = density(vgpm_samples$noise_var)
rho_1 = density(vgpm_samples$rho[,1])
rho_2 = density(vgpm_samples$rho[,2])
rho_3 = density(vgpm_samples$rho[,3])
xx <- (1:100)/100
xx2 <- (1:5000)/1000
xx3 <- (1:500)/10000


plot_rbf_var <- ggplot(data.frame(x = sapply(rbf_var$x, function(x) ifelse(x<0,0,x)), y = rbf_var$y, dummy = "Posterior"), aes(x,y)) +
        geom_polygon(fill = mygreen) +
        geom_line(aes(col=dummy)) +
        geom_line(data = data.frame(x = xx, y = dgamma(xx, 30, 100), dummy = "Prior"), aes(x,y, col = dummy), size = 1) +
        xlim(0,.5) +
        theme(#axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.y = element_blank(),
              legend.position = c(.85,.85)) +
        scale_color_manual(name=TeX("$Kernel - \\sigma^2\\;$"), values=c(mygreen, myred)) 


plot_rbf_lengthscale <-  ggplot(data.frame(x = sapply(rbf_lengthscale$x, function(x) ifelse(x<0,0,x)), y = rbf_lengthscale$y, dummy = "Posterior"), aes(x,y)) +
        geom_polygon(fill = mygreen) +
        geom_line(aes(col=dummy)) +
        geom_line(data = data.frame(x = xx2, y = dgamma(xx2, 11, 8), dummy = "Prior"), aes(x,y, col = dummy), size = 1) +
        xlim(0,3) +
        theme(#axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.y = element_blank(),
              legend.position = c(.85,.85)) +
        scale_color_manual(name=TeX("Lengthscale"), values=c(mygreen, myred)) 


plot_noise_var <- ggplot(data.frame(x = sapply(noise_var$x, function(x) ifelse(x<0,0,x)), y = noise_var$y, dummy = "Posterior"), aes(x,y)) +
        geom_polygon(fill = mygreen) +
        geom_line(aes(col=dummy)) +
        #geom_line(data = data.frame(x = xx3, y = dgamma(xx3, 1, 1), dummy = "prior"), aes(x,y, col = dummy), size = 1) +
        geom_line(data = data.frame(x = xx3, y = dgamma(xx3, 20, 1000), dummy = "Prior"), aes(x,y, col = dummy), size = 1) +
        xlim(0,.03) +
        theme(#axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.y = element_blank(),
              legend.position = c(.85,.85)) +
        scale_color_manual(name=TeX("Noise - $\\sigma^2_\\epsilon\\;$"), values=c(mygreen, myred)) 


plot_byv <- ggplot(data.frame(x = sapply(rho_1$x, function(x) ifelse(x<0,0,x)), y = rho_1$y, dummy = "Posterior"), aes(x,y)) +
        geom_polygon(fill = mygreen) +
        geom_line(aes(col=dummy)) +
        geom_line(data = data.frame(x = xx, y = dbeta(xx, 7, 7), dummy = "Prior"), aes(x,y, col = dummy), size = 1) +
        xlim(0,1) +
        ylab("Density") +
        theme(#axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              axis.ticks.x = element_blank(),
              #axis.text.y = element_blank(),
              legend.position = c(.15,.85)) +
        scale_color_manual(name=TeX("$\\rho_{(EA,LP)}"), values=c(mygreen, myred)) 


plot_byw <-  ggplot(data.frame(x = sapply(rho_2$x, function(x) ifelse(x<0,0,x)), y = rho_2$y, dummy = "Posterior"), aes(x,y)) +
        geom_polygon(fill = mygreen) +
        geom_line(aes(col=dummy)) +
        geom_line(data = data.frame(x = xx, y = dbeta(xx, 7, 7), dummy = "Prior"), aes(x,y, col = dummy), size = 1) +
        xlim(0,1) +
        ylab("Density") +
        theme(#axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              axis.ticks.x = element_blank(),
              #axis.text.y = element_blank(),
              legend.position = c(.15,.85)) +
        scale_color_manual(name=TeX("$\\rho_{(EA,LPWP)}"), values=c(mygreen, myred)) 

plot_bvw <- ggplot(data.frame(x = sapply(rho_3$x, function(x) ifelse(x>1,1,x)), y = rho_3$y, dummy = "Posterior"), aes(x,y)) +
        geom_polygon(fill = mygreen) +
        geom_line(aes(col=dummy)) +
        geom_line(data = data.frame(x = xx, y = dbeta(xx, 7, 1), dummy = "Prior"), aes(x,y, col = dummy), size = 1) +
        xlim(0,1) +
        ylab("Density") +
        theme(#axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              axis.ticks.x = element_blank(),
              #axis.text.y = element_blank(),
              legend.position = c(.15,.85)) +
        scale_color_manual(name=TeX("$\\rho_{(LP,LPWP)}"), values=c(mygreen, myred)) 


#fig_posteriors <- ggdraw(xlim = c(0,12), ylim = c(0,8)) +
#            draw_plot(plot_rbf_var, x = 0, y = 4, width = 4, height = 3.7) +
#            draw_plot(plot_rbf_lengthscale, x = 4, y = 4, width = 4, height = 3.7) +
#            draw_plot(plot_noise_var, x = 8, y = 4, width = 4, height = 3.7) +
#            draw_plot(plot_byv, x = 0, y = 0, width = 4, height = 3.7) +
#            draw_plot(plot_byw, x = 4, y = 0, width = 4, height = 3.7) +
#            draw_plot(plot_bvw, x = 8, y = 0, width = 4, height = 3.7) +
#            draw_plot_label(c("A", "B", "C", "D", "E", "F"), c(0, 4, 8, 0, 4, 8), c(8, 8, 8, 4, 4, 4), size = 18, color = "grey")
#            save_plot("figs/fig_posteriors.pdf", fig_posteriors, base_width = 12, base_height = 8)

fig_posteriors <- ggdraw(xlim = c(0,12), ylim = c(0,4)) +
            draw_plot(plot_byv, x = 0, y = 0, width = 4, height = 3.7) +
            draw_plot(plot_byw, x = 4, y = 0, width = 4, height = 3.7) +
            draw_plot(plot_bvw, x = 8, y = 0, width = 4, height = 3.7) +
            draw_plot_label(c("A", "B", "C"), c(0, 4, 8), c(4, 4, 4), size = 18, color = "grey")

save_plot("figs/fig_posteriors.pdf", fig_posteriors, base_width = 12, base_height = 4)

