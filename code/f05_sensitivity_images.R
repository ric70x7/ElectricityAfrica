# Fig: Sensitivity Analysis Images
# --------------------------------

rm(list = ls())
graphics.off()
library(ggplot2)
library(INLA)
library(maptools)


load("code_output/offset_sensitivity.RData")
load("code_output/synthetic_params.RData")

# Define INLA mesh
afri_main <- rgdal::readOGR("data/Africa_main_country/Africa_main_country.shp")
afri_border <- unionSpatialPolygons(afri_main, rep(1, nrow(afri_main)))
afri_segment <- inla.sp2segment(afri_border)
max_ <- 1.5
cut_ <- 1.4
afri_mesh <- inla.mesh.2d(boundary = afri_segment, max.edge = max_, offset=c(1, 2), cutoff = cut_)
afri_spde <- inla.spde2.matern(mesh = afri_mesh, alpha = 2)
u.f <- inla.spde.make.index(name = "u.field", n.spde = afri_spde$n.spde)
afri_A_tr <- inla.spde.make.A(mesh = afri_mesh, loc = as.matrix(dftruth[, c("lon", "lat")]))
afri_A_ob <- inla.spde.make.A(mesh = afri_mesh, loc = as.matrix(dfobfsc[, c("lon", "lat")]))

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

font_size <- 8
plt_legend1 <- ggplot() + theme_void() + coord_equal() + xlim(0, 12) + ylim(0, 6) 
  #geom_text() + annotate("text", label="Population\n(x 1000)", x=2, y=6, size=font_size+1)
intervals = c("Exact locations", "Displaced locations")
for (i in 2:3) {
  square <- data.frame(x=c(0, 1, 1, 0, 0), y=c(i, i, i+1, i+1, i))
  plt_legend1 <- plt_legend1 + geom_boxplot(data=square, aes(x=x, y=y),
                                          fill = viridis::viridis_pal(option="E")(2)[i-1], alpha=.8) +
    geom_text() + annotate("text", label=intervals[i-1], x = 1.5, y=i + .5, size = font_size, hjust=0)
}



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
        cowplot::draw_plot(plt10, x=2*psc, y=1*psc, width = 1*psc, height = 1*psc) +

        cowplot::draw_plot(plt9, x=0*psc, y=0*psc, width = 1*psc, height = 1*psc) +
        cowplot::draw_plot(plt_legend1, x=1*psc, y=0*psc, width = 2*psc, height = 1*psc) +

        cowplot::draw_plot_label(LETTERS[1:10], c(rep(0:2, 3), 1)*psc,
                                 c(sort(rep(2:4, 3)*psc, decreasing = TRUE), 1*psc),
                                 size=18, colour = "grey")

#cowplot::ggsave('figs/comparison_fixed.pdf', plt_fixed, width=12, height=16)
cowplot::ggsave('figs/comparison_fixed.eps', plt_fixed, width=12, height=16, device=cairo_ps)

# Panel plot random effects
psc <- 4
plt_random <- cowplot::ggdraw(xlim=c(0,4*psc), ylim = c(0,1*psc)) +
        cowplot::draw_plot(plt11, x=0*psc, y=0*psc, width = 1*psc, height = 1*psc) +
        cowplot::draw_plot(plt12, x=1*psc, y=0*psc, width = 1*psc, height = 1*psc) +
        cowplot::draw_plot(plt13, x=2*psc, y=0*psc, width = 1*psc, height = 1*psc) +
        cowplot::draw_plot(plt_legend1, x=3*psc, y=0*psc, width = 1*psc, height = 1*psc) +

        cowplot::draw_plot_label(LETTERS[1:3], (0:2)*psc, rep(1, 3)*psc,
                                 size=18, colour = "grey")

#cowplot::ggsave('figs/comparison_random.pdf', plt_random, width=12, height=4)
cowplot::ggsave('figs/comparison_random.eps', plt_random, width=16, height=4, device=cairo_ps)


# Obfuscated vs Non-obfuscated estimates

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

ix <- dftruth$y > 0 & dftruth$y < dftruth$total #sample(1:nrow(dftruth), 5000)
logitf <- function(x) {return(log(x/(1-x)))}
dfvs <- data.frame(raw = logitf(model_p1_tr$summary.fitted.values$mean[stack_ix_tr$data])[ix],
                   obfuscated = logitf(model_p1_ob$summary.fitted.values$mean[stack_ix_ob$data])[ix],
                   true = logitf((dftruth$y/dftruth$total)[ix]))

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

#cowplot::ggsave('figs/comparison_estimates.pdf', plt_vs, width=12, height=4)
cowplot::ggsave('figs/comparison_estimates.eps', plt_vs, width=12, height=4, device=cairo_ps)

centered_diff <- data.frame(lon = (dfobfsc$lon - dftruth$lon)*111,
                            lat = (dfobfsc$lat - dftruth$lat)*111)

distob <- data.frame(distance = sqrt(rowSums((dftruth[, c("lon", "lat")] - dfobfsc[, c("lon", "lat")])^2)) * 111)

pltcirc <- ggplot(centered_diff[seq(1,nrow(centered_diff), by=4),], aes(lon, lat)) +
  geom_point(col="magenta", alpha=.4) + geom_point(data=data.frame(lon=0, lat=0), color="darkred", size=2.5) +
  geom_text(data=data.frame(lon=5, lat=0, txt="|"), aes(label=txt), color="darkred", size=8) +
  geom_text(data=data.frame(lon=10, lat=0, txt="|"), aes(label=txt), color="darkred", size=8) +
  geom_text(data=data.frame(lon=5, lat=0, txt="5 km"), nudge_x=-1.2, nudge_y=-1.2, aes(label=txt), color="darkred", size=8) +
  geom_text(data=data.frame(lon=10, lat=0, txt="10 km"), nudge_x=-1.2, nudge_y=-1.2, aes(label=txt), color="darkred", size=8) +
  geom_line(data=data.frame(lon=c(0, 10), lat=c(0, 0)), color="darkred", size=1) +
  scale_y_continuous("Offset (km)") +
  scale_x_continuous("Offset (km)") +
  coord_fixed() +
  theme_void() #+ gtheme

plthist <- ggplot(distob, aes(distance)) +
  geom_histogram(breaks=c(seq(0,10, by=1)), fill=viridis::viridis_pal(option="E")(2)[2],
                 col=viridis::viridis_pal(option="E")(2)[1]) +
  scale_y_continuous("Count") +
  scale_x_continuous("Offset (km)") +
  theme_classic() +
  gtheme

# Panel plot
psc <- 4
plt_off <- cowplot::ggdraw(xlim=c(0,3*psc), ylim = c(0,1*psc)) +
           cowplot::draw_plot(pltcirc, x=0*psc, y=0*psc, width = 1.5*psc, height = 1*psc) +
           cowplot::draw_plot(plthist, x=1.5*psc, y=0*psc, width = 1.5*psc, height = 1*psc) +
           cowplot::draw_plot_label(LETTERS[1:2], c(0, 1.5)*psc, rep(1, 2)*psc,
                                    size=18, colour = "grey")

#cowplot::ggsave('figs/offset_distance.pdf', plt_off, width=12, height=4)
cowplot::ggsave('figs/offset_distance.eps', plt_off, width=12, height=4, device=cairo_ps)
