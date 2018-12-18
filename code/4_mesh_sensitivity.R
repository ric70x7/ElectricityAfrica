# INLA mesh_sensitivity
# ------------------
#
# Edited: November 19, 2018


library(INLA)
library(raster)
library(maptools)
library(ggplot2)

graphics.off()
rm(list = ls())
set.seed(100)
load("code_output/df_model.RData")
afri_main <- rgdal::readOGR("data/Africa_main_country/Africa_main_country.shp")
afri_border <- unionSpatialPolygons(afri_main, rep(1, nrow(afri_main)))
afri_segment <- inla.sp2segment(afri_border)

# This function takes a dataset, a mesh and trains an INLA model
train_inla <- function(afri_mesh, afri_data) {
  
  afri_spde <- inla.spde2.matern(mesh = afri_mesh, alpha = 2)
  
  # INLA objects 
  predictor <- y ~ -1 + f(u.field, model=afri_spde) 
  u.f <- inla.spde.make.index(name = "u.field", n.spde = afri_spde$n.spde)
  afri_A <- inla.spde.make.A(mesh = afri_mesh, loc = as.matrix(afri_data[, c("lon", "lat")]))
  afri_stack <- inla.stack(data = list(y = afri_data$has_electricity),
                           A = list(afri_A), effects = list(u.f),
                           tag = "afri_data")
  stack_ix <- inla.stack.index(afri_stack, tag = "afri_data")
  
  # Fit model
  afri_model <- inla(predictor,  data = inla.stack.data(afri_stack), family = "binomial",
                     Ntrials = afri_data$total,
                     control.predictor = list(A=inla.stack.A(afri_stack) , compute=TRUE),
                     control.compute = list(dic=TRUE, cpo=TRUE, config=TRUE))
  
  
  # Make predictions
  afri_pred <- c()
  for(i in seq(nrow(df_10))){
    afri_pred[i] <- inla.emarginal(inla.link.invlogit,
                                   afri_model$marginals.linear.predictor[stack_ix$data][[i]] )
  }
  
  return(list(model=afri_model, pred=afri_pred))
}

# Use 1/4 of the data
sample_ix <- caret::createFolds(y=df_model$has_electricity, k = 4)
df_10 <- df_model[sample_ix$Fold1, ]

# Define multiple mesh grids increasing max.edge
max_ <- c(1.5)
cut_ <- seq(.2, 2, by=.2)
mesh_list <- list()
spde_list <- list()
model_list <- list()
pairs_list <- list()
k <-1
for (i in seq(max_)) {
  for (j in seq(cut_)) {
    print(paste("k:", k, ", i:", i, ", j:",j))
    mesh_list[[k]] <- inla.mesh.2d(boundary = afri_segment, max.edge = c(max_[i], 4),
                                   offset=c(1, 2), cutoff = cut_[j])
    
    spde_list[[k]] <- inla.spde2.matern(mesh = mesh_list[[k]], alpha = 2)
    model_list[[k]] <- train_inla(mesh_list[[k]], df_10)
    pairs_list[[k]] <- c(max_[i],cut_[j])
    k <- k + 1
  }
}

#save(mesh_list, spde_list, model_list, pairs_list, file="code_output/mesh_sensitivity.RData")

pairs_list
# Plots

# Mesh
#load("code_output/mesh_sensitivity.RData")
graphics.off()

pdf(file = "new_figs/afri_mesh.pdf")
raster::plot(afri_main, border="grey")
raster::plot(mesh_list[[7]], vertex.color = "steelblue",  edge.color = "gray", main=NULL,
             draw.segments = FALSE,
             draw.vertices = TRUE,
             add=TRUE)
raster::plot(afri_main, border="darkblue", add=TRUE)
dev.off()

# CPO - cutoff
cpos <- c()
for (i in seq(model_list)) {
    cpos[i] <- sum(log(model_list[[i]][[1]]$cpo$cpo))
}
  
sensdf <- data.frame(max_edge=sort(rep(max_, length(cut_))), cutoff=rep(cut_, length(max_)), cpo=cpos)

pdf(file = "new_figs/cpo_cutoff.pdf")
ggplot(sensdf, aes(cut_, cpo)) +
  geom_vline(xintercept = 1.5, col="darkred") +
  geom_line(col="steelblue") +
  geom_text(data=data.frame(x=1.7, y = -73500, m='Max edge'), aes(x,y, label=m), col="darkred", size=5) +
  theme_classic() +
  scale_x_continuous("cut off threshold") +
  scale_y_continuous("log CPO") +
  theme(panel.border = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15)) 

dev.off()





