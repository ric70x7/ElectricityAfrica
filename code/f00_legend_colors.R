# Fig: Legend color squares
# -------------------------

library(ggplot2)

# Two classes - viridis E
square <- data.frame(x=c(0, 1, 1, 0, 0), y=c(0, 0, 1, 1, 0))
plt <- ggplot(square, aes(x=x, y=y)) +
  geom_boxplot(fill = viridis::viridis_pal(option="E")(2)[1], alpha=.7) +
  theme_void()
#ggsave('figs/binary_label_1.pdf', plot = plt)
ggsave("figs/binary_label_1.eps", plot = plt, device=cairo_ps)

plt <- ggplot(square, aes(x=x, y=y)) +
  geom_boxplot(fill = viridis::viridis_pal(option="E")(2)[2], alpha=.7 ) +
  theme_void()
#ggsave('figs/binary_label_2.pdf', plot = plt)
ggsave('figs/binary_label_2.eps', plot = plt, device=cairo_ps)


# Three classes - viridis E
square <- data.frame(x=c(0, 1, 1, 0, 0), y=c(0, 0, 1, 1, 0))
for (i in 1:3) {
  plt <- ggplot(square, aes(x=x, y=y)) +
    geom_boxplot(fill = viridis::viridis_pal(option="E")(3)[i]) +
    theme_void()
  #ggsave(paste0("figs/three_labels_",i, ".pdf"), plot = plt)
  ggsave(paste0("figs/three_labels_",i, ".eps"), plot = plt, device=cairo_ps)
}


# Five classes - viridis E
square <- data.frame(x=c(0, 1, 1, 0, 0), y=c(0, 0, 1, 1, 0))
for (i in 1:5) {
  plt <- ggplot(square, aes(x=x, y=y)) +
    geom_boxplot(fill = viridis::viridis_pal(option="E")(5)[i]) +
    theme_void()
  #ggsave(paste0("figs/five_labels_",i, ".pdf"), plot = plt)
  ggsave(paste0("figs/five_labels_",i, ".eps"), plot = plt, device=cairo_ps)
}


# Five classes - viridis magma
square <- data.frame(x=c(0, 1, 1, 0, 0), y=c(0, 0, 1, 1, 0))
for (i in 1:5) {
  plt <- ggplot(square, aes(x=x, y=y)) +
    geom_boxplot(fill = viridis::viridis_pal(option="magma")(5)[i]) +
    theme_void()
  #ggsave(paste0("figs/five_magma_",i, ".pdf"), plot = plt)
  ggsave(paste0("figs/five_magma_",i, ".eps"), plot = plt, device=cairo_ps)
}


# Five classes - viridis
square <- data.frame(x=c(0, 1, 1, 0, 0), y=c(0, 0, 1, 1, 0))
for (i in 1:5) {
  plt <- ggplot(square, aes(x=x, y=y)) +
    geom_boxplot(fill = viridis::viridis_pal(option="viridis")(5)[i]) +
    theme_void()
  #ggsave(paste0("figs/five_viridis_",i, ".pdf"), plot = plt)
  ggsave(paste0("figs/five_viridis_",i, ".eps"), plot = plt, device=cairo_ps)
}
