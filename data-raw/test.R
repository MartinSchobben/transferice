library(pls)
library(tidymodels)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gganimate)
library(glue)
  library(ggrepel)

# colnames
parms <- colnames(complete_data)[grepl("_an$", colnames(complete_data))]
meta <- c("station","depth","weight","lycopodium","lyc_tab", "geometry")
meta_lg <- colnames(complete_data) %in% meta
dino <- as_tibble(complete_data)[!params_lg & !meta_lg]
# remove zero columns
dino <- dino[,sapply(dino, sum) != 0]

# start with dimension reduction

# pcr
pcr <- prcomp(dino, scale. = TRUE)

eigenvalues <- pcr[[1]] 
eigenvectors <- pcr[["x"]] %>% 
  as_tibble() %>% 
  bind_cols(as_tibble(complete_data)[params_lg | meta_lg], .)
names(eigenvalues) <- colnames(eigenvectors)



grid <- with(tidyr::drop_na(eigenvectors), interp::interp(PC1, PC2, t_an))
griddf <- subset(data.frame(x = rep(grid$x, nrow(grid$z)),
                            y = rep(grid$y, each = ncol(grid$z)),
                            z = as.numeric(grid$z)),
                 !is.na(z))

ggplot(griddf, aes(x, y, z = z, color = z)) +
  geom_contour_filled() +
  geom_point(data = eigenvectors, aes(x = PC1, y = PC2), inherit.aes = FALSE) +
  geom_text_repel(data = eigenvectors, aes(x = PC1, y = PC2, label = station), inherit.aes = FALSE) 
  


