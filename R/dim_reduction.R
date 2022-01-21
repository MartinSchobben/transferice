#' Title
#'
#' @param data 
#' @param parm 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' dim_reduction(as_tibble(complete_data), t_an, dino)
dim_reduction <- function(data, parm, dino) {
  

  # select dinos
  counts <- select(data, any_of(dino))
  remainder <- select(data, -c(any_of(dino)))
  
  # prep
  # remove zero columns
  counts <- counts[,sapply(counts, sum) != 0]
  
  # pcr
  pcr <- prcomp(counts, scale. = TRUE)
  
  # outcomes
  eigenvalues <- pcr[[1]] 
  eigenvectors <- pcr[["x"]] %>% as_tibble() 
  names(eigenvalues) <- colnames(eigenvectors)
  
  # bind to original
  eigenvectors <- bind_cols(remainder, eigenvectors) %>% tidyr::drop_na()
  
  # interpolate
  grid <- interp::interp(
    eigenvectors[["PC1"]], 
    eigenvectors[["PC2"]], 
    eigenvectors[[parm]]
    )

  griddf <- tibble(
      x = rep(grid$x, nrow(grid$z)),
      y = rep(grid$y, each = ncol(grid$z)),
      z = as.numeric(grid$z)
      ) %>%  tidyr::drop_na()
  
  # plot
  ggplot(griddf, aes(x, y, z = z, color = z)) +
    geom_contour_filled() +
    geom_point(
      data = eigenvectors, 
      aes(x = PC1, y = PC2), 
      inherit.aes = FALSE
      ) +
    geom_text_repel(
      data = eigenvectors, 
      aes(x = PC1, y = PC2, label = station), 
      inherit.aes = FALSE
      )  +
    theme_classic()
}