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
#' dim_reduction(as_tibble(complete_data), "t_an", dino)
dim_reduction <- function(data, parm, dino, scale) {
  

  # select dinocysts
  counts <- select(data, any_of(dino))
  # the other variables
  remainder <- select(data, -c(any_of(dino)))
  
  # pcr
  pcr <- prcomp(counts)
  
  # outcomes
  eigenvalues <- pcr[[1]] 
  eigenvectors <- pcr[["x"]] %>% as_tibble() 
  names(eigenvalues) <- colnames(eigenvectors)
  
  # bind to original
  eigenvectors <- bind_cols(remainder, eigenvectors) %>% 
    tidyr::drop_na()
  
  # interpolate 2D contours for environmental parameteres
  gridint <- interp::interp(
    eigenvectors[["PC1"]], 
    eigenvectors[["PC2"]], 
    eigenvectors[[parm]],
    nx = 100,
    ny = 100
  )
  griddf <- tibble(
      x = rep(gridint$x, nrow(gridint$z)),
      y = rep(gridint$y, each = ncol(gridint$z)),
      z = as.numeric(gridint$z)
      ) %>% 
    tidyr::drop_na()

  # get species / parameter names
  parm <- sub("_an$", "", parm)
  if (parm %in% c("i", "p", "o", "n")) {
    element <- c(i = "SiO", p = "PO", o = "O", n = "NO")
    index <- c(i = 2, p = 4, o = 2, n = 3)
    xc <- substitute(a[b]~"("*mu*"mol kg"^{"-"}*")", list(a = element[parm], b = index[parm]))
  }
  if (parm == "t") {
    xc <- expression('Temp ('*degree~C*')')
  }
  if (parm == "s") {
    xc <- "Salinity"
  }
  if (parm == "I") {
    xc <- expression("Density (kg m"^{"-3"}*")")
  }
  
  # proportion variance explained 
  eig1 <- eigenvalues[1] ^ 2 / sum(eigenvalues ^ 2) * 100
  eig2 <- eigenvalues[2] ^ 2 / sum(eigenvalues ^ 2) * 100
    
  # plot
  p <- ggplot2::ggplot(griddf, ggplot2::aes(x, y, z = z, color = z)) +
    ggplot2::geom_contour_filled() +
    ggplot2::scale_fill_viridis_d(xc) +
    ggplot2::geom_point(
      data = eigenvectors, 
      ggplot2::aes(x = PC1, y = PC2), 
      inherit.aes = FALSE
      ) +
    ggplot2::theme_classic() +    
    ggrepel::geom_text_repel(
      data = eigenvectors, 
      ggplot2::aes(x = PC1, y = PC2, label = station), 
      inherit.aes = FALSE
    )  +
    ggplot2::labs(
      x = paste0(names(eig1), " ", round(eig1, 1), "%"), 
      y =  paste0(names(eig2), " ", round(eig2, 1), "%")
      ) +
    ggplot2:: theme_classic()

  # suppress warning message of overlaps ggrepel
  suppressWarnings(print(p))
  
}