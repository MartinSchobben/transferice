#' Dimension reduction with PCA
#'
#' @param data 
#' @param parm 
#' @param ... 
#'
#' @return
#' @export
reduce_dims <- function(props, parm, var, id, loc, component_x = "PC1", 
                        component_y = "PC2") {
  
  # pcr
  pcr <- prcomp(dplyr::select(props, -any_of(id)))
  
  # outcomes
  eigenvalues <- pcr[[1]] 
  eigenvectors <- pcr[["x"]] 
  # make tibble and vector
  eigenvectors <- tibble::as_tibble(eigenvectors) 
  names(eigenvalues) <- colnames(eigenvectors)
  # bind id back
  eigenvectors <- dplyr::bind_cols(
    dplyr::select(props, any_of(id)),
    eigenvectors 
  )
 
  # join with environmental data
  eigenvectors <- dplyr::left_join(eigenvectors, parm, by = id) 

  # proportion variance explained 
  eig1 <- eigenvalues[1] ^ 2 / sum(eigenvalues ^ 2) * 100
  eig2 <- eigenvalues[2] ^ 2 / sum(eigenvalues ^ 2) * 100
  
  # format variable
  fvar <- oceanexplorer::env_parm_labeller(var)
  
  # plot
  if (component_x != component_y) {
    ggplot2::ggplot(
      data = eigenvectors, 
      mapping = ggplot2::aes(
        x = .data[[component_x]], 
        y = .data[[component_y]], 
        color = .data[[var]]
       )
      ) +
      ggplot2::geom_point(alpha = 0.3) +
      ggplot2::scale_color_viridis_c(fvar) +
      ggplot2::geom_hline(yintercept = 0, lty = 2) +
      ggplot2::geom_vline(xintercept = 0, lty = 2) +
      ggplot2::labs(
        x = paste0(names(eig1), " ", round(eig1, 1), "%"), 
        y = paste0(names(eig2), " ", round(eig2, 1), "%") 
      ) +
      transferice_theme() 
  } else {
    ggplot2::ggplot(
      data = tidyr::drop_na(eigenvectors, any_of(var)), 
      mapping = ggplot2::aes(
        x = .data[[component_x]],
        fill = ggplot2::cut_interval(.data[[var]], 5)
      )
    ) +
      ggplot2::geom_histogram(
        binwidth = function(x) 2 * IQR(x) / (length(x) ^ (1 / 3)), 
        alpha = 0.3, 
        position = "identity"
      ) +
      ggplot2::scale_fill_viridis_d(fvar) +
      ggplot2::labs(x = paste0(names(eig1), " ", round(eig1, 1), "%")) +
      transferice_theme() 
  }
}
