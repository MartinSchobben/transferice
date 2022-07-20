#' Taxon ggplot
#'
#' @param Taxon Species abundance dataframe. 
#' @param var Species name of interest.
#' @param id Identifier for
#' @param cutoff Cuttoff value for number of goups
#'
#' @return
#' @export
ggtaxon <- function(taxon, odds, id, cutoff = 20) {
  
  ggplot2::ggplot(
    taxon, 
    ggplot2::aes(
      x = forcats::fct_reorder(.data[[id]], .data[[odds]]), 
      y = .data[[odds]]
    )
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(y = "odds", x = "") +
    ggplot2::scale_y_continuous(expand = c(0, 0))  +
    transferice_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' Dimension reduction with PCA
#'
#' @param data 
#' @param parm 
#' @param ... 
#'
#' @return
#' @export
ggcompare <- function(props, var, id, component_x = "PC1", 
                      component_y = "PC2") {
  
  # pcr
  pcr <- prcomp(dplyr::select(props, -any_of(c(id, var))))
  
  # outcomes
  eigenvalues <- pcr[[1]] 
  eigenvectors <- pcr[["x"]] 
  # make tibble and vector
  eigenvectors <- tibble::as_tibble(eigenvectors) 
  names(eigenvalues) <- colnames(eigenvectors)
  # bind id back
  eigenvectors <- dplyr::bind_cols(
    dplyr::select(props, any_of(c(id, var))),
    eigenvectors 
  )
  
  # proportion variance explained 
  eig1 <- eigenvalues[component_x] ^ 2 / sum(eigenvalues ^ 2) * 100
  eig2 <- eigenvalues[component_y] ^ 2 / sum(eigenvalues ^ 2) * 100
  
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
        x = paste0(component_x, " ", round(eig1, 1), "%"), 
        y = paste0(component_y, " ", round(eig2, 1), "%") 
      ) +
      transferice_theme() 
  } else {
    ggplot2::ggplot(
      data = tidyr::drop_na(eigenvectors, any_of(var)), 
      mapping = ggplot2::aes(
        x = .data[[component_x]],
        fill = ggplot2::cut_interval(.data[[var]], n = 3, labels = c("low", "med", "high"))
      )
    ) +
      ggplot2::geom_histogram(
        binwidth = function(x) 2 * IQR(x) / (length(x) ^ (1 / 3)), 
        alpha = 0.3, 
        position = "identity"
      ) +
      ggplot2::scale_fill_viridis_d(fvar) +
      ggplot2::labs(x = paste0(component_x, " ", round(eig1, 1), "%")) +
      transferice_theme() 
  }
}

