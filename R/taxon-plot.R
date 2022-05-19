#' Taxon ggplot
#'
#' @param Taxon Species abundance dataframe. 
#' @param var Species name of interest.
#' @param id Identifier for
#' @param cutoff Cuttoff value for number of rgoups
#'
#' @return
#' @export
taxon_plot <- function(taxon, var, id, cutoff = 30) {
  # prepare fossil count data for plotting
 
  taxon <- dplyr::mutate(
    taxon, 
    # odds  
    odds = .data[[var]] / (1 - .data[[var]]),
    # find highest n ranks
    rank =  rank(.data$odds),
    # make character
    {{id}} := as.character(.data[[id]])
  ) |> 
    dplyr::filter(.data$rank >= dplyr::n() - cutoff) 

  ggplot2::ggplot(
    taxon, 
    ggplot2::aes(
      x = forcats::fct_reorder(.data[[id]], .data$odds), 
      y = .data$odds
    )
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(y = "odds") +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::scale_x_discrete(name = "", breaks = NULL) + 
    transferice_theme() 
}


