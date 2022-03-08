#' ggpartial
#' 
#' partial conditional regression for trained model on test data
#'
#' @param model Parsnip model object
#' @param x x variable
#' @param y y variable
#'
#' @return
#' @export
ggpartial <-  function(dat, model, tot_rng, x, y) {
  x <- enquo(x)
  y <- enquo(y)

  rng <- dplyr::summarise(tot_rng, dplyr::across(dplyr::everything(), range)) |> 
    dplyr::mutate(!!x := c(-8, 8)) # PCA set to this range
  
  ggplot2::ggplot(model, ggplot2::aes(x = !!x, y = !!y)) +
    # use a geom_blank to set the min to max ranges of the axis to a fixed value
    ggplot2::geom_blank(data = rng, mapping = ggplot2::aes(x = !!x, y = !!y)) +
    ggplot2::geom_point() +
    ggplot2::geom_line(
      data = model, 
      mapping = ggplot2::aes(y = !!rlang::sym(paste0(".pred_",  rlang::as_name(y)))),
      linetype = 2,
      color = "blue"
    )
}

cv_model_extraction <- function(tuned_cv) {
  dplyr::select(tuned_cv, .data$id, .data$.extracts) |> 
    tidyr::unnest(cols = c(.data$.extracts)) |>
    dplyr::select(-.data$.config) |> 
    dplyr::mutate(.input = purrr::map(.data$.extracts, ~calc_partials(.x))) 
}

calc_partials <- function(fold) {
  dplyr::bind_cols(
    fold$fit$model[[1]], 
    fold$fit$model[,-1, drop = FALSE]
  )  
}

