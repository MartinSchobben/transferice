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
ggpartial <-  function(model, x, y) {
  x <- enquo(x)
  y <- enquo(y)
  partial <- dplyr::bind_cols(model$fit$model[[1]], model$fit$model[,-1, drop = FALSE]) |>
    dplyr::mutate(dplyr::across(-c(!!x, !!y), mean, na.rm = TRUE)) 
  
  dplyr::bind_cols(partial, predict(model, partial)) |> 
    ggplot2::ggplot(ggplot2::aes(x = !!x, y = !!y)) +
    ggplot2::geom_point() +
    ggplot2::geom_line(
      aes(y = !!rlang::sym(paste0(".pred_",  rlang::as_name(y)))),
      linetype = 2,
      color = "blue"
    )
}

tuned_partials <- function(tuned_cv, fold, tune_parm, tune_val) {
  l1 <- tuned_cv |> dplyr::filter(id == fold) |> dplyr::pull(.extracts) 
  l2 <- l1[[1]] |> dplyr::filter(num_comp == tune_val) |> dplyr::pull(.extracts)
  l2[[1]]
}