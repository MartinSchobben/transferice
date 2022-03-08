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

  rng <- dplyr::summarise(tot_rng, !!y := range(!!y)) |> 
    dplyr::mutate(!!x := c(-8, 8)) # PCA set to this range
  
  
  ggplot2::ggplot(
    dplyr::bind_cols(dat, model), 
    ggplot2::aes(x = !!x, y = !!y)
    ) +
    # use a geom_blank to set the min to max ranges of the axis to a fixed value
    ggplot2::geom_blank(data = rng, mapping = ggplot2::aes(x = !!x, y = !!y)) +
    ggplot2::geom_point() +
    ggplot2::geom_line(
      mapping = ggplot2::aes(y = !!rlang::sym(paste0(".pred_",  rlang::as_name(y)))),
      linetype = 2,
      color = "blue"
    )
}

cv_model_extraction <- function(tuned_cv) {
  dplyr::select(tuned_cv, .data$id, .data$.extracts) |> 
    tidyr::unnest(cols = c(.data$.extracts)) |>
    dplyr::select(-.data$.config) |> 
    dplyr::mutate(
      .input = purrr::map(.data$.extracts, ~calc_partials(.x)),
      .output = purrr::map2(.data$.extracts, .data$.input, ~predict(.x, .y))
    )
  }

# memoise function
cv_model_extraction_mem <- memoise::memoise(
  cv_model_extraction, 
  cache = cachem::cache_disk(fs::path_package(package = "transferice", "appdir"))
)


calc_partials <- function(fold) {
  dplyr::bind_cols(
    fold$fit$model[[1]], 
    fold$fit$model[,-1, drop = FALSE]
  )  
}

