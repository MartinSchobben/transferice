cv_model_extraction <- function(tuned_cv) {
  dplyr::select(tuned_cv, .data$splits, .data$id, .data$.extracts) |> 
    tidyr::unnest(cols = c(.data$.extracts)) |>
    dplyr::select(-.data$.config) |> 
    dplyr::mutate(
      .input = purrr::map(.data$.extracts, ~bind_partials(.x))
    )
}

calc_partials <- function(model, newdat, x, y) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  dplyr::mutate(
    newdat, 
    dplyr::across(-c(.data[[!!x]], .data[[!!y]]), mean, na.rm = TRUE)
  ) |> 
    predict(object = model)
}

bind_partials <- function(fold) {
  dplyr::bind_cols(
    fold$fit$model[[1]], 
    fold$fit$model[,-1, drop = FALSE]
  )  
}

# memoise function
cv_model_extraction_mem <- memoise::memoise(
  cv_model_extraction, 
  cache = cachem::cache_disk(
    fs::path_package(package = "transferice", "appdir")
  )
)
