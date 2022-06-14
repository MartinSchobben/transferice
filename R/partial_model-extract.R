cv_extraction <- function(tuned_cv = NULL, type = "fit") {
  
  # extraction of partials model from CV sub-samples  
  dplyr::select(tuned_cv, .data$splits, .data$id, .data$.extracts) |> 
    tidyr::unnest(cols = c(.data$.extracts)) |>
    dplyr::select(-.data$.config) |> 
    dplyr::mutate(
    .input = purrr::map(.data$.extracts, ~bind_partials(.x, type = type))
  )
  
}

calc_partials <- function(partials, x, y) {
  
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  
  # partial predictions
  dplyr::transmute(
    partials, 
    id = .data$id,
    .input = purrr::map2(
      .data$.extracts,
      .data$.input,
      # reverse normalization
      function(x, y) reverse_normalize(y, x$recipe)
    ),
    .output = purrr::map2(
      .data$.extracts, 
      .data$.input, 
      ~calc_partials_(.x, .y, !!x, !!y)
    )
  ) 
 
}

calc_partials_ <- function(model, newdat, x, y) {
  
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  
  dplyr::mutate(
    newdat, 
    dplyr::across(-c(.data[[!!x]], .data[[!!y]]), mean, na.rm = TRUE)
  ) |> 
    predict(object = model$fit) |> 
    # reverse outcome normalization
    reverse_normalize(model$recipe, TRUE)
  
}

bind_partials <- function(fold, type = "fit") {
  
  # select type of extract
  if (type == "fit") {
  
    dplyr::bind_cols(
      fold[[type]]$fit$model[[1]], 
      fold[[type]]$fit$model[,-1, drop = FALSE]
    ) 
    
  } else if (type == "recipe") {
    
    fold[[type]]
    
  } else {
    
    stop("type of partial extract unkown", call. = FALSE)
  }
}
