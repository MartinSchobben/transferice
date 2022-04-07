cv_model_extraction <- function(tuned_cv = NULL, wfl = NULL) {
  
  # name file
  nm  <- sanitize_workflow(wfl)
  
  # dir for partials models
  cache_part <- fs::path_package("transferice", "appdir", "cache", "model_extracts")
  
  cache_file <- try(
    fs::path_package("transferice", "appdir", "cache", "model_extracts", nm, 
                     ext = "rds"), 
    silent = TRUE
  )
  
  # if the cache does not exist then render from scratch
  if (inherits(cache_file, "try-error")) {
    
    # extraction of partials model from CV sub-samples  
    out <- dplyr::select(tuned_cv, .data$splits, .data$id, .data$.extracts) |> 
      tidyr::unnest(cols = c(.data$.extracts)) |>
      dplyr::select(-.data$.config) |> 
      dplyr::mutate(
      .input = purrr::map(.data$.extracts, ~bind_partials(.x))
    )

    # save
    saveRDS(out, fs::path(cache_part, nm, ext = "rds"))
  } 
  # depending whether the data is  already generated this function executes 
  # fast or slow
  readRDS(fs::path(cache_part, nm, ext = "rds"))
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
