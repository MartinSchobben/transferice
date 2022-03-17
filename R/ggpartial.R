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
ggpartial <- function(partials, tune, pred, type = "regression", 
                       base_map = NULL) {
  x <- rlang::sym(paste0("PC", tune))
  y <- rlang::enquo(pred) 
  
  # filter tune parameter setting
  tpart <- dplyr::filter(partials, .data$num_comp == tune)

  # predicted values
  output <- transmute(
    tpart, 
    id = id,
    .output = purrr::map2(.extracts, .input, ~calc_partials(.x, .y, !!x, !!y))
    ) 
  
  # name file and potential path
  nm <- paste("folds", type, rlang::as_name(y), rlang::as_name(x), sep = "_")
  gif_path <- try(fs::path_package("transferice", "anims", nm, ext = "gif"), silent = TRUE)
  
  # if the file does not exit then render from scratch
  if (!inherits(gif_path, "fs_path")) {

    if (type == "spatial") {
      # original data
      origin <- transmute(tpart, origin = purrr::map(splits, as_tibble)) 
  
      # spatial interpolation for each fold
      z <- dplyr::bind_cols(origin, output) |> 
        dplyr::mutate(z = 
          purrr::map2(
            .data$origin, 
            .data$.output, 
            ~interpolate_model(.x, .y, y = !!y, base_map = base_map)
            )
          )
  
      # calculate differences
      diff <- lapply(z$z, function(x) base_map - x)
      st <- rlang::inject(c(!!!diff, nms = z$id)) # combine and rename
      st = merge(st) # collapse to dimension
      names(st) = rlang::as_name(y) # rename
      
      # plot
      p <- oceanexplorer::plot_NOAA(st) +
        # fix limits of color scale
        ggplot2::scale_fill_distiller(
          paste0("diff_", rlang::as_name(y)),
          type = "div",
          palette = "PuOr",
          limits = range(st[[rlang::as_name(y)]], na.rm = TRUE), 
          na.value = "transparent"
          )  +
        gganimate::transition_states(attributes)
  
    } else if (type == "regression") {
    
      part <- tidyr::unnest(tpart,  cols = .input) |> select(-c(splits, .extracts))
      output <- tidyr::unnest(output, cols = c(.output)) |> select(-id)
      p <- ggplot2::ggplot(
        dplyr::bind_cols(part, output), 
        ggplot2::aes(x = .data[[!!x]], y = .data[[!!y]])
        ) +
        ggplot2::geom_point() +
        ggplot2::geom_line(
          mapping = ggplot2::aes(y = .data[[!!rlang::sym(paste0(".pred_",  rlang::as_name(y)))]]),
          linetype = 2,
          color = "blue"
        ) + 
        gganimate::transition_states(id)
    } 
    # render
    p <- gganimate::animate(p, renderer = gganimate::magick_renderer())
    # saving
    gganimate::anim_save(fs::path(nm, ext = "gif"), animation = p, path = fs::path_package(package = "transferice", "anims"))
  } 
  # depending whether the figure is newly rendered this will go fast or slow
  fs::path_package("transferice", "anims", nm, ext = "gif") #|> rstudioapi::viewer()
  
}

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
  dplyr::mutate(newdat, dplyr::across(-c(.data[[!!x]], .data[[!!y]]), mean, na.rm = TRUE)) |> 
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
ggpartial_mem <- memoise::memoise(
  ggpartial, 
  cache = cachem::cache_disk(
    fs::path_package(package = "transferice", "appdir")
  )
)

interpolate_model <- function(origin, output, y, base_map) {
  y <- rlang::enquo(y)
  # make coordinate sf object
  crds <- sf::st_as_sf(bind_cols(origin, output), coords = c("longitude", "latitude"), crs =  4326)
  # remove duplicate locations
  crds <- dplyr::distinct(crds, hole_id, .keep_all = TRUE)
  # formula
  fml <- as.formula(paste0(".pred_",  rlang::as_name(y), "~ 1"))
  # variogram
  vario <- automap::autofitVariogram(fml, as(crds, "Spatial"), model = "Exp")
  # model
  g <- gstat::gstat(formula = fml, model = vario$var_model, data = crds)
  # interpolate on base grid
  z <- predict(g, base_map)
  z = z["var1.pred",,] # extract prediction
  z
}