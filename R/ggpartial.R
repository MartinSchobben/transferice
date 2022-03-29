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
ggpartial <- function(partials, pred = NULL, tune = NULL, out, type = "regression", 
                       base_map = NULL, plot_type =  "dynamic", preprocessor = NULL) {
  
  # predictor variable (either for tuning or just inspection)
  if (!is.null(tune) & is.null(pred)) {
    x <- rlang::sym(paste0("PC", tune))
  } else if (!is.null(pred) & is.null(tune)) {
    x <- rlang::enquo(pred) 
  } else {
    stop("Either a predictor or tune dial should be supplied, not both.", call. = FALSE)
  }

  # outcome variable
  y <- rlang::enquo(out) 
  
  # check for base_map
  if (is.null(base_map) & type == "spatial") {
    stop(paste0("Provide a basemap as a raster object when selecting,", 
                "`type` = 'spatial'."), call. = FALSE)
  }
  
  # filter tune parameter setting
  if (!is.null(tune)) {
    partials <- dplyr::filter(partials, .data$num_comp == tune)
  }
  
  # predicted values
  output <- dplyr::transmute(
    partials, 
    id = .data$id,
    .output = purrr::map2(
      .data$.extracts, 
      .data$.input, 
      ~calc_partials(.x, .y, !!x, !!y)
    )
  ) 
  
  # plot y-axis label for the predicted values
  lbl <- oceanexplorer::env_parm_labeller(gsub("_.*$", "", rlang::as_name(y)))
  
  # name file and potential path
  nm <- paste("folds", type, preprocessor, rlang::as_name(y), rlang::as_name(x), sep = "_")
  if (plot_type == "dynamic") {
    
    gif_path <- try(
      fs::path_package("transferice", "anims", nm, ext = "gif"), 
      silent = TRUE
    )
    
  } else if (plot_type == "static") {
    
    gif_path <- try(
      fs::path_package("transferice", "plots", nm, ext = "png"), 
      silent = TRUE
    )
    
  }
  
  # if the file does not exist then render from scratch
  if (inherits(gif_path, "try-error")) {
    
    # determine type of statistics and build plot base
    if (type == "spatial") {
      # original data
      origin <- dplyr::transmute(
        partials, 
        origin = purrr::map(splits, tibble::as_tibble)
      ) 
  
      # spatial interpolation for each fold
      z <- dplyr::bind_cols(origin, output) |> 
        dplyr::mutate(
          z = 
            purrr::map2(
              .data$origin, 
              .data$.output, 
              ~interpolate_model(.x, .y, y = !!y, base_map = base_map)
            )
        )
  
      st <- rlang::inject(c(!!!z$z, nms = z$id)) # combine and rename
      st = merge(st) # collapse to dimension
      names(st) = rlang::as_name(y) # rename
        
      # plot
      p <- oceanexplorer::plot_NOAA(st, rng = range(st[[1]], na.rm = TRUE)) 

    } else if (type == "regression") {
    
      part <- tidyr::unnest(partials,  cols = .data$.input) |> 
        dplyr::select(-c(.data$splits, .data$.extracts))
      output <- tidyr::unnest(output, cols = .data$.output) |> 
        dplyr::select(-.data$id)
      comb <- dplyr::bind_cols(part, output)
      # range x
      x_rng <- comb |> dplyr::pull(!!x) |> range(na.rm = TRUE)
      # range y
      y_rng <- comb |> dplyr::pull(!!y) |> range(na.rm = TRUE)
  
      p <- ggplot2::ggplot(
        comb, 
        # see documentation `gganimate` for grouping structure
        ggplot2::aes(x = .data[[!!x]], y = .data[[!!y]], group = .data$id) 
        ) +
        ggplot2::geom_point() +
        ggplot2::geom_line(
          mapping = ggplot2::aes(
            y = .data[[!!rlang::sym(paste0(".pred_",  rlang::as_name(y)))]]
            ),
          linetype = 2,
          color = "blue"
        ) +
        ggplot2::scale_x_continuous(limits = x_rng) +
        ggplot2::scale_y_continuous(limits = y_rng) 
      
    } 

    if (plot_type  == "dynamic") {
    
      if (type == "spatial") {
        
        p <- p+ gganimate::transition_states(attributes) +
          ggplot2::labs(title = 'Predicted values ({closest_state})')
        
        pms <- c(height = 300, width = 600) # dimensions of gif
        
      } else  if (type == "regression") {
        
        p <- p + gganimate::transition_states(.data$id) + 
          ggplot2::labs(title = 'Partial regression ({closest_state})', y = lbl)
        
        pms <- c(height = 400, width = 400) # dimensions of gif
        
      }
    
        # render
        p <- rlang::inject(
          gganimate::animate(
            p, 
            renderer = gganimate::magick_renderer(),
            !!! pms
          )
        )
      
        # saving
        gganimate::anim_save(
          fs::path(nm, ext = "gif"), 
          animation = p, 
          path = fs::path_package(package = "transferice", "anims")
        )
        
    } else if (plot_type == "static") {
      
      if (type == "spatial") { 
        p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$attributes)) + 
          ggplot2::labs(title = "Partial regressions")
        
      } else if (type == "regression") {
          
        p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$id)) + 
          ggplot2::labs(title = 'Partial regressions', y = lbl)

      }
    
      # saving
      ggplot2::ggsave(
        fs::path(nm, ext = "png"),
        plot = p, 
        path = fs::path_package(package = "transferice", "plots"),
        width = 20,
        height = 20,
        units = "cm"
      )
    
    } 
  } 
    
  # depending whether the figure is newly rendered this function executes 
  # fast or slow
  if (plot_type == "static") {
    
    fs::path_package("transferice", "plots", nm, ext = "png") 
    
  } else if (plot_type == "dynamic") {
    
    fs::path_package("transferice", "anims", nm, ext = "gif")  
    
  }
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

interpolate_model <- function(origin, output, y, base_map) {
  y <- rlang::enquo(y)
  comb <- dplyr::bind_cols(origin, output)
  # make coordinate sf object
  crds <- sf::st_as_sf(
    comb,
    coords = c("longitude", "latitude"),
    crs = 4326
  )
  # remove duplicate locations with tolerance 250 km radius
  d <- sf::st_is_within_distance(crds, dist = 2500)
  dupl <- unlist(mapply(function(x,y) x[x < y], d, seq_along(d)))
  crds <- crds[-dupl, ]
  # estimate
  est <- paste0(".pred_",  rlang::as_name(y))
  # formula
  fml <- as.formula(paste0(est, "~", rlang::as_name(y)))
  # nugget based on the RMSE
  mean_var <- yardstick::rmse(comb, !!y, !!rlang::sym(est))$.estimate 
  #  partial sill based on the variance of the outcome
  tot_var <- var(dplyr::pull(comb, !!y))
  # vario fram
  vario <- gstat::variogram(fml, crds) 
  # variogram model fit
  vario_model <- gstat::fit.variogram(
    vario, 
    gstat::vgm(tot_var, "Sph", 700, mean_var)
  )
  # model
  g <- gstat::gstat(formula = fml, data = crds, model = vario_model)
  # interpolate on base grid
  z <- predict(g, base_map)
  z = z["var1.pred",,] # extract prediction
  names(z) = est # rename
  z
}