#' ggpartial
#' 
#' partial conditional regression for trained model on test data
#'
#' @param obj Rsample mc_split object or tune tune_results object
#' @param x x variable
#' @param y y variable
#'
#' @return
#' 
#' @export
ggpartial <- function(obj, ...) { 
  UseMethod("ggpartial")
}
#' @rdname ggpartial
#' 
#' @export
ggpartial.mc_split <- function(
    obj, 
    recipe, 
    pred = NULL, 
    tune = NULL, 
    out, 
    type = "regression",
    preprocessor = NULL
  ) {

  # predictor variable
  x <- pred_check(recipe, pred, tune)
  
  # outcome variable
  y <- rlang::ensym(out) 
  
  # fallback for no supplied tune with a tuned recipe
  dls <- hardhat::extract_parameter_set_dials(recipe)
  if (nrow(dls) > 0) {
    depth_dial <- length(recipe$steps) # depth of dial
    name_dial <- dls$name # name of dial
    # replace tuning parameter with fixed parameter based on `tune`
    purrr::pluck(recipe, "steps", depth_dial, name_dial) <- tune 
  }
  
  # split object for training recipe
  cast <- recipes::prep(recipe, training = rsample::training(obj)) |> 
    # apply to training data
    recipes::bake(new_data = NULL)
  
  # file name
  nm <- paste("prep", type, preprocessor, rlang::as_name(y), rlang::as_name(x), 
              sep = "_")
  ggpath <- try(
    fs::path_package("transferice", "plots", nm, ext = "png"), 
    silent = TRUE
  )
  
  # if the file does not exist then render from scratch
  if (inherits(ggpath, "try-error")) {
    
    # plot y-axis label for the predicted values
    y_lbl <- oceanexplorer::env_parm_labeller(gsub("_.*$", "", rlang::as_name(y)))
    
    if (!isTruthy(pred) & !isTruthy(preprocessor)) {
      x_lbl <- paste0(preprocessor, "(", as_name(x), ")")
    } else {
      x_lbl <- as_name(x)
    }
    
    # plot
    p <- ggbase(cast, x, y, id = FALSE)  + 
      ggplot2::labs(title = 'Feature engineering', y = y_lbl, x = x_lbl)
    
    # saving
    ggplot2::ggsave(
      fs::path(nm, ext = "png"),
      plot = p, 
      path = fs::path_package(package = "transferice", "plots"),
      width = if (type == "regression") 400 else 600,
      height = if (type == "regression") 400 else 300,
      dpi = 72,
      units = "px"
    )
    
  } 
  # depending whether the figure is newly rendered this function executes 
  # fast or slow
  fs::path_package("transferice", "plots", nm, ext = "png") 
}
#' @rdname ggpartial
#' 
#' @export
ggpartial.tune_results <- function(
    obj, 
    recipe = NULL,
    pred = NULL, 
    tune = NULL, 
    out, 
    type = "regression", 
    base_map = NULL, 
    plot_type =  "dynamic", 
    preprocessor = NULL
  ) {
  
  # memoised partials function
  partials <- cv_model_extraction_mem(obj)
  
  # predictor variable
  x <- pred_check(obj, pred, tune)

  # outcome variable
  y <- rlang::enquo(out) 
  
  # check for base_map
  if (!isTruthy(base_map) & type == "spatial") {
    stop(paste0("Provide a basemap as a raster object when selecting,", 
                "`type` = 'spatial'."), call. = FALSE)
  }
  
  # check if tuned then subset num_comp
  trytune <- try(dplyr::filter(dat, .data$num_comp), silent = TRUE)
  if (!inherits(trytune, "try-error")) partials <- trytune

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
  
  # name file 
  nm <- paste("folds", type, preprocessor, rlang::as_name(y), rlang::as_name(x), sep = "_")
  
  # potential paths
  if (plot_type == "dynamic") {
    
    ggpath <- try(
      fs::path_package("transferice", "anims", nm, ext = "gif"), 
      silent = TRUE
    )
    
  } else if (plot_type == "static") {
    
    ggpath <- try(
      fs::path_package("transferice", "plots", nm, ext = "png"), 
      silent = TRUE
    )
    
  }
  
  # if the file does not exist then render from scratch
  if (inherits(ggpath, "try-error")) {
    
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
  
      # plot
      p <- ggbase(comb, x, y, id = TRUE)  + 
        ggplot2::geom_line(
          mapping = ggplot2::aes(
            y = .data[[!!rlang::sym(paste0(".pred_",  rlang::as_name(y)))]]
            ),
          linetype = 2,
          color = "blue"
        ) 
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

# predictor check
# predictor variable (either for tuning or just inspection)
pred_check <- function(dat, pred, tune) {
  
  if (!isTruthy(pred) & !isTruthy(tune)) {
    stop(paste("Either `pred` or `tune` needs to be supplied!"), call. = FALSE)
  }
  
  if (inherits(dat, "recipe")) {
    # check if recipe has tune dials
    dls <- hardhat::extract_parameter_set_dials(dat)$name
    if (length(dls) == 0 & !isTruthy(pred)) {
      stop(paste("The model has NOT been tuned and therefore `pred` needs to be", 
                 "supplied!"), call. = FALSE)    
    } else  if (length(dls) == 0 & isTruthy(pred)) {
      x <- rlang::ensym(pred) 
    } else if (length(dls) > 0 & !isTruthy(tune)) {
      stop(paste("The model has been tuned and therefore `tune` needs to be", 
                 "supplied!"), call. = FALSE)
    } else if (length(dls) > 0 & isTruthy(tune)) {
      x <- rlang::sym(paste0("PC", tune))
    }
    
  }
  
  if (inherits(dat, "tune_results")) {
    
    if (inherits(dat, "last_fit")) {
      
      # number of components used for prediction
      mold <- dat$.workflow[[1]] |> 
        workflows::extract_mold()
      n_comps <- ncol(mold$predictors) # dimensions after processing
      n_preds <- mold$blueprint$recipe$var_info |>  # dimensions original
        dplyr::filter(role == "predictor") |> 
        nrow()
      
      if (n_preds == n_comps) {
        x <- NULL # nothing to return
      } else if (n_preds > n_comps) {
        x <- paste0(n_comps, "PCs") # return number of components tuning
      }
      
    } else {
    
      # check if results are tuned (class 'resample_results' is not tuned)
      nottuned <- inherits(dat, 'resample_results')
      
      if (nottuned & !isTruthy(pred)) {
        stop(paste("The model has NOT been tuned and therefore `pred` needs to be", 
                   "supplied!"), call. = FALSE)    
      } else  if (nottuned & isTruthy(pred)) {
        x <- rlang::ensym(pred) 
      } else if (!nottuned & !isTruthy(tune)) {
        stop(paste("The model has been tuned and therefore `tune` needs to be", 
                   "supplied!"), call. = FALSE)
      } else if (!nottuned& isTruthy(tune)) {
        x <- rlang::sym(paste0("PC", tune))
      }
    }
  }
  
  # return
  x
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

# base ggplot
ggbase <- function(dat, x, y, id = TRUE) {
  
  # range x
  x_rng <- dat |> dplyr::pull(!!x) |> range(na.rm = TRUE)
  # range y
  y_rng <- dat |> dplyr::pull(!!y) |> range(na.rm = TRUE)
  
  # grouping for gganiamte
  if (isTRUE(id)) {
    baese <- ggplot2::aes(x = .data[[!!x]], y = .data[[!!y]], group = .data$id)
  } else {
    baese <- ggplot2::aes(x = .data[[!!x]], y = .data[[!!y]])
  }
  
  ggplot2::ggplot(
    data = dat, 
    # see documentation `gganimate` for grouping structure
    mapping = baese
  ) +
    ggplot2::geom_point(alpha = 0.3) +
    ggplot2::scale_x_continuous(limits = x_rng) +
    ggplot2::scale_y_continuous(limits = y_rng) 
  
}

