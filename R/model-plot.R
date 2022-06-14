#' ggpartial
#' 
#' This function plots several aspect of the machine learning workflow. 
#'
#'  - Feature engineering for subsets of variables of the data. 
#'  - Partial conditional regression for trained models on the test data split. 
#'  - R squared plot for the final fit on all variables.
#'
#' @param obj Rsample mc_split object or tune tune_results object.
#' @param recipe Recipe for feature engineering (recipe object).
#' @param pred Predictor variable (character string).
#' @param tune Tune variable (character or numeric string).
#' @param out Outcome variable (character or symbol).
#' @param type Plot output type. "Regression" for a simple scatter plot with a 
#'  regression line. "Spatial" for a projection on a global map.
#' @param preprocessor Character string detailing the engineering steps.
#'
#' @return A \code{ggplot2:\link[ggplot2:ggplot()]{ggplot}} or pathway to a plot 
#'  or animation.
#' 
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
    workflow, 
    pred = NULL, 
    tune = NULL, 
    out, 
    type = "xy",
    base_map = NULL,
    return_type =  "plot",
    id
  ) {

  # extract recipe
  recipe <- workflows::extract_preprocessor(workflow)
  
  # predictor variable
  x <- pred_check(recipe, pred, tune)

  # outcome variable
  y <- out
  
  # fallback for no supplied tune with a tuned recipe
  dls <- hardhat::extract_parameter_set_dials(recipe)
  if (nrow(dls) > 0) {
    depth_dial <- length(recipe$steps) # depth of dial
    name_dial <- dls$name # name of dial
    # replace tuning parameter with fixed parameter based on `tune`
    purrr::pluck(recipe, "steps", depth_dial, name_dial) <- tune 
  }
  
  # split object for training recipe
  prep <- recipes::prep(recipe, training = rsample::training(obj))  
    # apply to training data
  cast <-  recipes::bake(prep, new_data = NULL) |> 
    # reverse normalization
    reverse_normalize(prep)
  
  if (return_type == "cast") return(cast)

  # plot y-axis label for the predicted values
  y_lbl <- oceanexplorer::env_parm_labeller(gsub("_.*$", "", y))
  x_lbl <- species_naming(workflow, as_name(x))
  
  # recipe details (is it tuned or not?)
  recipe_specs <- sanitize_workflow(workflow, model = FALSE)
  
  if (all(!isTruthy(pred), !isTruthy(recipe_specs))) {
    x_lbl <- paste0("transform(", x_lbl, ")")
  } 
    
  if (type == "spatial") {
    
    # get coordinates and turn into sf object
    spat <- tibble::as_tibble(obj) |> 
      dplyr::select(.data$longitude, .data$latitude)
    cast <- dplyr::bind_cols(spat, cast) |> 
      sf::st_as_sf(
        coords = c("longitude", "latitude"), 
        crs = sf::st_crs(base_map)
      ) 
    
    # plot
    p <- oceanexplorer::plot_NOAA(
      base_map,
      rng = range(base_map[[1]], na.rm = TRUE)
    ) + 
      ggplot2::geom_sf(
        data= cast,
        # discretise abundance data
        mapping = ggplot2::aes(
          color =
            ggplot2::cut_interval(
              .data[[!!x]],
              3,
              labels = c("low", "mid", "high")
            )
          )
      ) +
      ggplot2::scale_color_manual(
        as_name(x),
        values = c("#fff7bc", "#fec44f", "#d95f0e"),
        guide = ggplot2::guide_legend(title.position = "top")
      ) + 
      ggplot2::coord_sf(
        xlim = c(-180, 180),
        ylim = c(-90, 90),
        default_crs = sf::st_crs(base_map),
        crs = sf::st_crs(base_map),
        expand = FALSE
    )
  } else if (type == "xy") {

    # plot
    p <- ggbase(cast, x, y, id = FALSE)  + 
      ggplot2::labs(title = 'Feature engineering', y = y_lbl, x = x_lbl)
  
  }
  
  # add theme and return
  p + transferice_theme() 
}
#' @rdname ggpartial
#' 
#' @export
ggpartial.tune_results <- function(
    obj, 
    workflow = NULL,
    pred = NULL, 
    tune = NULL, 
    out, 
    type = "xy", 
    base_map = NULL, 
    plot_type =  "dynamic",
    id,
    mc_cores = 2
  ) {
  
  # memoised partials function
  nm_fit <- file_namer("rds", "training", id, "partial_fit", 
                       trans = sanitize_workflow(workflow))
  nm_rcp <- file_namer("rds", "training", id, "partial_recipe", 
                       trans = sanitize_workflow(workflow))
  
  # partial fits
  partials_fit <- cv_extraction(obj) |> 
    app_caching("rds", nm_fit) # caching
  
  # partial trained recipes
  partials_rcp <- cv_extraction(obj, "recipe") |> 
    app_caching("rds", nm_rcp) # caching
  
  # predictor variable
  x <- pred_check(obj, pred, tune)

  # outcome variable
  y <- out 
  
  # check for base_map
  if (!isTruthy(base_map) & type == "spatial") {
    stop(paste0("Provide a basemap as a raster object when selecting,", 
                "`type` = 'spatial'."), call. = FALSE)
  }
  
  # check if tuned then subset num_comp (filter only what's needed)
  trytune <- try(dplyr::filter(partials_fit, .data$num_comp == tune), silent = TRUE)
  if (!inherits(trytune, "try-error")) partials_fit <- trytune

  # predicted values
  output <- calc_partials(partials_fit, !!x, !!y)
  
  # plot y-axis label for the predicted values
  y_lbl <- oceanexplorer::env_parm_labeller(gsub("_.*$", "", y))
  x_lbl <- species_naming(workflow, as_name(x))
  
  # determine type of statistics and build plot base
  if (type == "spatial") {
    
    # original data
    origin <- dplyr::transmute(
      partials_fit, 
      origin = purrr::map(splits, tibble::as_tibble)
    ) 

    # spatial interpolation for each fold
    # make parallel
    z <- parallel::mcMap(
      interpolate_model, 
      origin = origin$origin, 
      output = output$.output, 
      MoreArgs = list(y = y, base_map = base_map),
      mc.cores = mc_cores 
    )

    st <- rlang::inject(c(!!!z, nms = output$id)) # combine and rename
    st = merge(st) # collapse to dimension
    names(st) = rlang::as_name(y) # rename
      
    # plot
    p <- oceanexplorer::plot_NOAA(st, rng = range(st[[1]], na.rm = TRUE)) 

  } else if (type == "xy") {
  
    # prepare data
    comb <- tidyr::unnest(output, cols = c(.data$.input, .data$.output))

    # plot
    p <- ggbase(comb, x, y, id = TRUE)  + 
      ggplot2::geom_line(
        mapping = ggplot2::aes(
          y = .data[[!!rlang::sym(paste0(".pred_",  y))]]
          ),
        linetype = 2,
        color = "blue"
      ) 
  } 

  if (plot_type  == "dynamic") {
  
    if (type == "spatial") {
      
      p <- p + gganimate::transition_states(attributes) +
        ggplot2::labs(title = 'Predicted values ({closest_state})')
      
    } else  if (type == "xy") {
      
      p <- p + gganimate::transition_states(.data$id) + 
        ggplot2::labs(
          title = 'Partial regression ({closest_state})', 
          y = y_lbl,
          x = x_lbl
        )

    }
  }

  # add theme
  p + transferice_theme() 

}
#' @rdname ggpartial
#' 
#' @export
ggpartial.last_fit <- function(
    obj, 
    workflow = NULL, 
    pred = NULL, 
    tune = NULL, 
    out, 
    type = "xy",
    base_map = NULL,
    id
) {
  
  # predictor variable
  x <- pred_check(obj, pred, tune)
  
  # extract averaging
  averaging <- gsub("^(.)*_", "", out)

  # all parameters
  pms <- paste(abbreviate_vars(parms), averaging, sep = "_") 
  
  # name file, plot title and potential path
  parsed_pm <- parms[abbreviate_vars(parms) == gsub("_(.)*$", "", out)]
  ttl_reg <- paste0("R-squared Plot (", parsed_pm, ")")
  ttl_spat <- paste0("Difference in prediction")
  
  # extract predictions
  preds <- tune::collect_predictions(obj)
  
  if (type == "spatial") {
    
    # original data
    origin <- dplyr::transmute(
      obj, 
      origin = purrr::map(splits, rsample::testing)
    ) |> 
      tidyr::unnest(cols = c(origin))
    # predictions
    output <- dplyr::select(preds, - dplyr::any_of(dplyr::any_of(pms)))
    # interpolate
    z <- interpolate_model(origin, output, !! rlang::sym(out), base_map)
      
    # difference on raster (predicted - truth)
    z <- z - base_map
    names(z) = out # rename
    
    # label
    y_lbl <- oceanexplorer::env_parm_labeller(out, prefix = "Delta")
    x_lbl <- species_naming(workflow, as_name(x))
    
    # plot 
    p <- oceanexplorer::plot_NOAA(z) +
      ggplot2::scale_fill_gradient2(y_lbl) +
      ggplot2::labs(title = ttl_spat, x = x_lbl)
      
  } else if (type == "xy") {
      
    # rename original true values
    obj <- dplyr::rename_with(
      preds, 
      .cols = dplyr::any_of(pms), 
      .fn = ~paste0(".truth_", .x)
    )
    
    # pivot to long format
    long_obj <-tidyr::pivot_longer(
      obj,
      cols = dplyr::ends_with(paste0("_", averaging)),
      names_to = c(".value", "parameter"),
      names_pattern = paste0("(.*)_(._", averaging ,")")
    )
   
    # check if param exists
    chk <- out %in% unique(long_obj$parameter)
    if (!chk) stop("Selected parameter does not exist in data.", 
                   call. = FALSE)
      
    long_obj <- dplyr::filter(long_obj, .data$parameter == out)
  
    p <- ggplot2::ggplot(
      data = long_obj, 
      mapping = ggplot2::aes(x = .data$.truth, y = .data$.pred)
    ) +  
      ggplot2::geom_point(alpha = 0.3) +  
      ggplot2::geom_abline(color = 'blue', linetype = 2) +
      ggplot2::labs(
        title = ttl_reg,       
        y = 'Predicted',        
        x = 'Actual'
      )
    }
  
    # add theme
    p + transferice_theme() 
    
}

#-------------------------------------------------------------------------------
# helper functions
#-------------------------------------------------------------------------------
# predictor check
# predictor variable (either for tuning or just inspection)
pred_check <- function(dat, pred, tune) {
  
  if (all(!isTruthy(pred),  !isTruthy(tune))) {
    stop(paste("Either `pred` or `tune` needs to be supplied!"), call. = FALSE)
  }
  
  if (inherits(dat, "recipe")) {
    # check if recipe has tune dials
    dls <- hardhat::extract_parameter_set_dials(dat)$name
    if (all(length(dls) == 0, !isTruthy(pred))) {
      stop(paste("The model has NOT been tuned and therefore `pred` needs to be", 
                 "supplied!"), call. = FALSE)    
    } else  if (all(length(dls) == 0, isTruthy(pred))) {
      x <- rlang::ensym(pred) 
    } else if (all(length(dls) > 0, !isTruthy(tune))) {
      stop(paste("The model has been tuned and therefore `tune` needs to be", 
                 "supplied!"), call. = FALSE)
    } else if (all(length(dls) > 0, isTruthy(tune))) {
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
        x <- pred
      } else if (n_preds > n_comps) {
        x <- paste0(n_comps, "PCs") # return number of components tuning
      }
      
    } else {
    
      # check if results are tuned (class 'resample_results' is not tuned)
      nottuned <- inherits(dat, 'resample_results')
      
      if (all(nottuned, !isTruthy(pred))) {
        stop(paste("The model has NOT been tuned and therefore `pred` needs to", 
                   " be supplied!"), call. = FALSE)    
      } else  if (all(nottuned, isTruthy(pred))) {
        x <- rlang::ensym(pred) 
      } else if (all(!nottuned, !isTruthy(tune))) {
        stop(paste("The model has been tuned and therefore `tune` needs to be", 
                   "supplied!"), call. = FALSE)
      } else if (all(!nottuned, isTruthy(tune))) {
        x <- rlang::sym(paste0("PC", tune))
      }
    }
  }
  
  # return
  x
}


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

