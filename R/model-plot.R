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
  # path to package
  pkg_path <- fs::path_package("transferice")
  # if directories don't exist then create them
  if (!fs::dir_exists(fs::path(pkg_path, "www", "img"))) {
    fs::dir_create(pkg_path, "www", "img")
  }
  if (!fs::dir_exists(fs::path(pkg_path, "www", "vid"))) {
    fs::dir_create(pkg_path, "www", "vid")
  }
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
    type = "regression",
    base_map = NULL,
    return_type =  "plot", 
    height = NA,
    width = NA
  ) {

  # extract recipe
  recipe <- workflows::extract_preprocessor(workflow)
  
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
  
  if (return_type == "cast") return(cast)
  
  # file name
  pred_nm <- sanitize_taxa(rlang::as_name(x))
  recipe_specs <- sanitize_workflow(workflow, model = FALSE)
  nm <- paste("prep", type, recipe_specs, rlang::as_name(y), pred_nm, 
              sep = "_") 
     
  ggpath <- try(
    fs::path_package("transferice", "www", "img", nm, ext = "png"), 
    silent = TRUE
  )
  
  # if the file does not exist then render from scratch
  if (inherits(ggpath, "try-error")) {
    
    # plot y-axis label for the predicted values
    y_lbl <- oceanexplorer::env_parm_labeller(
      gsub("_.*$", "", rlang::as_name(y))
    )
    
    if (!isTruthy(pred) & !isTruthy(recipe_specs)) {
      x_lbl <- paste0("transform(", as_name(x), ")")
    } else {
      x_lbl <- as_name(x)
    }
    
    if (type == "spatial") {
      
      # get coordinates and turn into sf object
      spat <- tibble::as_tibble(obj) |> 
        dplyr::select(.data$longitude, .data$latitude)
      cast <- dplyr::bind_cols(spat, cast) |> 
        sf::st_as_sf(coords = c("longitude", "latitude"), crs = sf::st_crs(base_map)) 
      
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
    } else if (type == "regression") {
    
      # plot
      p <- ggbase(cast, x, y, id = FALSE)  + 
        ggplot2::labs(title = 'Feature engineering', y = y_lbl, x = x_lbl)
    
    }
    
    # add theme
    p <- p + transferice_theme() 
    
    # saving
    ggplot2::ggsave(
      fs::path(nm, ext = "png"),
      plot = p, 
      path = fs::path_package(package = "transferice", "www", "img"),
      width = if (type == "regression") height else width,
      height = if (type == "regression") height else height,
      dpi = 72,
      units = "px"
    )
    
  } 
  # depending whether the figure is newly rendered this function executes 
  # fast or slow
  fs::path_package("transferice", "www", "img", nm, ext = "png") 
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
    type = "regression", 
    base_map = NULL, 
    plot_type =  "dynamic", 
    mc_cores = 4,
    renderer = "mkv",
    height = NA,
    width = NA
  ) {
  
  # memoised partials function
  partials <- cv_model_extraction(obj, workflow)
  
  # predictor variable
  x <- pred_check(obj, pred, tune)

  # outcome variable
  y <- rlang::enquo(out) 
  
  # check for base_map
  if (!isTruthy(base_map) & type == "spatial") {
    stop(paste0("Provide a basemap as a raster object when selecting,", 
                "`type` = 'spatial'."), call. = FALSE)
  }
  
  # check if tuned then subset num_comp (filter only what's needed)
  trytune <- try(dplyr::filter(partials, .data$num_comp == tune), silent = TRUE)
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
  pred_nm <- sanitize_taxa(rlang::as_name(x))
  workflow_specs <- sanitize_workflow(workflow)
  nm <- paste("folds", type, workflow_specs, rlang::as_name(y), pred_nm, 
              sep = "_") 
  
  # potential paths
  if (plot_type == "dynamic") {
    
    ggpath <- try(
      fs::path_package("transferice", "www", "vid", nm, ext = renderer), 
      silent = TRUE
    )
    
  } else if (plot_type == "static") {
    
    ggpath <- try(
      fs::path_package("transferice", "www", "img", nm, ext = "png"), 
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
      # make parallel
      z <- parallel::mcMap(
        interpolate_model, 
        origin = origin$origin, 
        output = output$.output, 
        MoreArgs = rlang::inject(list(y = !!y, base_map = base_map)),
        mc.cores = mc_cores 
      )
  
      st <- rlang::inject(c(!!!z, nms = output$id)) # combine and rename
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
        
      } else  if (type == "regression") {
        
        p <- p + gganimate::transition_states(.data$id) + 
          ggplot2::labs(title = 'Partial regression ({closest_state})', y = lbl)

      }
    
      # choose renderer 
      if (renderer == "gif") {
        renderer_fn <- gganimate::magick_renderer() 
      } else if (renderer == "mkv") {
        renderer_fn <- gganimate::av_renderer(fs::path(nm, ext = renderer))
      } else {
        stop("Unkown renderer.", call. = FALSE)
      }
      
        # add theme
        p <- p + transferice_theme() 
      
        # parallel (relies on Bengston PR: https://github.com/thomasp85/gganimate/pull/403)
        # future::plan("multicore", workers = 6L)
        # render
        p <- gganimate::animate(
          p,        
          renderer =  renderer_fn,
          width = if (type == "regression") height else width,
          height = if (type == "regression") height else height
        )
        
    
        # saving
        gganimate::anim_save(
          fs::path(nm, ext = renderer), 
          fps = 3,
          animation = p, 
          path = fs::path_package(package = "transferice", "www", "vid"),
          width = if (type == "regression") height else width,
          height = if (type == "regression") height else height
        )
        
    } else if (plot_type == "static") {
      
      if (type == "spatial") { 
        p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$attributes)) + 
          ggplot2::labs(title = "Partial regressions")
        
      } else if (type == "regression") {
          
        p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$id)) + 
          ggplot2::labs(title = 'Partial regressions', y = lbl)

      }
      
      # add theme
      p <- p + transferice_theme() 
    
      # saving
      ggplot2::ggsave(
        fs::path(nm, ext = "png"),
        plot = p, 
        path = fs::path_package(package = "transferice", "www", "img"),
        width = if (type == "regression") height else width,
        height = if (type == "regression") height else height,
        dpi = 72,
        units = "px"
      )
    
    } 
  } 
    
  # depending whether the figure is newly rendered this function executes 
  # fast or slow
  if (plot_type == "static") {
    
    fs::path("img", nm, ext = "png") 
    
  } else if (plot_type == "dynamic") {
    
    fs::path("vid", nm, ext = renderer)  
    
  }
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
    type = "regression",
    base_map = NULL,
    height = NA,
    width = NA
) {
  
  # predictor variable
  x <- pred_check(obj, pred, tune)
  
  # outcome variable
  y <- rlang::ensym(out) 
  
  # extract averaging
  averaging <- gsub("^(.)*_", "", as_name(y))
  
  # parameter of interest
  pm <- as_name(y)
  
  # all parameters
  pms <- paste(abbreviate_vars(parms), averaging, sep = "_") 
  
  # name file, plot title and potential path
  parsed_pm <- parms[abbreviate_vars(parms) == gsub("_(.)*$", "", pm)]
  ttl_reg <- paste0("R-squared Plot (", parsed_pm, ")")
  ttl_spat <- paste0("Difference in prediction")
  
  # name
  workflow_specs <- sanitize_workflow(workflow)
  nm <- paste("final", type, workflow_specs, pm, x, sep = "_") 
  
  # potential path
  ggpath <- try(
    fs::path_package("transferice", "www", "img", nm, ext = "png"), 
    silent = TRUE
  )
  
  if (inherits(ggpath, "try-error")) {
    
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
      z <- interpolate_model(origin, output, !! rlang::sym(pm), base_map)
      
      # difference on raster (predicted - truth)
      z <- z - base_map
      names(z) = pm # rename
      
      # label
      lbl <- oceanexplorer::env_parm_labeller(as_name(y), prefix = "Delta")
      
      # plot 
      p <- oceanexplorer::plot_NOAA(z) +
        ggplot2::scale_fill_gradient2(lbl) +
        ggplot2::ggtitle(ttl_spat)
      
    } else if (type == "regression") {
      
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
      chk <- pm %in% unique(long_obj$parameter)
      if (!chk) stop("Selected parameter does not exist in data.", 
                     call. = FALSE)
      long_obj <- dplyr::filter(long_obj, .data$parameter == pm)
      
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
      
      # if (selected == "all") {
      #   p <- p + ggplot2::facet_wrap(ggplot2::vars(parameter), scales = "free") 
      # } else {
      #   p <- p
      # }
    }
  
    # add theme
    p <- p + transferice_theme() 
    
    # save plot
    ggplot2::ggsave(
      fs::path(nm, ext = "png"),
      plot = p, 
      path = fs::path_package(package = "transferice", "www", "img"),
      width = if (type == "regression") height else width,
      height = if (type == "regression") height else height,
      dpi = 72,
      units = "px"
    )
  }
  
  # depending whether the figure is newly rendered this function executes 
  # fast or slow
  fs::path_package("transferice", "www", "img", nm, ext = "png") 
}

#-------------------------------------------------------------------------------
# helper functions
#-------------------------------------------------------------------------------
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
        x <- pred
      } else if (n_preds > n_comps) {
        x <- paste0(n_comps, "PCs") # return number of components tuning
      }
      
    } else {
    
      # check if results are tuned (class 'resample_results' is not tuned)
      nottuned <- inherits(dat, 'resample_results')
      
      if (nottuned & !isTruthy(pred)) {
        stop(paste("The model has NOT been tuned and therefore `pred` needs to", 
                   " be supplied!"), call. = FALSE)    
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

