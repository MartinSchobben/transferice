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
    out,
    pred = NULL,
    tune = NULL, 
    type = "xy",
    return_type =  "plot",
    id
  ) {

  # extract recipe
  recipe <- workflows::extract_preprocessor(workflow)
  
  # predictor variable
  x <- pred_check(recipe, pred, tune)

  # outcome variable
  y <- out
  
  # plot y-axis label for the predicted values
  y_lbl <- oceanexplorer::env_parm_labeller(gsub("_.*$", "", y))
  x_lbl <- x_labeller(workflow, x)

  # fallback for no supplied tune with a tuned recipe
  recipe <- untune(recipe, tune)

  # split object for training recipe
  prep <- recipes::prep(recipe, training = rsample::training(obj))  
  # apply to training data
  cast <-  recipes::bake(prep, new_data = NULL) 

  # recipe details (is it tuned or not?)
  recipe_specs <- sanitize_workflow(workflow, model = FALSE)
    
  if (type == "xy") {

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
    workflow,
    out,
    pred = NULL,
    tune = NULL, 
    type = "xy", 
    plot_type =  "dynamic",
    id = NULL,
    mc_cores = 2
  ) {

  # exclude from predicted values
  exclude <- NULL
  
  # partial fits
  if (is.null(id)) {
    
    # partials function
    partials_fit <- cv_extraction(obj) 
    
  } else {
    
    # memoised partials function
    partials_fit <- cv_extraction(obj) |> 
      app_caching("rds", id) # caching
    
    # exclude from predicted values
    if(stringr::str_detect(id, "gls")) {
      exclude <- syms(c("longitude", "latitude"))
    }
    
  }
  
  # predictor variable
  x <- pred_check(obj, pred, tune)

  # outcome variable
  y <- out 
  
  # plot y-axis label for the predicted values
  y_lbl <- oceanexplorer::env_parm_labeller(gsub("_.*$", "", y))
  x_lbl <- x_labeller(workflow, x)
  
  # check if tuned then subset num_comp (filter only what's needed)
  trytune <- try(dplyr::filter(partials_fit, .data$num_comp == tune), silent = TRUE)
  if (!inherits(trytune, "try-error")) {
    partials_fit <- trytune
    # label will be component name
    x_lbl <- as_name(x)
  }

  output <- calc_partials(partials_fit, !!x, !!y, exclude)
  
  # determine type of statistics and build plot base
  if (type == "xy") {
  
    # prepare data
    comb <- tidyr::unnest(output, cols = c(.data$.input, .data$.output))

    # plot
    p <- ggbase(comb, x, y, id = TRUE)  + 
      ggplot2::geom_line(
        mapping = ggplot2::aes(
          y = .data[[".pred"]]
          ),
        linetype = 2,
        color = "blue"
      ) 
  } 

  if (plot_type  == "dynamic") {
  
   if (type == "xy") {
      
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
    workflow, 
    out, 
    pred = NULL, 
    tune = NULL, 
    type = "xy",
    id
) {
  
  # extract averaging
  averaging <- gsub("^(.)*_", "", out)

  # all parameters
  pms <- paste(abbreviate_vars(parms), averaging, sep = "_") 
  
  # name file, plot title and potential path
  parsed_pm <- parms[abbreviate_vars(parms) == gsub("_(.)*$", "", out)]

  ttl_spat <- paste0("Difference in prediction")
  
  # extract predictions
  test <- rsample::assessment(obj$splits[[1]]) |> 
    dplyr::select(!dplyr::any_of(out))
  preds <- tune::collect_predictions(obj) |> 
    dplyr::bind_cols(test)
  
  if (type == "xy") {
    # title
    ttl_reg <- paste0("R-squared Plot (", parsed_pm, ")")
      
    p <- ggplot2::ggplot(
      data = preds, 
      mapping = ggplot2::aes(x = .data[[out]], y = .data[[".pred"]])
    ) +  
      ggplot2::geom_point(alpha = 0.3) +  
      ggplot2::geom_abline(color = 'blue', linetype = 2) +
      # ggplot2::coord_fixed() +
      ggplot2::labs(
        title = ttl_reg,       
        y = 'Predicted',        
        x = 'Actual'
      )
  } else if (type == "bubble") {
    
    # title
    ttl_bub <- paste0("Bubble plot (", parsed_pm, ")")
    
    # get residuals and their sign
    preds <- dplyr::mutate(
      preds,
      residuals = .data[[out]] - .data[[".pred"]]
    )
   
    # bounds
    lw <- round(quantile(preds$residuals, 0.25, na.rm = TRUE), 0) |> unname()
    up <- round(quantile(preds$residuals, 0.75, na.rm = TRUE), 0) |> unname()

    p <- ggplot2::ggplot(
      data = preds, 
      mapping = ggplot2::aes(
        x = .data$longitude, 
        y = .data$latitude, 
        size = .data$residuals,
        fill = .data$residuals
      )
    ) +  
      ggplot2::geom_point(alpha = 0.3, shape = 21) +
      ggplot2::labs(
        x = "Longitude", y = "Latitude", title = ttl_bub
      ) +
      ggplot2::scale_size_continuous(breaks = c(lw, 0, up), range = c(1, 10)) +
      ggplot2::scale_fill_distiller(direction = -1, palette="RdYlBu",  breaks = c(lw, 0, up)) +
      # merge scales
      ggplot2::guides(fill = ggplot2::guide_legend(), size = ggplot2::guide_legend()) 
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
  
  if (!isTruthy(pred)) {
    stop("`pred` needs to be supplied!", call. = FALSE)
  }
  
  if (inherits(dat, "recipe")) {
    
    # check if recipe has tune dials
    dls <- hardhat::extract_parameter_set_dials(dat)$name
    
    if (all(length(dls) != 0, !isTruthy(tune))) {

      stop("The model has been tuned and therefore `tune` needs to be ", 
           "supplied!", call. = FALSE)
    } 
    
    x <- rlang::ensym(pred)
    
  }
  
  if (inherits(dat, "tune_results")) {
    
    if (inherits(dat, "last_fit")) {
      
      # number of components used for prediction
      mold <- dat$.workflow[[1]] |> 
        workflows::extract_mold()
      # dimensions after processing
      n_comps <- ncol(mold$predictors)
      # dimensions original
      n_preds <- mold$blueprint$recipe$var_info |> 
        dplyr::filter(role == "predictor") |> 
        nrow()
      
      if (n_preds == n_comps) {
        
        x <- pred
        
      } else if (all(n_preds > n_comps, isTruthy(tune))) {
        
        x <- paste0(n_comps, "PCs") # return number of components tuning
      
      } else {
        
        stop("The model has been tuned and therefore `tune` needs to be ", 
             "supplied!", call. = FALSE)
      }
      
    } else {
    
      # check if results are tuned (class 'resample_results' is not tuned)
      nottuned <- inherits(dat, 'resample_results')
      
      if (all(!nottuned, !isTruthy(tune))) {
        
        stop("The model has been tuned and therefore `tune` needs to be ", 
             "supplied!", call. = FALSE)
      }
        
      x <- rlang::ensym(pred)
      
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

# untune recipe for easier plotting of feature engineered data
untune <- function(recipe, tune) {
  
  dls <- hardhat::extract_parameter_set_dials(recipe)
  if (nrow(dls) > 0) {
    depth_dial <- purrr::map_chr(recipe$steps, list("id")) |> 
      stringr::str_detect("pca|pls") |> which() # depth of dial
    name_dial <- dls$name # name of dial
    # replace tuning parameter with fixed parameter based on `tune`
    purrr::pluck(recipe, "steps", depth_dial, name_dial) <- tune 
    recipe
  }
  recipe
}

x_labeller <- function(workflow, x) {
  if (grepl("PC", as_name(x))) {
    as_name(x)
  } else {
    taxa_naming(workflow, as_name(x))
  }
}
