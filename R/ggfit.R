#' @rdname ggpartial
#' 
#' @export
ggpartial.last_fit <- function(
    obj, 
    recipe = NULL, 
    pred = NULL, 
    tune = NULL, 
    out, 
    type = "regression",
    base_map = NULL,
    preprocessor = NULL
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
  ttl_reg <- paste('R-squared Plot', x)
  ttl_spat <- paste("Difference in prediction", x)
  
  # name
  nm <- paste("final", type, preprocessor, pm, x, sep = "_")
  # potential path
  ggpath <- try(
    fs::path_package("transferice", "plots", nm, ext = "png"), 
    silent = TRUE
  )
  
  if (inherits(ggpath, "try-error")) {
  
    # extract predictions
    preds <- tune::collect_predictions(obj)
    
    if (type == "spatial") {
      
      # original data
      origin <- dplyr::transmute(obj, origin = purrr::map(splits, rsample::testing)) |> 
        tidyr::unnest(cols = c(origin))
      # predictions
      output <- dplyr::select(preds, - dplyr::any_of(dplyr::any_of(pms)))
      # interpolate
      z <- interpolate_model(origin, output, !!rlang::sym(pm), base_map)
      
      # difference on raster (predicted - truth)
      z <- z - base_map
      names(z) = pm # rename
      
      # label
      lbl <- oceanexplorer::env_parm_labeller(as_name(y), prefix = "Delta")
      
      # plot (suppres waqrning of replacing fill scale)
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
      
      # if (selected != "all")  {
        # check if param exists
        chk <- pm %in% unique(long_obj$parameter)
        if (!chk) stop("Selected parameter does not exist in data.", call. = FALSE)
        long_obj <- dplyr::filter(long_obj,  .data$parameter ==  pm)
      # }
    
      p <- ggplot2::ggplot(
        long_obj, 
        ggplot2::aes(x = .data$.truth, y = .data$.pred)
        ) +  
        ggplot2::geom_point() +  
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
    
    # save plot
    suppressWarnings(
      ggplot2::ggsave(
        fs::path(nm, ext = "png"),
        plot = p, 
        path = fs::path_package(package = "transferice", "plots"),
        width = if (type == "regression") 400 else 600,
        height = if (type == "regression") 400 else 300,
        dpi = 72,
        units = "px"
      )
    )
  }
  
  # depending whether the figure is newly rendered this function executes 
  # fast or slow
  fs::path_package("transferice", "plots", nm, ext = "png") 
}