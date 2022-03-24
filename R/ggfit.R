#' R squared plot
#'
#' @param final 
#' @param parms 
#' @param averaging 
#' @param selected 
#'
#' @return
#' @export
#'
ggfit <- function(final, parms, averaging = "an", selected = "all", 
                  type = "regression", base_map = NULL) {

  # parameter of interest
  pm <- paste(selected, averaging, sep = "_") # single
  # all parameter
  pms <- paste(parms, averaging, sep = "_") # all
  
  # name file and potential path
  nm <- paste("final", type, selected, averaging, sep = "_")
  im_path <- try(
    fs::path_package("transferice", "plots", nm, ext = "png"), 
    silent = TRUE
   )
    
  if (inherits(im_path, "try-error")) {
  
    # extract predicitons
    preds <- tune::collect_predictions(final)
    
    if (type == "spatial") {
      # original data
      origin <- dplyr::transmute(final, origin = purrr::map(splits, rsample::testing)) |> 
        tidyr::unnest(cols = c(origin))
      output <- dplyr::select(preds, - dplyr::any_of(dplyr::any_of(pms)))
      # interpolate
      z <- interpolate_model(origin, output, !!rlang::sym(pm), base_map)
      
      # difference
      z <- z - base_map
      names(z) = pm # rename
      
      # label
      lbl <- oceanexplorer::env_parm_labeller(selected, prefix = "Delta")
      
      # plot (suppres waqrning of replacing fill scale)
      p <- oceanexplorer::plot_NOAA(z) +
        ggplot2::scale_fill_distiller(lbl, type = "div", palette = "PuOr")
      
    } else if (type == "regression") {
  
      # rename original true values
      final <- dplyr::rename_with(
        preds, 
        .cols = dplyr::any_of(pms), 
        .fn = ~paste0(".truth_", .x)
      )
      
      # pivot to long format
      long_final <-tidyr::pivot_longer(
          final,
          cols = dplyr::ends_with(paste0("_", averaging)),
          names_to = c(".value", "parameter"),
          names_pattern = paste0("(.*)_(._", averaging ,")")
        )
      
      if (selected != "all")  {
        # check if param exists
        chk <- pm %in% unique(long_final$parameter)
        if (!chk) stop("Selected parameter does not exist in data.", call. = FALSE)
        long_final <- dplyr::filter(long_final,  .data$parameter ==  pm)
      }
    
      p <- ggplot2::ggplot(
        long_final, 
        ggplot2::aes(x = .data$.truth, y = .data$.pred)
        ) +  
        ggplot2::geom_point() +  
        ggplot2::geom_abline(color = 'blue', linetype = 2) +
        ggplot2::labs(
          title = 'R-squared Plot',       
          y = 'Predicted',        
          x = 'Actual'
        )
      
      if (selected == "all") {
        p <- p + ggplot2::facet_wrap(ggplot2::vars(parameter), scales = "free") 
      } else {
        p <- p
      }
    
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