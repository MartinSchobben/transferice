# object can be workflow or parsnip model
ggpredict <- function(obj, new_data, pm, time_unit = "age") {
  
  pm <- stringr::str_split(pm, "_")[[1]]
  
  new_data <- tidyr::drop_na(new_data, age_ma)
  
  pred <- predict(obj, new_data = new_data)
  # pred_int <- predict(obj$.workflow[[1]], new_data = new_data, type = "pred_int")
  
  if (time_unit == "age") {
    xlab <- "Age (Ma)"
    x <- "age_ma"
  }
  
  # add age to original data
  dplyr::bind_cols(new_data, pred) |> 
    # dplyr::bind_cols(pred_int) |> 
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[[x]], 
        y = .pred
      )
    ) +
      # ggplot2::geom_ribbon(
      #   mapping =  ggplot2::aes(
      #     ymin = .pred_lower, 
      #     ymax = .pred_upper
      #     ),
      #   fill = "lightgrey"
      # ) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::scale_x_reverse() +
      ggplot2::labs(x = xlab, y = oceanexplorer::env_parm_labeller(pm)) +
      transferice_theme()
}