ggpredict <- function(obj, new_data, meta, time_unit = "age") {
  
  pred <- predict(obj$.workflow[[1]], new_data = new_data)
  pred_int <- predict(obj$.workflow[[1]], new_data = new_data, type = "pred_int")
  
  if (time_unit == "age") {
    xlab <- "Age (Ma)"
    x <- "age_ma"
  }
  
  # add age to original data
  dplyr::bind_cols(new_data, pred) |> 
    dplyr::bind_cols(pred_int) |> 
    dplyr::left_join(meta, by = "sample_id") |> 
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[[x]], 
        y = .pred, 
        ymin = .pred_lower, 
        ymax = .pred_upper)
      ) +
      ggplot2::geom_ribbon(fill = "lightgrey") +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::scale_x_reverse() +
      ggplot2::labs(x = xlab, y = oceanexplorer::env_parm_labeller("t")) +
      transferice_theme()
}