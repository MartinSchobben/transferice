#' print HTML model setup
#' Model print
#' 
#' 
#' @export
print_model <- function(obj, ...) { 
  UseMethod("print_model")
}
#' @rdname print_model
#' 
#' @export
print_model.mc_split <- function(    
    obj, 
    workflow, 
    pred = NULL,
    tune = NULL,
    out = NULL,
    exclude = c("sample_id", "hole_id", "longitude", "latitude")
  ) {
  
  # number of outcome
  rcp <- workflows::extract_preprocessor(workflow)
  
  var_info <- rcp$var_info |> 
    dplyr::filter(.data$role == "outcome") 
  # number of outcome
  nout <- nrow(var_info)
  
  # untune
  rcp <- untune(rcp, tune)
  # number of predictors
  cast <- recipes::prep(rcp, training = rsample::training(obj)) |> 
    # apply to training data
    recipes::bake(new_data = NULL)
  # number of predictors after cast
  npred <- length(cast[!names(cast) %in% c(var_info$variable, exclude)])
  
  out <- paste("<b>features</b>: <br/> <br/> outcome ($Y$):", nout)
  pred <- paste("predictors ($X$):", npred)
  
  # operations
  ops <- gsub("_", " ", sanitize_workflow(workflow, model = F))
  ops <- paste("<b>operations</b>: <br/> <br/> ", ops, collapse = "")
  
  # model component
  mdl <-paste("<b>model</b>: <br/> <br/> $Y = \\beta_{0} + \\beta_{1} X + \\epsilon$", 
              sep = "<br/> <br/>")
  
  # print with mathjax
  tagList(
    tags$br(),
    withMathJax(HTML(paste(out, pred, ops, mdl, sep = "<br/> <br/> "))),
    tags$br(),
    tags$br(),
    HTML("<b>Click on the button 'Train model' to proceed</b>.")
  )
}
#' @rdname print_model
#' 
#' @export
print_model.tune_results  <- function(    
    obj, 
    workflow, 
    pred = NULL,
    tune = NULL,
    out = NULL,
    height = NULL,
    width = NULL,
    id = character(0)
 ) {
 
  # predictor variable
  x <- pred_check(obj, pred, tune)
  
  # fitted data requires a species name variable selection
  if (isTruthy(tune))  {
    
    obj <- tune::collect_metrics(obj, summarize = FALSE) |>
      dplyr::mutate(
        var = .data$num_comp,
        slct = dplyr::if_else(.data$num_comp == tune, TRUE, FALSE)
      ) 
     
  } else {
  # tuned data requires a dimension variable selection   
    obj <- tune::collect_metrics(obj, summarize = FALSE) |>
      dplyr::mutate(var = 1, slct = 1)  
        
  }
  

  workflow_specs <- sanitize_workflow(workflow)
  # file name
  if (length(unique(obj$var)) != 1) viz <- "boxplot" else viz <- "histogram"
  file_name <- file_namer("png", "training", id, trans = workflow_specs, 
                          viz = viz, x = x)
  
  # boxplot of model RMSE
  ggperformance(obj) |> 
      app_caching(type = "png", file_name = file_name, width = width, height = height)
  
  # plot and print with mathjax
  tagList(    
    tags$br(),
    tags$br(), 
    tags$div(
      tags$img(src = fs::path("img", file_name, ext = "png")), 
      style="text-align: center;"
    ),
    tags$br(),
    tags$br(), 
    withMathJax(
      HTML(
        paste0(
          "$RMSE = \\sqrt{\\frac{1}{N}\\sum{\\left(Y - \\hat{Y} \\right)^2}}$",  
          "<br/><br/> <b>Click the button 'Reset model' to select ", 
          "another model.</b>"
        )
      )
    )
  )
}
#' @rdname print_model
#' 
#' @export
print_model.last_fit  <- function(    
    obj, 
    workflow, 
    pred = NULL,
    tune = NULL,
    out = NULL
  ) {
  
  # all collected metrics
  mts <- tune::collect_metrics(obj)
  # a <- print_metric(mts, "rsq")
  b <- print_metric(mts, "rmse")
  # c <- print_metric(mts, "rmsre")
  # all <- paste(a, b, c, sep = "<br/><br/>")
  # print with mathjax
  withMathJax(HTML(paste0("<br/> <br/> <b>model fit metrics</b>: <br/> <br/>", b)))
  
}


ggperformance <- function(obj) {
  
  if (length(unique(obj$var)) != 1) {
    p <- ggplot2::ggplot(
      data = obj,
      mapping = ggplot2::aes(
        x = .data$var,
        y = .data$.estimate,
        group = .data$var,
        fill = .data$slct
      )
    ) +
      ggplot2::geom_boxplot(show.legend = FALSE) +
      ggplot2::labs(y = "RMSE", x = "Principal Component") 
  } else {
    p <- ggplot2::ggplot(
      data = obj,
      mapping = ggplot2::aes(x = .data$.estimate)
    ) +
      ggplot2::labs(x = "RMSE") +
      ggplot2::geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))
  }
  
  p + transferice_theme()
  

} 