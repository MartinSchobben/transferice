#print HTML model setup
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
    out = NULL
  ) {
  
  
  # number of outcome
  var_info <- workflows::extract_preprocessor(workflow)$var_info |> 
    dplyr::filter(.data$role == "outcome") 
  # number of outcome
  nout <- nrow(var_info)
  
  # number of predictors
  cast <- ggpartial(
    obj = obj, 
    workflow = workflow,         
    pred = pred,
    tune = tune,
    out = out, 
    return_type = "cast"
  )
  # number of predictors after cast
  npred <- length(cast[!names(cast) %in% var_info$variable])
  
  out <- paste("<b>features</b>: <br/> <br/> outcome ($Y$):", nout)
  pred <- paste("predictors ($X$):", npred)
  
  # operations
  ops <- gsub("_", " ", sanitize_workflow(workflow, model = F))
  ops <- paste("<b>operations</b>: ", ops, collapse = "")
  
  # model component
  mdl <-paste("<b>model</b>: $Y = \\beta_{0} + \\beta_{1} X + \\epsilon$", 
              sep = "<br/> <br/>")
  
  # print with mathjax
  tagList(
    wellPanel(
      withMathJax(
      HTML(
        paste0("This section deals with preparing the data before model ", 
               "training. It mainly involves variance stabilization of ", 
               "predictors and the reduction of dimensionality. All models ", 
               "are based on a multivariate multiple linear regression, based ", 
               "on a matrix of n-predictors ($X$) and a matrix of m-outcomes ", 
               "($Y$). <br/><br/> <b>Click on the button 'Train model' to ",
               "proceed</b>.")
        )
      )),
    tags$br(),
    withMathJax(HTML(paste(out, pred, ops, mdl, sep = "<br/> <br/> ")))
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
    width = NULL
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
  
  obj <- dplyr::filter(obj, .data$.estimate < 100) # remove extreme outliers
  
  # name
  y <- rlang::ensym(out) 
  workflow_specs <- sanitize_workflow(workflow)
  nm <- paste("folds", "boxplot", workflow_specs, as_name(y), x, sep = "_") 
  
  # potential path
  ggpath <- try(
    fs::path_package("transferice", "www", "img", nm, ext = "png"), 
    silent = TRUE
  )
  
  if (inherits(ggpath, "try-error")) {
  
  # boxplot of model RMSE
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
    ggplot2::scale_x_discrete(
      "components", 
      labels = as.character(1:10), 
      breaks = 1:10
    ) +
    ggplot2::labs(y = "RMSRE")
  
  # add theme
  p <- p + transferice_theme() 
  
  # save plot
  ggplot2::ggsave(
    fs::path(nm, ext = "png"),
    plot = p, 
    path = fs::path_package(package = "transferice", "www", "img"),
    width = width,
    height = height,
    dpi = 72,
    units = "px"
  )
  
  }
  
  # plot and print with mathjax
  tagList(
    wellPanel(
      withMathJax(
        HTML(
          paste0("Training of a model has two purposes. <br/><br/>", 
                 "1) It measures the generalization capacity of the model.",
                 "In the animation on the left we would ideally want to see ",
                 "a minimal amount of wobbling in the regression line. <br/>",
                 "2) It helps select the optimal number of dimensions after ",
                 "dimension reduction by principal component analyses. <br/>",
                 "<br/>A good model has a low Root Mean Square Relative ",
                 "Error. <br/><br/>",  
                 "$RMSRE = \\sqrt{\\frac{1}{N}\\sum{\\left( \\frac{\\hat{Y}}{Y} - 1\\right)^2}}$",  
                 "<br/><br/> <b>Click the button 'Reset model' to select ", 
                 "another model.</b>")
        )
      )),
    tags$br(),  
    tags$div(
      tags$img(src = fs::path("img", nm, ext = "png")), 
      style="text-align: center;"
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
  a <- print_metric(mts, "rsq")
  b <- print_metric(mts, "rmse")
  c <- print_metric(mts, "rmsre")
  all <- paste(a, b, c, sep = "<br/><br/>")
  # print with mathjax
  withMathJax(HTML(paste0("<b>model fit metrics</b>: <br/> <br/>", all)))
}