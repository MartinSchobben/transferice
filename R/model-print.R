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
  ops <- paste("<b>operations</b>: <br/> <br/> ", ops, collapse = "")
  
  # model component
  mdl <-paste("<b>model</b>: <br/> <br/>  $Y = \\beta_{0} + \\beta_{1} X + \\epsilon$", 
              sep = "<br/> <br/>")
  
  # print with mathjax
  withMathJax(HTML(paste(out, pred, ops, mdl, sep = "<br/> <br/> ")))
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