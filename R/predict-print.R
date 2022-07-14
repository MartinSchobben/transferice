#' print HTML model setup
#' Model print
#' 
#' 
#' @export
print_predict <- function(obj, new_data, ...) { 
  UseMethod("print_predict")
}
#' @rdname print_predict
#' 
#' @export
print_predict.last_fit <- function(obj, new_data, out, ...) { 
  
  # percent taxa precent in traing set
  prec_present <- impute_taxa(obj, new_data, out, return_type = "percent")
    
  tx <- paste(prec_present, "% of the fossil taxa are present in the training data.")

  tagList(
    HTML(tx)
  )
}