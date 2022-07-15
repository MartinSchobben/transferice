#' print HTML model setup
#' Model print
#' 
#' 
#' @export
print_predict <- function(obj, new_data, out, ...) { 
  
  # percent taxa precent in traing set
  prec_present <- impute_taxa(obj, new_data, out, return_type = "percent")
    
  tx <- sprintf("%.1f %% of the fossil taxa are present in the training data.", prec_present)

  tagList(
    HTML(tx)
  )
}