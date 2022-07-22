#' print HTML model setup
#' Model print
#' 
#' 
#' @param obj Character string identifying the finalized workflow.
#' @param new_data The dataframe used for the prediction.
#' @param out The parameter of interest.
#' 
#' @return HTML text
#'
#' @export
print_predict <- function(obj, new_data, out) { 
  
  # percent taxa present in training set
  prec_present <- impute_taxa(obj, new_data, out, return_type = "percent")
    
  tx <- sprintf("%.1f %% of the fossil taxa are present in the training data.", 
                prec_present)

  tagList(HTML(tx))
}