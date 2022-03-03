#' Root mean square relative error
#'
#' @param data 
#' @param ... 
#'
#' @return
#' @export
rmsre <- function(data, ...) {
  UseMethod("rmsre")
}

rmsre <- yardstick::new_numeric_metric(rmsre, direction = "minimize")

#' @rdname rmsre
#' @export
rmsre.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {
  
  yardstick::metric_summarizer(
    metric_nm = "rmsre",
    metric_fn = rmsre_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate), 
    na_rm = na_rm,
    ...
  )
  
}

#' @export
#' @rdname rmsre
rmsre_vec <- function(truth, estimate, na_rm = TRUE, ...) {
  
  rmsre_impl <- function(truth, estimate) {
    sqrt( mean( ( estimate / truth - 1) ^ 2) ) 
  }
  
  yardstick::metric_vec_template(
    metric_impl = rmsre_impl,
    truth = truth, 
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
  
}