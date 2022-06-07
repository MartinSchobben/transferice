step_pivot <- function(
  recipe, 
  ..., 
  role = "outcome", 
  trained = FALSE,
  options = list(names_to = "names", values_to = "value"), 
  skip = FALSE,
  id = recipes::rand_id("pivot")
) {
  
  recipes::add_step(
    recipe, 
    step_pivot_new(
      terms = enquos(...), 
      trained = trained,
      role = role, 
      options = options,
      skip = skip,
      id = id
    )
  )
  
}

step_pivot_new <- function (
    terms,
    role, 
    trained,
    options,
    skip,
    id
  ) {
  
  recipes::step(
    subclass = "pivot", 
    terms = terms,
    role = role,
    trained = trained,
    options = options,
    skip = skip,
    id = id
  )
}

#' @name step_pivot  
#'
#' @importMethodsFrom recipes prep
#' @export
prep.step_pivot <- function(x, training, info = NULL, ...) {
  
  # extract the selection columns
  col_names <- recipes::recipes_eval_select(x$terms, training, info) 
  
  # the transformation
  get_train_pivot(training, col_names, args)
  
  ## Use the constructor function to return the updated object. 
  ## Note that `trained` is now set to TRUE
  step_pivot_new(
    terms = x$terms, 
    trained = TRUE,
    role = x$role, 
    options = x$options,
    skip = x$skip,
    id = x$id
  )
}

get_train_pivot <- function(data, cols, args = NULL) {
  rlang::inject(
    tidyr::pivot_longer(data, cols = -c(!!! rlang::parse_exprs(cols)), !!!args)
  )
}