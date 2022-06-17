#' Pivot Table
#'
#' `step_pivot` creates a *specification* of a recipe
#'  step that will pivot data for multilevel models.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables
#'  for this step. See [selections()] for more details.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param trained A logical to indicate if the quantities for
#'  preprocessing have been estimated.
#' @param options A list of options to the default method for
#'  [tidyr::pivot_longer()]. Argument defaults are set to `names_to = "names` 
#'  and `values_to = "values"`.
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake()]? While all operations are baked
#'  when [prep()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations.
#' @param id A character string that is unique to this step to identify it.
#'
#' @return step
#' @export
step_pivot <- function(
  recipe, 
  ..., 
  role = c("predictor", "outcome"), 
  trained = FALSE,
  options = list(names_to = "name", values_to = "value"), 
  skip = FALSE,
  id = recipes::rand_id("pivot")
) {
  
  recipes::add_step(
    recipe, 
    step_pivot_new(
      terms = enquos(...), 
      trained = trained,
      role = role[1], 
      options = options$names_to,
      skip = skip,
      id = id
    )
  ) |> 
    recipes::add_step(
  
      recipes:::step_mutate_new(
        trained = trained,
        role = role[2],
        inputs = options$values_to,
        skip = skip,
        id = rand_id("mutate")
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
#' @importFrom recipes prep
#' @export
prep.step_pivot <- function(x, training, info = NULL, ...) {
  
  # extract the selection columns
  terms <- recipes::recipes_eval_select(x$terms, training, info) 
  
  ## Use the constructor function to return the updated object. 
  ## Note that `trained` is now set to TRUE

  step_pivot_new(
    terms = terms, 
    trained = TRUE,
    role = x$role, 
    options = x$options,
    skip = x$skip,
    id = x$id
  )
}
#' @importFrom recipes bake
#' @export
bake.step_pivot <- function(object, new_data, ...) {
  rlang::inject(
    tidyr::pivot_longer(new_data, cols = dplyr::all_of(object$terms), !!!object$options)
  ) 
}
#' @importFrom recipes tidy
#' @export
tidy.step_pivot <- function(x, ...) {
    term_names <- sel2char(x$terms)
    res <-
      tibble(
        terms = term_names,
        id = x$id
      )
}