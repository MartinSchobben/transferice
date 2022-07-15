#' Reduce taxonomic depth
#'
#' `step_taxa` creates a *specification* of a recipe
#'  step that will reduce the taxonomic depth.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables
#'  for this step. See [selections()] for more details.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param trained A logical to indicate if the quantities for
#'  preprocessing have been estimated.
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
step_taxo <- function(
  recipe, 
  ..., 
  role = "predictor", 
  trained = FALSE, 
  skip = FALSE,
  id = recipes::rand_id("taxo")
) {
  
  recipes::add_step(
    recipe, 
    step_taxo_new(
      terms = enquos(...), 
      trained = trained,
      role = role,
      skip = skip,
      id = id
    )
  ) 
  
}

step_taxo_new <- function (
    terms,
    role, 
    trained,
    skip,
    id
  ) {
  
  recipes::step(
    subclass = "taxo", 
    terms = terms,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}
#' @importFrom recipes prep
#' @export
prep.step_taxo <- function(x, training, info = NULL, ...) {
  
  # extract the selection columns
  terms <- recipes::recipes_eval_select(x$terms, training, info) 
  
  ## Use the constructor function to return the updated object. 
  ## Note that `trained` is now set to TRUE

  step_taxo_new(
    terms = terms, 
    trained = TRUE,
    role = x$role, 
    skip = x$skip,
    id = x$id
  )
}
#' @importFrom recipes bake
#' @export
bake.step_taxo <- function(object, new_data, ...) {
  tidyr::pivot_longer(new_data, !dplyr::all_of(object$terms)) |> 
    tidyr::separate(name, into = c("genera", "species"), extra = "merge", 
                    fill = "right") |> 
    dplyr::group_by(!!!rlang::syms(c(object$terms, "genera"))) |> 
    dplyr::summarise(value = sum(value), .groups = "drop") |> 
    tidyr::pivot_wider(names_from = "genera", values_from = "value") 

}
#' @importFrom recipes tidy
#' @export
tidy.step_taxo <- function(x, ...) {
    term_names <- sel2char(x$terms)
    res <-
      tibble(
        terms = term_names,
        id = x$id
      )
}