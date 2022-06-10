#' Filter Minimum Distance for Spatial Coordinates
#'
#' `step_zerogeodist` creates a *specification* of a recipe
#'  step that will pivot data for multilevel models.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param lon,lat Selector functions to choose which variables are
#'  used by the step. See [selections()] for more details.
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
step_zerogeodist <- function(
    recipe, 
    lon = NULL,
    lat = NULL,
    role = "spatial", 
    trained = FALSE,
    options = list(min_dist = 2500),
    skip = FALSE,
    id = recipes::rand_id("zerogeodist")
) {
  
  recipes::add_step(
    recipe, 
    step_zerogeodist_new(
      lon = enquos(lon),
      lat = enquos(lat),
      trained = trained,
      options = options,
      role = role, 
      skip = skip,
      id = id
    )
  )
  
}

step_zerogeodist_new <- function (
    lon,
    lat,
    role, 
    trained,
    options,
    skip,
    id
) {
  
  recipes::step(
    subclass = "zerogeodist", 
    lon = lon,
    lat = lat,
    role = role,
    trained = trained,
    options = options,
    skip = skip,
    id = id
  )
}
#' @importFrom recipes prep
#' @export
prep.step_zerogeodist <- function(x, training, info = NULL, ...) {
  
  lon_name <- recipes::recipes_eval_select(x$lon, training, info)
  lat_name <- recipes::recipes_eval_select(x$lat, training, info)
  
  ## Use the constructor function to return the updated object. 
  ## Note that `trained` is now set to TRUE
  step_zerogeodist_new(
    lon = lon_name,
    lat = lat_name,
    trained = TRUE,
    role = x$role, 
    options = x$options,
    skip = x$skip,
    id = x$id
  )
}
#' @importFrom recipes bake
#' @export
bake.step_zerogeodist <- function(object, new_data, ...) {
  
  # convert to sf object
  sf_data <- sf::st_as_sf(
    new_data,
    coords = c(object$lon, object$lat),
    crs = 4326
  )
  
  # remove duplicate locations with tolerance 2.5 km radius
  d <- sf::st_is_within_distance(sf_data, dist = object$options$min_dist)
  dupl <- unlist(mapply(function(x, y) x[x < y], d, seq_along(d)))
  
  sf_data <- sf_data[-dupl, ]
  
  # coordinates as numbers
  coord <- sf::st_coordinates(sf_data$geometry)
  colnames(coord) <- c(object$lon, object$lat)
  
  # return tibble
  dplyr::bind_cols(sf::st_drop_geometry(sf_data), coord)
  
}

