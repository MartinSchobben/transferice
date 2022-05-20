# wrapper function to extract coordinates from all environmental parameters
extract_NOAA <- function(coords, averaging = "annual", depth = 30) {
  
  # get NOAA averaged data for parameters on a 1 degree grid 
  ls_data <- purrr::map(parms, ~oceanexplorer::get_NOAA(.x, 1, averaging))

  # get locations parameters
  ls_parms <- purrr::map(
    ls_data, 
    ~oceanexplorer::filter_NOAA(.x, depth = depth,  coord = coords)
  )

  # make normal tibble
  reduce_sf <- function(parms) {
  
    # remove depth
    purrr::map(parms, ~dplyr::select(.x, -.data$depth)) |> 
      # drop geometry as we will use it for ...
      purrr::map(~sf::st_drop_geometry(.x)) |> 
      dplyr::bind_cols()
  }
  
  names(coords) <- c("longitude", "latitude")
  dplyr::bind_cols(coords, reduce_sf(ls_parms))
}