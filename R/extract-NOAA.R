# wrapper function to extract coordinates from all environmental parameters
extract_NOAA <- function(NOAA, coords, depth = 30) {
  
  # get locations parameters
  ls_parms <- purrr::map(
    NOAA, 
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