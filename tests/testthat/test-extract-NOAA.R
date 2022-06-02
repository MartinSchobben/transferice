test_that("extracting NOAA variables works", {
  # coordinates
  crds <- list(lon = 140, lat = 10)
  # get NOAA averaged data for parameters on a 1 degree grid 
  all_NOAA <- purrr::map(parms, ~oceanexplorer::get_NOAA(.x, 1, "annual")) |> 
    setNames(abbreviate_vars(parms))
  extract_NOAA(all_NOAA, crds)
})
