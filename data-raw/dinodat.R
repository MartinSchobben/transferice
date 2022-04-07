# ------------------------------------------------------------------------------
# SQL queries
# ------------------------------------------------------------------------------
dbpath <- fs::path_package(package = "transferice", "extdata", 
                           "transferice.sqlite")
con <- DBI::dbConnect(drv = RSQLite::SQLite(),  dbname = dbpath)
dino_prop <- calc_taxon_prop("neptune_sample_taxa", "neptune_sample", 
                             "neptune_taxonomy", con) 
# environment
locs <- DBI::dbGetQuery(
  con, 
  "SELECT l.hole_id, site, longitude, latitude, sample_id 
    FROM neptune_hole_summary l 
      LEFT JOIN neptune_sample s ON l.hole_id = s.hole_id"
)

# ------------------------------------------------------------------------------
# environmental parameters
# ------------------------------------------------------------------------------
parms <- c("temperature", "phosphate", "nitrate", "silicate", "oxygen", 
           "salinity", "density")

# save parameter names
usethis::use_data(parms, overwrite = TRUE)

# get NOAA annually averaged data for parameters on a 1 degree grid 
ls_data <- purrr::map(parms, ~oceanexplorer::get_NOAA(.x, 1, "annual"))

crd <- locs[, !names(locs) %in%  c("hole_id", "site", "sample_id"), drop = FALSE]
# cast location as list before extraction
pts <- setNames(as.list(crd), nm = c("lon", "lat"))

# get locations parameters
ls_parms <- purrr::map(
  ls_data, 
  ~oceanexplorer::filter_NOAA(.x, depth = 30,  coord = pts)
  )

# make normal tibble
reduce_sf <- function(parms) {
  
  # remove depth
  purrr::map(parms, ~dplyr::select(.x, -.data$depth)) |> 
    # drop geometry as we will use it for ...
    purrr::map(~sf::st_drop_geometry(.x)) |> 
    dplyr::bind_cols()
}

environ_dat <- dplyr::bind_cols(locs, reduce_sf(ls_parms)) |> 
  dplyr::select(-c(.data$site))

# ------------------------------------------------------------------------------
# dinocysts
# ------------------------------------------------------------------------------
# combine
dinodat <- dplyr::left_join(
  environ_dat, 
  dino_prop, 
  by = c("hole_id", "sample_id")
  ) |> 
  # remove NAs
  tidyr::drop_na() 
  
# save data
usethis::use_data(dinodat, overwrite = TRUE)

# disconnect SQL
DBI::dbDisconnect(con)
