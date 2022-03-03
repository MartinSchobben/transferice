# dinocysts
dbpath <- fs::path_package(package = "transferice", "extdata", 
                           "transferice.sqlite")
con <- DBI::dbConnect(drv = RSQLite::SQLite(),  dbname = dbpath)
dino_prop <- calc_taxon_prop("neptune_sample_taxa", "neptune_sample", con) 
# environment
locs <- DBI::dbGetQuery(
  con, 
  "SELECT l.hole_id, site, longitude, latitude, sample_id 
    FROM neptune_hole_summary l 
      LEFT JOIN neptune_sample s ON l.hole_id = s.hole_id"
)

# # scale variance of dinocyst data
# dino_prep <- dplyr::mutate(
#   dino_prop, 
#   dplyr::across(-c(sample_id , hole_id), rescale)
# )

parms <- c("temperature", "phosphate", "nitrate", "silicate", "oxygen", 
            "salinity", "density")

usethis::use_data(parms, overwrite = TRUE)

# get NOAA annually averaged data for parameters on a 1 degree grid 
ls_data <- purrr::map(params, ~oceanexplorer::get_NOAA(.x, 1, "annual"))


crd <- locs[, !names(locs) %in%  c("hole_id", "site", "sample_id"), drop = FALSE]
# cast location as list before extraction
pts <- setNames(as.list(crd), nm = c("lon", "lat"))

# get locations parameters
ls_params <- purrr::map(ls_data, ~oceanexplorer::filter_NOAA(.x, depth = 30,  coord = pts))

# make normal tibble
reduce_sf <- function(params) {
  
  # remove depth
  purrr::map(params, ~dplyr::select(.x, -.data$depth)) |> 
    # drop geometry as we will use it for ...
    purrr::map(~sf::st_drop_geometry(.x)) |> 
    dplyr::bind_cols()
}

environ_dat <- dplyr::bind_cols(locs, reduce_sf(ls_params)) |> 
  dplyr::select(-c(.data$site, .data$longitude, .data$latitude))

# combine
dinodat <- dplyr::left_join(environ_dat, dino_prop, by = c("hole_id", "sample_id")) |> 
  dplyr::select(-.data$hole_id, -.data$sample_id) |> 
  tidyr::drop_na() 
  
# response  <- as.matrix(dplyr::select(dinodat, dplyr::any_of(paste(abbreviate_vars(parms), "an", sep = "_"))))
# predictor  <- as.matrix(dplyr::select(dinodat, -dplyr::any_of(paste(abbreviate_vars(parms), "an", sep = "_"))))
# 
# dinodat <- data.frame(response = I(response), predictor = I(predictor ))
  
usethis::use_data(dinodat, overwrite = TRUE)
