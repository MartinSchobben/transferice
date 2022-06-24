# ------------------------------------------------------------------------------
# SQL queries
# ------------------------------------------------------------------------------
dbpath <- fs::path_package(package = "transferice", "extdata", 
                           "transferice.sqlite")
con <- DBI::dbConnect(drv = RSQLite::SQLite(),  dbname = dbpath)

# calculate proportional dino counts
dino_prop <- calc_taxon_prop(con) 

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
dt  <- oceanexplorer::get_NOAA("temperature", 1, "annual")

crd <- locs[, !names(locs) %in%  c("hole_id", "site", "sample_id"), drop = FALSE]
# cast location as list before extraction
pts <- setNames(as.list(crd), nm = c("lon", "lat"))

# get locations parameters
parms <- oceanexplorer::filter_NOAA(dt, depth = 30,  coord = pts) |> 
  dplyr::select(-.data$depth) |> 
  sf::st_drop_geometry() 

# location informaiton
environ_dat <- dplyr::bind_cols(locs, parms) |> 
  dplyr::select(-c(.data$site))

# save data
usethis::use_data(environ_dat , overwrite = TRUE)

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
  tidyr::drop_na() |> 
  # sample poriton
  dplyr::slice_sample(n = 600)
  
# save data
usethis::use_data(dinodat, overwrite = TRUE)

# disconnect SQL
DBI::dbDisconnect(con)
