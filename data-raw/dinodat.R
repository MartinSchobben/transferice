# ------------------------------------------------------------------------------
# SQL queries
# ------------------------------------------------------------------------------
dbpath <- fs::path_package(package = "transferice", "extdata", 
                           "transferice.sqlite")
con <- DBI::dbConnect(drv = RSQLite::SQLite(),  dbname = dbpath)

# calculate proportional dino counts
dino_prop <- calc_taxon_prop(con) 

# environment
locs <- dplyr::select(dino_prop, longitude, latitude)
  
# ------------------------------------------------------------------------------
# environmental parameters
# ------------------------------------------------------------------------------
parms <- c("temperature", "phosphate", "nitrate", "silicate", "oxygen", 
           "salinity", "density")


# meta
meta <- c("sample_id", "sample_depth_mbsf", "hole_id", "site_hole", 
           "longitude", "latitude", "source_citation", "age_ma")

# save parameter names
usethis::use_data(parms, meta, internal = TRUE, overwrite = TRUE)

# get NOAA annually averaged data for parameters on a 1 degree grid 
dt  <- oceanexplorer::get_NOAA("temperature", 1, "annual")

# cast location as list before extraction
pts <- setNames(as.list(locs), nm = c("lon", "lat"))

# get locations parameters
parms <- oceanexplorer::filter_NOAA(dt, depth = 30,  coord = pts) |> 
  dplyr::select(-.data$depth) |> 
  sf::st_drop_geometry() 

# location informaiton
environ_dat <- dplyr::bind_cols(locs, parms) 

# save data
# usethis::use_data(environ_dat , overwrite = TRUE)

# ------------------------------------------------------------------------------
# dinocysts
# ------------------------------------------------------------------------------
# combine
modern <- tibble::add_column(dino_prop, t_an = environ_dat$t_an) |> 
  # remove NAs
  tidyr::drop_na(-age_ma) 

# save data
usethis::use_data(modern, overwrite = TRUE)

# fossil
fossil <- calc_taxon_prop(con, "predict")   
  
fossil <- age_finder(tidyr::drop_na(fossil, -age_ma))

# save data
usethis::use_data(fossil, overwrite = TRUE)

# disconnect SQL
DBI::dbDisconnect(con)
