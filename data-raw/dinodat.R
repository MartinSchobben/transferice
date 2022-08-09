# ------------------------------------------------------------------------------
# SQL queries
# ------------------------------------------------------------------------------
dbpath <- fs::path_package(package = "transferice", "extdata", 
                           "transferice.sqlite")
con <- DBI::dbConnect(drv = RSQLite::SQLite(),  dbname = dbpath)

# calculate proportional dino counts
dino_prop <- calc_taxon_prop(con, "train") 

# environment
locs <- dplyr::select(dino_prop, longitude, latitude)
  
# ------------------------------------------------------------------------------
# environmental parameters
# ------------------------------------------------------------------------------

# get NOAA annually averaged data for parameters on a 1 degree grid 
dt  <- oceanexplorer::get_NOAA("temperature", 1, "annual")

# cast location as list before extraction
pts <- purrr::set_names(as.list(locs), nm = c("lon", "lat"))

# get locations parameters
parms <- oceanexplorer::filter_NOAA(dt, depth = 30,  coord = pts) |> 
  dplyr::select(-.data$depth) |> 
  sf::st_drop_geometry() 

# ------------------------------------------------------------------------------
# dinocysts
# ------------------------------------------------------------------------------
# combine
modern <- dplyr::bind_cols(dino_prop, parms) |> 
  tidyr::drop_na("t_an")  

# save data
usethis::use_data(modern, overwrite = TRUE)

# fossil
fossil <- calc_taxon_prop(con, "predict")   
  
fossil <- age_finder(tidyr::drop_na(fossil, -age_ma)) |> 
  tidyr::drop_na(age_ma)

# save data
usethis::use_data(fossil, overwrite = TRUE)

# disconnect SQL
DBI::dbDisconnect(con)
