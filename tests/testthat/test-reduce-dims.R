test_that("dimensions ca be reduced", {
   
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()
  
  # dinos
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
  
  dino_prep <- recipes::recipe(dino_prop) |> 
    recipes::step_logit(offset = 0.025) |> 
    recipes::prep() |> 
    recipes::bake(new_data = NULL)
  
  environ <- oceanexplorer::get_NOAA( "temperature", 1, "annual")
  crd <- locs[, !names(locs) %in%  c("hole_id", "site", "sample_id"), drop = FALSE]
  # cast location as list before extraction
  pts <- setNames(as.list(crd), nm = c("lon", "lat"))
  pts <- oceanexplorer::filter_NOAA(environ, depth = 30, coord = pts)
  # drop geometry as we will use it for ...
  parm <- sf::st_drop_geometry(pts)
  environ_dat <- dplyr::bind_cols(locs, parm)
  # apply function and compare to reference plot
  vdiffr::expect_doppelganger(
    "pca plot of taxon overlain with environmental parameter",
    reduce_dims(dino_prep, environ_dat, var = "t_an", 
                id = c("sample_id", "hole_id"), loc = "hole_id")
  )
  on.exit(DBI::dbDisconnect(con))  
})
