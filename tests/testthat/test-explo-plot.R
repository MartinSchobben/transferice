test_that("multiplication works", {
  
  # path
  dbpath <- fs::path_package(package = "transferice", "extdata", 
                             "transferice.sqlite")
  
  # connection
  con <- DBI::dbConnect(drv = RSQLite::SQLite(),  dbname = dbpath)
  
  # taxon
  taxon <- calc_taxon_prop(con)
  
  ggtaxon(taxon, "Brigantedinium spp.", "sample_id")
})

test_that("dimensions ca be reduced", {
  
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()
  
  # dinos
  dbpath <- fs::path_package(package = "transferice", "extdata", 
                             "transferice.sqlite")
  con <- DBI::dbConnect(drv = RSQLite::SQLite(),  dbname = dbpath)
  dino_prop <- calc_taxon_prop(con) 
  
  # variables
  vars <- role_organizer(dino_prop, NULL)
  
  # taxa
  txa <- vars[names(vars) == "predictor"] |> unname()
  
  # variables not used in transform
  terms <- vars[names(vars) != "predictor"]
  
  # re-scale (logit from recipes)
  taxon_prep <- recipes::recipe(x = dino_prop , vars = vars, roles = names(vars)) |>
    recipes::step_logit(dplyr::any_of(txa) , offset = 0.025) |>
    recipes::prep() |>
    recipes::bake(new_data = NULL)
  
  environ <- oceanexplorer::get_NOAA("temperature", 1, "annual")
  crd <- dplyr::select(dino_prop, longitude, latitude)
  # cast location as list before extraction
  pts <- setNames(as.list(crd), nm = c("lon", "lat"))
  pts <- oceanexplorer::filter_NOAA(environ, depth = 30, coord = pts)
  # drop geometry as we will use it for ...
  parm <- sf::st_drop_geometry(pts) |> select(-depth)
  train <- dplyr::bind_cols(dino_prop, parm)
  
  meta <- c("sample_id", "hole_id", "sample_depth_mbsf", "site_hole", "source_citation", "longitude", "latitude")
  # apply function and compare to reference plot
  vdiffr::expect_doppelganger(
    "pca plot of taxon overlain with environmental parameter",
    reduce_dims(train, var = "t_an", id = meta, loc = "hole_id")
  )
  on.exit(DBI::dbDisconnect(con))  
})
