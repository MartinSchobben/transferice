test_that("multiplication works", {
  
  # path
  dbpath <- fs::path_package(package = "transferice", "extdata", 
                             "transferice.sqlite")
  
  # connection
  con <- DBI::dbConnect(drv = RSQLite::SQLite(),  dbname = dbpath)
  
  # taxon
  taxon <- calc_taxon_prop(con, "train")
  
  # filter the dataset
  taxon_loc <- dplyr::distinct( taxon, .data$sample_id, .keep_all = TRUE) |> 
    dplyr::mutate(
      # odds  
      odds = .data[["Xandarodinium xanthum"]] / (1 - .data[["Xandarodinium xanthum"]]),
      # find highest n ranks
      rank = rank(.data$odds)
    ) |> 
    dplyr::filter(.data$rank >= dplyr::n() - 20) |> 
    tidyr::drop_na(-age_ma)
  
  # plot
  vdiffr::expect_doppelganger(
    "taxon plot", 
    ggtaxon(taxon_loc, "Xandarodinium xanthum", "site_hole")
  )
  
  on.exit(DBI::dbDisconnect(con))  
})

test_that("dimensions ca be reduced", {
  
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
  parm <- sf::st_drop_geometry(pts) |> dplyr::select(-depth)
  train <- dplyr::bind_cols(dino_prop, parm) |> tidyr::drop_na(-age_ma)
  
  # apply function and compare to reference plot
  vdiffr::expect_doppelganger(
    "pca plot of taxon overlain with environmental parameter",
    ggcompare(train, var = "t_an", id = meta)
  )
  on.exit(DBI::dbDisconnect(con))  
})
