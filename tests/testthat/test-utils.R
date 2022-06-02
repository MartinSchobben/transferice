test_that("rows add to 1", {
  dbpath <- fs::path_package(package = "transferice", "extdata", 
                             "transferice.sqlite")
  con <- DBI::dbConnect(drv = RSQLite::SQLite(),  dbname = dbpath)
  # check if samples add to 1
  chk <- function(x) {sum(x) / length(x)}
  expect_equal(
    calc_taxon_prop("neptune_sample_taxa", "neptune_sample", con)  |> 
      dplyr::rowwise() |> 
      dplyr::mutate(
        dino_tot = 
          sum(dplyr::c_across(-c(.data$sample_id, .data$hole_id)), na.rm = T),
        ) |> 
      dplyr::pull(.data$dino_tot) |> 
      chk()
      ,
    1
    )
  on.exit(DBI::dbDisconnect(con))
})

test_that("species can be named",{
  
  # connection
  dbpath <- fs::path_package(package = "transferice", "extdata", 
                             "transferice.sqlite")
  con <- DBI::dbConnect(drv = RSQLite::SQLite(),  dbname = dbpath)
  
  # get taxa names
  expect_snapshot(
    species_naming(con)
  )
  
  # get taxa names with id selection
  expect_snapshot(
    species_naming(con, c(83, 91))
  )
  
  # get ids
  tx <- c("Trinovantedinium pallidifulvum", "Polarella glacialis")
  expect_snapshot(
    species_naming(con, parms, taxa_name = tx)
  )
  
  on.exit(DBI::dbDisconnect(con))
})

test_that("workflow with no steps can be sanitized", {
  # resample
  splt <- rsample::initial_split(dinodat, prop = 0.75) 
  
  # recipe
  rcp <- transferice_recipe(dinodat)
  
  # model
  mdl <- parsnip::linear_reg() |>
    parsnip::set_engine('lm') |>
    parsnip::set_mode('regression')
  
  # workflow
  wfl <- workflows::workflow() |>
    workflows::add_recipe(rcp) |>
    workflows::add_model(mdl)
  
  sanitize_workflow(wfl)
  
})