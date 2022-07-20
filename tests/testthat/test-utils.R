test_that("rows add to 1", {
  
  # path
  dbpath <- fs::path_package(package = "transferice", "extdata", 
                             "transferice.sqlite")
  
  # connection
  con <- DBI::dbConnect(drv = RSQLite::SQLite(),  dbname = dbpath)
  
  # check if samples add to 1
  chk <- function(x) {sum(x) / length(x)}
  expect_equal(
    calc_taxon_prop(con)  |> 
      dplyr::rowwise() |> 
      dplyr::mutate(
        dino_tot = 
          sum(dplyr::c_across(-c(sample_id, hole_id, sample_depth_mbsf, site_hole, source_citation, longitude, latitude)), na.rm = TRUE),
        ) |> 
      dplyr::pull(.data$dino_tot) |> 
      chk()
      ,
    1
    )
  on.exit(DBI::dbDisconnect(con))
})

test_that("species can be named",{
  
  # recipes
  rcp <- transferice_recipe(dinodat)

    # model
  mdl <- parsnip::linear_reg() |>
    parsnip::set_engine('lm') |>
    parsnip::set_mode('regression')
  
  # workflow
  wfl <- workflows::workflow() |>
    workflows::add_recipe(rcp) |>
    workflows::add_model(mdl)
  
  species_naming(wfl)
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