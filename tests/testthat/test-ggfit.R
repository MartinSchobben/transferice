test_that("final model output can be plotted with tuning", {
  
  set.seed(1)
  # resample
  splt <- rsample::initial_split(dinodat, prop = 0.75) 
  
  # model
  mdl <- parsnip::linear_reg() |>
    parsnip::set_engine('lm') |>
    parsnip::set_mode('regression')
  
  # recipe
  rcp <- transferice_recipe(dinodat, trans = "logit", dim_reduction = "PCA")
  
  # workflow
  wfl <- workflows::workflow() |>
    workflows::add_recipe(rcp) |>
    workflows::add_model(mdl)

  # final fit
  final <- transferice_finalize(splt, wfl, 3)
  
  # r squared plot
  ggpartial(final, tune = 1, out = t_an, preprocessor = "logit")
  
  # map projection
  base <- oceanexplorer::get_NOAA("phosphate", 1, "annual") |>
    oceanexplorer::filter_NOAA(depth = 0) |>
    stars::st_warp(crs = 4326) |>
    stars::st_downsample(n = 5)

  ggfit(final, abbreviate_vars(parms), selected = "p", type = "spatial", base_map = base)
})

test_that("final model output can be plotted without tuning", {
  
  set.seed(1)
  # resample
  splt <- rsample::initial_split(dinodat, prop = 0.75) 
  
  # model
  mdl <- parsnip::linear_reg() |>
    parsnip::set_engine('lm') |>
    parsnip::set_mode('regression')
  
  # recipe
  rcp <- transferice_recipe(dinodat, trans = "logit")
  
  # workflow
  wfl <- workflows::workflow() |>
    workflows::add_recipe(rcp) |>
    workflows::add_model(mdl)
  
  # final fit
  final <- transferice_finalize(splt, wfl)
  
  # r squared plot
  ggpartial(final, pred = "1", out = t_an, preprocessor = "logit")
  
  # map projection
  base <- oceanexplorer::get_NOAA("phosphate", 1, "annual") |>
    oceanexplorer::filter_NOAA(depth = 0) |>
    stars::st_warp(crs = 4326) |>
    stars::st_downsample(n = 5)
  
  ggfit(final, abbreviate_vars(parms), selected = "p", type = "spatial", base_map = base)
})

test_that("variable check with tuning", {
  
  set.seed(1)
  # resample
  splt <- rsample::initial_split(dinodat, prop = 0.75) 
  
  # model
  mdl <- parsnip::linear_reg() |>
    parsnip::set_engine('lm') |>
    parsnip::set_mode('regression')
  
  # recipe
  rcp <- transferice_recipe(dinodat, trans = "logit", dim_reduction = "PCA")
  
  # workflow
  wfl <- workflows::workflow() |>
    workflows::add_recipe(rcp) |>
    workflows::add_model(mdl)
  
  # final fit
  final <- transferice_finalize(splt, wfl, 3)
  
  pred_check(final, NULL, 1)
  
})
