test_that("partial regressions can be plotted with tuning", {
  
  # partial map projection
  base <- oceanexplorer::get_NOAA("nitrate", 1, "annual") |> 
    oceanexplorer::filter_NOAA(depth = 0) |> 
    stars::st_warp(crs = 4326) |> 
    stars::st_downsample(n = 5)
  
  # resample
  set.seed(1)
  splt <- rsample::initial_split(dinodat, prop = 0.75) 
  
  # recipe
  rcp <- transferice_recipe(dinodat, trans = "logit", dim_reduction = "PCA")
  
  # model
  mdl <- parsnip::linear_reg() |>
    parsnip::set_engine('lm') |>
    parsnip::set_mode('regression')
  
  # workflow
  wfl <- workflows::workflow() |>
    workflows::add_recipe(rcp) |>
    workflows::add_model(mdl)
  
  # tuning
  set.seed(2)
  tuned_cv <- transferice_tuning(splt, wfl)
  
  
  # final fit
  final <- transferice_finalize(splt, wfl, 3)
  
  
  # inspect feature engineering
  vdiffr::expect_doppelganger(
    "feature engineering",
    ggpartial(splt, wfl, tune = 1, out = t_an)
  )
  
  # inspect feature engineering (on map)
  vdiffr::expect_doppelganger(
    "feature engineering",
    ggpartial(splt, wfl, tune = 1, out = n_an, type = "spatial", base_map = base)
  )
  
  # what happens if `pred` is supplied with a tuned recipe?
  expect_error(
    ggpartial(splt, wfl, pred = "33", out = t_an),
    "The model has been tuned and therefore `tune` needs to be supplied!"
  )
  
  # partial regressions
  vdiffr::expect_doppelganger(
    "partial regression",
    ggpartial(tuned_cv, wfl, tune = 1, out = t_an, plot_type = "static")
  )
  
  vdiffr::expect_doppelganger(
    "partial spatial",
    ggpartial(tuned_cv, wfl, tune = 1, out = n_an, type = "spatial", 
              base_map = base, plot_type = "static")
  )
  
  # final fit
  
  ggpartial(final, wfl, tune = 1, out = t_an)
  
  ggpartial(final, wfl, tune = 1, out = n_an, type = "spatial", base_map = base)
})
  
test_that("partial regressions can be plotted without tuning", {
  
  # partial map projection
  base <- oceanexplorer::get_NOAA("nitrate", 1, "annual") |> 
    oceanexplorer::filter_NOAA(depth = 0) |> 
    stars::st_warp(crs = 4326) |> 
    stars::st_downsample(n = 5)
  
  set.seed(1)
  # resample
  splt <- rsample::initial_split(dinodat, prop = 0.75) 
  
  # recipe
  rcp <- transferice_recipe(dinodat, trans = "log")
  
  # model
  mdl <- parsnip::linear_reg() |>
    parsnip::set_engine('lm') |>
    parsnip::set_mode('regression')
  
  # workflow
  wfl <- workflows::workflow() |>
    workflows::add_recipe(rcp) |>
    workflows::add_model(mdl)
  
  # fitting
  set.seed(2)
  fitted_cv <- transferice_tuning(splt, wfl)
  
  # final fit
  final <- transferice_finalize(splt, wfl)
  
  
  # inspect feature engineering
  vdiffr::expect_doppelganger(
    "feature engineering",
    ggpartial(splt, wfl, pred = "Pentapharsodinium dalei", out = t_an)
  )

  # partial regressions
  vdiffr::expect_doppelganger(
    "partial regression",
    ggpartial(fitted_cv, wfl, pred = "Pentapharsodinium dalei", out = t_an, 
              plot_type = "static")
  )
  
  # create error by supplying tune
  expect_error(
    ggpartial(fitted_cv, wfl, tune = 1, pred = NULL, out = t_an, plot_type = "static"),
    "The model has NOT been tuned and therefore `pred` needs to be supplied!"
  )

  vdiffr::expect_doppelganger(
    "partial spatial",
    ggpartial(fitted_cv, wfl, pred = "Pentapharsodinium dalei", out = n_an, 
              type = "spatial", base_map = base, plot_type = "static")
  )
  
  # r squared plot
  ggpartial(final, wfl, pred = "Pentapharsodinium dalei", out = t_an)

  ggpartial(final, wfl, pred = "Pentapharsodinium dalei", 
            out = t_an, type = "spatial", base_map = base)
})

test_that("predictor arguments are supplied (not tuned)", {
  
  expect_error(
    pred_check(rcp, NULL, NULL),
    "Either `pred` or `tune` needs to be supplied!"
  )

  # resample
  splt <- rsample::initial_split(dinodat, prop = 0.75) 
  
  # recipe
  rcp <- transferice_recipe(dinodat, trans = "log")
  
  expect_error(
    pred_check(rcp, "1", NULL),
    rlang::sym("1")
  )
  
  expect_error(
    pred_check(rcp, NULL, 1),
    "The model has NOT been tuned and therefore `pred` needs to be supplied!"
  )
  
  # model
  mdl <- parsnip::linear_reg() |>
    parsnip::set_engine('lm') |>
    parsnip::set_mode('regression')
  
  # workflow
  wfl <- workflows::workflow() |>
    workflows::add_recipe(rcp) |>
    workflows::add_model(mdl)
  
  # fitting
  fitted_cv <- transferice_tuning(splt, wfl)
  
  # final fit
  final <- transferice_finalize(splt, wfl, 3)
  
  expect_error(
    pred_check(fitted_cv, "1", NULL),
    rlang::sym("1")
  )
  
  expect_error(
    pred_check(fitted_cv, NULL, 1),
    "The model has NOT been tuned and therefore `pred` needs to be supplied!"
  )
  
  pred_check(final, NULL, 1)
  
  
})
  
  
test_that("predictor arguments are supplied (tuned)", { 
  
  # resample
  splt <- rsample::initial_split(dinodat, prop = 0.75) 
  
  # recipe
  rcp <- transferice_recipe(dinodat, trans = "logit", dim_reduction = "PCA")
  
  expect_error(
    pred_check(rcp, "1", NULL),
    "The model has been tuned and therefore `tune` needs to be supplied!"
  )
  
  expect_equal(
    pred_check(rcp, NULL, 1),
    rlang::sym("PC1")
  )
  
  # model
  mdl <- parsnip::linear_reg() |>
    parsnip::set_engine('lm') |>
    parsnip::set_mode('regression')
  
  # workflow
  wfl <- workflows::workflow() |>
    workflows::add_recipe(rcp) |>
    workflows::add_model(mdl)
  
  # fitting
  tuned_cv <- transferice_tuning(splt, wfl)
  
  expect_error(
    pred_check(tuned_cv, NULL, 1),
    rlang::sym("PC1")
  )
  
  expect_error(
    pred_check(tuned_cv, "!", NULL),
    "The model has NOT been tuned and therefore `pred` needs to be supplied!"
  )
})

