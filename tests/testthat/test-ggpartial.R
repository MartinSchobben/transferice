test_that("partial regressions can be plotted with tuning", {
  
  set.seed(1)
  # resample
  splt <- rsample::initial_split(dinodat, prop = 0.75) 
  
  # recipe
  rcp <- transferice_recipe(dinodat, trans = "logit", dim_reduction = "PCA")
  
  # inspect feature engineering
  vdiffr::expect_doppelganger(
    "feature engineering",
    ggpartial(splt, rcp, tune = 1, out = t_an, preprocessor = "logit")
  )
  
  # what happens if pred is supplied with a tuned recipe?
  ggpartial(splt, rcp, pred = "33", out = t_an, preprocessor = "logit") 
  
  
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
  
  # partial regressions
  vdiffr::expect_doppelganger(
    "partial regression",
    ggpartial(tuned_cv, tune = 1, out = t_an, plot_type = "static")
  )
  
  # partial map projection
  base <- oceanexplorer::get_NOAA("nitrate", 1, "annual") |> 
    oceanexplorer::filter_NOAA(depth = 0) |> 
    stars::st_warp(crs = 4326) |> 
    stars::st_downsample(n = 5)
  
  vdiffr::expect_doppelganger(
    "partial spatial",
    ggpartial(tuned_cv, tune = 1, out = n_an, type = "spatial", base_map = base, 
              plot_type = "static")
  )
})
  
test_that("partial regressions can be plotted without tuning", {
  
  set.seed(1)
  # resample
  splt <- rsample::initial_split(dinodat, prop = 0.75) 
  
  # recipe
  rcp <- transferice_recipe(dinodat, trans = "log")
  
  # inspect feature engineering
  vdiffr::expect_doppelganger(
    "feature engineering",
     ggpartial(splt, rcp, pred = "44", out = t_an, preprocessor = "log")
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
  set.seed(2)
  fitted_cv <- transferice_tuning(splt, wfl)

  # partial regressions
  vdiffr::expect_doppelganger(
    "partial regression",
    ggpartial(fitted_cv, pred = "44", out = t_an, plot_type = "static", preprocessor = "log")
  )
  
  # create error by supplying tune
  ggpartial(fitted_cv, tune = 1, pred = "44", out = t_an, plot_type = "static", preprocessor = "log")
  
  # partial map projection
  base <- oceanexplorer::get_NOAA("nitrate", 1, "annual") |> 
    oceanexplorer::filter_NOAA(depth = 0) |> 
    stars::st_warp(crs = 4326) |> 
    stars::st_downsample(n = 5)
  
  vdiffr::expect_doppelganger(
    "partial spatial",
    ggpartial(fitted_cv, pred = "44", out = n_an, type = "spatial", base_map = base, 
              plot_type = "static", preprocessor = "log")
  )
})

