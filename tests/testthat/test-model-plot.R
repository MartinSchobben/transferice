test_that("partial regressions can be plotted with tuning", {
  
  # partial map projection
  # base <- oceanexplorer::get_NOAA("temperature", 1, "annual") |> 
  #   oceanexplorer::filter_NOAA(depth = 0) |> 
  #   stars::st_warp(crs = 4326) |> 
  #   stars::st_downsample(n = 5)
  
  # resample
  set.seed(1)
  splt <- rsample::initial_split(dinodat, prop = 0.75, strata = "latitude") 
  
  # recipe
  rcp <- transferice_recipe(
    dinodat, 
    "t_an", 
    trans = "log", 
    dim_reduction = "PCA", 
    model = "gls"
  )
  
  # model
  # mdl <- parsnip::linear_reg() |>
  #   parsnip::set_engine('lm') |>
  #   parsnip::set_mode('regression')

  # model
  library(multilevelmod)
  mdl <- parsnip::linear_reg() |> 
    parsnip::set_engine(
      "gls",  
      control = nlme::lmeControl(opt = 'optim'),
      correlation = nlme::corSpatial(form = ~longitude|latitude, type = "g" , nugget = TRUE)
    )  |> 
    # usage of the model for regression
    parsnip::set_mode('regression')
  
  # workflow
  fx <- formula_parser(dinodat, "t_an", exclude = c("longitude", "latitude"))
  wfl <- workflows::workflow() |>
    workflows::add_recipe(rcp) |>
    workflows::add_model(mdl, formula = fx)
  
  # tuning
  set.seed(2)
  tuned_cv <- transferice_tuning(splt, wfl)
  
  # final fit
  final <- transferice_finalize(splt, wfl, 2)
  
  # inspect feature engineering
  vdiffr::expect_doppelganger(
    "feature engineering",
    ggpartial(splt, wfl,  out = "t_an", pred = "PC1", tune = 1)
  )
  
  # inspect feature engineering (on map)
  # vdiffr::expect_doppelganger(
  #   "feature engineering",
  #   ggpartial(splt, wfl, tune = 1, out = "t_an", type = "spatial", base_map = base)
  # )
  # 
  # what happens if `pred` is supplied with a tuned recipe?
  expect_error(
    ggpartial(splt, wfl, out = "t_an", pred = "PC1"),
    "The model has been tuned and therefore `tune` needs to be supplied!"
  )
  
  # partial regressions
  vdiffr::expect_doppelganger(
    "partial regression",
    ggpartial(tuned_cv, wfl,  out = "t_an", pred = "PC1", tune = 1, plot_type = "static")
  )
  
  # vdiffr::expect_doppelganger(
  #   "partial spatial",
  #   ggpartial(tuned_cv, wfl, tune = 1, out = "n_an", type = "spatial", 
  #             base_map = base, plot_type = "static", 
  #             id = "dinocyst_annual_global")
  # )
  
  # final fit
  vdiffr::expect_doppelganger(
    "final fit regression",
    ggpartial(final, wfl, out = "t_an")
  )
  
  # bubble
  vdiffr::expect_doppelganger(
    "final fit regression",
    ggpartial(final, wfl, out = "t_an", type = "bubble")
  )
  
  # vdiffr::expect_doppelganger(
  #   "final fit spatial",
  #   ggpartial(final, wfl, out = "t_an", type = "spatial", 
  #             base_map = base)
  # )
})
  
test_that("partial regressions can be plotted without tuning", {
  
  # partial map projection
  # base <- oceanexplorer::get_NOAA("temperature", 1, "annual") |> 
  #   oceanexplorer::filter_NOAA(depth = 0) |> 
  #   stars::st_warp(crs = 4326) |> 
  #   stars::st_downsample(n = 5)
  
  set.seed(1)
  # resample
  splt <- rsample::initial_split(dinodat, prop = 0.75, strata = "latitude") 
  
  # recipe
  rcp <- transferice_recipe(dinodat, "t_an", trans = "log")
  
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
    ggpartial(splt, wfl, out = 't_an', pred = "taxa_1")
  )

  # partial regressions
  vdiffr::expect_doppelganger(
    "partial regression",
    ggpartial(fitted_cv, wfl, out = 't_an', pred = "taxa_1", plot_type = "static")
  )
  
  # create error by supplying tune
  expect_error(
    ggpartial(fitted_cv, wfl, out = "t_an", pred = NULL, plot_type = "static"),
    "`pred` needs to be supplied!"
  )

  # vdiffr::expect_doppelganger(
  #   "partial spatial",
  #   ggpartial(fitted_cv, wfl, pred = "taxa_3", out = "n_an", 
  #             type = "spatial", base_map = base, plot_type = "static")
  # )
  
  # r squared plot
  ggpartial(final, wfl, out = "t_an", pred = "taxa_2")
  
  # bubbles
  ggpartial(final, wfl, out = "t_an", pred = "taxa_2", type = "bubble")

})

test_that("predictor arguments are supplied (not tuned)", {
  
  expect_error(
    pred_check(rcp, NULL, NULL),
    "`pred` needs to be supplied!"
  )

  # resample
  splt <- rsample::initial_split(dinodat, prop = 0.75) 
  
  # recipe
  rcp <- transferice_recipe(dinodat, "t_an", trans = "log")
  
  expect_error(
    pred_check(rcp, "taxa_1", NULL),
    rlang::sym("taxa_1")
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
    pred_check(fitted_cv, "taxa_1", NULL),
    rlang::sym("taxa_1")
  )
  
  expect_error(
    pred_check(fitted_cv, NULL, 1),
    "`pred` needs to be supplied!"
  )
  
  pred_check(final, NULL, 1)
  
  
})
  
  
test_that("predictor arguments are supplied (tuned)", { 
  
  # resample
  splt <- rsample::initial_split(dinodat, prop = 0.75) 
  
  # recipe
  rcp <- transferice_recipe(dinodat, "t_an", trans = "logit", dim_reduction = "PCA")
  
  expect_error(
    pred_check(rcp, "taxa_1", NULL),
    "The model has been tuned and therefore `tune` needs to be supplied!"
  )
  
  expect_equal(
    pred_check(rcp, "PC1", 1),
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
  
  # final fit
  final <- transferice_finalize(splt, wfl, 3)
  
  expect_equal(
    pred_check(tuned_cv, "PC1", 1),
    rlang::sym("PC1")
  )
  
  expect_error(
    pred_check(tuned_cv, "PC1", NULL),
    "The model has been tuned and therefore `tune` needs to be supplied!"
  )
  
  expect_equal(
    pred_check(final, "PC1", 1),
    "3PCs"
  )
})

