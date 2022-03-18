test_that("partial regressions can be plotted", {
  
  set.seed(1)
  # resample
  splt <- rsample::initial_split(dinodat, prop = 0.75) 
  # recipe
  rcp <- transferice_recipe(dinodat)
  # model
  mdl <- parsnip::linear_reg() |>
    parsnip::set_engine('lm') |>
    parsnip::set_mode('regression')
  # tuning
  set.seed(2)
  tuned_cv <- transferice_tuning(splt, rcp, mdl)
  
  xc <- cv_model_extraction(tuned_cv)
  
  # partial regressions
  vdiffr::expect_doppelganger(
    "partial regression",
    ggpartial(xc, tune = 1, pred = t_an)
  )
  # partial map projection
  base <- oceanexplorer::get_NOAA("nitrate", 1, "annual") |> 
    oceanexplorer::filter_NOAA(depth = 0) |> 
    stars::st_warp(crs = 4326) |> 
    stars::st_downsample(n = 5)
  
  vdiffr::expect_doppelganger(
    "partial spatial",
    ggpartial(xc, tune = 1, pred = n_an, type = "spatial", base_map = base)
  )
})
  
