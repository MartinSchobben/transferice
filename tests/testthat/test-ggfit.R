test_that("final model output can be plotted", {
  
  set.seed(1)
  # resample
  splt <- rsample::initial_split(dinodat, prop = 0.75) 
  
  # model
  mdl <- parsnip::linear_reg() |>
    parsnip::set_engine('lm') |>
    parsnip::set_mode('regression')
  
  # workflow
  wfl <- transferice_workflow(dinodat, mdl)
  
  # tuning
  set.seed(2)
  tuned_cv <- transferice_tuning(splt, wfl)
  
  xc <- cv_model_extraction(tuned_cv)
  
  final_wfl <- tune::finalize_workflow(
    wfl,
    tibble::tibble(num_comp = 2)
  )
  
  final <- tune::last_fit(
    final_wfl,
    split =  splt,
    metrics = 
      yardstick::metric_set(
        transferice::rmsre, 
        yardstick::rmse, 
        yardstick::rsq
      )
  )
  
  # r squared plot
  ggfit(final, abbreviate_vars(parms), selected = "t")
  
  # map projection
  base <- oceanexplorer::get_NOAA("temperature", 1, "annual") |>
    oceanexplorer::filter_NOAA(depth = 0) |>
    stars::st_warp(crs = 4326) |>
    stars::st_downsample(n = 5)

  ggfit(final, abbreviate_vars(parms), selected = "t", type = "spatial", base_map = base)
})

