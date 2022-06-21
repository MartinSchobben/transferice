test_that("partial model extract works", {
  
  set.seed(1)
  # resample
  splt <- rsample::initial_split(dinodat, prop = 0.75) 
  
  # recipes
  rcp <- transferice_recipe(dinodat, "t_an", trans = "logit", dim_reduction = "PCA")

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
  
  # extracts
  ext <- cv_extraction(tuned_cv)
  
  # parts fit
  expect_snapshot(
    ext
  )
  
  # partial fits
  out <- calc_partials(ext, PC1, t_an)
 
  expect_snapshot(out)
  
  # parts trained receipe
  expect_snapshot(
    cv_extraction(tuned_cv, "recipe")
  )
  
  # partials calcs
  calc_partials(ext, PC1, t_an)
  
})
