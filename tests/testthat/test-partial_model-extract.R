test_that("partial model extract works", {
  
  set.seed(1)
  # resample
  splt <- rsample::initial_split(dinodat, prop = 0.75) 
  
  # recipes
  rcp <- transferice_recipe(dinodat, trans = "logit")

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
  
  # parts
  expect_snapshot(
    cv_model_extraction(tuned_cv)
  )
})
