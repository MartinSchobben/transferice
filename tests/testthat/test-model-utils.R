test_that("transferice workflows and model tuning works", {
  
  set.seed(1)
  # resample
  splt <- rsample::initial_split(dinodat, prop = 0.75) 
  
  # recipes
  rcp <- transferice_recipe(dinodat)
  rcp <- transferice_recipe(dinodat, trans = character(0), dim_reduction = character(0))
  rcp <- transferice_recipe(dinodat, trans = character(1), dim_reduction = character(1))
  rcp <- transferice_recipe(dinodat, trans = "logit")
  rcp <- transferice_recipe(dinodat, trans = "normalize")
  
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

})
