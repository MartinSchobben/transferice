test_that("multiplication works", {
  
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
  
  print_model(splt, wfl, tune = 1, out = t_an)

})
