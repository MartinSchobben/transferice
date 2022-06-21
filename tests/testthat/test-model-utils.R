test_that("transferice workflows and model tuning works", {
  
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
  
  # recipes
  rcp <- transferice_recipe(dinodat, "t_an", trans = "logit", dim_reduction = "PCA", model = "gls")
  
  # model
  gls_spec <- parsnip::linear_reg() |> 
    parsnip::set_engine(
      "gls",
      control = nlme::lmeControl(opt = 'optim')
    )  |> 
    # usage of the model for regression
    parsnip::set_mode('regression')
  
  gls_wfl <- workflows::update_model(wfl, gls_spec)
  gls_wfl <- workflows::update_recipe(wfl, rcp)
  
  transferice_tuning(splt, gls_wfl)
})
