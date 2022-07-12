test_that("multiplication works", {
  
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
  
  print_model(splt, wfl, pred = "PC1", tune = 1, out = "t_an")

  print_model(tuned_cv, wfl,  pred = "PC1", tune = 1, out = "t_an")
  
  print_model(final, wfl)
})
