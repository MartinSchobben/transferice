test_that("transferice workflows and model tuning works", {
  
  set.seed(1)
  # resample
  splt <- rsample::initial_split(dinodat, prop = 0.75) 
  
  # model
  mdl <- parsnip::linear_reg() |>
    parsnip::set_engine('lm') |>
    parsnip::set_mode('regression')
  
  # workflow
  wfl <- transferice_workflow(dinodat, mdl)
  wfl2 <- transferice_workflow(dinodat, mdl, trans = c("logit", "center"))
  
  # tuning
  set.seed(2)
  tuned_cv <- transferice_tuning(splt, wfl)
  
  xc <- cv_model_extraction(tuned_cv)
})
