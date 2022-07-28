test_that("partial regressions can be plotted with tuning", {
  
  # inspect feature engineering
  vdiffr::expect_doppelganger(
    "feature engineering",
    ggpartial(transferice:::modern_split, transferice:::modern_workflow,  
              out = "t_an", pred = "PC1", tune = 1, id = "test")
  )

  # what happens if `pred` is supplied with a tuned recipe?
  expect_error(
    ggpartial(transferice:::modern_split, transferice:::modern_workflow, 
              out = "t_an", pred = "PC1", id = "test"),
    "The model has been tuned and therefore `tune` needs to be supplied!"
  )
  
  # partial regressions
  vdiffr::expect_doppelganger(
    "partial regression",
    ggpartial(transferice:::tuned_workflow, transferice:::modern_workflow,  
              out = "t_an", pred = "PC1", tune = 1, plot_type = "static")
  )
  
  # final fit
  vdiffr::expect_doppelganger(
    "validation final fit regression",
    ggpartial(transferice:::validation_modern, transferice:::modern_workflow, 
              out = "t_an")
  )
  
  # bubble
  vdiffr::expect_doppelganger(
    "validation final fit regression with bubbles",
    ggpartial(transferice:::validation_modern, transferice:::modern_workflow, 
              out = "t_an", type = "bubble")
  )
  
})
  
# test_that("partial regressions can be plotted without tuning", {
#   
# 
# })

test_that("predictor arguments are supplied (not tuned)", {
  
  # all variables and their roles
  vars <- role_organizer(transferice::modern, "t_an")
  
  # taxa
  txa <- vars[names(vars) == "predictor"] |> unname()
  
  # preprocess recipe
  dt <- recipes::recipe(x = transferice::modern, vars = vars, roles = names(vars)) |>
    # remove the very rare taxa
    recipes::step_nzv(dplyr::any_of(txa)) |> 
    # prep the data
    recipes::prep(training = transferice::modern) |> 
    recipes::bake(NULL)
  
  # resample
  splt <- rsample::initial_split(dt, prop = 0.75) 
  
  # recipe
  rcp <- transferice_recipe(dt, "t_an", trans = "log")
  
  expect_error(
    pred_check(rcp, NULL, NULL),
    "`pred` needs to be supplied!"
  )
  
  expect_equal(
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
  
  expect_equal(
    pred_check(fitted_cv, "taxa_1", NULL),
    rlang::sym("taxa_1")
  )
  
  expect_error(
    pred_check(fitted_cv, NULL, 1),
    "`pred` needs to be supplied!"
  )
  
  expect_error(
    pred_check(final, NULL, 1),
    "`pred` needs to be supplied!"
  )
  
})
  
test_that("predictor arguments are supplied (tuned)", { 

  expect_error(
    pred_check(tuned_recipe, "taxa_1", NULL),
    "The model has been tuned and therefore `tune` needs to be supplied!"
  )
  
  expect_equal(
    pred_check(tuned_recipe, "PC1", 1),
    rlang::sym("PC1")
  )
  
  expect_equal(
    pred_check(tuned_workflow, "PC1", 1),
    rlang::sym("PC1")
  )
  
  expect_error(
    pred_check(tuned_workflow, "PC1", NULL),
    "The model has been tuned and therefore `tune` needs to be supplied!"
  )
  
  expect_equal(
    pred_check(validation_modern, "PC1", 1),
    "6PCs"
  )
})
