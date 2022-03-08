test_that("partial can plot", {
  
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
  prms<- paste0(transferice:::abbreviate_vars(parms), "_an")
  
  # partial regressions
  vdiffr::expect_doppelganger(
    "partial rgression",
    ggpartial(
      xc$.input[[1]],
      xc$.output[[1]],
      dplyr::select(dinodat, dplyr::any_of(prms)),  
      PC1, 
      t_an
    )
  )
  
  })
  
test_that("ranges of plots stay the same for different splits", {
  
  library(rsample)
  library(recipes)
  library(parsnip)
  
  dinodat_split <- initial_split(dinodat, prop = 0.75) 

  pms <- paste0(transferice:::abbreviate_vars(parms), "_an", collapse = "+")
  fml <- as.formula(paste0(pms, "~."))
  dinodat_recipe <- recipe(fml, data = dinodat) |> 
    step_logit(all_predictors(), offset = 0.025) |> 
    step_pca(all_predictors(), options = list(center =  TRUE))
  
  dinodat_train_prep <- prep(dinodat_recipe, training = training(dinodat_split)) |> 
    bake(new_data = NULL)
  
  dinodat_test_prep <- prep(dinodat_recipe, training = training(dinodat_split)) |> 
    bake(new_data = testing(dinodat_split))
  
  lm_model <- linear_reg() %>%  
    set_engine('lm') %>%  
    set_mode('regression')
  
  sl_parms <- dinodat_test_prep |> select(-contains("PC")) |>  names() 
  sl_parms <- paste0(transferice:::abbreviate_vars(sl_parms), "_an")
  pms2 <- paste0(sl_parms, collapse = ",")
  fml2 <- as.formula(paste0("cbind(", pms2, ")~."))
  
  lm_fit <- lm_model %>%   
    fit(fml2,  data = dinodat_train_prep)
  
  # partial regressions
  gg <- ggpartial(calc_partials(lm_fit), predict(lm_fit, calc_partials(lm_fit)),  dplyr::select(dinodat, dplyr::any_of(sl_parms)),  PC1, t_an)
  
  y_rng1 <- ggplot2::ggplot_build(gg)$layout$panel_params[[1]]$y.range
  x_rng1 <- ggplot2::ggplot_build(gg)$layout$panel_params[[1]]$x.range
  
  dinodat_split <- initial_split(dinodat, prop = 0.75) 
  
  pms <- paste0(transferice:::abbreviate_vars(parms), "_an", collapse = "+")
  fml <- as.formula(paste0(pms, "~."))
  dinodat_recipe <- recipe(fml, data = dinodat) |> 
    step_logit(all_predictors(), offset = 0.025) |> 
    step_pca(all_predictors(), options = list(center =  TRUE))
  
  dinodat_train_prep <- prep(dinodat_recipe, training = training(dinodat_split)) |> 
    bake(new_data = NULL)
  
  dinodat_test_prep <- prep(dinodat_recipe, training = training(dinodat_split)) |> 
    bake(new_data = testing(dinodat_split))
  
  lm_model <- linear_reg() %>%  
    set_engine('lm') %>%  
    set_mode('regression')
  
  sl_parms <- dinodat_test_prep |> select(-contains("PC")) |>  names() 
  sl_parms <- paste0(transferice:::abbreviate_vars(sl_parms), "_an")
  pms2 <- paste0(sl_parms, collapse = ",")
  fml2 <- as.formula(paste0("cbind(", pms2, ")~."))
  
  lm_fit <- lm_model %>%   
    fit(fml2,  data = dinodat_train_prep)
  
  # partial regressions
  gg <- ggpartial(calc_partials(lm_fit), predict(lm_fit, calc_partials(lm_fit)),  dplyr::select(dinodat, dplyr::any_of(sl_parms)),  PC1, t_an)
  y_rng2 <- ggplot2::ggplot_build(gg)$layout$panel_params[[1]]$y.range
  x_rng2 <- ggplot2::ggplot_build(gg)$layout$panel_params[[1]]$x.range
  
  # check that ranges remained the same
  expect_equal(x_rng1, x_rng2)
  expect_equal(y_rng1, y_rng2)
  
})
