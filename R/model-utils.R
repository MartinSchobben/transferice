transferice_recipe <- function(dat, tunable = TRUE) {

  # parameter names
  pms <- paste0(abbreviate_vars(parms), "_an", collapse = "+")
  # dinocysts ids
  rm <- c(paste0(abbreviate_vars(parms), "_an"), "hole_id", "sample_id", "longitude", "latitude")
  dns <- paste0(paste0("`", names(dat)[!names(dinodat) %in% rm], "`"), collapse = "+")
 
  # formula
  fml <- as.formula(paste0(pms, "~", dns))
  # recipe
  rcp <- recipes::recipe(fml, data = dat)
  
  # scaling
  rcp <- recipes::step_logit(rcp, recipes::all_predictors(), offset = 0.025) 
  # dimensions reduction (PCA)
  if (isTRUE(tunable)) {
    recipes::step_pca(
      rcp,
      recipes::all_predictors(), 
      num_comp = tune::tune(), 
      options = list(center = TRUE)
      )
  } else {
    recipes::step_pca(
      rcp,
      recipes::all_predictors(), 
      options = list(center = TRUE)
    )
  }
}

transferice_tuning <- function(split, recipe, model) {
  
  # workflow
  wfl <- workflows::workflow() |>
    workflows::add_recipe(recipe) |>
    workflows::add_model(model)
  
  # cross validation resampling
  dat_cv <- rsample::vfold_cv(rsample::training(split), v = 10)

  # setting model tuning parameters
  dls <- wfl %>%
    dials::parameters() %>%
    update(num_comp = dials::num_comp(c(1, 9))) # set PCA steps to 9 components total
  # message(glue::glue("{hardhat::extract_parameter_dials(dls, 'num_comp')}"))
  tune_grid <- dials::grid_regular(dls, levels = 9)
  # tuning
  tune::tune_grid(
    wfl,
    resamples = dat_cv,
    grid = tune_grid,
    metrics = yardstick::metric_set(transferice::rmsre), # custom metric
    control = ctrl
    )
}

# memoise function
transferice_tuning_mem <- memoise::memoise(
  transferice_tuning, 
  cache = cachem::cache_disk(fs::path_package(package = "transferice", "appdir"))
  )

# control resample of model fit
ctrl <- tune::control_resamples(extract = function(x) tune::extract_fit_parsnip(x))