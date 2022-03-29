transferice_workflow <- function(dat, model, trans = "none", 
                                 dim_reduction = "none", tunable = TRUE) {

  # parameter names
  pms <- paste0(abbreviate_vars(parms), "_an", collapse = "+")
  # dinocysts ids
  rm <- c(
    paste0(abbreviate_vars(parms), "_an"), 
    "hole_id", 
    "sample_id", 
    "longitude",
    "latitude"
  )
  dns <- paste0(paste0("`", names(dat)[!names(dat) %in% rm], "`"), collapse = "+")
 
  # formula
  fml <- as.formula(paste0(pms, "~", dns))
  # recipe
  rcp <- recipes::recipe(fml, data = dat)
  
  # transforming 
  rcp <- purrr::reduce(trans, transform_predictor, .init = rcp)
  
  # dimensions reduction (PCA)
  if (dim_reduction == "PCA") {
    if (isTRUE(tunable)) {
      rcp <- recipes::step_pca(
        rcp,
        recipes::all_predictors(), 
        num_comp = tune::tune()
        )
    } else {
      rcp <- recipes::step_pca(
        rcp,
        recipes::all_predictors()
      )
    }
  }
  
  # workflow
  workflows::workflow() |>
    workflows::add_recipe(rcp) |>
    workflows::add_model(model)
}

transferice_tuning <- function(split, wfl) {
  
  # cross validation resampling
  dat_cv <- rsample::vfold_cv(rsample::training(split), v = 10)
  
  if(nrow(hardhat::extract_parameter_set_dials(wfl)) == 0) {
    
    tune::fit_resamples(
      wfl, 
      resamples = dat_cv, 
      metrics = yardstick::metric_set(transferice::rmsre), 
      control = ctrl
    )
    
  } else {

    # setting model tuning parameters
    dls <- wfl %>%
      dials::parameters() %>%
      # set PCA steps to 9 components total
      update(num_comp = dials::num_comp(c(1, 9))) 
    
    # tuning grid
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
}

# memoise function
transferice_tuning_mem <- memoise::memoise(
  transferice_tuning, 
  cache = cachem::cache_disk(fs::path_package(package = "transferice", "appdir"))
  )

# print model metric
print_metric <- function(dat, metric, sig = 1) {
  x <- dat[dat[[".metric"]] == metric,".estimate", drop  = TRUE]
  mts <- c(rsq = "$r^{2}$", rmse = "RMSE", rmsre = "RMSRE")
  spr <- paste0(mts[metric], " = %0.",sig, "f")
  sprintf(spr, x)
}

# transform
transform_predictor <- function(rcp, trans) {
  switch(
    trans,
    logit = recipes::step_logit(rcp, recipes::all_predictors(), offset = 0.025),
    center = recipes::step_center(rcp, recipes::all_predictors())
  )
}

# control resample of model fit
ctrl <- tune::control_resamples(extract = function(x) tune::extract_fit_parsnip(x))