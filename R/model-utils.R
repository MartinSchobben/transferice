transferice_recipe <- function(dat, trans = NULL, dim_reduction = NULL, 
                               tunable = TRUE) {

  # parameter names
  pms <- paste0(abbreviate_vars(parms), "_an", collapse = "+")
  
  # taxa names
  rm <- c(
    paste0(abbreviate_vars(parms), "_an"), 
    "hole_id", 
    "sample_id", 
    "longitude",
    "latitude"
  )
  tns <- paste0(paste0("`", names(dat)[!names(dat) %in% rm], "`"), collapse = "+")
 
  # formula
  fml <- as.formula(paste0(pms, "~", tns))
  # recipe
  rcp <- recipes::recipe(fml, data = dat)
  
  # transforming 
  if (isTruthy(trans)) {
    rcp <- purrr::reduce(trans, transform_predictor, .init = rcp)
  }
  
  # dimensions reduction (PCA)
  if (isTruthy(dim_reduction) && dim_reduction == "PCA") {
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
  
  rcp
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

transferice_finalize <- function(split, wfl, dial = NULL) {
  
  # finalize workflow by selecting optimal sub-model (if tuned)
  if (isTruthy(dial)) {
    wfl <- tune::finalize_workflow(
      wfl,
      tibble::tibble(num_comp = dial)
    )
  }
  
  # metrics to return
  mts <- yardstick::metric_set(
    transferice::rmsre,
    yardstick::rmse,
    yardstick::rsq
  )
  
  # fit the final model (does not need to be tuned)
  tune::last_fit(wfl, split = split, metrics = mts)
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
    logit =  step_logit_center(rcp),
    log = step_log_center(rcp),
    normalize = step_nz_normalize(rcp) 
  )
}

# remove near zero varianve before normalizing
step_nz_normalize <- function(rcp) {
  recipes::step_nzv(rcp, recipes::all_predictors()) |> 
    recipes::step_normalize(recipes::all_predictors())

}

step_logit_center <- function(rcp) {
  recipes::step_logit(rcp, recipes::all_predictors(), offset = 0.025) |>
    recipes::step_center(recipes::all_predictors())
}

step_log_center <- function(rcp) {
  recipes::step_log(rcp, recipes::all_predictors(), offset = 0.025) |>
    recipes::step_center(recipes::all_predictors())
}

# control resample of model fit
ctrl <- tune::control_resamples(extract = function(x) tune::extract_fit_parsnip(x))