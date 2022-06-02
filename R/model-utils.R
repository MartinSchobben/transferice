# dat = dataset
# parms = oceanographic parameters
# averaging = averaging period
# remove = variables not used in regression
formula_parser <- function(
    dat, 
    parms, 
    averaging = "an",
    remove = c("site", "sample_id", "longitude", "latitude", "hole_id", "depth"),
    type = "ordinary"
  ) {
  
  averaging <- paste0("_", averaging)

  if (type ==  "ordinary") {
    
    # parameter names
    pms <- paste0(transferice:::abbreviate_vars(parms), averaging, collapse = "+")
    
    # taxa names
    rm <- c(paste0(transferice:::abbreviate_vars(parms), averaging), remove)
    dns <- paste0(paste0("`", names(dat)[!names(dat) %in% rm], "`"), 
                  collapse = "+")
    
    # formula
    as.formula(paste0(pms, "~", dns))
    
  } else if (type == "tidymodels") {
    
    # parameter names
    pms <- paste0(transferice:::abbreviate_vars(parms), averaging, collapse = ",")

    # need to rewrite formula as parsnip does not accept multivariate model
    as.formula(paste0("cbind(", pms, ")~."))
    
  }
}

transferice_recipe <- function(
    dat, 
    trans = NULL, 
    dim_reduction = NULL, 
    tunable = TRUE, 
    averaging = "an", 
    remove = c("site", "sample_id", "longitude", "latitude", "hole_id", "depth")
  ) {

  # formula
  fml <- formula_parser(dat, parms, averaging, remove)
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
  
  # are there tune parameter then tune them
  if (nrow(hardhat::extract_parameter_set_dials(wfl)) == 0) {
      
    out <- tune::fit_resamples(
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
    out <- tune::tune_grid(
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

# remove near zero variance before normalizing
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

default_message <- function() {
  list(
    placeholder = 'Select to apply',
    onInitialize = I('function() { this.setValue(null); }')
  )
}
