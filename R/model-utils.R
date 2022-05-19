# dat = dataset
# parms = oceanographic parameters
# averaging = averaging period
# remove = variables not used in regression
formula_parser <- function(
    dat, 
    parms, 
    averaging = "an",
    remove = c("sample_id", "longitude", "latitude", "hole_id"),
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

transferice_recipe <- function(dat, trans = NULL, dim_reduction = NULL, 
                               tunable = TRUE, averaging = "an", 
                               remove = c("sample_id", "longitude", "latitude", "hole_id")) {


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
  
  # path to package
  pkg_path <- fs::path_package("transferice")
  # if directories don't exist then create them
  if (!fs::dir_exists(fs::path(pkg_path, "appdir", "cache", "tuning"))) {
    fs::dir_create(pkg_path, "appdir", "cache", "tuning")
  }
  if (!fs::dir_exists(fs::path(pkg_path, "appdir", "cache", "model_extracts"))) {
    fs::dir_create(pkg_path, "appdir", "cache", "model_extracts")
  }
  
  # cross validation resampling
  dat_cv <- rsample::vfold_cv(rsample::training(split), v = 10)
  
  # name file
  nm  <- sanitize_workflow(wfl)
  
  # dir for tuning and fitting
  cache_dir <- fs::path_package("transferice", "appdir", "cache", "tuning")
  # dir for partials models
  cache_part <- fs::path_package("transferice", "appdir", "cache", "model_extracts")
                   
  cache_file <- try(
    fs::path_package("transferice", "appdir", "cache", "tuning", nm, 
                     ext = "rds"), 
    silent = TRUE
  )
  
  # if the cache does not exist then render from scratch
  if (inherits(cache_file, "try-error")) {
      
    if(nrow(hardhat::extract_parameter_set_dials(wfl)) == 0) {
      
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
    
    # save tuning or fitted data
    saveRDS(out, fs::path(cache_dir, nm, ext = "rds"))
    
    # now also extract the partial models
    saveRDS(cv_model_extraction(out, wfl), fs::path(cache_part, nm, ext = "rds"))
  } 
  # depending whether the data is  already generated this function executes 
  # fast or slow
  readRDS(fs::path(cache_dir, nm, ext = "rds"))
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

default_message <- function() {
  list(
    placeholder = 'Select to apply',
    onInitialize = I('function() { this.setValue(null); }')
  )
}

