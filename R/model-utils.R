role_organizer <- function(
    dat, 
    outcome, 
    group = "hole_id", 
    temporal = "sample_id", 
    spatial = c("longitude", "latitude"),
    aliases = NULL
  ) {
  
  # variables
  txa <- colnames(dat)[!colnames(dat) %in% c(outcome, group, temporal, spatial)] # taxa
  # aliases for taxa
  if (!is.null(aliases)) txa <- paste0(aliases, seq_along(txa))
  
  # roles
  c(
    temporal =  temporal,
    group = group,
    spatial = spatial[1],
    spatial = spatial[2],
    outcome = outcome
  ) |> 
    append(setNames(txa, rep("predictor", length(txa))))
  
}
# dat = dataset
# parms = oceanographic parameters
# averaging = averaging period
# remove = variables not used in regression
formula_parser <- function(
    dat,
    outcome, 
    type = "ordinary",
    aliases = FALSE
  ) {
  
  vars <- role_organizer(dat, outcome, aliases = aliases)
  
  x <- paste0(vars[names(vars) == "predictor"], collapse = "+")
  
  if (type ==  "ordinary") {
    

    as.formula(paste0(outcome, "~", x))

    
  } else if (type == "random") {
    
    as.formula(paste0("~", x, "|" , vars[names(vars) == "group"]))
    
  } else if (type == "correlation") {
    
    as.formula(paste0("~", paste(vars[names(vars) == "spatial"], collapse = "+")))
  }
}

transferice_recipe <- function(
    dat,
    outcome,
    trans = NULL, 
    dim_reduction = NULL, 
    tunable = TRUE,
    model = "ols"
  ) {


  vars <- role_organizer(dat, outcome)
  # taxa
  txa <- vars[names(vars) == "predictor"]
  
  # recipe
  rcp <- recipes::recipe(x = dat, vars = vars, roles = names(vars)) |> 
    # scale all outcomes
    recipes::step_normalize(recipes::all_outcomes()) |> 
    # rename taxa
    recipes::step_rename(!!!rlang::set_names(txa, paste0("taxa_", seq_along(txa)))) 
  
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
  
  #in case of spatial component with GLS add
  if (model == "gls") {
    rcp <- step_zerogeodist(rcp, lon = longitude, lat = latitude, skip = TRUE)
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
      metrics = yardstick::metric_set(yardstick::rmse), 
      control = ctrl
    )
    
  } else {

    # setting model tuning parameters
    dls <- wfl %>%
      dials::parameters() %>%
      # set PCA steps to 5 components total
      update(num_comp = dials::num_comp(c(1, 5))) 
    
    # tuning grid
    tune_grid <- dials::grid_regular(dls, levels = 5)
    # tuning
    out <- tune::tune_grid(
      wfl,
      resamples = dat_cv,
      grid = tune_grid,
      metrics = yardstick::metric_set(yardstick::rmse), 
      control = ctrl
    )
  }
  out
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
    yardstick::rmse
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
ctrl <- tune::control_resamples(
  extract = function(x) {
    list(fit = tune::extract_fit_parsnip(x), recipe = tune::extract_recipe(x), mold  = hardhat::extract_mold(x))
  }
)


default_message <- function() {
  list(
    placeholder = 'Select to apply',
    onInitialize = I('function() { this.setValue(null); }')
  )
}
