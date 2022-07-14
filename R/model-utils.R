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
  if (!is.null(aliases)) {
    txa <- paste0(aliases, seq_along(txa)) 
  } 
  
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
    type = "fixed",
    aliases = NULL,
    exclude = NULL
  ) {
  
  vars <- role_organizer(dat, outcome, aliases = aliases)
  
  x <- paste0(vars[names(vars) == "predictor"], collapse = "+")
  
  if (type ==  "fixed") {
    
    
    fm <- paste0(outcome, "~ .")
    
    # in case of correlation structure and tuning
    if (!is.null(exclude)) {
      exclude <- paste0(exclude, collapse = "-")
      fm <- paste0(fm, "-", exclude)

    }

    as.formula(fm)
    
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
  
  # new names taxa
  new_txa <- paste0("taxa_", seq_along(txa))
  
  # recipe
  rcp <- recipes::recipe(x = dat, vars = vars, roles = names(vars)) #|>
    # rename taxa
    # recipes::step_rename(!!!rlang::set_names(txa, new_txa)) 
  
  # transforming 
  if (isTruthy(trans)) {
    rcp <- purrr::reduce(trans, transform_predictor, .init = rcp, txa = new_txa)
  }
  
  # dimensions reduction (PCA)
  if (isTruthy(dim_reduction) && dim_reduction == "PCA") {
    if (isTRUE(tunable)) {
      rcp <- recipes::step_pca(
        rcp,
        dplyr::any_of(new_txa), 
        num_comp = tune::tune()
      )
    } else {
      rcp <- recipes::step_pca(
        rcp,
        dplyr::any_of(new_txa)
      )
    }
  }
  
  #in case of spatial component with GLS add
  if (model == "gls") {
    rcp <- step_zerogeodist(rcp, lon = longitude, lat = latitude, skip = TRUE) |> 
      recipes::update_role(longitude, latitude, new_role = "predictor")
  }
  
  # make everything predictors
  rcp 
}

transferice_tuning <- function(split, wfl) {
  
  # cross validation resampling
  dat_cv <- rsample::vfold_cv(rsample::training(split), v = 10, strata = "latitude")
  
  # are there tune parameter then tune them
  if (nrow(hardhat::extract_parameter_set_dials(wfl)) == 0) {
      
    out <- tune::fit_resamples(
      wfl, 
      resamples = dat_cv, 
      metrics = yardstick::metric_set(yardstick::rmse), 
      control = ctrl
    )
    
  } else {

    # tune grid
    tune_grid <- dials::grid_regular(tune::extract_parameter_set_dials(wfl), levels = 4)
    
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
transform_predictor <- function(rcp, trans, txa) {
  switch(
    trans,
    logit =  step_logit_center(rcp, txa),
    log = step_log_center(rcp, txa),
    normalize = step_nz_normalize(rcp, txa) 
  )
}

# remove near zero variance before normalizing
step_nz_normalize <- function(rcp, txa) {
  recipes::step_normalize(rcp, dplyr::any_of(txa))
}

step_logit_center <- function(rcp, txa) {
  recipes::step_logit(rcp, dplyr::any_of(txa), offset = 0.025) |>
    recipes::step_center(dplyr::any_of(txa))
}

step_log_center <- function(rcp, txa) {
  recipes::step_log(rcp, dplyr::any_of(txa), offset = 0.025) |>
    recipes::step_center(dplyr::any_of(txa))
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
