# impute missing taxa in the fossil dataset
impute_taxa <- function(
    obj, 
    new_data, 
    out, 
    return_type = "percent", 
    exclude = meta
  ) {
  
  # get model
  fit <- readRDS(fs::path_package("transferice", "appdir", "cache", obj, ext = "rds")) 
  
  # get recipe
  recipe <- workflows::extract_recipe(fit, estimated = TRUE) 
  number <- recipes::tidy(recipe) |>
    dplyr::filter(.data$type == "rename") |>
    dplyr::pull(.data$number)

  # roles
  train_nm <- recipes::tidy(recipe, number)$value |> 
    stringr::str_remove_all("\"")
  new_nm <- role_organizer(new_data, out) 
  new_nm <- new_nm[names(new_nm) == "predictor"] |> unname()

  # which ones are existing in training set
  lgl_nm <- new_nm %in% train_nm
  
  if (return_type == "percent") {
    
    # percent taxa present in training set
    sum(lgl_nm) / length(train_nm) * 100
    
  } else if (return_type == "impute") {
    
    lgl_new <- train_nm %in% new_nm
    zero_vc <- rlang::rep_named(train_nm[!lgl_new], 0)
    
    # select taxa that exist in training set
    new_data <- dplyr::select(
      new_data, 
      dplyr::any_of(c(exclude, train_nm))
    ) |> 
      # impute those that do not exist in training set
      tibble::add_column(!!!zero_vc) 
      
    list(model = fit, new_data = new_data)
  
  }
}

# prep taxa with different composition to PCA
reduce_taxa <- function(obj, new_data) {
  
  # get model
  fit <- readRDS(fs::path_package("transferice", "appdir", "cache", obj, ext = "rds")) 

  # make a new recipe
  recipe <- transferice_recipe(
    new_data,
    outcome = NULL, 
    trans = stringr::str_extract(obj, "log|logit|normalize"), 
    dim_reduction = stringr::str_extract(obj, "pca|pls"),
    component = as.numeric(tail(stringr::str_split(obj, "_")[[1]], 1)),
    model = stringr::str_extract(obj, "lm|gls")
  )
  
  # bake it
  new_data <- prep(recipe, training = new_data) |> 
    bake(new_data = NULL)
  
  # extract the model from the fit
  model <- workflows::extract_fit_parsnip(fit)
  
  list(model = model, new_data = new_data)
}


age_finder <- function(new_data) {
  
  # connection
  nsb <- NSBcompanion::nsbConnect("guest", "arm_aber_sexy")
  
  # unique event name
  id <- dplyr::pull(new_data, .data$site_hole) |> unique() |> stringr::str_replace("-", "_")
  dplyr::mutate(
    new_data, 
    age_ma = NSBcompanion::findAge(nsb, id, depth_mbsf = .data$sample_depth_mbsf)$age_ma
  )  |> 
    tidyr::drop_na(age_ma)
  
}



