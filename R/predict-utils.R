# impute missing taxa in the fossil dataset
impute_taxa <- function(obj, new_data, out, return_type = "percent") {
  
  # datasets
  train <- obj$splits[[1]] |> tibble::as_tibble()
  
  # roles
  train_nm <- role_organizer(train, out)
  new_nm <- role_organizer(new_data, out)

  lgl_nm <- new_nm[names(new_nm) == "predictor"] %in% train_nm[names(train_nm) == "predictor"] 
  
  if (return_type == "percent") {
    
    
    # percent taxa present in training set
    sum(lgl_nm) / length(train_nm) * 100
    
  } else if (return_type == "names") {
    
    lgl_new <- train_nm[names(train_nm) == "predictor"] %in% new_nm[names(new_nm) == "predictor"]
    zero_vc <- rlang::rep_named(train_nm[names(train_nm) == "predictor" ][!lgl_new], 0)
    
    # select taxa that exist in training set
    dplyr::select(new_data, dplyr::any_of(train_nm[names(train_nm) == "predictor"])) |> 
      # impute those that do not exist in training set
      tibble::add_column(!!!zero_vc)
  
  }
}

