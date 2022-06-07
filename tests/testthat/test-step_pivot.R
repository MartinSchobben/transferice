test_that("pivot prep internl function works", {
  get_train_pivot(relig_income, religion)
  
  # prepare
  dinodat <- dinodat[, !names(dinodat) %in% c("hole_id", "longitude", "latitude", "sample_id")] 
  pms <-  paste(abbreviate_vars(parms), temp[temp == "an"], sep = "_")
  get_train_pivot(dinodat, pms)
})

test_that("recipe step works", {
  
  data(meats, package = "modeldata")
  recipes::recipe(water + fat + protein ~ ., meats) |> 
    step_pivot() |> 
    prep.step_pivot()
  
})