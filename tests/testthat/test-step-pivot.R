test_that("recipe step works", {
  
  data(meats, package = "modeldata")
  rcp <- recipes::recipe(water + fat + protein ~ ., meats) |> 
    step_pivot(recipes::all_outcomes()) |> 
    prep(training = meats) |> 
    bake(NULL)   
  
  pvt <- tidyr::pivot_longer(meats, cols = c(protein, fat, water),
                             names_ptypes = factor())
  
  expect_equal(rcp, pvt)
  
})

test_that("new step classes exist", {
  
  data(meats, package = "modeldata")
  st <- recipes::recipe(water + fat + protein ~ ., meats) |> 
    step_pivot()
  
  expect_s3_class(st$steps[[1]], "step_pivot")
})