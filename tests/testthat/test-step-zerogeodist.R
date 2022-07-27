test_that("recipe step works", {
  
  rcp <- transferice_recipe(modern, "t_an") |> 
    step_zerogeodist(lon = longitude, lat = latitude) |> 
    recipes::prep(training = modern) |> 
    recipes::bake(NULL)   
  
  expect_equal(nrow(rcp), 2360)
  
})
