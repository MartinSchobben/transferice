test_that("recipe step works", {
  
  rcp <- recipes::recipe(
    x = dinodat,
    vars = c("p_an", "88", "longitude", "latitude"),
    roles = c("outcome", "predictor", "spatial", "spatial")
  ) |> 
    step_zerogeodist(lon = longitude, lat = latitude) |> 
    prep(training = dinodat) |> 
    bake(NULL)   
  
  expect_equal(nrow(rcp), 2360)
  
})
