test_that("file naming conventions work", {
  expect_snapshot(file_namer("rds", "raw", "dinocyst"))
  expect_snapshot(file_namer("rds", "prep", "dinocyst", trans = "lm"))
  
  # errors
  expect_snapshot(file_namer("rds", "something", "dinocyst", "count"))
  expect_snapshot(file_namer("png", "prep", "dinocyst", viz = "xy"))
})
