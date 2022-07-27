test_that("partial model extract works", {

  # extracts
  ext <- cv_extraction(tuned_workflow)
  
  # parts fit
  expect_snapshot(
    ext
  )
  
  # partials calcs
  out <- calc_partials(ext, "PC1", "t_an", rlang::syms(c("longitude", "latitude")))
 
  expect_snapshot(out)
  
  # parts trained receipe
  expect_snapshot(
    cv_extraction(tuned_workflow, "recipe")
  )
  
})
