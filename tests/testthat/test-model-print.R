test_that("print model results works", {

  expect_snapshot(
    print_model(transferice:::modern_split, transferice:::modern_workflow, 
                pred = "PC1", tune = 1, out = "t_an")
  )

  expect_snapshot(
    print_model(transferice:::tuned_workflow, transferice:::modern_workflow,  
                pred = "PC1", tune = 1, out = "t_an")
  )
  
  expect_snapshot(
    print_model(transferice:::validation_modern, transferice:::modern_workflow)
  )
  
  vdiffr::expect_doppelganger(
    "sub-model comparison",
    ggperformance(transferice:::tuned_workflow, "t_an", 1)
  )
})
