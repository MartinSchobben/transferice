test_that("impute taxa", {
  
  # save data
  pt_pkg <- fs::path_package("transferice", "appdir", "cache")
  nm <- "final_dinocyst_t_an_global_0mbsf_species_prop_log_center_pca_zerogeodist_gls_4"
  saveRDS(transferice:::final_fit, fs::path(pt_pkg, nm, ext = "rds"))
  
  # impute data
  expect_snapshot(
    impute_taxa(nm, fossil, "t_an", return_type = "impute") 
  )

  # percent taxa present in training set
  expect_snapshot(
    impute_taxa(nm, fossil, "t_an", return_type = "percent") 
  )
  
  # reduce data
  expect_snapshot(
    reduce_taxa(nm, fossil) 
  )
  
  # delete file
  fs::file_delete(fs::path(pt_pkg, nm, ext = "rds"))
  
})

test_that("get age", {
  
  expect_snapshot(age_finder(fossil))
  
})
