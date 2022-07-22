test_that("print how many taxa can be found in fossil data", {
  
  # save data
  pt_pkg <- fs::path_package("transferice", "appdir", "cache")
  nm <- "final_dinocyst_t_an_global_0mbsf_species_prop_log_center_pca_zerogeodist_gls_4"
  saveRDS(transferice:::final_fit, fs::path(pt_pkg, nm, ext = "rds"))
  
  # print
  expect_snapshot(
    print_predict(nm, fossil, "t_an")
  )
  
  # delete file
  fs::file_delete(fs::path(pt_pkg, nm, ext = "rds"))
})
