test_that("multiplication works", {

  # save data
  pt_pkg <- fs::path_package("transferice", "appdir", "cache")
  nm <- "final_dinocyst_t_an_global_0mbsf_species_prop_log_center_pca_zerogeodist_gls_4"
  saveRDS(transferice:::final_fit, fs::path(pt_pkg, nm, ext = "rds"))

  # reduce data
  fit <- reduce_taxa(nm, fossil)
  
  ggpredict(fit$model, fit$new_data) |> chrono_bldr()
  
  # delete file
  fs::file_delete(fs::path(pt_pkg, nm, ext = "rds"))
  
  
  
})
