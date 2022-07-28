test_that("chronostratigraphic chart plots", {
  
  # save data
  pt_pkg <- fs::path_package("transferice", "appdir", "cache")
  nm <- "final_dinocyst_t_an_global_0mbsf_species_prop_log_center_pca_zerogeodist_gls_4"
  saveRDS(transferice:::final_fit, fs::path(pt_pkg, nm, ext = "rds"))
  
  # reduce
  taxa <- reduce_taxa(nm, fossil) 
  
  # plot
  p <- ggpredict(
    taxa$model, 
    taxa$new_data |> age_finder(),
    "t_an"
  )  |> chrono_bldr()
  
  vdiffr::expect_doppelganger("chrono strat", p)
  
  # delete file
  fs::file_delete(fs::path(pt_pkg, nm, ext = "rds"))
})
