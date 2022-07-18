test_that("impute taxa", {
  

  # impute data
  impute_taxa(final, fossil_dinodat, "t_an", return_type = "names")  
  
  # reduce data
  reduce_taxa("validation_dinocyst_t_an_global_0mbsf_species_prop_log_center_pca_lm_1", dplyr::select(new_data, -depth))
  
})

test_that("get age", {
  
  age_finder(fossil_dinodat)
  
})
