test_that("multiplication works", {

  final <- readRDS("~/Documents/work/code/transferice/inst/appdir/cache/validation_dinocyst_t_an_global_count_species_rename_lm.rds")

  # impute data
  imputed_fossil_dinodat <- impute_taxa(final, fossil_dinodat, "t_an", return_type = "names")  
  
  
  ggpredict(final, imputed_fossil_dinodat, fossil_dinodat_meta)
})
