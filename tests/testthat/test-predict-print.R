test_that("print how many taxa can be found in fossil data", {
  final <- readRDS("~/Documents/work/code/transferice/inst/appdir/cache/validation_dinocyst_t_an_global_count_species_rename_lm.rds")
  
  print_predict(final, fossil_dinodat, "t_an")
})
