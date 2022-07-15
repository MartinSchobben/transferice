test_that("impute taxa", {
  
  final <- readRDS("~/Documents/work/code/transferice/inst/appdir/cache/validation_dinocyst_t_an_global_count_species_rename_lm.rds")
  
  # impute data
  impute_taxa(final, fossil_dinodat, "t_an", return_type = "names")  
  
})

test_that("get age", {
  
  age_finder(fossil_dinodat)
  
})
