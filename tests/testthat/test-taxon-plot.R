test_that("species are plotted in nice histogram", {
  # dinos
  dbpath <- fs::path_package(package = "transferice", "extdata", "transferice.sqlite")
  con <- DBI::dbConnect(drv = RSQLite::SQLite(),  dbname = dbpath)
  dino_prop <- calc_dino_prop("dino_cnt", con) 

  taxon_plot(dino_prop, "srei")
})
