test_that("species are plotted in nice histogram", {
  # dinos
  dbpath <- fs::path_package(package = "transferice", "extdata", "transferice.sqlite")
  con <- DBI::dbConnect(drv = RSQLite::SQLite(),  dbname = dbpath)
  dino_prop <- calc_taxon_prop("neptune_sample_taxa", "neptune_sample",  con)  

  vdiffr::expect_doppelganger(
    "taxon odds ranking per sample",
    taxon_plot(dino_prop, "22", "sample_id")
  )
  on.exit(DBI::dbDisconnect(con))
})
