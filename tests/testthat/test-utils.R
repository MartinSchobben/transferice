test_that("rows add to 1", {
  dbpath <- fs::path_package(package = "transferice", "extdata", 
                             "transferice.sqlite")
  con <- DBI::dbConnect(drv = RSQLite::SQLite(),  dbname = dbpath)
  # check if samples add to 1
  chk <- function(x) {sum(x) / length(x)}
  expect_equal(
    calc_taxon_prop("neptune_sample_taxa", "neptune_sample", con)  |> 
      dplyr::rowwise() |> 
      dplyr::mutate(
        dino_tot = 
          sum(dplyr::c_across(-c(.data$sample_id, .data$hole_id)), na.rm = T),
        ) |> 
      dplyr::pull(.data$dino_tot) |> 
      chk()
      ,
    1
    )
  on.exit(DBI::dbDisconnect(con))
})
