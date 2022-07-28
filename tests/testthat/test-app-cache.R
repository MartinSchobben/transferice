test_that("file naming conventions work", {
  
  # snaps
  expect_snapshot(
    file_namer("rds", "raw", "dinocyst_t_an_global")
  )
  expect_snapshot(
    file_namer("rds", "engineering", "raw_dinocyst_t_an_global_prop_species", 
               taxa ="genera", method = "partial_fit", trans = "lm")
  )
  
  # errors
  expect_error(
    file_namer("rds", "something", "dinocyst", "count"),
    NULL
  )
  expect_error(
    file_namer("png", "prep", "dinocyst", viz = "xy"),
    NULL
  )
})

test_that("dir is created", {
  
  # path package
  pkg <- fs::path_package("transferice")
  
  # do they exist?
  expect_true(
    {cache_dir("rds") ; fs::dir_exists(fs::path(pkg, "appdir", "cache"))}
  )
  expect_true(
    {cache_dir("png") ; fs::dir_exists(fs::path(pkg, "www", "img"))}
  )
  expect_true(
    {cache_dir("mkv") ; fs::dir_exists(fs::path(pkg, "www", "vid"))}
  )
  
})

test_that("method for saving are selected", {
  
  # path package
  pkg <-fs::path_package("transferice")
  
  # connection
  dbpath <- fs::path_package(package = "transferice", "extdata", 
                             "transferice.sqlite")
  con <- DBI::dbConnect(drv = RSQLite::SQLite(),  dbname = dbpath)
  
  # count data
  prop <- calc_taxon_prop(con)
  
  # file name
  nm <- file_namer("rds", "raw", "dinocyst", trans = "prop")
  
  # save file
  method_selector(nm, prop, "rds")
  
  expect_true(
    fs::file_exists(fs::path(pkg, "appdir", "cache", nm, ext = "rds"))
  )
  
  # load file
  expect_snapshot(
    method_selector(nm, type = "rds")
  )
  
  # delete file
  fs::file_delete(fs::path(pkg, "appdir", "cache", nm, ext = "rds"))
  
  # filter the dataset
  taxon_loc <- dplyr::distinct(prop, .data$sample_id, .keep_all = TRUE) |> 
    dplyr::mutate(
      # odds  
      odds = .data[["Amiculosphaera umbracula"]] / (1 - .data[["Amiculosphaera umbracula"]]),
      # find highest n ranks
      rank = rank(.data$odds)
    ) |> 
    dplyr::filter(.data$rank >= dplyr::n() - 20) |> 
    tidyr::drop_na(-age_ma)
  
  # plot
  p <- ggtaxon(taxon_loc, "Amiculosphaera umbracula", "site_hole")
  
  # file name
  nm <- file_namer("png", "raw", "dinocyst", trans = "raw", viz = "xy", 
                   x = "locations")
  
  # save file
  method_selector(nm, p, "png", width = 250, height = 250)
  
  expect_true(
    fs::file_exists(fs::path(pkg, "www", "img", nm, ext = "png"))
  )
  
  # load file
  expect_equal(
    method_selector(nm, type = "png"),
    fs::path(pkg, "www", "img", nm, ext = "png")
  )
  
  # delete file
  fs::file_delete(fs::path(pkg, "www", "img", nm, ext = "png"))
  
  # close connection
  on.exit(DBI::dbDisconnect(con))
})


test_that("caching works", {
  
  # path package
  pkg <-fs::path_package("transferice")
  
  # connection
  dbpath <- fs::path_package(package = "transferice", "extdata", 
                             "transferice.sqlite")
  con <- DBI::dbConnect(drv = RSQLite::SQLite(),  dbname = dbpath)

  # file name
  nm <- file_namer("rds", "raw", "dinocyst", trans = "prop")
  
  # count data
  prop <- calc_taxon_prop(con) |> 
    app_caching("rds", nm)
  
  expect_true(
    fs::file_exists(fs::path(pkg, "appdir", "cache", nm, ext = "rds"))
  )
  
  # load file
  expect_snapshot(
    method_selector(nm, type = "rds")
  )
  
  nm <- file_namer("png", "raw", "dinocyst_annual_global", viz ="xy", x = "22")
  
  # filter the dataset
  taxon_loc <- dplyr::distinct(prop, .data$sample_id, .keep_all = TRUE) |> 
    dplyr::mutate(
      # odds  
      odds = .data[["Amiculosphaera umbracula"]] / (1 - .data[["Amiculosphaera umbracula"]]),
      # find highest n ranks
      rank = rank(.data$odds)
    ) |> 
    dplyr::filter(.data$rank >= dplyr::n() - 20) |> 
    tidyr::drop_na(-age_ma)
  
  # plot
  p <- ggtaxon(taxon_loc, "Amiculosphaera umbracula", "site_hole")|> 
    app_caching("png", nm, width = 400, height =400)
  
  # check file exists
  expect_true(
    fs::file_exists(fs::path(pkg, "www", "img", nm, ext = "png"))
  )
  
  # delete file
  fs::file_delete(fs::path(pkg, "www", "img", nm, ext = "png"))
  
  on.exit(DBI::dbDisconnect(con))
})

