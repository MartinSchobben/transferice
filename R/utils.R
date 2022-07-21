# some dinocyst count datasets have absolute counts, so it is converted to
# relative proportions for each sample
calc_taxon_prop <- function(
    con, 
    type = "all",
    count = "neptune_sample_taxa", 
    sample =  "neptune_sample", 
    taxon = "neptune_taxonomy",
    site = "neptune_hole_summary",
    meta = "neptune_sod"
  ) {
  
  if (type == "all") {
    x <- ";"
  } else if (type == "train") {
    x <- " WHERE sample_depth_mbsf = 0 ;"
  } else if (type == "predict") {
    x <- " WHERE sample_depth_mbsf > 0 ;"
  }
  
  # obtain counts and make wide format
  sql_db <- DBI::dbGetQuery(
    con ,
    paste0(
      "SELECT 
        c.sample_id, 
        t.genus,
        t.species,
        s.sample_depth_mbsf,
        s.hole_id,
        g.site_hole,
        g.longitude,
        g.latitude,
        m.source_citation,
        taxon_abundance / SUM(taxon_abundance) 
          OVER (PARTITION BY c.sample_id) AS taxon_prop
                  FROM ", count, " AS c
                    LEFT JOIN ", sample, " AS s ON c.sample_id = s.sample_id
                      LEFT JOIN ", taxon, " AS t ON c.taxon_id = t.taxon_id
                        LEFT JOIN ", site, " AS g ON s.hole_id = g.hole_id
                          LEFT JOIN ", meta, " AS m ON s.dataset_id = m.dataset_id",
                            x
    )
  )  
  
  taxa_nms <- unique(paste(sql_db$genus, sql_db$species)) |> stringr::str_trim()
 
  tidyr::unite(sql_db, col = "name", .data$genus, .data$species, sep = " ") |>
    dplyr::mutate(
      # tidy the names
      name = stringr::str_trim(name),
      # add provisional age variable
      age_ma = NA_real_

    ) |>
    tidyr::pivot_wider(
      names_from = name,
      values_from = taxon_prop,
      values_fill = numeric(1)
    ) |> 
   # sort alphabetically
   dplyr::select(dplyr::any_of(transferice:::meta), dplyr::any_of(sort(taxa_nms))) 
  
}

# species names for selection
# workflow object
# id: id to search (if NULL all names are give)
taxa_naming <- function(workflow, id = NULL) {
  
  rcp <- hardhat::extract_preprocessor(workflow)  
  # which is the rename step?
  n_step <- recipes::tidy(rcp) |> 
    dplyr::filter(type == "rename") |> 
    dplyr::pull(number)
  # select rename step
  nms <- recipes::tidy(rcp, n_step) 
  
  if (is.null(id)) {
    id <- paste0("taxa_", 1:nrow(nms))
  }
  
  dplyr::filter(nms, .data$terms %in% !!id) |> 
    dplyr::pull(.data$value) |> 
    stringr::str_replace_all("\"", "")
  
}

reverse_normalize <- function(data, recipe, predicted = FALSE) {
  
  stp_norm <- recipes::tidy(recipe, 1)
  
  if (isTRUE(predicted)) {
    stp_norm <- dplyr::mutate(stp_norm, terms = ".pred")
  }

  pms <- unique(stp_norm$terms)
  
  
  dplyr::mutate(
    data, 
    dplyr::across(
      dplyr::any_of(pms),
      ~reverse_normalize_(
        .x, 
        stp_norm[stp_norm$terms == dplyr::cur_column(), , drop = FALSE]
      )
    ))
}

reverse_normalize_ <- function(parm, stat) {
  
  mean <- dplyr::filter(stat, .data$statistic == "mean") |> 
    dplyr::pull(.data$value)
  sd <- dplyr::filter(stat, .data$statistic == "sd") |> 
    dplyr::pull(.data$value)
  
  (parm * sd) + mean 
}

# parameters
abbreviate_vars <- function(x, type = "parms") {
  x <- abbreviate(x, 1, strict = TRUE)
  x[names(x) == "density"] <- "I" # density is different
  x[names(x) == "silicate"] <- "i" # salinity is different
  x
}


#' Title
#'
#' @return
#' @export
get_pool <- function() {  
  # path to db
  dbpath <- fs::path_package(package = "transferice", "extdata", "transferice.sqlite")
  # pool connections
  pool::dbPool(drv = RSQLite::SQLite(),  dbname = dbpath)
}


# sanitize taxa names for file names
sanitize_taxa <- function(nm) {
  stringr::str_trim(nm) #|>  stringr::str_replace("[[:blank:]]", "_")
}

# sanitize workflow
sanitize_workflow <- function(wfl, model = TRUE) {
  model_spec <- tail(workflows::extract_spec_parsnip(wfl), 1)
  # extract steps
  x <- workflows::extract_preprocessor(wfl)
  if (!is.null(x$steps)) {
    # make vector
    x <- c(sapply(x$steps, class)[1,])
    # remove "step_" prefix
    x <- gsub("step_", "", x)
    # collapse
    recipe_spec <- paste(x, collapse = "_")
  } else {
    recipe_spec <- "unprocessed"
  }
  # add model
  if (isTRUE(model)) {
    paste(recipe_spec, model_spec, sep = "_")
  } else {
    recipe_spec
  }
}

transferice_theme <- function(base = ggplot2::theme_classic(), ...) {
  `%+replace%` <- ggplot2::`%+replace%`
  base %+replace%
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 15, face = "bold"),
      axis.text =  ggplot2::element_text(size = 10),
      axis.title =  ggplot2::element_text(size = 12),
      legend.text =  ggplot2::element_text(size = 10),
      legend.title =  ggplot2::element_text(size = 12),
      strip.text =   ggplot2::element_text(size = 12),
      legend.box = "horizontal",
      legend.position = "top",
      rect =  ggplot2::element_rect(
        fill = "transparent",
        color = "transparent"
      ),
      panel.background = ggplot2::element_rect(
        fill = "transparent",
        color = "transparent"
      ),
      plot.background = ggplot2::element_rect(
        fill = "transparent",
        color = "transparent"
      ),
      legend.background = ggplot2::element_rect(
        fill = "transparent",
        color = "transparent"
      )
    )
}

clean_cache <- function(module = "training", type = "all") {
  cache_pkg <- fs::path_package(package = "transferice", "inst", "appdir", "cache")
  img_pkg <- fs::path_package(package = "transferice", "inst", "www", "img")
  vid_pkg <- fs::path_package(package = "transferice", "inst", "www", "vid")
  model_pkg <- fs::path_package(package = "transferice",  "inst", "appdir", "model-module")
  
  if (module == "training") {
    if (type == "all" || type == "data") {
      files_train <- list.files(cache_pkg) 
      list.files(cache_pkg, full.names = TRUE)[grepl("^raw|^training|^validation|^final", files_train)] |> 
        fs::file_delete()
    }
    if (type == "all" || type == "img") {
      files_img <- list.files(img_pkg)
      list.files(img_pkg, full.names = TRUE)[grepl("^raw|^training|^validation|^engineering", files_img)] |>
        fs::file_delete()
    }
    if (type == "all" || type == "vid") {
      fs::file_delete(list.files(vid_pkg, full.names = TRUE))
      files_model <- list.files(model_pkg)
    }
    if (type == "all" || type == "module") {
      list.files(model_pkg, full.names = TRUE)[grepl("[^app.R]", files_model)] |> 
        fs::file_delete()
    }
  }
  
  
}
