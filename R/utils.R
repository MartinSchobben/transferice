# some dinocyst count datasets have absolute counts, so it is converted to
# relative proportions for each sample
calc_taxon_prop <- function(
    con, 
    count = "neptune_sample_taxa", 
    meta =  "neptune_sample", 
    taxon = "neptune_taxonomy"
  ) {
  # obtain counts and make wide format
  DBI::dbGetQuery(
    con ,
    paste0(
      "SELECT 
        c.sample_id, 
        taxon_abundance / SUM(taxon_abundance) 
          OVER (PARTITION BY c.sample_id) AS taxon_prop, 
            c.taxon_id,
            hole_id,
            genus ||  
              COALESCE(' ' || species, '') || 
                COALESCE(' ' || subspecies, '')  AS name
                  FROM ", count, " AS c
                    LEFT JOIN ", meta, " AS s ON c.sample_id = s.sample_id
                      LEFT JOIN ", taxon, " AS t 
                        ON c.taxon_id = t.taxon_id"
    )
  ) |> 
    # drop taxon id
    dplyr::select(- .data$taxon_id) |> 
    dplyr::mutate(name = stringr::str_trim(name)) |>  
    tidyr::pivot_wider(
      names_from = name,
      values_from = taxon_prop,
      values_fill = numeric(1)
    ) 
  
}

# species names for selection
# workflow object
# id: id to search (if NULL all names are give)
species_naming <- function(workflow, id = NULL) {
  
  nms <- hardhat::extract_preprocessor(workflow) |> 
    recipes::tidy(2) 
  
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
