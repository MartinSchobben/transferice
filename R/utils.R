# some dinocyst count datasets have absolute counts, so it is converted to
# relative proportions for each sample
calc_taxon_prop <- function(count, meta, con) {
  # obtain counts and make wide format
  DBI::dbGetQuery(
    con ,
    paste0(
      "SELECT 
        c.sample_id, 
        c.taxon_id,
        hole_id,
        taxon_abundance / SUM(taxon_abundance) 
          OVER (PARTITION BY c.sample_id) AS taxon_prop 
            FROM ", count, " AS c
              LEFT JOIN ", meta, " AS s ON c.sample_id = s.sample_id")
    ) |>
  tidyr::pivot_wider(
    names_from = taxon_id,
    values_from = taxon_prop,
    values_fill = numeric(1)
  )
  
}

# species names for selection
# dat: dataset
# name: species name
# con: database connection
# taxon: name of datatable
species_naming <- function(
    con,
    ids = NULL,
    taxa_name = NULL, 
    taxon = "neptune_taxonomy",
    averaging = "an", 
    remove = c("hole_id", "longitude", "latitude", "sample_id")
) {
  
  # get taxon list
  nms <- DBI::dbGetQuery(
    con ,
    paste0("SELECT taxon_id, genus ||  
              COALESCE(' ' || species, '') || 
              COALESCE(' ' || subspecies, '')  AS name
                FROM ", taxon)
  ) 
    
  # named vector
  nms <- setNames(
    # ids
    nms$taxon_id,
    # sanitize names
    nm = vapply(nms$name, sanitize_taxa, character(1))
  )

  # provide names if data is supplied
  if (!is.null(ids))  return(nms[nms %in% ids]) 
    
  # provide id if name is supplied
  if (!is.null(taxa_name)) return(nms[names(nms) %in% taxa_name]) 
  
  nms
  
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
