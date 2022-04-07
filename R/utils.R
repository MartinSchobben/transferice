# some dinocyst count datasets have absolute counts, so it is converted to
# relative proportions for each sample
calc_taxon_prop <- function(count, meta, taxon, con) {
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
    tidyr::pivot_wider(
      names_from = name, 
      values_from = taxon_prop, 
      values_fill = numeric(1)
    )
  
}

# species names for selection
species_naming <- function(dat, parms, averaging) {
  
  pms <- paste(abbreviate_vars(parms), averaging, sep = "_")
  names(dat)[!names(dat) %in% c(pms, "hole_id", "longitude", "latitude","sample_id")]
    
 # species_tb <- DBI::dbReadTable(dat, "neptune_taxonomy") |> 
 #   dplyr::mutate(
 #     dplyr::across(
 #       tidyselect::vars_select_helpers$where(is.character),  
 #        ~tidyr::replace_na(.x, "")
 #      )
 #    )
 # 
 #  setNames(
 #    species_tb$taxon_id, 
 #    nm = paste(species_tb$genus, species_tb$species, species_tb$subspecies)
 #  )
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
  stringr::str_trim(nm) |>  stringr::str_replace("[[:blank:]]", "_")
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
