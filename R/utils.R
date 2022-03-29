# some dinocyst count datasets have absolute counts, so it is converted to
# relative proportions for each sample
calc_taxon_prop <- function(count, meta, con) {
  DBI::dbGetQuery(
    con ,
    paste0(
      "SELECT 
        c.sample_id, 
        taxon_abundance / SUM(taxon_abundance) 
          OVER (PARTITION BY c.sample_id) AS taxon_prop, 
            taxon_id,
            hole_id
              FROM ", count, " AS c
                LEFT JOIN ", meta, " AS s ON c.sample_id = s.sample_id" 
    )
  ) |> 
    tidyr::pivot_wider(
      names_from = taxon_id, 
      values_from = taxon_prop, 
      values_fill = numeric(1)
      )
}

# species names for selection
species_naming <- function(dat) {
    
 species_tb <- DBI::dbReadTable(dat, "neptune_taxonomy") |> 
   dplyr::mutate(
     dplyr::across(
       tidyselect::vars_select_helpers$where(is.character),  
        ~tidyr::replace_na(.x, "")
      )
    )
 
  setNames(
    species_tb$taxon_id, 
    nm = paste(species_tb$genus, species_tb$species, species_tb$subspecies)
  )
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

rescale <- function(var, fun = "logit", ns = "car") {
  
  # rescale all p's to (0.025, 0.975)
  if (fun == "logit" && ns == "car") {
    var <- scales::rescale(var, c(0.025, 0.0975))
  }
  rlang::call2(fun, var, .ns = ns) |> eval()
}

