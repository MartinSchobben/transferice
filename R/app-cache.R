# This function first searches in the cache dirs for the figure or movie before 
# rendering it
app_caching <- function(expr, type = "rds", file_name) {
  
  # check for cache dirs
  cache_dir()
  
  # different pathways for different extensions
  pt <- switch(
    type,
    rds = fs::path_package("transferice", "appdir", "cache", nm, ext = type)
  )

  # check if file exists
  try_file <- try(pt, silent = TRUE)
  
  if (inherits(cache_file, "try-error")) {
    
    out <- expr
  } 
  
  
}

# select method
method_selector <- function(file_name, type = "rds", direction = "write",  
                            width = NULL, height = NULL) {
  
}

# select path
path_selector <- function(file_name, type = "rds") {
  switch(
    type,
    rds = fs::path_package("transferice", "appdir", "cache", file_name, ext = type)
  )
}

# introduce filenaming convention
file_namer <- function(type, prefix, taxa, method = "count", trans = "unprocessed", 
                       viz = NULL, y = NULL, x = NULL) {
  
  stopifnot(prefix %in% c("raw", "prep", "train", "final"))
  if (type != "rds") stopifnot(viz %in% c("spatial", "xy"))
  if (type != "rds") stopifnot(all(!is.null(viz), !is.null(y), !is.null(x)))
  
  # core names
  nm <- paste(prefix, taxa, method, trans, sep = "_")
  
  # extend if needed
  if (type != "rds") {
    paste(nm, viz, y, x, sep = "_")
  } else {
    nm
  }

}
  
# make surethat caching dirs exist  
cache_dir <- function() {
  
  # path to package
  pkg_path <- fs::path_package("transferice")
  
  # if directories don't exist then create them
  
  # data 
  if (!fs::dir_exists(fs::path(pkg_path, "appdir", "cache"))) {
    fs::dir_create(pkg_path, "appdir", "cache")
  }
  
  # images
  if (!fs::dir_exists(fs::path(pkg_path, "www", "img"))) {
    fs::dir_create(pkg_path, "www", "img")
  }
  
  # videos
  if (!fs::dir_exists(fs::path(pkg_path, "www", "vid"))) {
    fs::dir_create(pkg_path, "www", "vid")
  }

}