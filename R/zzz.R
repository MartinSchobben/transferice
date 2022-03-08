.onLoad <- function(libname, pkgname){
  # caching dir app data
  cd <- cachem::cache_disk(fs::path_package(package = "transferice", "appdir"))
}
