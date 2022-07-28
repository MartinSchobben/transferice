#' Launch the transferice app
#' 
#' The video animations of this app are in mkv format which is currently only 
#' supported by the latest versions for the Google Chrome browser.
#' 
#' @param port Numeric indicating the port number.
#'
#' @return Interactive shiny session in localhost
#' @export
transferice_app <- function(port = 5912) {
  
  # resources vids and images
  addResourcePath('img', system.file('www/img', package = 'transferice'))
  addResourcePath('vid', system.file('www/vid', package = 'transferice'))
  
  # the database
  rlang::env_bind(rlang::global_env(), pool = get_pool())
  
  # shiny app
  runApp(fs::path_package("transferice", "appdir", "transferice_app"), 
         host = "127.0.0.1", port = port, launch.browser = FALSE)
  
}