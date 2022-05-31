addResourcePath('img', system.file('www/img', package = 'transferice'))
addResourcePath('vid', system.file('www/vid', package = 'transferice'))
pool <- get_pool()
# shiny app
runApp(fs::path_package("transferice", "appdir", "transferice_app"), 
       host = "127.0.0.1", port = 5912, launch.browser = FALSE)