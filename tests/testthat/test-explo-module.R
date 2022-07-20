pool <- get_pool()
addResourcePath('img', system.file('www/img', package = 'transferice'))
# shiny app
runApp(fs::path_package("transferice", "appdir", "explo-module"), 
       host = "127.0.0.1", port = 5918, launch.browser = FALSE)