pool <- get_pool()
# shiny app
runApp(fs::path_package("transferice", "appdir", "predict-module"), 
       host = "127.0.0.1", port = 5920, launch.browser = FALSE)