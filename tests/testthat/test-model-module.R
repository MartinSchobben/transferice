# get db
pool <- get_pool()
# shiny app
runApp(fs::path_package("transferice", "appdir", "model-module"))