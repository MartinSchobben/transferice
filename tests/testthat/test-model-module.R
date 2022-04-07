# # get db
# pool <- get_pool()
# resources
addResourcePath('vid', system.file('www/vid', package = 'transferice'))
addResourcePath('img', system.file('www/img', package = 'transferice'))
options(shiny.launch.browser = .rs.invokeShinyWindowExternal)
# shiny app
runApp(fs::path_package("transferice", "appdir", "model-module"))