# get db
pool <- get_pool() 

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "spacelab"),
  explo_ui("explo")
)

server <- function(input, output, session) {
 
  thematic::thematic_shiny(bg = "transparent", fg = "black")
  ggplot2::theme_set(ggplot2::theme_classic())
  explo_server("explo")
}

shinyApp(ui, server)

