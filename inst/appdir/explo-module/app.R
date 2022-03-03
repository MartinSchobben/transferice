# get db
pool <- get_pool() 

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "slate"),
  explo_ui("explo")
)

server <- function(input, output, session) {
 
  thematic::thematic_shiny()
  ggplot2::theme_set(ggplot2::theme_minimal())
  explo_server("explo")
}

shinyApp(ui, server)

