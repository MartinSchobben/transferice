# # get db
# pool <- get_pool() 

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "slate"),
  model_ui("model")
)

server <- function(input, output, session) {
  
  thematic::thematic_shiny()
  ggplot2::theme_set(ggplot2::theme_minimal())
  model_server("model")
}

shinyApp(ui, server)

