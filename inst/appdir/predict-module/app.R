ui <- fluidPage(
  predict_ui("predict")
)

server <- function(input, output, session) {
  
  thematic::thematic_shiny(bg = "transparent", fg = "black")
  predict_server("predict")
}

shinyApp(ui, server)
  
