ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "spacelab"),
  explo_ui("explo")
)

server <- function(input, output, session) {
 
  thematic::thematic_shiny(bg = "transparent", fg = "black")
  
  explo_server("explo")
}

shinyApp(ui, server)

