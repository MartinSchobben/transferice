  # get db
  pool <- get_pool()
  
  ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "spacelab"),
    withMathJax(),
    # section below allows in-line LaTeX via $ in mathjax.
    tags$script(HTML("MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\{','\\}']]}});")),
    model_ui("model")
  )
  
  server <- function(input, output, session) {
    
    thematic::thematic_shiny(bg = "transparent", fg = "black")
    ggplot2::theme_set(ggplot2::theme_classic())
    model_server("model")
  }
  
  shinyApp(ui, server)
  
