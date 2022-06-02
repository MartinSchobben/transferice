
titlepage <- fixedPage(
  column(
    width = 12,
    wellPanel(
      div(h1("Transferice"), align ="center"), 
      div(h2("Reconstructing the past with machine learning"), align ="center")
    )
  )
)

# different wizard panels
tabs <- tagList(titlepage, explo_ui("explo"), model_ui("model"))
pool <- get_pool()

ui <- fluidPage(

  theme = bslib::bs_theme(bootswatch = "spacelab"),
  withMathJax(),
  # section below allows in-line LaTeX via $ in mathjax.
  tags$script(HTML("MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\{','\\}']]}});")),
  wizard_ui("wizard", tabs)

)

server <- function(input, output, session) {
  
  thematic::thematic_shiny(bg = "transparent", fg = "black")
  wizard_server("wizard", 3)
  explo_server("explo")
  model_server("model")
}

shinyApp(ui, server)


 