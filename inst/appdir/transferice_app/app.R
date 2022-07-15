
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
tabs <- tagList(titlepage, explo_ui("explo"), model_ui("model"), predict_ui("predict"))
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
  wizard_server("wizard", 4)
  data_id <- explo_server("explo")
  observe(message(glue::glue("{data_id()}")))
  model_id <- model_server("model", data_id)
  predict_server("predict", model_id)
  observe(message(glue::glue("{model_id()}")))
}

shinyApp(ui, server)


 