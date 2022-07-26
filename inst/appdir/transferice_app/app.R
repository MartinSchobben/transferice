# make a title page as first page of the wizard
titlepage <- fluidPage(
  fluidRow(
    column(
      width = 12,
      br(), 
      br(),
      br(),
      wellPanel(
        div(h1("Transferice"), align ="center"), 
        div(h2("Reconstructing the Past with Machine Llearning"), align = "center")
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      br(), 
      br(),
      br(),
      div(img(src = fs::path("img", "oceanice-logo", ext = "jpg")), align = "center")
    )
  )
)

# different wizard panels
tabs <- tagList(titlepage, explo_ui("explo"), model_ui("model"), predict_ui("predict"))
# pool <- get_pool()

ui <- fluidPage(

  theme = bslib::bs_theme(bootswatch = "spacelab"),
  withMathJax(),
  # section below allows in-line LaTeX via $ in mathjax.
  tags$script(HTML("MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\{','\\}']]}});")),
  wizard_ui("wizard", tabs)

)

server <- function(input, output, session) {
  
  thematic::thematic_shiny(bg = "transparent", fg = "black")
  
  file <- reactiveValues(data = NULL, model = NULL, n = 2)
  observe({
    file$data <- fs::path_package("transferice", "appdir", "cache", 
                                  data_id(), ext = "rds")
    file$model <- fs::path_package("transferice", "appdir", "cache", 
                                   model_id(), ext = "rds")
    if (file_checker(model_id(), file$model)) {
      file$n <- 4 
    } else if (file_checker(data_id(), file$data)) {
      file$n <- 3
    }
  })
  
  # wizard
  observe(wizard_server("wizard", file$n))
  
  # data selection
  data_id <- explo_server("explo")
  # observe(message(glue::glue("{data_id()}")))
  # model training
  model_id <- model_server("model", data_id)
  # prediction
  predict_server("predict", model_id)
  # observe(message(glue::glue("{model_id()}")))
}

shinyApp(ui, server)


 