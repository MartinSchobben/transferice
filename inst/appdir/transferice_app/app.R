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
  
  # counter
  num <- reactive({
    # browser()
    
    dt <- try(
      fs::path_package("transferice", "appdir", "cache", data_id(), ext = "rds"),
      silent = TRUE
    )
    mdl <- try(
      fs::path_package("transferice", "appdir", "cache", model_id(), ext = "rds"),
      silent = TRUE
    )
    
    # initial 
    n <- 2
    # browser()
    
    if (!inherits(dt, "try-error")) {
      n <- 3
    }
    if (all(!inherits(dt, "try-error"), !inherits(mdl, "try-error"))) {
      n <- 4
    }
    # return
    n
   
  })
  
  # wizard
  observe({wizard_server("wizard", num())})
  
  # data selection
  data_id <-  explo_server("explo")
  
  # model training
  model_id  <- model_server("model", data_id)
  
  # prediction
  predict_server("predict", model_id)

}

shinyApp(ui, server)


 