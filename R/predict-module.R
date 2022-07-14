#' Shiny predict module
#'
#' @param id namespace
#'
#' @return
#' @export
predict_ui <- function(id) {
  # namespace
  ns <- NS(id)
  # elements
  tagList(
    
    # Application title
    titlePanel("Predict the Past"),
    
    # Parameter and statistic selection
    fluidRow(
      column(
        width = 3,
        wellPanel(
          selectInput(ns("study"), "Study", c("Brinkhuis et al.,")),
          style = "height:100%;"
        )
      ),
      column(
        width = 6
      ),
      column(
        width = 3,
        wellPanel(
          div(uiOutput(ns("comp")), style = "text-align: center;"),
          style = "height:100%;"
        )
      )
    )
  )
}
#' @rdname 
#' 
#' @export
predict_server <- function(id, model_id = "model") { # data id is based on query use R6 in future
  moduleServer(id, function(input, output, session) {
  
    final <- reactive({
      readRDS("~/Documents/work/code/transferice/inst/appdir/cache/validation_dinocyst_t_an_global_count_species_rename_logit_center_lm.rds")
    })
    
    # extract workflow
    wfl <- reactive({
      final()$.workflow[[1]] |> workflows::extract_preprocessor()
    })
    
    new_data <- reactive({
      # if the recipe has a pca step then do not impute
      if ("pca" %in% wfl()$type) {
        impute_taxa(final(), fossil_dinodat, "t_an")
      } else {
        fossil_dinodat
      }
    })  
    
    output$comp <- renderUI({print_predict(final(), fossil_dinodat, "t_an")})
    
  })
}