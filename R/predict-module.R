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
          selectInput(ns("study"), "Select study", c("Brinkhuis et al., 2003")),
          uiOutput(ns("rng")),
          style = "height:100%;"
        )
      ),
      column(
        plotOutput(ns("pred")),
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
predict_server <- function(id, model_id) { # data id is based on query use R6 in future
  
  stopifnot(is.reactive(model_id))
  
  moduleServer(id, function(input, output, session) {
  
    # selected workflow
    final <- reactive({
      readRDS(fs::path_package("transferice", "appdir", "cache", model_id(), ext = "rds")) 
    })
    
    # the new data
    new <- reactive({
      fossil_dinodat
    })
    

    # parameter
    pm <- reactive({
      pm <- strsplit(model_id(), "_")[[1]][3]
      av <- strsplit(model_id(), "_")[[1]][4]
      paste(pm, av, sep = "_")
    })
    
    # render slider for age or depth
    output$rng <- renderUI({
      sliderInput(
        NS(id, "int"), 
        "Filter range", 
        round(min(time()$age_ma), -1),
        round(max(time()$age_ma), -1),
        round(max(time()$age_ma), -1),
        round = -1
      )
    })
    

    fossil <- reactive({
      # for now data is from a local source but later-on it should be sourced 
      # from the explo-module
      dt <- new() 
 
      if (stringr::str_detect(model_id(), "genera")) {

        # all variables and their roles
        vars <- role_organizer(dt, pm(), temporal = c("depth", "sample_id"))
        
        # taxa
        txa <- vars[names(vars) == "predictor"] |> unname()
        
        # variables not used in transform
        terms <- vars[!names(vars) %in% c("predictor", "outcome")]
        
        dt <- recipes::recipe(x = dt, vars = vars[names(vars) != "outcome"], roles = names(vars)[names(vars) != "outcome"]) |>
          step_taxo(dplyr::any_of(unname(terms))) |>
          recipes::prep(training = dt) |>
          recipes::bake(NULL)
        
      }
      
      # recipe
      rcp <- final()$.workflow[[1]] |> workflows::extract_preprocessor()
    
      # if the recipe has a pca step then do not impute
      if (!"pca" %in% rcp$type) {
        impute_taxa(final(), dt, pm(), return_type = "names")
      } else {
        dt
      }
    })  
    
  
    # metadat (notably age or depth)
    time <- reactive({

      age_finder(fossil()) |> dplyr::select(sample_id, age_ma)
    })
    
    output$pred <- renderPlot({
      ggpredict(
        final(), 
        fossil()[time()$age_ma < req(input$int), , drop = FALSE], 
        time()
      )
    })
    
    output$comp <- renderUI({
      print_predict(
        final(), 
        fossil()[time()$age_ma < req(input$int), , drop = FALSE], 
        pm()
        )
      })
    
  })
}