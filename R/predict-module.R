#' Shiny predict module
#'
#' @param id namespace
#' @param model_id reactive value containing a character string for the selected
#'  model.
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
          uiOutput(ns("std")),
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
predict_server <- function(id, model_id) { 
  
  stopifnot(is.reactive(model_id))
  
  moduleServer(id, function(input, output, session) {
  
    # new data for predicting
    new <- reactive({
      
      calc_taxon_prop(pool, "predict") |> 
        tidyr::drop_na()
    })
    
    # render study selection
    output$std <- renderUI({
      std <- unique(new()$source_citation)
      selectInput(NS(id, "study"), "Select study", std)
    })
    
    # render slider for age or depth
    output$rng <- renderUI({
      req(time())
      sliderInput(
        NS(id, "int"), 
        "Filter range", 
        round(min(time()$age_ma, na.rm = TRUE), -1),
        round(max(time()$age_ma, na.rm = TRUE), -1),
        round(max(time()$age_ma, na.rm = TRUE), -1),
        round = -1
      )
    }) 
    
    # parameter
    pm <- reactive({
      pm <- strsplit(model_id(), "_")[[1]][3]
      av <- strsplit(model_id(), "_")[[1]][4]
      paste(pm, av, sep = "_")
    })
    
    # transform to fit format of model
    fossil <- reactive({
      
      req(input$study)
      dt <- new() |> 
        dplyr::filter(.data$source_citation == input$study)
 
      if (stringr::str_detect(model_id(), "genera")) {

        # all variables and their roles
        vars <- role_organizer(dt, pm())
        
        # taxa
        txa <- vars[names(vars) == "predictor"] |> unname()
        
        # variables not used in transform
        terms <- vars[!names(vars) %in% c("predictor", "outcome")]
        
        dt <- recipes::recipe(
          x = dt, 
          vars = vars[names(vars) != "outcome"], 
          roles = names(vars)[names(vars) != "outcome"]
        ) |>
          step_taxo(dplyr::any_of(unname(terms))) |>
          recipes::prep(training = dt) |>
          recipes::bake(NULL)
        
      }

     
      # if the recipe has a pca step then reduce dimensions
      if (stringr::str_detect(model_id(), "pca|pls")) {
        
        reduce_taxa(model_id(), dt) 
      } else {
        # otherwise impute missing taxa
        impute_taxa(model_id(), dt, pm(), return_type = "impute")
      }
    })  
    
    # metadata (notably age or depth)
    time <- reactive({
      req(fossil())
      age_finder(fossil()$new_data)
    })
    
    output$pred <- renderPlot({
      req(fossil(), time(), input$int)

      ggpredict(
        fossil()$model, 
        time()[time()$age_ma < input$int, , drop = FALSE]
      )
    })
    
    # output$comp <- renderUI({
    #   print_predict(
    #     final(), 
    #     fossil()[time()$age_ma < req(input$int), , drop = FALSE], 
    #     pm()
    #     )
    #   })
    
  })
}