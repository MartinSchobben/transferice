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
          tabsetPanel(
            id = ns("sl"),
            tabPanel(
              "temporal",
              br(),
              actionLink(
                ns("stdhelper"), 
                "", 
                icon = icon('question-circle')
              ),
              uiOutput(ns("std")),
              uiOutput(ns("rng"))
            ),
            tabPanel(
              "spatial"
            ),
            footer = 
              tagList(
                fluidRow(
                  column(
                    width = 6,
                    actionLink(
                      ns("printhelper"), 
                      "", 
                      icon = icon('question-circle')
                    ),
                    actionButton(ns("print"), "Print result")
                  ),
                  column(
                    width = 6, 
                    actionLink(
                      ns("codehelper"), 
                      "", 
                      icon = icon('question-circle')
                    ),
                    actionButton(ns("code"), "Expose code")
                  )
                ),
                tags$br(),
                wellPanel(uiOutput(ns("helptext")))
              )
          ),
          style = "height:100%;"
        )
      ),
      column(
        width = 6,
        shinycssloaders::withSpinner(plotOutput(ns("pred"))),
        plotOutput(ns("strat"), height = 100)
      ),
      column(
        width = 3,
        wellPanel(
          tags$h5("Results"), 
          tags$hr(),
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
    
    # sidebar helptext
    output$helptext <- renderUI({
      if (input$sl == "temporal") {
        helpText(paste0("Predict temporal trends for the selected oceanographic."))
      } else if (input$sl == "spatial") {
        helpText(paste0("Predict spatial trends for the selected oceanographic."))
      }
    })
    
  
    # new data for predicting
    new <- reactive({

      calc_taxon_prop(pool, "predict") |> tidyr::drop_na(-age_ma)
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
        "Filter range (My)", 
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
    
    # filter dataset
    filter <- reactive({
      
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
      # return
      dt
    })
      
    # transform to fit format of model
    trans <- reactive({
      if (stringr::str_detect(model_id(), "pca|pls")) {
        # if the recipe has a pca step then reduce dimensions
        reduce_taxa(model_id(), filter()) 
      } else {
        # otherwise impute missing taxa
        impute_taxa(model_id(), filter(), pm(), return_type = "impute")
      }
    })  
    
    time <- reactive({
      # add the age in Myr
      age_finder(trans()$new_data)
    })
    
    p <- reactive({
      req(trans(), time(), input$int)
      
      ggpredict(
        trans()$model, 
        time()[time()$age_ma < input$int, , drop = FALSE],
        pm()
      )  |> chrono_bldr()
    })
    # plot the predicted trend
    output$pred <- renderPlot({
      req(trans(), time(), input$int)
      p()[[1]]
      })
    output$strat <- renderPlot({
      req(trans(), time(), input$int)
      gridExtra::grid.arrange(p()$chrono)

    })
    
    # print how many fossil taxa are present int raining set
    output$comp <- renderUI({

      req(time(), input$int)
      
      # correct for time interval of selection
      time <- time()[time()$age_ma < input$int, , drop = FALSE]
      time <- dplyr::select(time, .data$sample_id, .data$age_ma)
      dt <- dplyr::inner_join(dplyr::select(new(), -.data$age_ma), time, by = "sample_id")
      

      # remove na rows
      dt <- tidyr::drop_na(dt)
      # remove zero columns
      fn <- function(x) is.numeric(x) && sum(x) != 0
      dt <- dplyr::select(dt, tidyselect::vars_select_helpers$where(~fn(.x)))

      print_predict(
        model_id(),
        dt,
        pm()
      )
    })
    
  })
}