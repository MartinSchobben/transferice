#' Shiny model module
#'
#' @param id namespace
#'
#' @return
#' @export
model_ui <- function(id) {
  tagList(
    # Application title
    titlePanel("Training transfer functions"),
    
    # Parameter and statistic selection
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(
          id = NS(id, "school"),
          tabPanel("frequentist"),
          tabPanel("bayesian"),
          footer = tagList(
            h3("Site selection"),
            selectInput(NS(id, "area"), "Region", choices = geo),
            selectInput(NS(id, "temp"), "Averaging", choices = temp[1]),
            actionButton(NS(id, "model"), "Run model")
          )
        )
      ),
      
      # Show a plot of the model outcome and evaluation
      mainPanel(
        tabsetPanel(
          id = NS(id, "specs"),
          tabPanel(
            "tuning",
            sliderInput(
              NS(id, "fold"), 
              "CV folds", 
              min = 1, 
              max = 10, 
              value = 1, 
              step = 1, 
              animate = animationOptions(interval = 800, loop = TRUE)
              ),
            selectInput(NS(id, "ncomp"), "Number of components", choices =  1:9),
            selectInput(NS(id, "comp"), "Selected component", choices =  1:9),
            selectInput(
              NS(id, "parm"), 
              "Parameter selection", 
              choices = abbreviate_vars(parms)
            ),
            #textOutput(NS(id, "table"))
            plotOutput(NS(id, "part"))
          ),
          tabPanel(
            "validation"
            
          )
        )
      )
    )
  )
}
#' @rdname 
#' 
#' @export
model_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # use seed for reproducibility
    # seed <- reactiveVal(NULL)
    # observeEvent(input$model, {
    #   seed(sample(2 ^ 31 - 1, 1))
    # },
    # once = TRUE
    # )

    dat <- reactive({
      # for now data is from a local source but later-on it should be sourced 
      # from the explo-module
      dinodat
    })
    
    tun <- eventReactive(input$model, {
      set.seed(1)
      # resample
      splt <- rsample::initial_split(dat(), prop = 0.75) 
      # recipe
      rcp <- transferice_recipe(dat())
      # model
      mdl <- parsnip::linear_reg() |>
        parsnip::set_engine('lm') |>
        parsnip::set_mode('regression')
      # tuning
      set.seed(2)
      transferice_tuning_mem(splt, rcp, mdl)
    })

    part <- reactive({
      # extract model data per fold
      cv_model_extraction_mem(tun())
    })
      
    # the different CV parts
    output$part <- renderPlot({
      vars <- paste(
        abbreviate_vars(parms), # variable
        temp[temp == input$temp], # temporal averaging
        sep = "_"
      )
      # this keeps environmental variable axis limits equal for different folds
      rng <- dplyr::select(dat(), dplyr::any_of(vars))
      # plot the different folds
      part <- part() |> 
        dplyr::filter(
          id == sprintf("Fold%02d", input$fold), 
          num_comp == input$ncomp
          )  
      sprm <- paste(
        input$parm, # variable
        temp[temp == input$temp], # temporal averaging
        sep = "_"
      )
      ggpartial(part$.input[[1]], part$.output[[1]], rng, !!rlang::sym(paste0("PC", input$comp)), !!rlang::sym(sprm))
    })
    
  })
}


