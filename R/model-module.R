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
            fixedRow(
              selectInput(NS(id, "comp"), "Components", choices =  1:9),
              selectInput(
                NS(id, "parm"), 
                "Parameter selection", 
                choices = abbreviate_vars(parms)
              )
            ),

            fluidRow(
              column(4, imageOutput(NS(id, "rpart"))),
              column(5, imageOutput(NS(id, "spart")))
            )
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

    mdls <- reactive({
      # extract model data per fold
      cv_model_extraction_mem(tun())
    })
    
    # the different CV parts (regression)
    output$rpart <- renderImage({
      # regression folds
      filename <- ggpartial(
        partials = xc,
        tune = input$comp,
        pred = !!rlang::sym(paste(input$parm, temp[temp == input$temp], sep = "_")),
        type = "regression"
      )
      # Return a list containing the filename
      list(src = filename)
    },
    deleteFile = FALSE
    )
    
    # the different CV parts (spatial)
    output$spart <- renderImage({
      # spatial folds
      # base map projection (replace later on)
      base <- oceanexplorer::get_NOAA("temperature", 1, "annual") |>
        oceanexplorer::filter_NOAA(depth = 0) |>
        stars::st_warp(crs = 4326) |> 
        stars::st_downsample(n = 5)
      filename <- ggpartial(
        partials = xc,
        tune = input$comp,
        pred = !!rlang::sym(paste(input$parm, temp[temp == input$temp], sep = "_")),
        type = "spatial",
        base_map = base
      )
      # Return a list containing the filename
      list(src = filename)
    },
    deleteFile = FALSE
    )


  })
}
