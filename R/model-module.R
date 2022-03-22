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
    fluidRow(
      column(
        width = 3,
        wellPanel(
          tabsetPanel(
            id = NS(id, "school"),
            tabPanel(
              "frequentist",
           
              selectInput(NS(id, "model"), "Model type", choices = c("linear model")),
            ),
            tabPanel("bayesian"),
            header = tagList(tags$br(), selectInput(NS(id, "feature"), "Feature selection", choices = c("PCA", "logit"), selected = c("PCA", "logit"), multiple = TRUE)),
            footer = actionButton(NS(id, "run"), "Run model")
          )
        )
      ),
      
      # Show a plot of the model outcome and evaluation
      column(
        width = 6, 
          tabsetPanel(
            id = NS(id, "specs"),
            tabPanel(
              "tuning",
              tags$br(),
              selectInput(
                NS(id, "parm"), 
                "Parameter selection", 
                choices = abbreviate_vars(parms)
              ),
              shinyWidgets::materialSwitch(
                NS(id, "toggle"), 
                "project on map"
              ),
              conditionalPanel(
                condition = "input.toggle==false",
                imageOutput(NS(id, "rpart")),
                ns = NS(id)
              ),
              conditionalPanel(
                condition = "input.toggle==true",
                imageOutput(NS(id, "spart")),
                ns = NS(id)
              )
            ),
            tabPanel(
              "validation"
            )
         )
      ),

      column(
        width = 3,
        wellPanel(
          tags$h4(textOutput(NS(id, "tuning"))),
          uiOutput(NS(id, "control")),
          plotOutput(NS(id, "metrics"))
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
    
    tun <- eventReactive(input$run, {
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

    parts <- reactive({
      # extract model data per fold
      cv_model_extraction_mem(tun())
    })
    
    # the different CV parts (regression)
    output$rpart <- renderImage({
      # regression folds
      filename <- ggpartial(
        partials = parts(),
        tune = input$comp,
        pred = !!rlang::sym(paste(input$parm, temp[temp == "an"], sep = "_")),
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
        partials = parts(),
        tune = input$comp,
        pred = !!rlang::sym(paste(input$parm, temp[temp == "an"], sep = "_")),
        type = "spatial",
        base_map = base
      )
      # Return a list containing the filename
      list(src = filename)
    },
    deleteFile = FALSE
    )
    
    output$tuning <- renderText({"Model tuning"}) |> 
      bindEvent(parts())
    
    # side panel (sub) model metrics
    output$metrics <- renderPlot({
      tune::collect_metrics(tun(), summarize = FALSE) |> 
        dplyr::mutate(slct = dplyr::if_else(.data$num_comp == input$comp, TRUE, FALSE)) |>
        filter(.estimate < 100) |>
        ggplot2::ggplot(
          ggplot2::aes(
            x = .data$num_comp, 
            y = .data$.estimate, 
            group = .data$num_comp, 
            fill = .data$slct
          )
        ) +
        ggplot2::geom_boxplot(show.legend = FALSE) +
        ggplot2::scale_x_discrete("components", labels = NULL) +
        ggplot2::labs(y = "RMSRE")
    },
    width = 250, 
    height = 250
    ) 
    
    output$control <- renderUI(sliderInput(NS(id, "comp"), "Components", 1, 10, 1)) #|>
    #   bindEvent(parts(), ignoreInit = TRUE)

  })
}
