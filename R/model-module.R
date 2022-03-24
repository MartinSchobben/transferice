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
              imageOutput(NS(id, "part"))
            ),
            tabPanel(
              "validation",
              imageOutput(NS(id, "final"))
            ),
            header = tagList(
              tags$br(),
              selectInput(
                NS(id, "parm"), 
                "Parameter selection", 
                choices = abbreviate_vars(parms)
              )
            ),
            footer = shinyWidgets::materialSwitch(
              NS(id, "toggle"), 
              "project on map"
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
    
    dat <- reactive({
      # for now data is from a local source but later-on it should be sourced 
      # from the explo-module
      dinodat
    })
    
    # resample
    splt <- eventReactive(input$run, {
      set.seed(1)
      splt <- rsample::initial_split(dat(), prop = 0.75) 
    })
    
    # model
    mdl <- eventReactive(input$run, {
      parsnip::linear_reg() |>
        parsnip::set_engine('lm') |>
        parsnip::set_mode('regression')
    })
    
    # workflow
    wfl <- eventReactive(input$run, {
      transferice_workflow(dat(), mdl())
    })
    
    # tuning
    tun <- eventReactive(input$run, {
      set.seed(2)
      transferice_tuning_mem(splt(), wfl())
    })
    
    # extract sub-models per fold
    parts <- reactive({
      cv_model_extraction_mem(tun())
    })
    
    # reactivevalues to store image path
    pth <- reactiveValues(part = NULL, fit = NULL)
    
    
    base <- reactive({
      # parameter
      pm <- parms[abbreviate_vars(parms) == input$parm]
      # base map (later on replace this)
      oceanexplorer::get_NOAA(pm, 1, "annual") |>
        oceanexplorer::filter_NOAA(depth = 0) |>
        stars::st_warp(crs = 4326) |>
        stars::st_downsample(n = 5)
    })
    
    observe({
      # regression folds
      filename <- ggpartial(
        partials = parts(), 
        tune = input$comp,
        pred = !!rlang::sym(paste(input$parm, temp[temp == "an"], sep = "_")),
        type = if (isTruthy(input$toggle))  "spatial" else "regression",
        base_map = base()
      )
      
      # save in `reactiveValue`
      pth$part <- filename
    })
    
    # the different CV parts (regression)
    output$part <- renderImage({
      req(pth$part)
      # filename
      list(src = pth$part)
    },
    deleteFile = FALSE
    )
    
    # finalize model
    final <- reactive({
      final_wfl <- tune::finalize_workflow(
        wfl(),
        tibble::tibble(num_comp = input$comp)
      )
      tune::last_fit(
        final_wfl,
        split =  splt(),
        metrics = 
          yardstick::metric_set(
            transferice::rmsre, 
            yardstick::rmse, 
            yardstick::rsq
          )
      )
    })
    
    # the final model plotted as R-squared plot
    observe({
      # parameter
      pm <- abbreviate_vars(parms)[abbreviate_vars(parms) == input$parm]
      filename <- ggfit(
        final(), 
        abbreviate_vars(parms), 
        selected = pm,
        type = if (isTruthy(input$toggle))  "spatial" else "regression",
        base_map = base()
        )
      
      # save in `reactiveValues`
      pth$final <- filename
    })

    # the different CV parts (regression)
    output$final <- renderImage({
      req(pth$final)
      # filename
      list(src = pth$final)
    },
    deleteFile = FALSE
    )
    
    # observe(message(glue::glue("{final()}")))
    # side panel (sub) model metrics
    # output$tuning <- renderText({"Model tuning"}) |> 
    #   bindEvent(parts())
    # 
    # output$metrics <- renderPlot({
    #   tune::collect_metrics(tun(), summarize = FALSE) |> 
    #     dplyr::mutate(slct = dplyr::if_else(.data$num_comp == input$comp, TRUE, FALSE)) |>
    #     filter(.estimate < 100) |>
    #     ggplot2::ggplot(
    #       ggplot2::aes(
    #         x = .data$num_comp, 
    #         y = .data$.estimate, 
    #         group = .data$num_comp, 
    #         fill = .data$slct
    #       )
    #     ) +
    #     ggplot2::geom_boxplot(show.legend = FALSE) +
    #     ggplot2::scale_x_discrete("components", labels = NULL) +
    #     ggplot2::labs(y = "RMSRE")
    # },
    # width = 250, 
    # height = 250
    # ) 
    # 
    output$control <- renderUI(sliderInput(NS(id, "comp"), "Components", 1, 10, 1))

  })
}
