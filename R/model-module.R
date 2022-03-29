#' Shiny model module
#'
#' @param id namespace
#'
#' @return
#' @export
model_ui <- function(id) {
  # namespace
  ns <- NS(id)
  # elements
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
              selectInput(
                NS(id, "model"), 
                "Model type", 
                choices = c("linear model (OLS)", 
                            "linear model (GLS)")
              ),
            ),
            tabPanel("bayesian"),
            header = tagList(
              tags$br(), 
              selectizeInput(
                ns("scale"), 
                "Predictor transforming", 
                choices = c("center", "logit"), # variance-stabilizing transformations 
                multiple = TRUE,
                options = list(
                  placeholder = 'Select to apply',
                  onInitialize = I('function() { this.setValue(null); }')
                )
              ),
              shinyBS::bsTooltip(
                id = ns("scale"), 
                title = "Scaling makes comparisons between predictors more straightforward.",
                trigger = "hover"
                # placement = "right", 
                # options = list(container = "body")
              ),
              selectizeInput(
                ns("dims"), 
                "Dimension reduction", 
                choices = c("PCA", "PLS"), 
                options = list(
                  placeholder = 'Select to apply',
                  onInitialize = I('function() { this.setValue(null); }')
                )
              ),
              shinyBS::bsTooltip(
                id = ns("dims"), 
                title = "The many species found at each site require a reduction of dimensions.",
                # placement = "right", 
                options = list(container = "body")
              ),
              uiOutput(NS(id, "control"))
            ),
            footer = tagList(
              tags$br(), 
              tags$br(), 
              actionButton(ns("run"), "Run model")
            )
          )
        )
      ),
      
      # Show a plot of the model outcome and evaluation
      column(
        width = 6, 
          tabsetPanel(
            id = ns("specs"),
            tabPanel(
              "tuning",
              imageOutput(ns("part"))
            ),
            tabPanel(
              "validation",
              imageOutput(ns("final"))
            ),
            header = tagList(
              tags$br(),
              fixedRow(
                selectInput(
                  ns("parm"), 
                  "Parameter selection", 
                  choices = abbreviate_vars(parms)
                ),
                selectInput(
                  ns("peek"), 
                  "Taxa selection",
                  choices = species_naming(pool)
                )
              )
            ),
            footer = shinyWidgets::materialSwitch(
              ns("toggle"), 
              "project on map"
            )
         )
      ),

      column(
        width = 3,
        wellPanel(
          tabsetPanel(
            id = ns("results"),
            header = tagList(tags$h5(textOutput(ns("results"))), tags$hr()),
            tabPanelBody(
              value = "tuning",
              plotOutput(ns("submetrics"))
            ),
            tabPanelBody(
              value = "validation",
              uiOutput(ns("finmetrics"))
            ),
            type = "hidden"
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
      transferice_workflow(
        dat(), 
        mdl(), 
        trans = input$scale, 
        dim_reduction = input$dims
      )
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
    
    # `reactiveValues` to store image path
    pth <- reactiveValues(part = NULL, fit = NULL)

    # base map (for kriging interpolation)
    base <- reactive({
      # parameter
      pm <- parms[abbreviate_vars(parms) == input$parm]
      # base map (later on replace this)
      oceanexplorer::get_NOAA(pm, 1, "annual") |>
        oceanexplorer::filter_NOAA(depth = 0) |>
        stars::st_warp(crs = 4326) |>
        stars::st_downsample(n = 5)
    })

    observe(message(glue::glue("{str(input$dims)}")))
    
    # different CV parts (regression)
    observe({
      # regression folds
      filename <- ggpartial(
        partials = parts(),
        pred = if (isTruthy(input$comp)) NULL else input$peek,
        tune = input$comp,
        out = !!rlang::sym(paste(input$parm, temp[temp == "an"], sep = "_")),
        type = if (isTruthy(input$toggle))  "spatial" else "regression",
        base_map = base(),
        preprocessor = paste(input$scale, sep = "_")
      )
      # save in `reactiveValues`
      pth$part <- filename
     
    })
    
    # CV animations
    output$part <- renderImage({
      req(pth$part)
      # filename
      list(src = pth$part)
    },
    deleteFile = FALSE
    )
    # 
    # # finalize model
    # final <- reactive({
    #   req(input$comp)
    #   # finalize workflow by selecting optimal sub-model
    #   final_wfl <- tune::finalize_workflow(
    #     wfl(),
    #     tibble::tibble(num_comp = input$comp)
    #   )
    #   # fit the final model
    #   tune::last_fit(
    #     final_wfl,
    #     split =  splt(),
    #     metrics = 
    #       yardstick::metric_set(
    #         transferice::rmsre, 
    #         yardstick::rmse, 
    #         yardstick::rsq
    #       )
    #   )
    # }) 
    # 
    # # final model plotted as R-squared plot
    # observe({
    #   # parameter
    #   pm <- abbreviate_vars(parms)[abbreviate_vars(parms) == input$parm]
    #   filename <- ggfit(
    #     final(), 
    #     abbreviate_vars(parms), 
    #     selected = pm,
    #     type = if (isTruthy(input$toggle))  "spatial" else "regression",
    #     base_map = base(),
    #     preprocessor = paste(input$scale, sep = "_")
    #   )
    #   # save in `reactiveValues`
    #   pth$final <- filename
    # })
    # 
    # # final model image
    # output$final <- renderImage({
    #   req(pth$final)
    #   # filename
    #   list(src = pth$final)
    # },
    # deleteFile = FALSE
    # )
    # 
    # # side panel (sub) model metrics
    # output$results <- renderText({
    #   if (input$specs == "tuning") {
    #     "Optimize model"
    #   } else if (input$specs == "validation") {
    #     "Model performance"
    #   }
    # }) 
    # 
    # # show locations selection controls when data loaded
    # observe({
    #   updateTabsetPanel(session, "results", selected = input$specs)
    # })
    # 
    # # collect sub-model metrics
    # submet <- reactive({     
    #   req(input$comp)
    #   tune::collect_metrics(tun(), summarize = FALSE) |>
    #     dplyr::mutate(
    #       slct = dplyr::if_else(.data$num_comp == input$comp, TRUE, FALSE)
    #       ) |>
    #     dplyr::filter(.data$.estimate < 100) # remove extreme outliers
    #   }) 
    # 
    # # plot the ssubmodel metrics as a boxplot
    # output$submetrics <- renderPlot({
    #   req(input$comp)
    #   ggplot2::ggplot(
    #     submet(),
    #     ggplot2::aes(
    #       x = .data$num_comp,
    #       y = .data$.estimate,
    #       group = .data$num_comp,
    #       fill = .data$slct
    #     )
    #   ) +
    #   ggplot2::geom_boxplot(show.legend = FALSE) +
    #   ggplot2::scale_x_discrete("components", labels = as.character(1:10), breaks = 1:10) +
    #   ggplot2::labs(y = "RMSRE")
    # },
    # width = 250,
    # height = 250
    # ) 
    # 
    # # final model metric as table in sidepanel
    # output$finmetrics <- renderUI({
    #   # all collected metrics
    #   mts <- tune::collect_metrics(final())
    #   a <- print_metric(mts, "rsq")
    #   b <- print_metric(mts, "rmse")
    #   c <- print_metric(mts, "rmsre")
    #   # print with mathjax
    #   withMathJax(paste(a, b, c, sep = "\\"))
    # }) 
    # 
    # render controller based on tuning results
    output$control <- renderUI({
      # if (length(input$dims) == 0) {
        
      if (input$dims == "PCA") {
        sliderInput(NS(id, "comp"), "Principal components", 1, 10, 1, ticks = FALSE)
      }
    }) 

  })
}
