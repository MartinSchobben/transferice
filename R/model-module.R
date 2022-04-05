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
    
    # use shinyjs
    shinyjs::useShinyjs(), 
    # shinyjs::inlineCSS(css),
    
    # waiter
    waiter::use_waiter(),
    
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
                choices = c("log", "logit", "normalize"), # variance-stabilizing transformations 
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
              fluidRow(
                column(6, actionButton(ns("reset"), "Reset model")),
                column(6, actionButton(ns("run"), "Train model"))
              )
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
              "engineering",
              imageOutput(ns("eng"))
            ),
            tabPanel(
              "tuning",
              #uiOutput(ns("part"))
              tags$video(src = 'folds_regression__t_an_1.mkv', controls = "controls")
              #imageOutput(ns("part"))
            ),
            tabPanel(
              "validation",
              imageOutput(ns("final"))
            ),
            header = tagList(
              tags$br(),
              fluidRow(
                column(4,
                  selectInput(
                    ns("parm"), 
                    "Parameter selection", 
                    choices = abbreviate_vars(parms)
                  )
                  ),
                column(4,
                  selectInput(
                    ns("temp"), 
                    "Averaging period", 
                    choices = temp
                  )
                ),
                column(4,
                  selectInput(
                    ns("peek"), 
                    "Taxa selection",
                    choices = species_naming(pool)
                  )
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
              value = "help"#,
              # plotOutput(ns("submetrics"))
            ),
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
    
#-------------------------------------------------------------------------------
# misc elements
#-------------------------------------------------------------------------------    
    
    # `reactiveValues` to store image path
    file <- reactiveValues(path = NULL)
    
    # selected parameter
    pm <- reactive({rlang::sym(paste(input$parm, temp[temp == "an"], sep = "_"))})
    
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
    
    # enable disable tabs based on whether run model has been clicked
    # https://stackoverflow.com/questions/64324152/shiny-disable-tabpanel-using-shinyjs?noredirect=1&lq=1
    observe({
      shinyjs::disable(selector = '.nav-tabs a[data-value="tuning"')
      shinyjs::disable(selector = '.nav-tabs a[data-value="validation"')
    })
    observeEvent(input$run, {
      shinyjs::enable(selector = '.nav-tabs a[data-value="tuning"')
      shinyjs::enable(selector = '.nav-tabs a[data-value="validation"')
    })

#-------------------------------------------------------------------------------    
# modelling
#------------------------------------------------------------------------------- 
    
    dat <- reactive({
      # for now data is from a local source but later-on it should be sourced 
      # from the explo-module
      dinodat
    })
    
    # re-sample
    splt <- reactive({
      set.seed(1)
      splt <- rsample::initial_split(dat(), prop = 0.75) 
    })
    
    # recipe
    rcp <- reactive({
      transferice_recipe(dat(), trans = input$scale, dim_reduction = input$dims)
    }) 
    
    # model
    mdl <- reactive({
      parsnip::linear_reg() |>
        parsnip::set_engine('lm') |>
        parsnip::set_mode('regression')
    })
    
    # workflow
    wfl <- reactive({
      workflows::workflow() |>
        workflows::add_recipe(rcp()) |>
        workflows::add_model(mdl())
    })
    
    # tuning
    tun <- eventReactive(input$run, {
      # waiter
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())
      # tuning
      set.seed(2)
      transferice_tuning_mem(splt(), wfl())
    })
    
    # finalize model (with or without tuning)
    final <- eventReactive(input$run, {
      transferice_finalize(splt(), wfl(), input$comp)
    })

#-------------------------------------------------------------------------------
# generate figures
#-------------------------------------------------------------------------------

    # feature engineering and training
    observe({
      # waiter
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())
      # initial split or fitted/tuned data
      if (input$specs == "engineering") {
        dat <- splt() 
      } else if (input$specs == "tuning") {
        dat <- tun()
      } else if (input$specs == "validation") {
        dat <- final()
      }
      # tuned data requires a dimension variable selection
      if (isTruthy(input$dims))  req(input$comp)
      # create plot or animation (save in `reactiveValues`)
      file$path <- ggpartial(
        obj = dat,
        recipe = rcp(),
        pred = input$peek,
        tune = input$comp,
        out = !! pm(),
        type = if (isTruthy(input$toggle))  "spatial" else "regression",
        base_map = base(),
        preprocessor = paste(input$scale, sep = "_")
      )
    })

    # cross plots
    output$eng <- renderImage(list(src = file$path), deleteFile = FALSE)
    # CV animations
    observe(message(glue::glue("{file.exists('folds_regression__t_an_1.mkv')}")))
    #output$part <- renderImage(list(src = file$path), deleteFile = FALSE)
    output$part  <- renderUI({
      tags$video(src = 'folds_regression__t_an_1.mkv', controls = "controls")
      # tags$iframe(
      #   width ="560",
      #   height ="315",
      #   seamless="seamless",
      #   src = 'folds_regression__t_an_1.mkv',
      #   frameborder = "0",
      #   allow = "accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture",
      #   allowfullscreen = NA
      # )
    })
    observe(message(glue::glue("{fs::path_file(file$path)}")))
    # final model image
    output$final <- renderImage(list(src = file$path), deleteFile = FALSE)
    
#-------------------------------------------------------------------------------    

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
      if (input$dims == "PCA") {
        sliderInput(NS(id, "comp"), "Principal components", 1, 10, 1, ticks = FALSE)
      }
    }) 

  })
}
