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
              # variance-stabilizing transformations 
              selectizeInput(
                ns("scale"), 
                "Predictor transforming", 
                choices = c("log", "logit", "normalize"), 
                options = default_message()
              ),
              shinyBS::bsTooltip(
                id = ns("scale"), 
                title = "Scaling makes comparisons between predictors more straightforward.",
                trigger = "hover"
                # placement = "right", 
                # options = list(container = "body")
              ),
              # dimension reduction 
              selectizeInput(
                ns("dims"), 
                "Dimension reduction", 
                choices = c("PCA", "PLS"), 
                options = default_message()
              ),
              shinyBS::bsTooltip(
                id = ns("dims"), 
                title = "The many species found at each site require a reduction of dimensions.",
                # placement = "right", 
                options = list(container = "body")
              )
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
              plotOutput(ns("eng"))
            ),
            tabPanel(
              "tuning",
              uiOutput(ns("part"))
            ),
            tabPanel(
              "validation",
              imageOutput(ns("final"))
            ),
            header = tagList(
              tags$br(),
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    ns("parm"), 
                    "Parameter selection", 
                    choices = abbreviate_vars(parms)
                  )
                  ),
                column(
                  width = 4,
                  selectInput(
                    ns("temp"), 
                    "Averaging period", 
                    choices = temp
                  )
                ),
                column(
                  width = 4,
                  uiOutput(NS(id, "control"))
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
              value = "engineering",
              uiOutput(ns("setup"))
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
    
    # `reactiveValues` to store image path and dimensions
    file <- reactiveValues(path = NULL, width = NULL, height  = NULL)
    observe({  
      # dynamic plot size
      file$width <- session$clientData[[paste0("output_",id ,"-eng_width")]]
      file$height <- session$clientData[[paste0("output_",id ,"-eng_height")]]
      message(file$height)
      message(file$width)
    })
    
    # selected parameter
    pm <- reactive({
      rlang::sym(paste(input$parm, temp[temp == "an"], sep = "_"))
      })
    
    ## all parameters
    pms <- reactive({
      paste(parms, temp[temp == "an"], sep = "_")
    })
    
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
    
    # enable disable tabs based on whether run model has been clicked or 
    # https://stackoverflow.com/questions/64324152/shiny-disable-tabpanel-using-shinyjs?noredirect=1&lq=1
    observe({
      # results
      shinyjs::disable(selector = '.nav-tabs a[data-value="tuning"')
      shinyjs::disable(selector = '.nav-tabs a[data-value="validation"')
    })
    
    observeEvent(input$run, {
      # results
      shinyjs::enable(selector = '.nav-tabs a[data-value="tuning"')
      shinyjs::enable(selector = '.nav-tabs a[data-value="validation"')
      # model setup
      shinyjs::disable("scale")
      shinyjs::disable("dims")
      shinyjs::disable("model")
    })
    
    observeEvent(input$reset, {
      # results
      shinyjs::disable(selector = '.nav-tabs a[data-value="tuning"')
      shinyjs::disable(selector = '.nav-tabs a[data-value="validation"')
      # model setup
      shinyjs::enable("scale")
      shinyjs::enable("dims")
      shinyjs::enable("model")
      # back to start
      updateTabsetPanel(session, "results", selected = "engineering")
      updateTabsetPanel(session, "specs", selected = "engineering")
    })
    
   
    # reset model settings
    observeEvent(input$reset, {
      updateSelectizeInput(
        session, 
        "scale",
        choices = c("log", "logit", "normalize"), 
        options = default_message(),
        selected = NULL 
      )
      updateSelectizeInput(
        session, 
        "dims", 
        choices = c("PCA", "PLS"), 
        options = default_message(),
        selected = NULL 
      )
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
      transferice_tuning(splt(), wfl())
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

      # initial split or fitted/tuned data
      if (input$specs == "engineering") {
        dat <- splt()
      } else if (input$specs == "tuning") {
        # waiter
        waiter <- waiter::Waiter$new()
        waiter$show()
        on.exit(waiter$hide())
        dat <- tun()
      } else if (input$specs == "validation") {
        dat <- final()
      }
      # fitted data requires a species name variable selection
      # tuned data requires a dimension variable selection
      if (isTruthy(input$dims))  req(input$comp) else req(input$peek)
      
      # create plot or animation (save in `reactiveValues`)
      file$path <- ggpartial(
        obj = dat,
        workflow = wfl(),
        pred = input$peek,
        tune = input$comp,
        out = !! pm(),
        type = if (isTruthy(input$toggle))  "spatial" else "regression",
        base_map = base(),
        height = file$height,
        width = file$width
      )
    })

    # cross plots
    output$eng <- renderImage({
      req(file$path)
      list(
        height = file$height, 
        width = if (isTruthy(input$toggle)) file$width else file$height, 
        src = file$path, 
        contentType = 'image/png'
      )
    },
    deleteFile = FALSE
    )
    # CV animations
    output$part  <- renderUI({
      req(file$path)
      tags$video(
        height = file$height, 
        width = if (isTruthy(input$toggle)) file$width else file$height, 
        type = "video/mkv", 
        src = file$path, 
        autoplay = TRUE,
        loop = TRUE
      )
    })
    # final model image
    output$final <- renderImage({
      req(file$path)
      list(
        height = file$height, 
        width = if (isTruthy(input$toggle)) file$width else file$height, 
        src = file$path, 
        contentType = 'image/png'
      )
    },
    deleteFile = FALSE
    )
    
    # render controller based on tuning results
    output$control <- renderUI({
      if (!isTruthy(input$dims)) {
        selectInput(
          NS(id, "peek"), 
          "Taxa selection",
          choices =  species_naming(dinodat, parms = parms, "an")
        )
      } else if (input$dims == "PCA") {
        sliderInput(NS(id, "comp"), "Principal components", 1, 9, 1, ticks = FALSE)
      }
    }) 
#-------------------------------------------------------------------------------    

    # side panel (sub) model metrics
    output$results <- renderText({
      if (input$specs == "engineering") {
        "Model setup"
      } else if (input$specs == "tuning") {
        "Model optimization"
      } else if (input$specs == "validation") {
        "Model performance"
      }
    })

    # show locations selection controls when data loaded
    observe({
      updateTabsetPanel(session, "results", selected = input$specs)
    })

    # collect sub-model metrics
    submet <- reactive({
      # fitted data requires a species name variable selection
      # tuned data requires a dimension variable selection
      if (isTruthy(input$dims))  {
        
        req(input$comp) 
    
        tune::collect_metrics(tun(), summarize = FALSE) |>
          dplyr::mutate(
            var = .data$num_comp,
            slct = dplyr::if_else(.data$num_comp == input$comp, TRUE, FALSE)
            ) |>
          dplyr::filter(.data$.estimate < 100) # remove extreme outliers
      } else {
        
        req(input$peek)
        tune::collect_metrics(tun(), summarize = FALSE) |>
          dplyr::mutate(var = 1, slct = 1) |> 
          dplyr::filter(.data$.estimate < 100) # remove extreme outliers
      }
    })
     
    # plot the submodel metrics as a boxplot
    output$submetrics <- renderPlot({

      ggplot2::ggplot(
        submet(),
        ggplot2::aes(
          x = .data$var,
          y = .data$.estimate,
          group = .data$var,
          fill = .data$slct
        )
      ) +
      ggplot2::geom_boxplot(show.legend = FALSE) +
      ggplot2::scale_x_discrete(
        "components", 
        labels = as.character(1:10), 
        breaks = 1:10
      ) +
      ggplot2::labs(y = "RMSRE")
    }
    ,
    width = 250,
    height = 250
    )

    # final model metric as table in sidepanel
    output$setup <- renderUI({
      # fitted data requires a species name variable selection
      # tuned data requires a dimension variable selection
      if (isTruthy(input$dims))  req(input$comp) else req(input$peek)
      print_model(
        obj = splt(),
        workflow = wfl(),
        pred = input$peek,
        tune = input$comp,
        out = !! pm()
      )
    })
    
    # final model metric as table in sidepanel
    output$finmetrics <- renderUI({
      # all collected metrics
      mts <- tune::collect_metrics(final())
      a <- print_metric(mts, "rsq")
      b <- print_metric(mts, "rmse")
      c <- print_metric(mts, "rmsre")
      # print with mathjax
      withMathJax(HTML(paste(a, b, c, sep = "<br/><br/>")))
    })
  })
}
