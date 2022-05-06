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
            id = ns("school"),
            tabPanel(
              "frequentist",
              actionLink(
                ns("modelhelper"), 
                "", 
                icon = icon('question-circle')
              ),
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
              actionLink(
                ns("scalehelper"), 
                "", 
                icon = icon('question-circle')
              ),
              selectizeInput(
                ns("scale"), 
                "Predictor transforming", 
                choices = c("log", "logit", "normalize"), 
                options = default_message()
              ),
              # dimension reduction 
              actionLink(
                ns("dimshelper"), 
                "", 
                icon = icon('question-circle')
              ),
              selectizeInput(
                ns("dims"), 
                "Dimension reduction", 
                choices = c("PCA", "PLS"), 
                options = default_message()
              ),
            ),
            footer = tagList(
              tags$br(), 
              fluidRow(
                column(
                  width = 6,
                  actionLink(
                    ns("resethelper"), 
                    "", 
                    icon = icon('question-circle')
                  ),
                  actionButton(ns("reset"), "Reset model")
                ),
                column(
                  width = 6, 
                  actionLink(
                    ns("trainhelper"), 
                    "", 
                    icon = icon('question-circle')
                  ),
                  actionButton(ns("run"), "Train model")
                )
              ),
              tags$br(),
              wellPanel(uiOutput(ns("helptext")))
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
              div(plotOutput(ns("eng")), style="text-align: center;")
            ),
            tabPanel(
              "training",
              div(uiOutput(ns("part")), style="text-align: center;")
            ),
            tabPanel(
              "validation",
              div(plotOutput(ns("final")), style="text-align: center;")
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
                  width = 8,
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
              shinyBS::bsCollapsePanel(title = "Explain more", uiOutput(ns("setup")))
            ),
            tabPanelBody(
              value = "training",
              shinyBS::bsCollapsePanel(title = "Explain more", uiOutput(ns("submetrics")))
            ),
            tabPanelBody(
              value = "validation",
              shinyBS::bsCollapsePanel(title = "Explain more", uiOutput(ns("finmetrics")))
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
      shinyjs::disable(selector = '.nav-tabs a[data-value="training"')
      shinyjs::disable(selector = '.nav-tabs a[data-value="validation"')
    })
    
    observeEvent(input$run, {
      # results
      shinyjs::enable(selector = '.nav-tabs a[data-value="training"')
      shinyjs::enable(selector = '.nav-tabs a[data-value="validation"')
      # model setup
      shinyjs::disable("scale")
      shinyjs::disable("dims")
      shinyjs::disable("model")
    })
    
    observeEvent(input$reset, {
      # results
      shinyjs::disable(selector = '.nav-tabs a[data-value="training"')
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
# Help text
#-------------------------------------------------------------------------------

    # sidebar helptext
    output$helptext <- renderUI({
      if (input$school == "frequentist") {
        helpText(paste0("Frequentist is the study of probability and draws ", 
                        "conclusions based on samples. It forms the basis for ", 
                        "hypothesis testing and confidence intervals."))
      } else if (input$school == "bayesian") {
        helpText(paste0("Bayesian statistics is an opposing philosophical ", 
                        "school. It's notion is that prior information can be ", 
                        "used to update beliefs and results in conditional ", 
                        "probability."))
      }
    })
      

    # help text modals
    observeEvent(input$scalehelper, {
      showModal(
        modalDialog(
          title = "Predictor transforming",
          paste0("Transforming of predictor variables can help clarify ", 
                 "relationships with the outcome. It also helps the data to ", 
                 "conform to model specifications such as linearity, ",
                 "normality and equal variance.")
        )
      )
    }) 
      
    observeEvent(input$dimshelper, {
      showModal(
        modalDialog(
          title = "Dimension reduction", 
          paste0("The datasets consist of many species. Hence a reduction of ", 
                 "dimension by principical component analysis prevents ", 
                 "problems like multicollinearity, where many predictors are ", 
                 "highly correlated.")
          )
        )
    }) 
    
    observeEvent(input$modelhelper, {
      showModal(
        modalDialog(
          title = "Model type", 
          paste0("Two mechanims for finding a model solution are implemented. ", 
                  "Traditional Ordinary Least Squares (OLS) regression and ", 
                 "Generalised Least Squares (GLS) regression. The latter deals", 
                 "with the lack of independence between observations due to ",
                 "spatial proximity of sampling sites.")
        )
      )
    }) 
    
    observeEvent(input$resethelper, {
      showModal(
        modalDialog(
          title = "Reset model", 
          "This resets all controls to the original values."
        )
      )
    })
    
    observeEvent(input$trainhelper, {
        showModal(
          modalDialog(
            title = "Train model", 
            paste0("The model is trained by 10-fold cross validation. This ",
                   "means that a subsample of the dataset is used to fit the ",
                   "model and a test set to validate the model fit. This ", 
                   "gives, an idea of how well the model will perform on ", 
                   "unkown datasets to make accurate predictions. ",
                   "It also provides the opportunity to tune unconstrained ",
                   "model parameters such as the principal components ",
                   "selected when performing dimension reduction before the ",
                   "model fit."),
            tags$br(),
            tags$br(),
            tags$div(
              tags$img(src= 'img/helper_crossvalidation.png'), 
              style="text-align: center;"
            ),
            size = "l"
          )
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
      } else if (input$specs == "training") {
        # waiter
        # waiter <- waiter::Waiter$new()
        # waiter$show()
        # on.exit(waiter$hide())
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
        sliderInput(
          NS(id, "comp"), 
          "Principal components", 1, 9, 1, 
          ticks = FALSE
        )
      } else if (input$specs == "validation") {
        NULL
      }
    }) 
    
#-------------------------------------------------------------------------------
# side panel    
#-------------------------------------------------------------------------------    

    # side panel (sub) model metrics
    output$results <- renderText({
      if (input$specs == "engineering") {
        "Model setup"
      } else if (input$specs == "training") {
        "Model optimization"
      } else if (input$specs == "validation") {
        "Model performance"
      }
    })

    # show locations selection controls when data loaded
    observe({
      updateTabsetPanel(session, "results", selected = input$specs)
    })

    # model features as text in side panel
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
    
    # plot the submodel metrics as a boxplot
    output$submetrics <- renderUI({
      # fitted data requires a species name variable selection
      # tuned data requires a dimension variable selection
      if (isTruthy(input$dims))  req(input$comp) else req(input$peek)
      print_model(
        obj = tun(), 
        workflow = wfl(), 
        pred = input$peek, 
        tune = input$comp, 
        out = !!pm(),
        width = 250,
        height = 250
      )
    })
    
    # final model metric as text in side panel
    output$finmetrics <- renderUI({print_model(final())})
  })
}
