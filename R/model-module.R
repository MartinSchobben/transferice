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
                choices = c("linear model (OLS)" = "ols",  "linear model (GLS)" = "gls")
              ),
            ),
            tabPanel("bayesian"),
            header = tagList(
              tags$br(),   
              # taxonomy
              actionLink(
                ns("taxohelper"), 
                "", 
                icon = icon('question-circle')
              ),
              selectizeInput(
                ns("taxo"), 
                "Taxomomic depth", 
                choices = c("subspecies", "species", "genera"), 
                options = default_message()
              ),
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
              uiOutput(ns("tune"))
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
              div(plotOutput(ns("eng")), style = "text-align: center;")
            ),
            tabPanel(
              "training",
              div(uiOutput(ns("part")), style = "text-align: center;")
            ),
            tabPanel(
              "validation",
              div(plotOutput(ns("final")), style = "text-align: center;")
            ),
            header = tagList(
              tags$br(),
              fluidRow(
                column(
                  width = 8,
                  uiOutput(NS(id, "control"))
                )
              )
            ),
            footer = 
              # shinyWidgets::materialSwitch(
              #   ns("toggle"), 
              #   "project on map"
              # )
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
              shinyBS::bsCollapsePanel(title = h5("Explain more"), info_eng()),
              uiOutput(ns("setup"))
            ),
            tabPanelBody(
              value = "training",
              shinyBS::bsCollapsePanel(title = h5("Explain more"), info_train()),
              uiOutput(ns("submetrics"))
            ),
            tabPanelBody(
              value = "validation",
              shinyBS::bsCollapsePanel(title = h5("Explain more"), ""),
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
model_server <- function(id, data_id = "dinocyst_t_an_global") { # data id is based on query use R6 in future
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
      pm <- strsplit(data_id, "_")[[1]][2]
      av <- strsplit(data_id, "_")[[1]][3]
      paste(pm, av, sep = "_")
    })
    
    # base map (for kriging interpolation)
    base <- reactive({
      browser()
      # parameter
      pm <- parms[abbreviate_vars(parms) == strsplit(data_id, "_")[[1]][2]]
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
      shinyjs::disable("taxo")
      shinyjs::disable("scale")
      shinyjs::disable("dims")
      shinyjs::disable("model")
    })
    
    observeEvent(input$reset, {
      # results
      shinyjs::disable(selector = '.nav-tabs a[data-value="training"')
      shinyjs::disable(selector = '.nav-tabs a[data-value="validation"')
      # model setup
      shinyjs::enable("taxo")
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
    
    # dynamic controllers
    output$tune <- renderUI({
      
      if (isTruthy(input$dims)) {
        sliderInput(
          NS(id, "ncomp"), 
          "Number of components", 
          min = 1, 
          max = 4, 
          value = 1, 
          ticks = FALSE
        )
      }
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
                 "spatial proximity of sampling sites.", 
                 "Tobler's First Law of Geography: \"everything is related to", 
                 "everything else, but near things are more related than ",
                 "distant things.\"")
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
              tags$img(src = 'img/helper_crossvalidation.png'), 
              style ="text-align: center;"
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
      nm <- file_namer("rds", "raw", data_id, "count", "prop")
      readRDS(fs::path_package("transferice", "appdir", "cache", nm, ext = "rds")) |> 
        dplyr::slice_sample(n = 600)
    })
    
    # re-sample
    splt <- reactive({
      set.seed(1)
      splt <- rsample::initial_split(dat(), prop = 0.75) 
    })
    
    # recipe
    rcp <- reactive({
      transferice_recipe(
        dat(), 
        outcome = pm(), 
        trans = input$scale, 
        dim_reduction = input$dims,
        model = input$model
      )
    }) 
    
    # model
    mdl <- reactive({
    
      # linear regression
      mdl <- parsnip::linear_reg() |>
        parsnip::set_mode('regression')
      
      # engine
      if (input$model == "ols") {
        
        mdl <- parsnip::set_engine(mdl, 'lm') 
        
      } else if (input$model == "gls") {
    
        mdl <- parsnip::set_engine(
          mdl,
          "gls",  
          control = nlme::lmeControl(opt = 'optim'),
          correlation = nlme::corSpatial(form = ~longitude + latitude, type = "g" )
        )
        
      } else if (input$model == "nlme") {
        

      }
      mdl
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
      
      # name for caching
      wfl_nm <- sanitize_workflow(wfl()) # workflow name
      nm <- file_namer("rds", "training", data_id, trans = wfl_nm)
      
      # tuning
      set.seed(2)
      transferice_tuning(splt(), wfl()) |> 
        app_caching("rds", nm) # caching
    })
    
    # finalize model (with or without tuning)
    final <- eventReactive(input$run, {
      transferice_finalize(splt(), wfl(), input$comp)
    })

#-------------------------------------------------------------------------------
# generate figures
#-------------------------------------------------------------------------------

    # feature engineering, training and validation viz
    file_info <- reactive({

      # initial split or fitted/tuned data
      if (input$specs == "engineering") {
        dat <- splt()
        type <- "png" # output type of file
      } else if (input$specs == "training") {
        dat <- tun()
        type <- "mkv" # output type of file
      } else if (input$specs == "validation") {
        dat <- final()
        type <- "png" # output type of file
      }
      
      # fitted data requires a species name variable selection
      if (isTruthy(input$dims))  {
        req(input$comp) 
        x <- input$comp
      # tuned data requires a dimension variable selection
      } else {
        req(input$peek)
        x <- input$peek
      }
      
      # file metadata
      if (isTruthy(input$toggle))  viz <- "spatial" else viz <- "xy"
      wfl_label <- sanitize_workflow(wfl())
      file_name <- file_namer(type, input$specs, data_id, trans = wfl_label, 
                              viz = viz, x = x)
      height <- file$height
      if (isTruthy(input$toggle)) width <- file$width else width <- file$height
      
      # output
      list(dat = dat, viz = viz, type = type, file_name = file_name, width = width, height = height) 
    })
    
    # create plot or animation (save in `reactiveValues`)
    observe({

      file$path <- ggpartial(
        obj = file_info()$dat,
        workflow = wfl(),
        pred = input$peek,
        tune = input$comp,
        out = pm(),
        type = file_info()$viz,
        base_map = NULL, #base(),
        id = data_id
      ) |>
        # caching of file
        app_caching(file_info()$type, file_info()$file_name, file_info()$width,
                    file_info()$height)

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
    
    observe(message(glue::glue("{file_info()$file_name}")))
    # CV animations
    output$part  <- renderUI({
      if (fs::path_ext_remove(basename(req(file$path))) == file_info()$file_name) {
        tags$video(
          height = file$height, 
          width = if (isTruthy(input$toggle)) file$width else file$height, 
          type = "video/mkv", 
          src = file$path, 
          autoplay = TRUE,
          loop = TRUE
        )
      }
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
      
      # species names
      spec_nms <- species_naming(wfl()) 
      spec_nms <- rlang::set_names(
        paste0("taxa_", seq_along(spec_nms)), 
        spec_nms
      )
      
      if (!isTruthy(input$dims) && input$specs != "validation") {
        selectInput(
          NS(id, "peek"), 
          "Taxa selection",
          choices = spec_nms
        )
      } else if (input$dims == "PCA") {
        selectInput(
          NS(id, "comp"), 
          "Principal component", 
           choices = 1:4
        )
      } else {
        selectInput(
          NS(id, "diag"), 
          "Diagnostic",
          choices = c("R-squared")
        )
      }
    }) 
    
#-------------------------------------------------------------------------------
# side panel    
#-------------------------------------------------------------------------------    

    # # side panel (sub) model metrics
    # output$results <- renderText({
    #   if (input$specs == "engineering") {
    #     "Model setup"
    #   } else if (input$specs == "training") {
    #     "Model optimization"
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
    # # model features as text in side panel
    # output$setup <- renderUI({
    #   # fitted data requires a species name variable selection
    #   # tuned data requires a dimension variable selection
    #   if (isTruthy(input$dims))  req(input$comp) else req(input$peek)
    #   print_model(
    #     obj = splt(),
    #     workflow = wfl(),
    #     pred = input$peek,
    #     tune = input$comp,
    #     out = pm()
    #   )
    # })
    # 
    # # plot the submodel metrics as a boxplot
    # output$submetrics <- renderUI({
    #   # fitted data requires a species name variable selection
    #   # tuned data requires a dimension variable selection
    #   if (isTruthy(input$dims))  req(input$comp) else req(input$peek)
    #   print_model(
    #     obj = tun(), 
    #     workflow = wfl(), 
    #     pred = input$peek, 
    #     tune = input$comp, 
    #     out = pm(),
    #     width = 250,
    #     height = 250
    #   )
    # })
    # 
    # # final model metric as text in side panel
    # output$finmetrics <- renderUI({print_model(final())})
  })
}

# detailed information for engineering section
info_eng <- function(...) {
  wellPanel(
    withMathJax(
      HTML(
        paste0("This section deals with preparing the data before model ", 
               "training. It mainly involves variance stabilization of ", 
               "predictors and the reduction of dimensionality. All models ", 
               "are based on a multivariate multiple linear regression, based ", 
               "on a matrix of n-predictors ($X$) and a matrix of m-outcomes ", 
               "($Y$).")
      )
    )
  )
}

# detailed information for training section
info_train <- function(...) {
  wellPanel(
    withMathJax(
      HTML(
        paste0("Training of a model has two purposes. <br/><br/>", 
               "1) It measures the generalization capacity of the model.",
               "In the animation on the left we would ideally want to see ",
               "a minimal amount of wobbling in the regression line. <br/>",
               "2) It helps select the optimal number of dimensions after ",
               "dimension reduction by principal component analyses. <br/>",
               "<br/>A good model has a low Root Mean Square Relative ",
               "Error.")
      )
    )
  )
}