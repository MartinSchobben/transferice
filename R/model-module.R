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
          footer = actionButton(NS(id, "model"), "Run model")
        )
      ),
      
      # Show a plot of the model outcome and evaluation
      mainPanel(
        tabsetPanel(
          id = NS(id, "specs"),
          tabPanel(
            "tuning",
            selectInput(NS(id, "fold"), "CV folds", choices = sprintf("Fold%02d", 1:10)),
            selectInput(NS(id, "comp"), "Components", choices =  1:9),
            #textOutput(NS(id, "table"))
            plotOutput(NS(id, "part"))
          ),
          tabPanel("validation")
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
    # seed <- reactive(input$model, {
    #   seed <- sample(2 ^ 31 - 1, 1)
    #   set.seed(seed)
    # })
    # 
    dat <- reactive({
      # for now data is from a local source but later-on it should be sourced 
      # from the explo-module
      dinodat
    })
    
    tun <- eventReactive(input$model, {
  
      # train and development split
      dat_split <- rsample::initial_split(dat(), prop = 0.75) 
      # parameters
      pms <- paste0(abbreviate_vars(parms), "_an", collapse = "+")
      # formula
      fml <- as.formula(paste0(pms, "~."))
      # recipe
      tuned_recipe <- recipes::recipe(fml, data = dat()) |>
        recipes::step_logit(recipes::all_predictors(), offset = 0.025) |>
        recipes::step_pca(recipes::all_predictors(), num_comp = tune::tune(), 
                 options = list(center = TRUE))
      # model
      model <- parsnip::linear_reg() |>
        parsnip::set_engine('lm') |>
        parsnip::set_mode('regression')

      # workflow
      wfl <- workflows::workflow() |>
        workflows::add_recipe(tuned_recipe) |>
        workflows::add_model(model)

      # cross validation resampling
      dat_cv <- rsample::vfold_cv(rsample::training(dat_split), v = 10)

      # setting model tuning parameters
      dls <- wfl %>%
        dials::parameters() %>%
        update(num_comp = dials::num_comp(c(1, 9))) # set PCA steps to 9 components total
      # message(glue::glue("{hardhat::extract_parameter_dials(dls, 'num_comp')}"))
      tune_grid <- dials::grid_regular(dls, levels = 9)
      # tuning
      tune::tune_grid(
        wfl,
        resamples = dat_cv,
        grid = tune_grid,
        metrics = yardstick::metric_set(transferice::rmsre),
        control = tune::control_resamples(extract = function(x) tune::extract_fit_parsnip(x))
      )
    })

    output$part <- renderPlot({
      tuned_partials(tun(), input$fold, num_comp, input$comp) |> 
        ggpartial(PC1, t_an)
    })
 
  })
}


