#' Shiny exploration module
#' 
#'
#' @param id Namespace id.
#'
#' @return Shiny module.
#' @export
explo_ui <- function(id) {
  
  # namespace
  ns <- NS(id)
  
  tagList(
    
    # waiter
    waiter::use_waiter(),
    
    # Application title
    titlePanel("Data Selection"),
    
    # Sidebar with global selection
    fluidRow(
      column(
        width = 3,
        wellPanel(
          tabsetPanel(
            id = ns("select"),
            tabPanel(
              "biota",
              br(),
              actionLink(
                ns("grouphelper"), 
                "", 
                icon = icon('question-circle')
              ),
              selectInput(
                ns("group"), 
                "Taxonomic group", 
                choices = "dinocyst"
              )
            ),
            tabPanel(
              "environment",
              br(),
              actionLink(
                ns("areahelper"), 
                "", 
                icon = icon('question-circle')
              ),
              selectInput(
                ns("area"), 
                "Region", 
                choices = area
              ),
              actionLink(
                ns("parmhelper"), 
                "", 
                icon = icon('question-circle')
              ),
              selectInput(
                ns("parm"), 
                "Parameter", 
                choices = abbreviate_vars(parms)
              ),
              actionLink(
                ns("depthhelper"), 
                "", 
                icon = icon('question-circle')
              ),
              sliderInput(
                ns("depth"), 
                "Waterdepth", 
                min = 0, 
                max = 3500, 
                value = 0
              ),
              actionLink(
                ns("temphelper"), 
                "", 
                icon = icon('question-circle')
              ),
              selectInput(
                ns("temp"), 
                "Averaging", 
                choices = temp
              )
            ),
            footer = 
              tagList(
                tags$br(), 
                fluidRow(
                  column(
                    width = 6,
                    actionLink(
                      ns("loadhelper"), 
                      "", 
                      icon = icon('question-circle')
                    ),
                    actionButton(ns("load"), "Load data")
                  ),
                  column(
                    width = 6, 
                    actionLink(
                      ns("savehelper"), 
                      "", 
                      icon = icon('question-circle')
                    ),
                    actionButton(ns("save"), "Save data")
                  )
                ),
                tags$br(),
                wellPanel(uiOutput(ns("helptext")))
              )
          ),
          style = "height:100%;"
        )
      ),
      
      # show a plot of the selected sites 
      column(
        width = 6, 
        tabsetPanel(
          id = ns("explore"),
          tabPanel("worldmap", plotOutput(ns("wmap"))),
          tabPanel(
            "comparison", 
            div(plotOutput(ns("pcr")), style = "text-align: center;"),
            fluidRow(
              column(
                width = 4,
                selectInput(ns("x"), "x-axis", choices = paste0("PC", 1:4))
              ),
              column(
                width = 4,
                selectInput(ns("y"), "y-axis", choices = paste0("PC", 1:4))
              )
            )
          )
        ),
      ),
      # taxon viewer
      column(
        width = 3,
        wellPanel(
          tabsetPanel(
            id = ns("results"),
            header = tagList(tags$br(), uiOutput(ns("control")), tags$br()),
            tabPanel(
              "abundance",
              div(plotOutput(ns("spc")), style = "text-align: center;")
            ),
            tabPanel(
              "appearance",
              imageOutput(ns("exm"))
            )
          ),
          style = "height:100%;"
        )
      )
      
    )
  )
}
#' @rdname explo_ui 
#' 
#' @export
explo_server <- function(id) {

  moduleServer(id, function(input, output, session) {
    
    # sidebar helptext
    output$helptext <- renderUI({
      if (input$select == "biota") {
        tagList(    
          tags$br(),
          tags$div(
            tags$img(
              src = fs::path("img", "neptune-logo", ext = "png"),
              height="50%", 
              width="50%"
            ), 
            style="text-align: center;"
          ),
          tags$br(),
          helpText(
            paste0("Select the taxomic group to construct a transfer function.")
          )
        )
      } else if (input$select == "environment") {
        tagList(    
          tags$br(), 
          tags$div(
            tags$img(
              src = fs::path("img", "NOAA-logo", ext = "png"),
              height="50%", 
              width="50%"
            ), 
            style="text-align: center;"
          ),
          tags$br(),
          helpText(
            paste0("Select the environmental parameters to train the transfer function.")
          )
        )
      }
    })
    
    # `reactiveValues` to store image path and dimensions
    file <- reactiveValues(path = NULL, side_path = NULL, width = NULL, 
                           height = NULL, side_width = NULL, side_height = NULL)
    observe({ 
      # dynamic plot size
      file$width <- session$clientData[[paste0("output_", id ,"-wmap_width")]]
      file$height <- session$clientData[[paste0("output_", id ,"-wmap_height")]]
      file$side_width <- session$clientData[[paste0("output_", id ,"-spc_width")]]
      file$side_height <- session$clientData[[paste0("output_", id ,"-spc_height")]]
    })

    # location metadata
    dat <- eventReactive(input$load, {
      # connection and query
      taxon_prop <- calc_taxon_prop(pool, "train")

      # filter latitudes for area selection
      lat_ft <- rep(TRUE, nrow(taxon_prop))
      if (input$area == "3031") lat_ft <- taxon_prop$latitude <= 0
      if (input$area == "3995") lat_ft <- taxon_prop$latitude >= 0
      taxon_prop[lat_ft, , drop = FALSE]
    })
    
    # common id
    tag <- reactive({ 
      paste(
        input$group,
        input$parm,
        input$temp, 
        names(area)[area == input$area], 
        paste0(input$depth, "mbsf"),
        sep = "_"
      )
    })
    
    # selected variable
    pm <- reactive({
      paste(
        input$parm, # variable
        input$temp, # temporal averaging
        sep = "_"
      )
    })
    
    # get all environmental data from NOAA for the specific averaging period
    environ_dat <- eventReactive(input$load, {
      
      # waiter (this can take a bit longer)
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())
      # parameter
      pm <- parms[abbreviate_vars(parms) == input$parm]
      # averaging period
      av <- names(temp)[temp == input$temp]
      # get the oceanographic data
      oceanexplorer::get_NOAA(pm, 1, av) 
    })
    
    # get unique locations as longitude/latitude dataframe
    coords <- eventReactive(input$load, {
      dplyr::select(dat(), .data$longitude, .data$latitude)
    })
    
    # file metadata
    file_info <- reactive({
      
      # taxa names
      tx <- dplyr::select(dat(), -dplyr::any_of(meta)) |> 
        dplyr::select(where(~sum(.x) > 0)) |> 
        colnames()
      
        # file name for sidebar
      if (isTruthy(input$taxa)) {
        side_file_name <- file_namer("png", "raw", tag(), viz = "barplot", 
                                     x = paste0("taxa_", which(tx %in% input$taxa)))
      } else {
        side_file_name <- NULL
      }
      
      if (input$explore == "worldmap") {
        viz <- "spatial"
        if (isTruthy(input$taxa)) {
          # main figure x label
          x <- paste0("taxa_", which(tx %in% input$taxa))
        } else {
          x  <- "all"
        }
      } else {      
        if (input$x == input$y) {
          viz <- "histogram" 
          x <- input$x  
        } else {
          viz  = "xy"
          x <- paste(input$x, input$y, sep = "_") 
        }
      }
      
      # file name
      file_name <- file_namer("png", "raw", tag(), viz = viz, x = x)

      # return
      list(file_name = file_name, side_file_name = side_file_name)
    })
    
    observe({
      
      req(input$explore == "worldmap", file_info())
      
      if(!isTruthy(input$taxa)) {
        # coordinates of sample locations
        pts <- sf::st_as_sf(coords(), coords =  c("longitude", "latitude"), 
                            crs = 4326)
      } else {
        # highlighted coordinates
        pts <- sf::st_as_sf(taxon_loc(), coords =  c("longitude", "latitude"), 
                            crs = 4326)
      }

      # filter beforehand for better contrasts
      NOAA <- oceanexplorer::filter_NOAA(environ_dat(), input$depth)
      # add sample locations
      file$path <- oceanexplorer::plot_NOAA(
        NOAA = NOAA, 
        depth = NULL,
        points = pts, 
        epsg = input$area
      ) |> 
        app_caching("png", file_info()$file_name, file$width, file$height)
      
    }) |> 
      bindEvent(input$load, environ_dat(), file_info(), ignoreInit = TRUE)
    
    # get environmental variable from sample locations
    train <- reactive({
      # cast location as list before extraction
      pts <- purrr::set_names(as.list(coords()), nm = c("lon", "lat"))
      # extract and cast in data.frame format
      NOAA <- oceanexplorer::filter_NOAA(environ_dat(), input$depth, pts) |> 
        dplyr::select(-.data$depth) |> 
        # drop geometry as we will use it for sub-setting only
        sf::st_drop_geometry() 

      # bind with location metadata
      dplyr::bind_cols(dat(), NOAA) |> 
        tidyr::drop_na(pm())  
    })

    # the odds of finding a taxon at a site
    taxon_loc <- reactive({
     
      # needs to be rendered first
      req(input$taxa) 
      # filter the dataset
      dplyr::distinct(train(), .data$sample_id, .keep_all = TRUE) |> 
        dplyr::mutate(
          # odds  
          odds = .data[[input$taxa]] / (1 - .data[[input$taxa]]),
          # find highest n ranks
          rank = rank(.data$odds)
        ) |> 
        dplyr::filter(.data$rank >= dplyr::n() - 20) 
    })
    
    # plot taxon
    observe({

      req(taxon_loc()) 
      file$side_path <- ggtaxon(taxon_loc(), "odds", "site_hole") |> 
        app_caching("png", file_info()$side_file_name, file$side_width, 
                    file$side_width)
    })

    # reduce dimensions
    observe({
      
      req(input$explore == "comparison", file_info())

      # roles of all variables 
      vars <- role_organizer(train(), pm())
      
      # taxa
      txa <- vars[names(vars) == "predictor"] |> unname()
      
      # variables not used in transform
      terms <- vars[names(vars) != "predictor"] |> unname()

      # re-scale (logit from recipes)
      taxon_prep <- recipes::recipe(x = train(), vars = vars, roles = names(vars)) |>
        recipes::step_logit(dplyr::any_of(txa) , offset = 0.025) |>
        recipes::prep() |>
        recipes::bake(new_data = NULL)

      # perform a pcr
      file$path <- ggcompare(
        taxon_prep,
        var = pm(),
        id = terms,
        component_x = input$x,
        component_y = input$y
      ) |> 
        app_caching("png", file_info()$file_name, file$height, file$height)
    }) |> 
      bindEvent(input$explore, input$x, input$y, train())
    
    # plot images
    output$pcr <- output$wmap <- renderImage({
      
      if (file_checker(req(file_info()$file_name), req(file$path))) {
        list(
          height = file$height, 
          width = if (input$explore == "worldmap") file$width else file$height, 
          src = file$path, 
          contentType = 'image/png'
        )
      }
    },
    deleteFile = FALSE
    ) |> 
      bindEvent(file$path)
    
    # plot sidebar bar plot
    output$spc <- renderImage({
      
      # only when taxa is selected
      req(input$taxa)
      
      if (file_checker(req(file_info()$side_file_name), req(file$side_path))) {
        list(
          height = file$side_width, 
          width = file$side_width, 
          src = file$side_path, 
          contentType = 'image/png'
        )
      }
    },
    deleteFile = FALSE
    ) |> 
      bindEvent(file$side_path)
    
    # render controller based on tuning results
    output$control <- renderUI({

      # require
      req(dat())
      
      # filter zero columns
      where <- tidyselect::vars_select_helpers$where
      tx <- dplyr::select(dat(), -dplyr::any_of(meta)) |> 
        dplyr::select(where(~sum(.x) > 0))
      
      selectizeInput(
        NS(id, "taxa"), 
        "Taxa abundance",
        choices =  colnames(tx),
        options = default_message()
      )
    })
    
    # data label
    nm <- eventReactive(input$save, {
      file_namer("rds", "raw", tag())
    })
    
    # save data
    observeEvent(input$save, {
      app_caching(train(), "rds", nm())
    })

    # return
    nm
  })
}


# averaging
temp <- c("annual", month.name, "winter", "spring", "summer", "autumn")
temp <- abbreviate(temp, 2)

# geographic window
area <- c("global" = "original", "southern" = "3031", "northern" = "3995")