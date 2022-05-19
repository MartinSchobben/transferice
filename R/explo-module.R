#' Shiny exploration module
#' 
#'
#' @param id Namespace id.
#'
#' @return
#' @export
explo_ui <- function(id) {
  
  tagList(
    # Application title
    titlePanel("Modern species distribution"),
    
    # Sidebar with global selection
    fluidRow(
      column(
        width = 3,
        wellPanel(        
          h3("Site selection"),
          selectInput(NS(id, "area"), "Region", choices = geo),
          selectInput(NS(id, "temp"), "Averaging", choices = temp[1])
        )
      ),
      
      # show a plot of the selected sites 
      column(
        width = 6, 
        selectInput(
          NS(id, "parm"), 
          "Parameter selection",
          choices = abbreviate_vars(parms)
        ),
        tabsetPanel(
          id ="explore",
          tabPanel("worldmap", plotOutput(NS(id, "wmap"))),
          tabPanel(
            "site comparison", 
            div(plotOutput(NS(id, "pcr")), style = "text-align: center;"),
            fluidRow(
              column(
                width = 4,
                selectInput(NS(id, "x"), "x-axis", choices = paste0("PC", 1:10))
              ),
              column(
                width = 4,
                selectInput(NS(id, "y"), "y-axis", choices = paste0("PC", 1:10))
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
            id = NS(id, "results"),
            header = tagList(tags$br(), uiOutput(NS(id, "control")), tags$br()),
            tabPanel(
              "abundance",
              div(plotOutput(NS(id, "spc")), style = "text-align: center;")
              ),
            tabPanel(
              "appearance",
              imageOutput(NS(id, "exm"))
            )
          )
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

    # area defined extent of latitude
    limit <- reactive({
      if (input$area != "original") 0
    })
    
    locs <- reactive({
      # connection and query
      locs <- DBI::dbGetQuery(
        pool, 
        "SELECT l.hole_id, site, longitude, latitude, sample_id 
          FROM neptune_hole_summary l 
            LEFT JOIN neptune_sample s ON l.hole_id = s.hole_id"
        )
      # filter latitudes for area selection
      lat_ft <- rep(TRUE, nrow(locs))
      if (input$area == "3031") lat_ft <- locs$latitude <= -limit()
      if (input$area == "3995") lat_ft <- locs$latitude >=  limit()
      locs[lat_ft, , drop = FALSE]
    })
    
    environ_dat <- reactive({
      oceanexplorer::get_NOAA(
        parms[abbreviate_vars(parms) == input$parm], 
        1, 
        "annual"
      ) 
    })
    
    coords <- reactive({
      locs()[, !names(locs()) %in%  c("hole_id", "site", "sample_id"), drop = FALSE]
    })
    
    output$wmap <- renderPlot({
      # coordinates of sample locations
      pts <- sf::st_as_sf(coords(), coords =  c("longitude", "latitude"), crs = 4326)
      # add sample locations
      oceanexplorer::plot_NOAA(
        NOAA = environ_dat(), 
        depth = 30,
        points = pts, 
        epsg = input$area
      ) + 
        transferice_theme() 
    })
  
    # get environmental variable from sample location
    environ_pts <- reactive({
      # cast location as list before extraction
      pts <- setNames(as.list(coords()), nm = c("lon", "lat"))
      pts <- oceanexplorer::filter_NOAA(environ_dat(), depth = 30, coord = pts)
      # drop geometry as we will use it for ...
      parm <- sf::st_drop_geometry(pts)
      dplyr::bind_cols(locs(), parm)
    })

    # proportional taxon data
    taxon_prop <- reactive({
      taxon_prop <- calc_taxon_prop(
        "neptune_sample_taxa", 
        "neptune_sample", 
        pool
        )
      # filter to available stations (linked to region selection)
      dplyr::filter(taxon_prop, .data$hole_id %in% locs()$hole_id)
    })

    # plot taxon
    output$spc <- renderPlot({
      
      req(input$taxa) # needs to be rendered first
      
      taxon_plot(
        taxon_prop(),
        input$taxa,
        "sample_id"
      )
      },
      width = 250,
      height = 250
    )

    # reduce dimensions
    output$pcr <- renderPlot({
      
      # selected variable
      selected_var <- paste(
        input$parm, # variable
        temp[temp == input$temp], # temporal averaging
        sep = "_"
      )
      
      # rescale (logit from recipes)
      taxon_prep <- recipes::recipe(taxon_prop()) |>
        recipes::step_logit(-c(sample_id, hole_id), offset = 0.025) |>
        recipes::prep() |>
        recipes::bake(new_data = NULL)
      
      # perform a pcr
      reduce_dims(
        taxon_prep,
        environ_pts(),
        var = selected_var,
        id = c("sample_id", "hole_id"),
        loc = "hole_id",
        component_x = input$x,
        component_y = input$y
      )
    },
    width = 400,
    height = 400
    )
    
    # render controller based on tuning results
    output$control <- renderUI({
        selectInput(
          NS(id, "taxa"), 
          "Taxa selection",
          choices =  species_naming(pool, parms, dat = taxon_prop())
        )
    }) 
  })
}


# averaging
temp <- c("annual", month.name, "winter", "spring", "summer", "autumn")
temp <- abbreviate(temp, 2)

# geographic window
geo <- c("global" = "original", "southern" = "3031", "northern" = "3995")