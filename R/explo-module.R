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
          selectInput(NS(id, "area"), "Region", choices = area),
          selectInput(NS(id, "temp"), "Averaging", choices = temp),
          selectInput(NS(id, "group"), "Taxa", choices = "dinocyst")
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

    # location metadata
    locs <- reactive({
      # connection and query
      locs <- DBI::dbGetQuery(
        pool, 
        "SELECT l.hole_id, site, longitude, latitude, sample_id 
          FROM neptune_hole_summary l 
            LEFT JOIN neptune_sample s ON l.hole_id = s.hole_id"
        )
      # remove redundant information (site name)
      locs <- dplyr::select(locs, -site)
      # filter latitudes for area selection
      lat_ft <- rep(TRUE, nrow(locs))
      if (input$area == "3031") lat_ft <- locs$latitude <= 0
      if (input$area == "3995") lat_ft <- locs$latitude >= 0
      locs[lat_ft, , drop = FALSE]
    })
    
    # get all environmental data from NOAA for the specific averaging period
    environ_dat <- reactive({
      purrr::map(
        parms, 
        ~oceanexplorer::get_NOAA(.x, 1, names(temp)[temp == input$temp])
        ) |> 
        purrr::set_names(abbreviate_vars(parms))
    })
    
    # get unique locations as longitude/latitude dataframe
    coords <- reactive({
      dplyr::distinct(locs(), .data$longitude, .data$latitude)
    })
    
    output$wmap <- renderPlot({
      # coordinates of sample locations
      pts <- sf::st_as_sf(coords(), coords =  c("longitude", "latitude"), crs = 4326)
      # select NOAA parameter of interest for plotting
      NOAA <- environ_dat()[[input$parm]]
      # add sample locations
      oceanexplorer::plot_NOAA(
        NOAA = NOAA, 
        depth = 30,
        points = pts, 
        epsg = input$area
      ) 
    })

    # get environmental variable from sample locations
    environ_pts <- reactive({
      # cast location as list before extraction
      pts <- purrr::set_names(as.list(coords()), nm = c("lon", "lat"))
      # extract and cast in data.frame format
      NOAA <- extract_NOAA(environ_dat(), pts)
      # bind with location metadata
      dplyr::left_join(locs(), NOAA, by = c("longitude", "latitude"))
    })

    # proportional taxon data
    taxon_prop <- reactive({
      # calculate proportions of taxa dataset (and turn to wide format)
      taxon_prop <- calc_taxon_prop(pool)
      # filter to available samples (linked to region selection above)
      dplyr::filter(taxon_prop, .data$sample_id %in% locs()$sample_id)
    })

    # plot taxon
    output$spc <- renderPlot({
      # needs to be rendered first
      req(input$taxa) 
      # then plot
      taxon_plot(taxon_prop(), input$taxa, "sample_id")
    },
    width = 250,
    height = 250
    )

    # reduce dimensions
    output$pcr <- renderPlot({
      
      # selected variable
      selected_var <- paste(
        input$parm, # variable
        input$temp, # temporal averaging
        sep = "_"
      )
      
      # re-scale (logit from recipes)
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
      
      # require
      req(taxon_prop(), cancelOutput = TRUE)
      
      # filter zero columns
      where <- tidyselect::vars_select_helpers$where
      tx <- dplyr::select(
        taxon_prop(), 
        where(~sum(.x) > 0),
        -c(.data$sample_id, .data$hole_id)
      ) 

      selectInput(
        NS(id, "taxa"), 
        "Taxa selection",
        choices =  colnames(tx)
      )
    })
    
    # combine taxon and environmental data, and export (later this should 
    # become a reactive for export)
    observe({
      
      # combine
      out <- dplyr::right_join(
        environ_pts(), 
        taxon_prop(), 
        by = c("sample_id", "hole_id")
      )

      # unique name
      id <- paste(
        input$group, 
        names(temp)[temp == input$temp], 
        names(area)[area == input$area], 
        sep = "_"
      )
      nm <- file_namer("rds", "raw", id, trans = "prop")
      
      # export
      pt_pkg <- fs::path_package("transferice", "appdir", "cache")
      saveRDS(out, fs::path(pt_pkg, nm, ext = "rds"))
    })
  })
}


# averaging
temp <- c("annual", month.name, "winter", "spring", "summer", "autumn")
temp <- abbreviate(temp, 2)

# geographic window
area <- c("global" = "original", "southern" = "3031", "northern" = "3995")