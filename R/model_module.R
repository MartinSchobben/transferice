model_app <- function() {
  
  # names
  params_lg <- grepl("_an$", colnames(complete_data))
  meta <- c("station","depth","weight","lycopodium","lyc_tab", "geometry")
  meta_lg <- colnames(complete_data) %in% meta
  params <- colnames(complete_data)[params_lg]
  species <- colnames(complete_data) [!params_lg & !meta_lg ]
  
  ui <- fluidPage(
    
    # Application title
    titlePanel("explor the dino's"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        selectInput("param", "Parameter", choices = params),
        selectInput("transform", "Transform", choices = "raw")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("plot")
      )
    )
  )
  
  # Define server logic required to draw a histogram
  server <- function(input, output) {
    
    output$plot <- renderPlot({

      
      ggplot2::ggplot(
        long_data, 
        ggplot2::aes(
          x = .data$count,  
          y = .data[[input$param]]
        )
      ) +
        ggplot2::geom_point() +
        ggplot2::geom_point(
          data = long_data[which(long_data$species == input$species), , drop = FALSE],
          mapping = ggplot2::aes(     
            x = .data$count,  
            y = .data[[input$param]]
          ),
          color= "red"
        )
      
    })
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}

