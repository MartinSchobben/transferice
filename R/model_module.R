model_app <- function() {
  

  ui <- fluidPage(
    
    # Application title
    titlePanel("explor the dino's"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        selectInput("param", "Parameter", choices = params)
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

      
    })
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}

