explo_app <- function() {
  
  ui <- fluidPage(
  
      # Application title
      titlePanel("explore the sites"),
  
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
          sidebarPanel(
             selectInput("parm", "Parameter", choices = parms)
          ),
  
          # Show a plot of the selected sites dinos and variables
          mainPanel(

         
              column(12, plotOutput("data", width = "100%"))
            
          )
      )
  )
  
  # Define server logic required to draw a histogram
  server <- function(input, output) {
  
      output$wmap <- renderPlot({
    
      })
      output$data <- renderPlot({
        dim_reduction(as_tibble(complete_data), input$parm, dino)
      })
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}

