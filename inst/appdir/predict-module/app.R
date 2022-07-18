ui <- fluidPage(
  predict_ui("predict")
)

server <- function(input, output, session) {
  
  thematic::thematic_shiny(bg = "transparent", fg = "black")
  predict_server("predict", model_id = reactive("validation_dinocyst_t_an_global_0mbsf_species_prop_log_center_pca_lm_1"))
}

shinyApp(ui, server)
  
