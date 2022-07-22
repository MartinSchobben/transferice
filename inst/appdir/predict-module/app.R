# save data
pt_pkg <- fs::path_package("transferice", "appdir", "cache")
nm <- "final_dinocyst_t_an_global_0mbsf_species_prop_log_center_pca_zerogeodist_gls_4"
saveRDS(transferice:::final_fit, fs::path(pt_pkg, nm, ext = "rds"))

ui <- fluidPage(
  predict_ui("predict")
)

server <- function(input, output, session) {
  
  thematic::thematic_shiny(bg = "transparent", fg = "black")
  predict_server("predict", model_id = reactive(nm))
}

shinyApp(ui, server)
  
